restore_root <- if (dir.exists("R/session_save_restore")) "." else "../.."
restore_env <- new.env(parent = globalenv())
restore_env$`%||%` <- function(x, y) if (is.null(x)) y else x
restore_env$debug_log <- function(...) invisible(NULL)
sys.source(file.path(restore_root, "R/session_save_restore/session_save_restore_core_helpers.R"),
           envir = restore_env)
sys.source(file.path(restore_root, "R/session_save_restore/session_save_restore_callbacks.R"),
           envir = restore_env)
sys.source(file.path(restore_root, "R/session_save_restore/session_save_restore_module_registration.R"),
           envir = restore_env)
sys.source(file.path(restore_root, "modules/dot/dotplot_utils.R"), envir = restore_env)

# A deliberately small integration driver.  It uses the production callback and
# job-registry boundaries, while testServer supplies real Shiny flush semantics.
restore_driver <- function(input, output, session, generation = 1L) {
  active_generation <- shiny::reactiveVal(generation)
  trigger <- shiny::reactiveVal(0L)
  callbacks <- list()
  reports <- list()
  counters <- new.env(parent = emptyenv())
  for (name in c("cache_key", "rebuild", "finalizer", "plot")) counters[[name]] <- 0L
  registry <- restore_env$.create_restore_job_registry(
    function() shiny::isolate(active_generation()),
    function(callback, delay) callbacks[[length(callbacks) + 1L]] <<- callback,
    function(report) reports[[length(reports) + 1L]] <<- report
  )
  registry$start_generation(generation)
  registry$set_phase(generation, "REPLAYING")

  queue <- function(owner, reason, callback, phase = "replay") {
    captured_generation <- shiny::isolate(active_generation())
    id <- registry$register_restore_job(owner, reason, phase, 30)
    session$onFlushed(once = TRUE, function() {
      restore_env$.run_session_restore_callback(
        owner, reason, captured_generation, phase, callback,
        job_metadata = list(job_id = id,
          resolve_job = registry$resolve_restore_job,
          current_generation = function() shiny::isolate(active_generation()))
      )
    })
    id
  }

  list(registry = registry, queue = queue, reports = function() reports,
       generation = active_generation, trigger = trigger, counters = counters)
}

test_that("Assign Rules primary and retry imperative replays are context safe", {
  skip_if_not_installed("shiny")
  shiny::testServer(restore_driver, {
    text <- shiny::reactiveValues(textin1 = NULL, textin2 = NULL)
    replay <- function() { text$textin1 <- "C"; text$textin2 <- "ERU" }
    queue("Assign Rules", "dynamic text primary replay", replay, "ui-replay")
    queue("Assign Rules", "dynamic text binding retry", replay, "ui-replay")
    expect_silent(session$flushReact())
    expect_identical(shiny::isolate(c(text$textin1, text$textin2)), c("C", "ERU"))
  })
})

test_that("an injected Assign Rules replay failure is contained and degraded", {
  shiny::testServer(restore_driver, {
    queue("Assign Rules", "injected replay", function() stop("ASSIGN_REPLAY_INJECTED"),
          "ui-replay")
    expect_silent(session$flushReact())
    registry$seal_generation(1L)
    expect_length(reports(), 1L)
    expect_true(reports()[[1L]]$state %in% c("FAILED", "DEGRADED"))
    expect_match(reports()[[1L]]$errors[[1L]], "ASSIGN_REPLAY_INJECTED")
  })
})

test_that("hydration keeps registration open through the trigger flush", {
  shiny::testServer(restore_driver, {
    expect_false(registry$generation_status(1L)$sealed)
    # Synchronous hydration completes first. Data Wizard's trigger is bumped in
    # flush one and intent-qualified consumers register in flush two.
    session$onFlushed(once = TRUE, function() {
      trigger(shiny::isolate(trigger()) + 1L)
      session$onFlushed(once = TRUE, function() {
        queue("Abundances", "intent-qualified replay", function() NULL)
        registry$seal_generation(1L)
      })
    })
    session$flushReact(); expect_false(registry$generation_status(1L)$sealed)
    session$flushReact(); expect_true(registry$generation_status(1L)$sealed)
    session$flushReact()
    expect_identical(reports()[[1L]]$state, "SETTLED")
  })
})

test_that("final success waits for Data Wizard and every replay job", {
  shiny::testServer(restore_driver, {
    ids <- vapply(c("Data Wizard", "Abundances", "Dotplot"), function(owner)
      registry$register_restore_job(owner, "required", "replay", 30), character(1))
    registry$seal_generation(1L)
    registry$resolve_restore_job(ids[[1L]], "success")
    registry$resolve_restore_job(ids[[2L]], "success")
    expect_empty(reports())
    registry$resolve_restore_job(ids[[3L]], "success")
    expect_identical(reports()[[1L]]$state, "SETTLED")
  })
})

test_that("Full Session Abundances-only intent rebuilds once and excludes peers", {
  fixture <- list(data_mod = data.frame(C = 1:3, ERU = 4:6),
                  data_def = data.frame(Column = c("C", "ERU"),
                    Content = "Abundance", Condition = c("C", "ERU")),
                  intents = c(abundances = TRUE, pca = FALSE, volcano = FALSE,
                    dotplot = FALSE, venn = FALSE, heatmap = FALSE,
                    sampleids = FALSE, string = FALSE, go = FALSE, gsea = FALSE))
  shiny::testServer(restore_driver, {
    registered <- names(fixture$intents)[fixture$intents]
    for (owner in registered) queue(owner, "valid cached replay", function() {
      counters$rebuild <- counters$rebuild + 1L
      counters$plot <- counters$plot + 1L
    })
    session$flushReact(); registry$seal_generation(1L)
    expect_identical(counters$rebuild, 1L)
    expect_identical(counters$plot, 1L)
    expect_identical(reports()[[1L]]$state, "SETTLED")
    expect_setequal(vapply(reports()[[1L]]$jobs, `[[`, "", "owner"), "abundances")
  })
})

test_that("Dotplot intent gates cache keys and diagnoses identity outcomes", {
  no_intent <- restore_env$dotplot_preprocess_restore_cache(
    list(plot_ready = FALSE, plot_data_cache_ref = "stale"), list())
  expect_identical(no_intent$restore_cache_resolution_mode, "none")
  expect_null(no_intent$restore_plot_data_cache)
  expect_error(restore_env$dotplot_build_cache_key(""), "malformed-cache-key")

  pair <- list(data_mod = data.frame(C = 1),
               data_def = data.frame(Column = "C", Content = "Abundance"))
  key <- restore_env$.build_plot_data_cache_id(data_mod = pair$data_mod,
                                               data_def = pair$data_def)
  valid <- restore_env$dotplot_preprocess_restore_cache(
    list(plot_ready = TRUE, plot_data_cache_ref = key), setNames(list(pair), key))
  expect_true(isTRUE(valid$restore_cache_resolved))
  # Compatible-live fallback is an explicit degraded outcome, never a cache hit.
  fallback <- restore_env$dotplot_preprocess_restore_cache(
    list(plot_ready = TRUE, plot_data_cache_ref = key), list())
  expect_false(isTRUE(fallback$restore_cache_resolved))
})

test_that("false plot intent registers no jobs and Heatmap keeps its skip", {
  states <- list(pca = list(had_plot = FALSE), volcano = list(had_static_plots = FALSE),
                 venn = list(had_plot = FALSE), heatmap = list(had_heatmap = FALSE))
  expect_false(any(mapply(restore_env$.registration_module_has_restore_intent,
                          names(states), states)))
  expect_identical(restore_env$.heatmap_registration_restore_cache_dependency(
    states$heatmap), "none")
})

test_that("generation N callbacks are stale no-ops for N plus one", {
  shiny::testServer(restore_driver, {
    guard <- shiny::reactiveVal("N+1")
    queue("Abundances", "late N callback", function() guard("corrupted"))
    generation(2L); registry$start_generation(2L)
    expect_silent(session$flushReact())
    expect_identical(shiny::isolate(guard()), "N+1")
  })
})

test_that("4198 by 46 Abundances cache remains authoritative across flushes", {
  cache <- list(data_mod = as.data.frame(matrix(seq_len(4198L * 46L), 4198L, 46L)),
                data_def = data.frame(Column = paste0("S", 1:46),
                  Content = rep("Abundance", 46), Condition = rep(c("C", "ERU"), 23)))
  shiny::testServer(restore_driver, {
    authoritative <- shiny::reactiveVal(cache)
    queue("Abundances", "large cached replay", function() {
      expect_identical(dim(authoritative()$data_mod), c(4198L, 46L))
      expect_identical(nrow(authoritative()$data_def), 46L)
      counters$rebuild <- counters$rebuild + 1L
      counters$plot <- counters$plot + 1L
    })
    session$flushReact(); session$flushReact(); session$flushReact()
    registry$seal_generation(1L)
    expect_identical(counters$rebuild, 1L)
    expect_identical(counters$plot, 1L)
    expect_false(is.null(shiny::isolate(authoritative())))
  })
})
