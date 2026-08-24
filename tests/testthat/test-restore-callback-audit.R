audit_root <- function() {
  normalizePath(file.path(testthat::test_path(), "..", ".."), mustWork = TRUE)
}

audit_patterns <- c(
  onFlushed = "session\\$onFlushed\\s*\\(",
  later = "later::later\\s*\\(",
  promise = "(?:%[.]{3}[!>]%|promises::(?:then|catch|finally)\\s*\\()",
  invalidateLater = "(?<![A-Za-z0-9_.])invalidateLater\\s*\\(",
  debounce = "(?<![A-Za-z0-9_.:])(?:shiny::)?debounce\\s*\\(",
  throttle = "(?<![A-Za-z0-9_.:])(?:shiny::)?throttle\\s*\\(",
  `cleanup registration` =
    "(?:cleanup_manager\\$register_module|session\\$onSessionEnded|reg\\.finalizer)\\s*\\(",
  `on.exit finalizer` = "on\\.exit\\s*\\(",
  `tryCatch finally` = "finally\\s*="
)

restore_audit_files <- function(root) {
  framework <- list.files(file.path(root, "R", "session_save_restore"),
                          pattern = "[.]R$", full.names = TRUE)
  modules <- list.files(file.path(root, "modules"), pattern = "[.]R$",
                        recursive = TRUE, full.names = TRUE)
  relative <- substring(modules, nchar(root) + 2L)
  included <- grepl(
    "^modules/(Grid|GO|GSEA|STRING|Heatmap|abundances|datawizard_module[.]R|Data Wizard/)",
    relative
  )
  sort(c(framework, modules[included]))
}

# Return source lines where invalidateLater is structurally below a Shiny
# consumer. This is AST based rather than a nearby-comment/text heuristic.
reactive_invalidation_lines <- function(path) {
  expressions <- parse(path, keep.source = TRUE)
  found <- integer()
  walk <- function(node, in_consumer = FALSE) {
    if (!is.call(node) && !is.expression(node) && !is.pairlist(node)) return()
    if (is.call(node)) {
      head_text <- paste(deparse(node[[1L]], width.cutoff = 500L), collapse = "")
      name <- sub("^.*::", "", head_text)
      consumer <- name %in% c("reactive", "observe", "observeEvent") ||
        startsWith(name, "render")
      if (identical(name, "invalidateLater") && in_consumer) {
        ref <- attr(node, "srcref")
        if (!is.null(ref)) found <<- c(found, as.integer(ref[[1L]]))
      }
      in_consumer <- in_consumer || consumer
    }
    for (child in as.list(node)) walk(child, in_consumer)
  }
  walk(expressions)
  unique(found)
}

scan_restore_boundaries <- function(root) {
  records <- list()
  for (path in restore_audit_files(root)) {
    relative <- substring(path, nchar(root) + 2L)
    lines <- readLines(path, warn = FALSE)
    reactive_lines <- reactive_invalidation_lines(path)
    counts <- stats::setNames(integer(length(audit_patterns)), names(audit_patterns))
    for (line_number in seq_along(lines)) {
      line <- lines[[line_number]]
      if (grepl("^\\s*#", line, perl = TRUE)) next
      for (mechanism in names(audit_patterns)) {
        hits <- gregexpr(audit_patterns[[mechanism]], line, perl = TRUE)[[1L]]
        number <- if (identical(hits[[1L]], -1L)) 0L else length(hits)
        if (!number) next
        for (unused in seq_len(number)) {
          counts[[mechanism]] <- counts[[mechanism]] + 1L
          id <- paste(relative, mechanism, counts[[mechanism]], sep = "::")
          records[[length(records) + 1L]] <- data.frame(
            boundary_id = id, file = relative, mechanism = mechanism,
            structurally_approved = identical(mechanism, "invalidateLater") &&
              line_number %in% reactive_lines,
            stringsAsFactors = FALSE
          )
        }
      }
    }
  }
  if (!length(records)) return(data.frame())
  do.call(rbind, records)
}

test_that("restore imperative boundaries have reviewed audit entries", {
  root <- audit_root()
  manifest <- read.csv(
    file.path(root, "R", "session_save_restore", "restore_callback_audit_manifest.csv"),
    stringsAsFactors = FALSE, check.names = FALSE
  )
  required <- c("boundary_id", "owner", "file", "mechanism", "reviewed_line",
                "trigger", "reactive_reads", "isolation", "catch_behavior",
                "generation_guard", "failure_behavior", "category", "routing",
                "callback_contract", "reactive_consumer", "restore_specific")
  expect_true(all(required %in% names(manifest)))
  expect_identical(anyDuplicated(manifest$boundary_id), 0L)
  expect_true(all(manifest$category %in% 1:3))
  expect_true(all(vapply(manifest[required], function(x) all(nzchar(x)), logical(1))))

  expect_true(all(manifest$restore_specific %in% c("yes", "no")))
  category_one <- manifest$category == 1L & manifest$restore_specific == "yes"
  approved_contracts <- c(
    "shared restore callback runner",
    "module wrapper delegating to shared runner",
    "documented captured-values-only implementation"
  )
  expect_true(all(manifest$callback_contract[category_one] %in% approved_contracts),
    info = paste(
      "Every Category 1 callback (including promises and custom timers) must",
      "name its approved imperative boundary; 'reviewed raw boundary' is not approval."
    ))
  expect_true(all(manifest$reactive_consumer[category_one] == "no"))
  captured_only <- category_one & manifest$callback_contract ==
    "documented captured-values-only implementation"
  expect_true(all(grepl("captured", manifest$reactive_reads[captured_only], ignore.case = TRUE)))
  expect_false(any(grepl(
    "reactiveVal|reactiveValues|(?:^|[^[:alnum:]_])input(?:\\$|\\[)|reactive expression",
    manifest$reactive_reads[captured_only], perl = TRUE, ignore.case = TRUE
  )), info = "A captured-values-only callback may not hide a direct or transitive reactive read.")

  reactive_categories <- manifest$category == 2L
  expect_true(all(manifest$reactive_consumer[reactive_categories] == "yes"))
  invalidators <- manifest$mechanism == "invalidateLater"
  expect_true(all(manifest$category[invalidators] == 2L &
                    manifest$reactive_consumer[invalidators] == "yes"),
    info = "invalidateLater() is approved only when armed by a genuine reactive consumer.")

  scanned <- scan_restore_boundaries(root)
  missing <- setdiff(
    scanned$boundary_id[!scanned$structurally_approved], manifest$boundary_id
  )
  stale <- setdiff(
    manifest$boundary_id[manifest$routing == "reviewed raw boundary"],
    scanned$boundary_id
  )
  expect_empty(missing, info = paste(
    "A raw restore boundary was added. Route it through an approved helper or",
    "review and update restore_callback_audit_manifest.csv."
  ))
  expect_empty(stale, info = "The manifest contains a boundary no longer in source.")
})

test_that("restore readiness does not downgrade Shiny context violations", {
  root <- audit_root()
  callbacks <- paste(readLines(file.path(
    root, "R", "session_save_restore", "session_save_restore_callbacks.R"
  ), warn = FALSE), collapse = "\n")
  expect_match(callbacks, "\\.evaluate_restore_readiness <- function", perl = TRUE)
  expect_match(callbacks,
    "\\.is_shiny_context_error\\(condition\\)[\\s\\S]*REACTIVE_CONTEXT_VIOLATION",
    perl = TRUE)

  # Readiness helpers which intentionally map ordinary errors to unavailable
  # must rethrow current-context failures for the shared boundary to classify.
  gsea <- paste(readLines(file.path(
    root, "modules", "GSEA", "GSEA_module_observer.R"
  ), warn = FALSE), collapse = "\n")
  helper <- regmatches(gsea, regexpr(
    "gsea_restore_unavailable <- function\\(default\\) \\{[\\s\\S]{0,300}\\n  \\}",
    gsea, perl = TRUE
  ))
  expect_match(helper, "\\.is_shiny_context_error\\(e\\)")
  expect_match(helper, "stop\\(e\\)")
})

test_that("the audit explicitly covers every requested restore owner", {
  root <- audit_root()
  manifest <- read.csv(file.path(
    root, "R", "session_save_restore", "restore_callback_audit_manifest.csv"
  ), stringsAsFactors = FALSE)
  files <- manifest$file
  for (prefix in c("Grid", "GO", "GSEA", "STRING", "Heatmap", "abundances")) {
    expect_true(any(grepl(paste0("^modules/", prefix), files)), info = prefix)
  }
  expect_true(any(startsWith(files, "modules/Data Wizard/")))
  datawizard_submodules <- c(
    "Annotation", "auto assign", "auto regex", "batch_effects", "core", "edit",
    "file_loader", "filtering", "imputation", "pivot", "tables", "assign rules"
  )
  for (submodule in datawizard_submodules) {
    prefix <- paste0("modules/Data Wizard/", submodule, "/")
    expect_true(any(startsWith(files, prefix)), info = paste("Data Wizard", submodule))
  }
})
