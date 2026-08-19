# ============================================================================
# MiraProt File Contract: modules/Data Wizard/file_loader/datawizard_file_loader_context.R
# Purpose:
#   Provide the file loader context portion of the Data Wizard without changing public behavior.
# Architectural Role:
#   File Loader implementation unit loaded by the historical datawizard_file_loader.R compatibility entry point.
# Responsibilities:
#   Define only the focused functions or composition wiring named by this file.
# Non-Responsibilities:
#   Do not redefine public APIs, create parallel state owners, or change workflow semantics.
# Main Interface:
#   Top-level functions defined here, or compatibility symbols exposed by its ordered sources.
# Dependencies:
#   MiraProt Data Wizard helpers and injected Shiny/package services used by those functions.
# State Ownership:
#   Loader session context owns upload/cache/header reactives; canonical primary and secondary datasets remain owned through injected adapters.
# Mutation Authority:
#   Only loader handlers using the shared loader context and injected adapter callbacks may mutate session or canonical data.
# Source-Order Assumptions:
#   Source through datawizard_file_loader.R in its declared dependency order; direct sourcing is supported only with its documented prerequisites.
# Session/Restore Implications:
#   Loader snapshots retain the unchanged get/set session-state contract and bounded, idempotent restore coordination.
# Important Invariants:
#   Preserve Section B symbols/returns, unchanged public APIs, one loader/Tables
#   context per module session, source-DAG acyclicity, and existing timing guards.
# ============================================================================

# modules/Data Wizard/file_loader/datawizard_file_loader_context.R
# Server-local state and helper closures for the Data Wizard file loader.

create_datawizard_file_loader_context <- function(input, output, session, rv = NULL,
                                                  debug_level = 0, debug_log,
                                                  safe_error_message, primary_data_state) {
    # Core data storage
    data_fixed  <- reactiveVal(NULL)
    data2_fixed <- reactiveVal(NULL)
    data_primary <- reactiveVal(NULL)

    data_additional <- reactiveVal(NULL)

    # Enhanced tracking
    loading_errors <- reactiveVal(list())
    loading_history <- reactiveVal(list())
    loading_active <- reactiveVal(FALSE)
    current_operation <- reactiveVal("")

    # Session-based file cache
    file_cache <- reactiveVal(list())
    telemetry_now <- function() unname(proc.time()[["elapsed"]])
    new_upload_correlation_id <- function() {
      paste0("dw-", paste(format(as.hexmode(sample.int(.Machine$integer.max, 2L)), width = 8L), collapse = ""))
    }
    upload_telemetry <- new.env(parent = emptyenv())
    upload_telemetry$id <- "no-upload"
    upload_telemetry$started <- telemetry_now()
    upload_telemetry$counters <- new.env(parent = emptyenv())
    telemetry_verbose <- isTRUE(DEBUG_LEVEL >= 2L)
    telemetry_log <- function(marker, phase, details = "", level = 1) {
      suffix <- if (nzchar(details)) paste0(" | ", details) else ""
      debug_log(sprintf(
        "DW LIFECYCLE | correlation_id=%s | elapsed_ms=%.3f | marker=%s | phase=%s%s",
        upload_telemetry$id, 1000 * (telemetry_now() - upload_telemetry$started), marker, phase, suffix
      ), level)
    }
    duplicate_work <- function(module, key) {
      if (!telemetry_verbose) return(invisible(NULL))
      counter_key <- paste(module, key, sep = ":")
      count <- (upload_telemetry$counters[[counter_key]] %||% 0L) + 1L
      upload_telemetry$counters[[counter_key]] <- count
      telemetry_log("counter", "duplicate_work", sprintf("module=%s | operation=%s | count=%d", module, key, count), 2)
      invisible(count)
    }
    data_metrics <- function(data, prefix = "") {
      if (!is.data.frame(data)) return(paste0(prefix, "dimensions=0x0 | object_mb=0"))
      sprintf("%sdimensions=%dx%d | %sobject_mb=%.3f", prefix, nrow(data), ncol(data), prefix,
              as.numeric(object.size(data)) / (1024^2))
    }
    filenames_may_be_logged <- function() {
      if (is.null(rv)) return(TRUE)
      !isTRUE(tryCatch(shiny::isolate(rv$privacy_mode), error = function(e) FALSE)) &&
        !identical(tryCatch(shiny::isolate(rv$log_filenames), error = function(e) TRUE), FALSE)
    }
    private_label <- function(value, fallback = "[redacted]") {
      if (filenames_may_be_logged()) as.character(value %||% fallback) else fallback
    }

    # Startup flag to prevent early observe triggers
    module_initialized <- reactiveVal(FALSE)

    # Track when actively loading new file
    new_file_loading <- reactiveVal(FALSE)

    # Session save/restore: persisted raw data and lazy Excel sheet cache
    # ----------------------------------------------------------------
    # sheet_cache_primary / sheet_cache_secondary are lazy per-sheet caches.
    # They store only sheets that have actually been selected, not an eager
    # workbook-wide parse. The cache is bounded below by sheet count and object
    # size to avoid keeping large workbooks resident in memory.
    # primary_data_original / secondary_data_original hold the frozen
    # first-load data frames so the Reset button has an authoritative original
    # to restore to — both before and after session restore.
    # primary_file_meta / secondary_file_meta carry lightweight identifiers
    # (name, ext, n_sheets, sheet_names) used by the post-restore UI to re-render
    # the sheet dropdown even when input$file is empty. Session snapshots persist
    # only the current sheet data plus this metadata; all-sheet persistence would
    # require intentionally saving every sheet and is avoided for memory reasons.
    # pending_loader_state stages a restore payload that the
    # session_restore_trigger observer applies once reactives have settled.
    sheet_cache_primary     <- reactiveVal(list())
    sheet_cache_secondary   <- reactiveVal(list())
    selected_sheet_primary  <- reactiveVal(NULL)
    selected_sheet_secondary <- reactiveVal(NULL)
    primary_data_original   <- reactiveVal(NULL)
    secondary_data_original <- reactiveVal(NULL)
    primary_file_meta       <- reactiveVal(NULL)
    secondary_file_meta     <- reactiveVal(NULL)
    pending_loader_state    <- reactiveVal(NULL)
    applying_loader_restore <- FALSE
    last_applied_loader_restore_signature <- NULL
    last_applied_loader_restore_generation <- NULL

    loader_restore_data_dims <- function(x) {
      if (is.null(x)) return("NULL")
      d <- tryCatch(dim(x), error = function(e) NULL)
      if (is.null(d) || length(d) < 2) return(class(x)[[1]] %||% "unknown")
      paste0(d[[1]], "x", d[[2]])
    }

    compute_loader_restore_signature <- function(state) {
      inputs <- state$inputs %||% list()
      payload <- list(
        mode = state$mode %||% "full",
        sheetDropdown = inputs$sheetDropdown %||% NULL,
        sheetDropdown2 = inputs$sheetDropdown2 %||% NULL,
        header_row = inputs$header_row %||% NULL,
        header_row2 = inputs$header_row2 %||% NULL,
        data_fixed_dim = loader_restore_data_dims(state$data_fixed),
        data2_fixed_dim = loader_restore_data_dims(state$data2_fixed),
        primary_original_dim = loader_restore_data_dims(state$primary_data_original),
        secondary_original_dim = loader_restore_data_dims(state$secondary_data_original)
      )
      paste(vapply(names(payload), function(name) {
        value <- payload[[name]]
        if (is.null(value)) value <- "NULL"
        paste0(name, "=", paste(value, collapse = ","))
      }, character(1)), collapse = "|")
    }

    get_loader_restore_generation <- function() {
      if (is.null(rv)) return(NULL)
      generation <- tryCatch(
        shiny::isolate(rv$session_restore_generation %||% rv$session_restore_trigger),
        error = function(e) NULL
      )
      generation
    }

    format_loader_restore_id <- function(generation, signature) {
      paste0(
        "generation=", (generation %||% "NA"),
        ", signature=", (signature %||% "NA")
      )
    }


    is_labels_only_loader_state <- function(state) {
      identical(state$mode %||% NULL, "labels_only") ||
        isTRUE(state$restore_labels_only)
    }

    log_loader_restore_mode <- function(state, mode, skip_publish_working_data) {
      debug_log(paste0(
        "Loader restore mode=", mode,
        ", skip_publish_working_data=", isTRUE(skip_publish_working_data),
        ", data_fixed_dim=", loader_restore_data_dims(state$data_fixed %||% state$primary_data_original)
      ), 1)
    }
    # Explicit loader mode state machine:
    # - interactive_load: live upload/sheet reload flows can reprocess
    #   header/data (clean_and_index).
    # - restore_replay: session-restore replay is in progress; all header
    #   reprocessing must be blocked.
    loader_mode             <- reactiveVal("interactive_load")
    # Guards to prevent session-restore replay from re-entering header-based
    # reprocessing (clean_and_index), which would recreate Row Index and
    # trigger metadata reset logic meant only for interactive file loads.
    restore_replay_active   <- reactiveVal(FALSE)
    # Short-lived reset guard: Reset publishes original data and resets header
    # inputs in one batch. Keep header observers from reprocessing stale input
    # values while those synchronized state writes settle.
    reset_replay_active     <- reactiveVal(FALSE)
    # Short-lived guard while the primary header-row observer rebuilds both
    # data and matching metadata. The DT preview can see the legacy rv flag and
    # avoid styling with stale metadata during the same reactive cycle.
    header_reprocess_active <- reactiveVal(FALSE)
    skip_next_restore_header_reprocess_primary <- reactiveVal(FALSE)
    skip_next_restore_header_reprocess_secondary <- reactiveVal(FALSE)
    skip_next_programmatic_header_update_primary <- reactiveVal(FALSE)
    skip_next_programmatic_header_update_secondary <- reactiveVal(FALSE)
    # Programmatic sheet-dropdown updates during file upload/reset/restore must
    # not be interpreted as user sheet switches. Otherwise the freshly loaded
    # first sheet is immediately republished from the lazy cache, causing a
    # second revision/rerender with no user action.
    skip_next_sheet_change_primary <- reactiveVal(NULL)
    skip_next_sheet_change_secondary <- reactiveVal(NULL)
    # One-shot guard: after session-restore replay updates sheetDropdown,
    # skip the first cache-based sheet handler run so restored rv$data_mod
    # is not overwritten by cached loader data.
    skip_next_cached_sheet_apply_primary <- reactiveVal(FALSE)
    skip_next_cached_sheet_apply_secondary <- reactiveVal(FALSE)

    set_header_reprocess_active <- function(active) {
      active <- isTRUE(active)
      header_reprocess_active(active)
      if (!is.null(rv)) {
        # Canonical shared flag consumed by downstream Data Wizard modules.
        rv$datawizard_header_reprocess_active <- active
        # Backward-compatible alias for older call sites.
        rv$header_reprocess_active <- active
      }
      invisible(active)
    }

    release_header_reprocess_after_flush <- function() {
      # Keep the guard raised through the current Shiny flush so output
      # renderers invalidated by the data write see the in-progress state and
      # avoid applying metadata-driven DT styling against stale columns.
      session$onFlushed(function() {
        set_header_reprocess_active(FALSE)
        debug_log("Header change: released header reprocess guard after data/metadata synchronization flush", level = 2)
      }, once = TRUE)
      invisible(TRUE)
    }

    set_loader_mode <- function(mode, reason = "") {
      # set_session_state() can be invoked from the global restore pipeline
      # outside a reactive consumer. Read reactiveVal via isolate so mode
      # transitions remain safe in both reactive and non-reactive call paths.
      old_mode <- tryCatch(
        shiny::isolate(loader_mode()),
        error = function(e) NA_character_
      )
      if (!identical(old_mode, mode)) {
        loader_mode(mode)
        debug_log(paste0(
          "Loader mode transition: ", old_mode, " -> ", mode,
          if (nzchar(reason)) paste0(" (", reason, ")") else ""
        ), 2)
      }
    }

    restore_phase_active <- function(phases = NULL) {
      if (is.null(rv)) return(FALSE)
      phases_current <- list(
        rv$restore_phase %||% NULL,
        rv$session_restore_phase %||% NULL
      )
      any(vapply(phases_current, function(phase) {
        !is.null(phase) && !identical(phase, "complete") &&
          (is.null(phases) || phase %in% phases)
      }, logical(1)))
    }

    restore_observer_guard_active <- function(observer_name = "observer") {
      active <- isTRUE(restore_replay_active()) ||
        isTRUE(applying_loader_restore) ||
        !is.null(pending_loader_state()) ||
        (!is.null(rv) && (
          isTRUE(rv$session_restoring) ||
            restore_phase_active()
        ))
      if (isTRUE(active)) {
        debug_log(paste0(
          observer_name, ": skipped during session restore replay",
          " | restore_replay_active=", isTRUE(restore_replay_active()),
          " | rv$session_restoring=", if (!is.null(rv)) isTRUE(rv$session_restoring) else FALSE,
          " | applying_loader_restore=", isTRUE(applying_loader_restore),
          " | pending_loader_state=", !is.null(pending_loader_state()),
          " | rv$restore_phase=", if (!is.null(rv)) (rv$restore_phase %||% "NULL") else "NULL",
          " | rv$session_restore_phase=", if (!is.null(rv)) (rv$session_restore_phase %||% "NULL") else "NULL"
        ), 2)
      }
      active
    }

    is_live_excel_upload <- function(file_input) {
      !is.null(file_input) &&
        !is.null(file_input$datapath) &&
        file.exists(file_input$datapath) &&
        tolower(tools::file_ext(file_input$name %||% file_input$datapath %||% "")) %in% c("xlsx", "xls")
    }

    restored_loader_state_settled <- function() {
      identical(loader_mode(), "interactive_load") &&
        !isTRUE(restore_replay_active()) &&
        !isTRUE(reset_replay_active()) &&
        (is.null(rv) || (!isTRUE(rv$session_restoring) && !restore_phase_active())) &&
        is.null(pending_loader_state()) &&
        !isTRUE(applying_loader_restore)
    }

    has_restored_sheet_cache <- function(cache_reactive, selected_sheet_reactive, input_sheet, live_file) {
      if (isTRUE(live_file)) {
        # This helper is for the post-restore path where fileInput datapaths
        # have expired. Live uploads continue through the normal Excel branch.
        return(FALSE)
      }

      selected_sheet <- selected_sheet_reactive() %||% input_sheet
      if (is.null(selected_sheet) || !nzchar(as.character(selected_sheet))) {
        return(FALSE)
      }

      cache <- normalize_sheet_cache(cache_reactive())
      !is.null(get_cached_sheet_data(cache, selected_sheet))
    }

    has_restored_primary_sheet_cache <- function() {
      has_restored_sheet_cache(sheet_cache_primary, selected_sheet_primary, input$sheetDropdown, is_live_excel_upload(input$file))
    }

    has_restored_secondary_sheet_cache <- function() {
      has_restored_sheet_cache(sheet_cache_secondary, selected_sheet_secondary, input$sheetDropdown2, is_live_excel_upload(input$file2))
    }

    restored_sheet_cache_header_guards_clear <- function() {
      !isTRUE(restore_replay_active()) &&
        !isTRUE(reset_replay_active()) &&
        is.null(pending_loader_state()) &&
        (is.null(rv) || !isTRUE(rv$session_restoring))
    }

    can_use_restored_sheet_cache_for_header_primary <- function() {
      isTRUE(has_restored_primary_sheet_cache()) &&
        isTRUE(restored_sheet_cache_header_guards_clear())
    }

    can_use_restored_sheet_cache_for_header_secondary <- function() {
      isTRUE(has_restored_secondary_sheet_cache()) &&
        isTRUE(restored_sheet_cache_header_guards_clear())
    }

    can_header_reprocess_primary <- function(restored_cache_allowed = FALSE) {
      is_live_file <- is_live_excel_upload(input$file)
      mode_ok <- identical(loader_mode(), "interactive_load")
      guards_clear <- restored_loader_state_settled()
      allow <- isTRUE((is_live_file && mode_ok && guards_clear) || isTRUE(restored_cache_allowed))
      debug_log(paste0(
        "Header reprocess gate (primary): allow=", allow,
        " | mode=", loader_mode(),
        " | live_file=", is_live_file,
        " | restored_cache_allowed=", isTRUE(restored_cache_allowed),
        " | restore_replay_active=", isTRUE(restore_replay_active()),
        " | reset_replay_active=", isTRUE(reset_replay_active()),
        " | rv$session_restoring=", if (!is.null(rv)) isTRUE(rv$session_restoring) else FALSE,
        " | pending_loader_state=", !is.null(pending_loader_state())
      ), 2)
      allow
    }

    can_header_reprocess_secondary <- function(restored_cache_allowed = FALSE) {
      is_live_file <- is_live_excel_upload(input$file2)
      mode_ok <- identical(loader_mode(), "interactive_load")
      guards_clear <- restored_loader_state_settled()
      allow <- isTRUE((is_live_file && mode_ok && guards_clear) || isTRUE(restored_cache_allowed))
      debug_log(paste0(
        "Header reprocess gate (secondary): allow=", allow,
        " | mode=", loader_mode(),
        " | live_file=", is_live_file,
        " | restored_cache_allowed=", isTRUE(restored_cache_allowed),
        " | restore_replay_active=", isTRUE(restore_replay_active()),
        " | reset_replay_active=", isTRUE(reset_replay_active()),
        " | rv$session_restoring=", if (!is.null(rv)) isTRUE(rv$session_restoring) else FALSE,
        " | pending_loader_state=", !is.null(pending_loader_state())
      ), 2)
      allow
    }

    is_loader_restore_replay_context <- function() {
      isTRUE(restore_replay_active()) ||
        isTRUE(reset_replay_active()) ||
        (!is.null(rv) && (isTRUE(rv$session_restoring) || restore_phase_active())) ||
        !is.null(pending_loader_state())
    }

    excel_sheet_cache_max_count <- 3L
    excel_sheet_cache_max_size_mb <- 150

    get_excel_sheet_names <- function(file_input) {
      telemetry_log("start", "sheet_enumeration", level = 1)
      if (is.null(file_input) || is.null(file_input$datapath) ||
          !file.exists(file_input$datapath)) {
        return(character(0))
      }
      ext <- tolower(tools::file_ext(file_input$name %||% file_input$datapath))
      if (!ext %in% c("xlsx", "xls")) {
        telemetry_log("end", "sheet_enumeration", "sheet_count=0", 1)
        return(character(0))
      }
      sheets <- tryCatch(readxl::excel_sheets(file_input$datapath), error = function(e) {
        debug_log(paste("Error reading Excel sheet names:", e$message), 1)
        character(0)
      })
      telemetry_log("end", "sheet_enumeration", sprintf("sheet_count=%d", length(sheets)), 1)
      sheets
    }

    make_excel_file_meta <- function(file_input, sheet_names = character(0)) {
      if (is.null(file_input)) return(NULL)
      list(
        workbook_name    = file_input$name %||% NA_character_,
        name             = file_input$name %||% NA_character_,
        ext              = tolower(tools::file_ext(file_input$name %||% file_input$datapath %||% "")),
        size             = file_input$size %||% NA_real_,
        n_sheets         = length(sheet_names),
        sheet_names      = as.character(sheet_names),
        selected_sheet   = NULL,
        header_row       = 1L,
        loaded_sheet_ids = character(0)
      )
    }

    get_sheet_choices_from_state <- function(cache, meta) {
      sheet_names <- meta$sheet_names %||% character(0)
      sheet_names <- as.character(sheet_names)
      if (length(sheet_names) > 0) return(sheet_names)
      names(cache$entries %||% cache)
    }

    get_reset_sheet_from_state <- function(cache, meta) {
      sheet_names <- meta$sheet_names %||% character(0)
      sheet_names <- as.character(sheet_names)
      if (length(sheet_names) > 0 && nzchar(sheet_names[[1]])) {
        return(sheet_names[[1]])
      }

      cached_sheet_names <- names(cache$entries %||% cache)
      if (length(cached_sheet_names) > 0 && nzchar(cached_sheet_names[[1]])) {
        return(cached_sheet_names[[1]])
      }

      NULL
    }

    new_sheet_cache <- function(sheet_names = character(0)) {
      entries <- setNames(lapply(as.character(sheet_names), function(sheet_name) {
        list(sheet_name = sheet_name, loaded = FALSE, data = NULL,
             dataset_id = NULL, size_mb = 0, last_access = as.numeric(Sys.time()))
      }), as.character(sheet_names))
      list(entries = entries, loaded_order = character(0))
    }

    normalize_sheet_cache <- function(cache, sheet_names = character(0)) {
      if (is.null(cache)) cache <- list()
      if (is.null(cache$entries)) {
        legacy <- cache
        cache <- new_sheet_cache(unique(c(as.character(sheet_names), names(legacy))))
        for (sheet_name in names(legacy)) {
          if (is.data.frame(legacy[[sheet_name]])) {
            cache$entries[[sheet_name]] <- list(
              sheet_name = sheet_name,
              loaded = TRUE,
              data = legacy[[sheet_name]],
              dataset_id = NULL,
              size_mb = as.numeric(object.size(legacy[[sheet_name]])) / (1024^2),
              last_access = as.numeric(Sys.time())
            )
            cache$loaded_order <- unique(c(cache$loaded_order, sheet_name))
          }
        }
      }
      for (sheet_name in as.character(sheet_names)) {
        if (!nzchar(sheet_name)) next
        if (is.null(cache$entries[[sheet_name]])) {
          cache$entries[[sheet_name]] <- list(sheet_name = sheet_name, loaded = FALSE,
                                             data = NULL, dataset_id = NULL,
                                             size_mb = 0, last_access = as.numeric(Sys.time()))
        }
      }
      cache$loaded_order <- cache$loaded_order[cache$loaded_order %in% names(cache$entries)]
      cache
    }

    loaded_sheet_names <- function(cache) {
      cache <- normalize_sheet_cache(cache)
      names(Filter(function(entry) isTRUE(entry$loaded) && is.data.frame(entry$data), cache$entries))
    }

    get_cached_sheet_data <- function(cache, sheet_name) {
      cache <- normalize_sheet_cache(cache)
      entry <- cache$entries[[sheet_name]]
      if (!is.null(entry) && isTRUE(entry$loaded) && is.data.frame(entry$data)) entry$data else NULL
    }

    prune_sheet_cache <- function(cache, max_count = excel_sheet_cache_max_count,
                                  max_size_mb = excel_sheet_cache_max_size_mb) {
      cache <- normalize_sheet_cache(cache)
      loaded <- loaded_sheet_names(cache)
      total_size <- sum(vapply(loaded, function(sheet_name) {
        cache$entries[[sheet_name]]$size_mb %||% 0
      }, numeric(1)), na.rm = TRUE)

      while (length(loaded) > max_count || (length(loaded) > 1 && total_size > max_size_mb)) {
        evict <- cache$loaded_order[[1]]
        cache$loaded_order <- cache$loaded_order[-1]
        if (!is.null(cache$entries[[evict]])) {
          cache$entries[[evict]]$loaded <- FALSE
          cache$entries[[evict]]$data <- NULL
          cache$entries[[evict]]$size_mb <- 0
        }
        loaded <- loaded_sheet_names(cache)
        total_size <- sum(vapply(loaded, function(sheet_name) {
          cache$entries[[sheet_name]]$size_mb %||% 0
        }, numeric(1)), na.rm = TRUE)
      }
      cache
    }

    cache_loaded_sheet <- function(cache_reactive, sheet_name, data_obj, context) {
      telemetry_log("start", "cache_insertion", data_metrics(data_obj), 2)
      duplicate_work("file_loader", "cache_insertion")
      if (is.null(sheet_name) || !nzchar(as.character(sheet_name)) ||
          is.null(data_obj) || !is.data.frame(data_obj)) {
        return(invisible(FALSE))
      }
      current_cache <- normalize_sheet_cache(cache_reactive())
      # Refresh recency by removing the old copy before appending it.
      current_cache$loaded_order <- setdiff(current_cache$loaded_order, sheet_name)
      current_cache$loaded_order <- c(current_cache$loaded_order, sheet_name)
      dataset_id <- paste0("sheet_", make.names(sheet_name), "_", format(Sys.time(), "%Y%m%d%H%M%OS3"))
      current_cache$entries[[sheet_name]] <- list(
        sheet_name = sheet_name,
        loaded = TRUE,
        data = data_obj,
        dataset_id = dataset_id,
        size_mb = as.numeric(object.size(data_obj)) / (1024^2),
        last_access = as.numeric(Sys.time())
      )
      current_cache <- prune_sheet_cache(current_cache)
      cache_reactive(current_cache)
      debug_log(sprintf(
        "%s cached sheet '%s' (cached sheets=%d, cache size=%.1f MB)",
        context, private_label(sheet_name), length(loaded_sheet_names(current_cache)),
        sum(vapply(loaded_sheet_names(current_cache), function(nm) current_cache$entries[[nm]]$size_mb %||% 0, numeric(1)))
      ), 2)
      telemetry_log("end", "cache_insertion", sprintf("cache_hit=false | cached_count=%d | %s",
        length(loaded_sheet_names(current_cache)), data_metrics(data_obj)), 2)
      invisible(TRUE)
    }

    update_workbook_manifest <- function(meta_reactive, selected_sheet = NULL, header_row = NULL,
                                         cache = NULL) {
      meta <- meta_reactive()
      if (is.null(meta)) return(invisible(NULL))
      if (!is.null(selected_sheet)) meta$selected_sheet <- selected_sheet
      if (!is.null(header_row)) meta$header_row <- suppressWarnings(as.integer(header_row %||% 1L))
      if (!is.null(cache)) {
        cache <- normalize_sheet_cache(cache, meta$sheet_names %||% character(0))
        meta$loaded_sheet_ids <- vapply(loaded_sheet_names(cache), function(sheet_name) {
          cache$entries[[sheet_name]]$dataset_id %||% sheet_name
        }, character(1), USE.NAMES = FALSE)
      }
      meta_reactive(meta)
      invisible(meta)
    }

    load_workbook_sheet_cache_for_session <- function(cache, meta, file_input, context) {
      if (!is_live_excel_upload(file_input) || is.null(meta)) {
        return(normalize_sheet_cache(cache, meta$sheet_names %||% character(0)))
      }

      sheet_names <- as.character(meta$sheet_names %||% get_excel_sheet_names(file_input))
      sheet_names <- sheet_names[nzchar(sheet_names)]
      cache <- normalize_sheet_cache(cache, sheet_names)
      if (length(sheet_names) == 0) return(cache)

      for (sheet_name in sheet_names) {
        if (!is.null(get_cached_sheet_data(cache, sheet_name))) next

        loaded <- tryCatch(
          load_file_with_recovery_dw(file_input, sheet = sheet_name, header = TRUE),
          error = function(e) {
            debug_log(sprintf(
              "%s: unable to snapshot workbook sheet '%s': %s",
              context, sheet_name, e$message
            ), 1)
            NULL
          }
        )
        if (is.list(loaded) && is.data.frame(loaded$data)) {
          cache$loaded_order <- setdiff(cache$loaded_order, sheet_name)
          cache$loaded_order <- c(cache$loaded_order, sheet_name)
          cache$entries[[sheet_name]] <- list(
            sheet_name = sheet_name,
            loaded = TRUE,
            data = as.data.frame(loaded$data, check.names = FALSE),
            dataset_id = paste0(
              "sheet_", make.names(sheet_name), "_session_",
              format(Sys.time(), "%Y%m%d%H%M%OS3")
            ),
            size_mb = as.numeric(object.size(loaded$data)) / (1024^2),
            last_access = as.numeric(Sys.time())
          )
        }
      }

      cache <- normalize_sheet_cache(cache, sheet_names)
      debug_log(sprintf(
        "%s: workbook sheet snapshot includes %d/%d primary/secondary sheet(s)",
        context, length(loaded_sheet_names(cache)), length(sheet_names)
      ), 1)
      cache
    }

    publish_primary_current_sheet <- function(data, source) {
      primary_data_state$set_current_primary_sheet(data, source)
      invisible(data)
    }

    publish_secondary_current_sheet <- function(data, source) {
      primary_data_state$set_dataset("secondary_working", data, source = source)
      if (!is.null(rv)) rv$data2_mod <- data
      invisible(data)
    }

    # ========================================
    # Enhanced Core Functions
    # ========================================

    #' Enhanced boolean check function
    safe_is_true <- function(x) {
      if (is.null(x) || length(x) == 0) return(FALSE)
      if (is.logical(x)) return(isTRUE(x[1]))
      if (is.numeric(x)) return(x[1] > 0)
      if (is.character(x)) return(tolower(x[1]) %in% c("true", "t", "yes", "y", "1"))
      if (is.list(x)) {
        if (!is.null(x$success)) return(safe_is_true(x$success))
        if (!is.null(x$data)) return(TRUE)
        return(length(x) > 0)
      }
      return(FALSE)
    }

    #' Robust data validation
    is_valid_data <- function(data_obj) {
      if (is.null(data_obj)) return(FALSE)
      if (!is.data.frame(data_obj)) return(FALSE)
      if (nrow(data_obj) == 0) return(FALSE)
      if (ncol(data_obj) == 0) return(FALSE)
      return(TRUE)
    }

    #' Add entry to loading log
    add_loading_log <- function(operation, status, message = "", duration = 0) {
      tryCatch({
        current_log <- loading_history()
        new_entry <- list(
          timestamp = Sys.time(),
          operation = operation,
          status = status,
          message = message,
          duration = duration
        )
        loading_history(c(current_log, list(new_entry)))

        if (status == "error") {
          debug_log(paste("Error in", operation, ":", message), 1)
          current_errors <- loading_errors()
          loading_errors(c(current_errors, list(new_entry)))
        } else if (status == "warning") {
          debug_log(paste("Warning in", operation, ":", message), 1)
        } else if (status == "success") {
          debug_log(paste("Success in", operation, sprintf("(%.2fs)", duration)), 2)
        }
      }, error = function(e) {
        debug_log(paste("Error adding to loading log:", e$message), 1)
      })
    }

    #' Cache loaded file
    cache_file <- function(file_name, data_obj, size_mb) {
      telemetry_log("start", "cache_insertion", data_metrics(data_obj), 2)
      duplicate_work("file_loader", "cache_insertion")
      tryCatch({
        current_cache <- file_cache()
        # Keep only last 3 files to manage memory
        if (length(current_cache) >= 3) {
          oldest <- which.min(sapply(current_cache, function(x) x$timestamp))
          current_cache <- current_cache[-oldest]
        }

        current_cache[[file_name]] <- list(
          data = data_obj,
          timestamp = Sys.time(),
          size_mb = size_mb
        )
        file_cache(current_cache)
        debug_log(paste("Cached file:", private_label(file_name), sprintf("(%.1f MB)", size_mb)), 2)
        telemetry_log("end", "cache_insertion", sprintf("cache_hit=false | cached_count=%d | %s",
          length(current_cache), data_metrics(data_obj)), 2)
      }, error = function(e) {
        debug_log(paste("Error caching file:", e$message), 1)
      })
    }

    #' Get cached file
    get_cached_file <- function(file_name) {
      tryCatch({
        current_cache <- file_cache()
        if (file_name %in% names(current_cache)) {
          cached <- current_cache[[file_name]]
          # Check if cache is still valid (within 1 hour)
          if (difftime(Sys.time(), cached$timestamp, units = "hours") < 1) {
            debug_log(paste("Using cached file:", file_name), 2)
            return(cached$data)
          }
        }
        return(NULL)
      }, error = function(e) {
        debug_log(paste("Error retrieving cached file:", e$message), 1)
        return(NULL)
      })
    }

    #' Enhanced file loading with caching and streaming
    load_file_enhanced <- function(file_input, sheet_name = NULL, header_flag = TRUE,
                                   operation_name = "file loading", use_file_cache = TRUE) {

      operation_start_time <- Sys.time()
      telemetry_log("start", "validation", level = 1)
      duplicate_work("file_loader", "load_file_enhanced")
      debug_log(paste("Starting enhanced", operation_name), 1)

      # Enhanced file validation
      if (is.null(file_input) || is.null(file_input$datapath) || !file.exists(file_input$datapath)) {
        error_msg <- "Invalid file input"
        add_loading_log(operation_name, "error", error_msg)
        return(list(success = FALSE, error = error_msg))
      }

      if (!is_supported_datawizard_upload_dw(file_input)) {
        add_loading_log(operation_name, "error", datawizard_unsupported_upload_message_dw)
        return(list(success = FALSE, error = datawizard_unsupported_upload_message_dw))
      }

      # Check the generic file cache first. Excel sheet loads use the dedicated
      # lazy per-sheet caches (all_sheets_*) instead so the same full data frame
      # is not retained in both file_cache and a workbook sheet cache.
      sheet_suffix <- if (!is.null(sheet_name)) paste0("_sheet_", sheet_name) else ""
      cache_key <- paste0(file_input$name, "_", file_input$size, "_", as.character(file_input$size), sheet_suffix)

      debug_log(paste("Cache key for", operation_name, ":", private_label(cache_key)), 2)
      debug_log(paste("Sheet name:", private_label(sheet_name)), 2)

      cached_data <- if (isTRUE(use_file_cache)) get_cached_file(cache_key) else NULL
      if (!is.null(cached_data)) {
        telemetry_log("end", "validation", "status=ok | cache_hit=true", 1)
        telemetry_log("end", "cache_lookup", paste("cache_hit=true |", data_metrics(cached_data)), 1)
        debug_log(paste("Using cached data for sheet:", private_label(sheet_name)), 1)
        duration <- as.numeric(difftime(Sys.time(), operation_start_time, units = "secs"))
        add_loading_log(operation_name, "success", "Loaded from cache", duration)
        return(list(success = TRUE, data = cached_data, message = "Loaded from cache", cache_hit = TRUE))
      } else {
        telemetry_log("end", "cache_lookup", "cache_hit=false", 1)
        debug_log(paste("No cached data found, loading sheet:", private_label(sheet_name)), 1)
      }

      # if (!is.null(cached_data)) {
      #   duration <- as.numeric(difftime(Sys.time(), operation_start_time, units = "secs"))
      #   add_loading_log(operation_name, "success", "Loaded from cache", duration)
      #   return(list(success = TRUE, data = cached_data, message = "Loaded from cache"))
      # }

      # Size validation with detailed feedback
      size_check <- validate_file_size_dw(file_input$datapath)
      telemetry_log("end", "validation", sprintf("status=%s | input_mb=%.3f", size_check$status, size_check$size_mb), 1)
      if (size_check$status == "error") {
        add_loading_log(operation_name, "error", size_check$message)
        return(list(success = FALSE, error = size_check$message))
      } else if (size_check$status == "warning") {
        add_loading_log(operation_name, "warning", size_check$message)
        showNotification(size_check$message, type = "warning", duration = 4)
      }

      # Load file with recovery mechanisms
      parse_phase <- if (!is.null(sheet_name)) "xlsx_parsing" else "delimited_parsing"
      telemetry_log("start", parse_phase, "cache_hit=false", 1)
      result <- tryCatch({
        load_file_with_recovery_dw(file_input, sheet_name, header_flag)
      }, error = function(e) {
        return(list(success = FALSE, error = e$message))
      })

      if (is.list(result) && !is.null(result$data)) {
        telemetry_log("end", parse_phase, paste("cache_hit=false |", data_metrics(result$data)), 1)
        # Cache successful result unless this is managed by the lazy Excel
        # per-sheet cache.
        if (isTRUE(use_file_cache)) {
          cache_file(cache_key, result$data, size_check$size_mb)
        }

        duration <- as.numeric(difftime(Sys.time(), operation_start_time, units = "secs"))
        success_msg <- sprintf("Loaded %d rows x %d columns (%.1f MB)",
                               nrow(result$data), ncol(result$data), size_check$size_mb)
        add_loading_log(operation_name, "success", success_msg, duration)

        return(list(success = TRUE, data = result$data, type = result$type, message = success_msg, cache_hit = FALSE))
      } else {
        error_msg <- if (is.list(result) && !is.null(result$error)) result$error else "Unknown loading error"
        add_loading_log(operation_name, "error", error_msg)
        return(list(success = FALSE, error = error_msg))
      }
    }

    #' Enhanced data processing with header logic
    process_data_with_header <- function(raw_data, header_row_input, operation_name = "data processing") {
      process_data_with_header_dw(raw_data, header_row_input, operation_name, debug_log = debug_log)
    }

    #' Normalize raw cached sheet data before publishing it to loader reactives.
    #'
    #' The all-sheets caches intentionally store raw sheet frames so saved
    #' header-row selections can be replayed after fileInput datapaths expire.
    normalize_cached_sheet_data <- function(cached_data, header_row_input, operation_name) {
      telemetry_log("start", "normalization", data_metrics(cached_data), 1)
      duplicate_work("file_loader", "normalization")
      if (is.null(cached_data) || !is.data.frame(cached_data)) {
        return(list(success = FALSE, error = "Cached sheet is not a valid data frame"))
      }

      raw_data <- as.data.frame(cached_data, check.names = FALSE)
      hr <- suppressWarnings(as.integer(header_row_input %||% 1L))
      if (is.na(hr) || hr < 1L) hr <- 1L

      # Replay saved header-row handling when required; otherwise apply the
      # same clean_and_index normalization used by live loads. Both paths
      # rebuild Row Index from scratch, guaranteeing it appears exactly once.
      if (hr > 1L) {
        result <- process_data_with_header(raw_data, hr, operation_name)
      } else {
        processed_data <- clean_and_index(raw_data)
        result <- list(success = TRUE, data = processed_data)
      }
      telemetry_log("end", "normalization", data_metrics(result$data), 1)
      result
    }

    clear_stale_metadata_for_data <- function(data, context = "Sheet change") {
      if (is.null(core_values) || is.null(data) || !is.data.frame(data)) {
        return(invisible(FALSE))
      }

      current_meta <- core_values$handson_metadata()
      if (is.null(current_meta)) {
        return(invisible(FALSE))
      }

      if (!metadata_matches_dataset(current_meta, data)) {
        if (!is.null(rv) && isTRUE(rv$session_restoring) &&
            restore_has_valid_canonical_pair(tryCatch(rv$data_mod, error = function(e) NULL),
                                             tryCatch(rv$data_def, error = function(e) NULL)) &&
            is_meaningful_metadata(tryCatch(rv$data_def, error = function(e) NULL))) {
          debug_log(paste(context, "Preserving valid canonical restore metadata despite loader-local column mismatch"), 1)
          return(invisible(FALSE))
        }
        debug_log(paste(context, "Clearing metadata due to column mismatch"), 1)
        primary_data_state$set_metadata_for_current_data(NULL)
        if (!is.null(core_values$final_processed_metadata)) {
          core_values$final_processed_metadata(NULL)
        }
        return(invisible(TRUE))
      }

      invisible(FALSE)
    }

    clear_derived_primary_state_for_sheet_change <- function(context = "Sheet change") {
      # Switching sheets is an explicit primary-data context change. Do not block
      # it just because the previous sheet produced modified/processed columns;
      # instead discard derived outputs that no longer belong to the new sheet.
      if (!is.null(core_values)) {
        if (is.function(core_values$filtered_data)) core_values$filtered_data(NULL)
        if (is.function(core_values$filter_applied)) core_values$filter_applied(FALSE)
        if (is.function(core_values$final_processed_data)) core_values$final_processed_data(NULL)
        if (is.function(core_values$final_processed_metadata)) core_values$final_processed_metadata(NULL)
        if (is.function(core_values$apply_triggered)) core_values$apply_triggered(FALSE)
        if (is.function(core_values$data_modified)) core_values$data_modified(FALSE)
        if (is.function(core_values$modification_history)) core_values$modification_history(list())
      }

      if (!is.null(rv)) {
        rv$filter_applied <- FALSE
        rv$filtered_data <- NULL
        rv$filtered_dataset_log <- NULL
        rv$apply_triggered <- FALSE
        rv$final_processed_data <- NULL
        rv$final_processed_metadata <- NULL
        rv$data_modified <- FALSE
        rv$modification_history <- list()
      }

      debug_log(paste(context, "cleared derived primary processing state"), 1)
      invisible(TRUE)
    }


    context_environment <- environment()
    context_exports <- setdiff(
      ls(envir = context_environment, all.names = TRUE),
      c("input", "output", "session", "rv", "debug_level", "debug_log",
        "safe_error_message", "primary_data_state", "context_environment", "context_exports")
    )
    attr(context_environment, "exports") <- context_exports
    context_environment
}
