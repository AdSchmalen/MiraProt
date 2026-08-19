# ============================================================================
# Sub-Script: Data Wizard Auto-Assign Reactive State
# Purpose:
#   Own all reactive objects used by Auto-Assign and provide a single,
#   reusable state container for orchestrator and internal sub-scripts.
# Architectural Role:
#   Source-of-truth state layer. No observer registration and no UI rendering.
# Responsibilities:
#   - Define and initialize all reactive values used by Auto-Assign.
#   - Provide metadata availability reactivity for downstream orchestration.
#   - Provide deterministic state reset behavior for cleanup.
# Non-Responsibilities:
#   - Must not register observe/observeEvent handlers.
#   - Must not call external module APIs directly.
# ============================================================================

create_auto_assign_empty_table_rules <- function() {
  data.frame(
    RuleId = character(),
    Content = character(),
    VariantId = character(),
    Priority = integer(),
    Include = character(),
    Exclude = character(),
    Transformation = character(),
    stringsAsFactors = FALSE
  )
}

create_auto_assign_empty_condition_rules <- function() {
  data.frame(
    RuleId = character(),
    Content = character(),
    VariantId = character(),
    Method = character(),
    Before = character(),
    After = character(),
    Separators = character(),
    Pos = integer(),
    stringsAsFactors = FALSE
  )
}

create_auto_assign_empty_ratio_rules <- function() {
  data.frame(
    RuleId = character(),
    Content = character(),
    VariantId = character(),
    Method = character(),
    Separators = character(),
    Invert = logical(),
    NumBefore = character(),
    NumAfter = character(),
    DenBefore = character(),
    DenAfter = character(),
    NumPos = integer(),
    DenPos = integer(),
    stringsAsFactors = FALSE
  )
}

create_auto_assign_reactive_state <- function(metadata_skeleton, debug_level = 0,
                                              debug_log_fn = NULL,
                                              add_processing_log_fn = NULL) {
  log_fn <- if (is.function(debug_log_fn)) {
    debug_log_fn
  } else {
    function(message, level = 1) {
      debug_auto_assign(message, level = level, debug_level = debug_level)
    }
  }

  processing_log_fn <- if (is.function(add_processing_log_fn)) {
    add_processing_log_fn
  } else {
    function(step, status, message = "", duration = 0) {
      add_processing_log(step = step, status = status, message = message, duration = duration)
    }
  }

  state <- list(
    rv_table_rules_autoassign_dw = reactiveVal(create_auto_assign_empty_table_rules()),
    rv_condition_rules_autoassign_dw = reactiveVal(create_auto_assign_empty_condition_rules()),
    rv_rules_autoassign_dw = reactiveVal(create_auto_assign_empty_ratio_rules()),

    extractedConds_autoassign_dw = reactiveVal(character()),
    rules_loaded_centrally = reactiveVal(FALSE),
    current_loaded_rules = reactiveVal(NULL),
    current_ui_config = reactiveVal(NULL),
    rule_envelope = reactiveVal(NULL),
    provenance_mappings = reactiveVal(list()),
    contrast_mappings = reactiveVal(list()),
    selected_content_rule = reactiveVal(NULL),
    selected_condition_rule = reactiveVal(NULL),
    selected_ratio_rule = reactiveVal(NULL),
    rule_priorities = reactiveVal(integer()),
    required_capabilities = reactiveVal(character()),

    template_loading_in_progress = reactiveVal(FALSE),
    template_export_status = reactiveVal("idle"),
    last_export_info = reactiveVal(NULL),
    last_import_info = reactiveVal(NULL),

    processing_errors = reactiveVal(list()),
    processing_history = reactiveVal(list()),
    last_processing_time = reactiveVal(NULL),
    module_health_status = reactiveVal("OK"),

    previous_metadata_status = reactiveVal(NULL)
  )

  state$metadata_content_ready <- reactive({
    tryCatch({
      if (is.null(metadata_skeleton)) {
        return(FALSE)
      }

      # Resolve the reactive in this reactive expression; the downstream
      # availability checker is intentionally pure.
      current_metadata <- if (is.reactive(metadata_skeleton)) metadata_skeleton() else metadata_skeleton
      is_ready <- check_metadata_content_available_central(current_metadata, debug_level)
      current_status <- isolate(state$previous_metadata_status())

      if (!is.null(current_status) && current_status != is_ready) {
        if (is_ready) {
          log_fn("Metadata content became available - modules can now perform validations", 1)
          processing_log_fn("metadata_status", "success", "Metadata content now available")
        } else {
          log_fn("Metadata content no longer available", 2)
          processing_log_fn("metadata_status", "info", "Metadata content not available")
        }
      }

      state$previous_metadata_status(is_ready)
      return(is_ready)

    }, error = function(e) {
      log_fn(paste("Error checking metadata content status:", e$message), 1)
      processing_log_fn("metadata_status", "error", e$message)
      return(FALSE)
    })
  })

  state$get_metadata_content_status <- reactive({
    list(
      ready = state$metadata_content_ready(),
      timestamp = Sys.time()
    )
  })

  state$reset_all <- function() {
    state$rv_table_rules_autoassign_dw(create_auto_assign_empty_table_rules())
    state$rv_condition_rules_autoassign_dw(create_auto_assign_empty_condition_rules())
    state$rv_rules_autoassign_dw(create_auto_assign_empty_ratio_rules())

    state$extractedConds_autoassign_dw(character())
    state$rules_loaded_centrally(FALSE)
    state$current_loaded_rules(NULL)
    state$current_ui_config(NULL)
    state$rule_envelope(NULL)
    state$provenance_mappings(list())
    state$contrast_mappings(list())
    state$selected_content_rule(NULL)
    state$selected_condition_rule(NULL)
    state$selected_ratio_rule(NULL)
    state$rule_priorities(integer())
    state$required_capabilities(character())

    state$template_loading_in_progress(FALSE)
    state$template_export_status("idle")
    state$last_export_info(NULL)
    state$last_import_info(NULL)

    state$processing_errors(list())
    state$processing_history(list())
    state$last_processing_time(NULL)
    state$module_health_status("OK")
    state$previous_metadata_status(NULL)

    invisible(TRUE)
  }

  state
}
