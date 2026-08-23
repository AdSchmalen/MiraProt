# ============================================================================
# Module: Data Wizard Auto-Assign
#
# What this module is:
#   The Auto-Assign server/UI entrypoint within Data Wizard. It manages automated
#   metadata assignment rules and template-based import/export of Auto-Assign
#   related configuration.
#
# Why this module exists:
#   To provide one stable integration contract (modAutoAssignUI/modAutoAssignServer)
#   for parent Data Wizard flows while coordinating Auto-Assign rule lifecycle,
#   template processing, and optional integration with sibling modules.
#
# Current structure (WP0 baseline):
#   - This file contains the main module entrypoints plus orchestration logic.
#   - UI composition helpers are sourced from:
#       modules/Data Wizard/auto assign/datawizard_auto_assign_UI.R
#   - Shared utility and resilience helpers are sourced from:
#       modules/Data Wizard/auto assign/datawizard_auto_assign_utils.R
#   - Reactive state source-of-truth is sourced from:
#       modules/Data Wizard/auto assign/datawizard_auto_assign_reactive_state.R
#   - Rule execution engine helpers are sourced from:
#       modules/Data Wizard/auto assign/datawizard_auto_assign_rule_engine.R
#   - Event/observer handler orchestration and shared helpers are sourced from:
#       modules/Data Wizard/auto assign/datawizard_auto_assign_handlers.R
#     with content, condition, ratio, and export registration split across the
#     corresponding datawizard_auto_assign_handlers_*.R peer files.
#   - External module integration adapters are sourced from:
#       modules/Data Wizard/auto assign/datawizard_auto_assign_integration_adapters.R
#   - Template loading/application pipeline is sourced from:
#       modules/Data Wizard/auto assign/datawizard_auto_assign_template_pipeline.R
#   - Output registration helpers are sourced from:
#       modules/Data Wizard/auto assign/datawizard_auto_assign_outputs.R
#   - Legacy edit compatibility helpers live in:
#       modules/Data Wizard/auto assign/datawizard_edit_legacy.R
#
# Architecture note for future WPs:
#   This header must be updated whenever Auto-Assign internal structure changes
#   (for example: new orchestration sub-files, reactive state ownership changes,
#   or moved responsibilities), so it remains a reliable maintainer guide.
#
# Boundaries and contracts:
#   - Keep caller-facing APIs and timing semantics backward compatible.
#   - Treat external Data Wizard modules as black-box integrations.
#   - Keep logging leveled and meaningful; avoid noisy debug output.
# ============================================================================


# Source utility functions and UI components
source("modules/Data Wizard/datawizard_condition_extraction.R", local = modEnv)
source("modules/Data Wizard/auto assign/datawizard_auto_assign_utils.R", local = modEnv)
source("modules/Data Wizard/auto assign/datawizard_auto_assign_UI.R", local = modEnv)
source("modules/Data Wizard/auto assign/datawizard_auto_assign_reactive_state.R", local = modEnv)
source("modules/Data Wizard/auto assign/datawizard_auto_assign_rule_engine.R", local = modEnv)
source("modules/Data Wizard/auto assign/datawizard_auto_assign_handlers_content.R", local = modEnv)
source("modules/Data Wizard/auto assign/datawizard_auto_assign_handlers_conditions.R", local = modEnv)
source("modules/Data Wizard/auto assign/datawizard_auto_assign_handlers_ratios.R", local = modEnv)
source("modules/Data Wizard/auto assign/datawizard_auto_assign_handlers_export.R", local = modEnv)
source("modules/Data Wizard/auto assign/datawizard_auto_assign_handlers.R", local = modEnv)
source("modules/Data Wizard/auto assign/datawizard_auto_assign_integration_adapters.R", local = modEnv)
source("modules/Data Wizard/auto assign/datawizard_auto_assign_template_pipeline.R", local = modEnv)
source("modules/Data Wizard/auto assign/datawizard_auto_assign_outputs.R", local = modEnv)

############
# UI

#' Auto-Assign Assistant Module UI
#' @param id module namespace id
#' @export
modAutoAssignUI <- function(id) {
  build_auto_assign_module_ui(id)
}

############
# Server - Enhanced with Modular Structure

#' Enhanced Auto-Assign Assistant Module Server with Modular Architecture
#'
#' Server logic for automatically assigning metadata and managing templates
#' @param id module namespace id
#' @param metadata_skeleton reactive containing the metadata structure to process
#' @param rule_files reactive containing available rule files
#' @param filter_module reactive containing filter module reference
#' @param edit_module reactive containing edit module reference
#' @param ratios_module reactive containing ratios module reference
#' @param batch_module reactive containing batch effects module reference
#' @param pivot_module reactive containing pivot module reference
#' @param merge_module reactive containing merge module reference
#' @param UI_config reactive containing imputation UI configuration
#' @param debug_level debug level (0=none, 1=critical, 2=verbose)
#' @export
modAutoAssignServer <- function(id, metadata_skeleton, rule_files = NULL,
                                current_data = reactive(NULL),
                                metadata_revision = reactive(NULL),
                                filter_module = NULL, edit_module = NULL,
                                ratios_module = NULL, batch_module = NULL,
                                pivot_module = NULL, merge_module = NULL,
                                imputation_module = NULL, basemean_module = NULL,
                                UI_config = NULL,
                                session_restore_trigger = reactive(NULL),
                                progress_callback = function(stage) invisible(NULL),
                                debug_level = 0) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ========================================
    # Enhanced Debug Management System
    # ========================================

    # Enhanced debug function that respects parent debug level
    debug_log <- function(message, level = 1) {
      rec <- get0(".miraprot_log_record", envir = globalenv(), inherits = FALSE)
      if (is.function(rec)) {
        rec(level, "AUTO ASSIGN", message)
      } else {
        effective_level <- tryCatch(
          get("DEBUG_LEVEL", envir = globalenv(), inherits = FALSE),
          error = function(e) debug_level
        )
        if (is.numeric(effective_level) && effective_level >= level)
          cat(paste0("[ AUTO ASSIGN ", format(Sys.time(), "%H:%M:%S"), " ] ", message), "\n")
      }
    }

    debug_log("Auto-assign module server starting", 2)

    # ========================================
    # Reactive State Ownership (WP6)
    # ========================================

    state <- create_auto_assign_reactive_state(
      metadata_skeleton = metadata_skeleton,
      debug_level = DEBUG_LEVEL,
      debug_log_fn = debug_log,
      add_processing_log_fn = add_processing_log
    )

    rv_table_rules_autoassign_dw <- state$rv_table_rules_autoassign_dw
    rv_condition_rules_autoassign_dw <- state$rv_condition_rules_autoassign_dw
    rv_rules_autoassign_dw <- state$rv_rules_autoassign_dw

    extractedConds_autoassign_dw <- state$extractedConds_autoassign_dw
    rules_loaded_centrally <- state$rules_loaded_centrally
    current_loaded_rules <- state$current_loaded_rules
    current_ui_config <- state$current_ui_config
    rule_envelope <- state$rule_envelope
    provenance_mappings <- state$provenance_mappings
    contrast_mappings <- state$contrast_mappings
    selected_content_rule <- state$selected_content_rule
    selected_condition_rule <- state$selected_condition_rule
    selected_ratio_rule <- state$selected_ratio_rule
    rule_priorities <- state$rule_priorities
    required_capabilities <- state$required_capabilities

    template_loading_in_progress <- state$template_loading_in_progress
    template_export_status <- state$template_export_status
    last_export_info <- state$last_export_info
    last_import_info <- state$last_import_info

    processing_errors <- state$processing_errors
    processing_history <- state$processing_history
    last_processing_time <- state$last_processing_time
    module_health_status <- state$module_health_status

    metadata_content_ready <- state$metadata_content_ready
    get_metadata_content_status <- state$get_metadata_content_status

    # ========================================
    # Auto-Convert Logic
    # ========================================

    # Most robust solution using isolate() to prevent unwanted reactivity
    get_auto_convert_state <- function(session_input, input_name, default = TRUE) {
      isolate({
        tryCatch({
          value <- session_input[[input_name]]
          if (is.null(value)) {
            return(default)
          }
          return(isTRUE(value))
        }, error = function(e) {
          return(default)
        })
      })
    }

    # ========================================
    # Enhanced Collection Functions with Error Handling
    # ========================================

    integration_adapters <- create_auto_assign_integration_adapters(environment())

    collect_batch_effects_ui_state <- integration_adapters$collect_batch_effects_ui_state
    collect_pivot_ui_state <- integration_adapters$collect_pivot_ui_state
    apply_pivot_ui_config <- integration_adapters$apply_pivot_ui_config
    get_pivot_state <- integration_adapters$get_pivot_state
    collect_merge_ui_state <- integration_adapters$collect_merge_ui_state
    collect_filter_ui_state <- integration_adapters$collect_filter_ui_state
    collect_ratio_configurations <- integration_adapters$collect_ratio_configurations
    collect_basemean_configurations <- integration_adapters$collect_basemean_configurations
    collect_edit_operations <- integration_adapters$collect_edit_operations
    collect_imputation_ui_config <- integration_adapters$collect_imputation_ui_config
    apply_filter_template <- integration_adapters$apply_filter_template
    apply_ratio_configurations <- integration_adapters$apply_ratio_configurations
    apply_edit_operations <- integration_adapters$apply_edit_operations
    apply_imputation_ui_config <- integration_adapters$apply_imputation_ui_config
    get_imputation_state <- integration_adapters$get_imputation_state

    # ========================================
    # Enhanced Central Rule Loading Interface
    # ========================================

    template_pipeline <- create_auto_assign_template_pipeline(environment())
    load_rules_directly <- template_pipeline$load_rules_directly

    # ========================================
    # Observer Registration (Orchestrator Wiring)
    # ========================================

    # ========================================
    # Enhanced Rule Management Functions
    # ========================================

    rule_engine <- create_auto_assign_rule_engine(
      debug_log = debug_log,
      add_processing_log = add_processing_log,
      rv_table_rules_autoassign_dw = rv_table_rules_autoassign_dw,
      rv_condition_rules_autoassign_dw = rv_condition_rules_autoassign_dw,
      rv_rules_autoassign_dw = rv_rules_autoassign_dw,
      rules_loaded_centrally = rules_loaded_centrally,
      extractedConds_autoassign_dw = extractedConds_autoassign_dw,
      progress_callback = progress_callback,
      debug_level = DEBUG_LEVEL
    )

    apply_rule_autoassign_dw <- rule_engine$apply_rule_autoassign_dw
    apply_condition_autoassign_dw <- rule_engine$apply_condition_autoassign_dw
    apply_ratio_rules_fixed <- rule_engine$apply_ratio_rules_fixed
    apply_auto_assign_rules <- rule_engine$apply_auto_assign_rules

    # ========================================
    # Enhanced Event Handlers with Error Prevention
    # ========================================

    register_auto_assign_handlers(environment())

    apply_rule_autoassign_dw <- rule_engine$apply_rule_autoassign_dw
    apply_condition_autoassign_dw <- rule_engine$apply_condition_autoassign_dw
    apply_ratio_rules_fixed <- rule_engine$apply_ratio_rules_fixed
    apply_auto_assign_rules <- rule_engine$apply_auto_assign_rules

    # ========================================
    # Enhanced Template Status Display
    # ========================================

    register_auto_assign_outputs(environment())

    # ========================================
    # Enhanced Session Cleanup
    # ========================================

    # Register cleanup function
    cleanup_manager$register_module("Auto-assign", function() {
      debug_log("Executing [Auto-assign] cleanup", 2)

      state$reset_all()

      debug_log("[Auto-assign] cleanup completed", 2)
    })

    # ========================================
    # Session-restore bridge
    # ========================================
    autoassign_session_state_base <- create_submodule_session_state(
      session      = session,
      input        = input,
      input_specs  = list(
        # Content rules
        lookup_content_dw              = "selectInput",
        transformation_col_dw          = "selectInput",
        string_include_autoassign_dw   = "textInput",
        string_exclude_autoassign_dw   = "textInput",
        auto_convert_content_regex_dw  = "checkboxInput",
        # Condition extraction rules
        cond_content_autoassign_dw     = "selectInput",
        cond_method_autoassign_dw      = "selectInput",
        cond_before_autoassign_dw      = "textInput",
        cond_pos_autoassign_dw         = "numericInput",
        cond_after_autoassign_dw       = "textInput",
        cond_sep_chars_autoassign_dw   = "checkboxGroupInput",
        auto_convert_sample_regex_dw   = "checkboxInput",
        # Ratio analysis rules
        new_content_autoassign_dw      = "selectInput",
        new_method_autoassign_dw       = "selectInput",
        new_num_pos_autoassign_dw      = "numericInput",
        new_den_pos_autoassign_dw      = "numericInput",
        new_sep_chars_autoassign_dw    = "checkboxGroupInput",
        new_invert_autoassign_dw       = "checkboxInput",
        new_num_before_autoassign_dw   = "textInput",
        new_num_after_autoassign_dw    = "textInput",
        new_den_before_autoassign_dw   = "textInput",
        new_den_after_autoassign_dw    = "textInput",
        auto_convert_regex_dw          = "checkboxInput"
      ),
      module_label = "AutoAssign",
      # Persist the three rule-set queue tables and related book-keeping
      # reactiveVals. These drive DT outputs directly, so we write them back
      # synchronously on restore (via apply_extra) rather than through
      # updateInput calls.
      get_extra = function() {
        tryCatch({
          list(
            table_rules        = isolate(rv_table_rules_autoassign_dw()),
            condition_rules    = isolate(rv_condition_rules_autoassign_dw()),
            ratio_rules        = isolate(rv_rules_autoassign_dw()),
            extracted_conds    = isolate(extractedConds_autoassign_dw()),
            rules_loaded       = isolate(rules_loaded_centrally()),
            current_loaded     = isolate(current_loaded_rules()),
            current_ui_config  = isolate(current_ui_config()),
            rule_envelope      = isolate(rule_envelope()),
            provenance_mappings = isolate(provenance_mappings()),
            contrast_mappings  = isolate(contrast_mappings()),
            selected_content_rule = isolate(selected_content_rule()),
            selected_condition_rule = isolate(selected_condition_rule()),
            selected_ratio_rule = isolate(selected_ratio_rule()),
            rule_priorities    = isolate(rule_priorities()),
            required_capabilities = isolate(required_capabilities())
          )
        }, error = function(e) {
          debug_log(paste("Error snapshotting auto-assign rule sets:",
                          e$message), 1)
          NULL
        })
      },
      apply_extra = function(extra) {
        shiny::isolate({
          if (!is.list(extra)) return(invisible(NULL))
          payload <- if (is.list(extra$rule_envelope) && is.list(extra$rule_envelope$rules)) {
            c(extra$rule_envelope$rules, list(
              provenance=extra$rule_envelope$provenance,
              contrast_mappings=extra$rule_envelope$contrasts,
              required_capabilities=extra$rule_envelope$required_capabilities))
          } else if (all(vapply(extra[c("table_rules", "condition_rules", "ratio_rules")], is.data.frame, logical(1)))) {
            list(table=extra$table_rules, condition=extra$condition_rules, ratio=extra$ratio_rules,
              provenance=extra$provenance_mappings, contrast_mappings=extra$contrast_mappings,
              required_capabilities=extra$required_capabilities)
          } else NULL
          if (!is.null(payload) && !isTRUE(load_rules_directly(payload, notify=FALSE)))
            stop("Auto-Assign aggregate restore was rejected")
          if (is.character(extra$extracted_conds)) extractedConds_autoassign_dw(extra$extracted_conds)
          if (!is.null(extra$current_ui_config)) current_ui_config(extra$current_ui_config)
          # Restore selections only when their stable identities still exist.
          selections <- list(content=extra$selected_content_rule,
            condition=extra$selected_condition_rule, ratio=extra$selected_ratio_rule)
          frames <- list(content=rv_table_rules_autoassign_dw(),
            condition=rv_condition_rules_autoassign_dw(), ratio=rv_rules_autoassign_dw())
          setters <- list(content=selected_content_rule, condition=selected_condition_rule,
            ratio=selected_ratio_rule)
          for (kind in names(selections)) if (length(selections[[kind]]) == 1L &&
              selections[[kind]] %in% frames[[kind]]$RuleId) setters[[kind]](selections[[kind]])
          invisible(NULL)
        })
      },
      restore_trigger = session_restore_trigger
    )

    get_session_state_fn <- function() {
      autoassign_session_state_base$get_session_state()
    }

    set_session_state_fn <- function(state) {
      if (is.null(state) || !is.list(state)) {
        return(invisible(NULL))
      }

      # The submodule aggregate callback is the sole restore reader.
      # Keep legacy path active for snapshots without current_loaded.
      autoassign_session_state_base$set_session_state(state)
      invisible(NULL)
    }

    # ========================================
    # Enhanced Return Interface (same as original)
    # ========================================

    return(list(
      # Session-restore bridge
      get_session_state = get_session_state_fn,
      set_session_state = set_session_state_fn,

      # Enhanced imputation UI configuration management
      collect_imputation_ui_config = collect_imputation_ui_config,
      apply_imputation_ui_config = apply_imputation_ui_config,
      get_imputation_state = get_imputation_state,

      # Enhanced status with imputation details
      get_enhanced_template_status = reactive({
        tryCatch({
          # Get basic status
          basic_status <- get_template_status()

          # Add enhanced imputation information
          imp_state <- get_imputation_state(imputation_module)
          basic_status$imputation_state <- imp_state
          basic_status$has_imputation_applied <- isTRUE(imp_state$applied)
          basic_status$imputation_method <- imp_state$method %||% "None"
          basic_status$imputation_columns_count <- length(imp_state$columns %||% character(0))

          return(basic_status)

        }, error = function(e) {
          debug_log(paste("Error getting enhanced template status:", e$message), 1)
          # Fallback to basic status
          return(get_template_status())
        })
      }),

      # Core functionality (unchanged)
      load_rules_directly = load_rules_directly,
      extract_conditions_from_rules = reactive({
        tryCatch({
          extracted_conditions <- character()

          condition_rules <- rv_condition_rules_autoassign_dw()

          if (nrow(condition_rules) > 0) {
            metadata_current <- metadata_skeleton()
            if (!is.null(metadata_current) && nrow(metadata_current) > 0) {
              for (i in seq_len(nrow(condition_rules))) {
                rule <- condition_rules[i, , drop = TRUE]
                temp_extracted <- extract_sample_from_column_rule(metadata_current, rule)
                if (length(temp_extracted) > 0) {
                  extracted_conditions <- c(extracted_conditions, temp_extracted)
                }
              }
            }
          }

          extracted_conditions <- unique(extracted_conditions)
          extracted_conditions <- extracted_conditions[!is.na(extracted_conditions) & nzchar(extracted_conditions)]

          extractedConds_autoassign_dw(extracted_conditions)
          return(extracted_conditions)

        }, error = function(e) {
          debug_log(paste("Error extracting conditions:", e$message), 1)
          add_processing_log("extract_conditions", "error", e$message)
          return(character())
        })
      }),
      apply_rules = apply_auto_assign_rules,

      # Rule sets
      table_rules = rv_table_rules_autoassign_dw,
      condition_rules = rv_condition_rules_autoassign_dw,
      ratio_rules = rv_rules_autoassign_dw,

      # Extracted conditions
      extracted_conditions = extractedConds_autoassign_dw,

      # Template management
      get_filter_template_state = reactive({
        tryCatch({
          collect_filter_ui_state(filter_module)
        }, error = function(e) {
          debug_log(paste("Error getting filter template state:", e$message), 1)
          NULL
        })
      }),
      collect_filter_ui_state = collect_filter_ui_state,
      apply_filter_template = apply_filter_template,
      template_loading_in_progress = template_loading_in_progress,

      # Edit operations management
      collect_edit_operations = collect_edit_operations,
      apply_edit_operations = apply_edit_operations,
      get_edit_operations_state = reactive({
        tryCatch({
          collect_edit_operations()
        }, error = function(e) {
          debug_log(paste("Error getting edit operations state:", e$message), 1)
          create_empty_structure("operations")
        })
      }),

      # Ratio configurations management
      collect_ratio_configurations = collect_ratio_configurations,
      apply_ratio_configurations = apply_ratio_configurations,
      get_ratio_configurations_state = reactive({
        tryCatch({
          collect_ratio_configurations(ratios_module)
        }, error = function(e) {
          debug_log(paste("Error getting ratio configurations state:", e$message), 1)
          create_empty_structure("ratio_configurations")
        })
      }),

      # Imputation UI configuration management
      get_current_ui_config = reactive({ current_ui_config() }),
      set_current_ui_config = function(ui_config) { current_ui_config(ui_config) },
      collect_imputation_ui_config = collect_imputation_ui_config,
      apply_imputation_ui_config = apply_imputation_ui_config,

      # Enhanced status functions
      has_rules = reactive({
        rules_loaded_centrally() ||
          nrow(rv_table_rules_autoassign_dw()) > 0 ||
          nrow(rv_condition_rules_autoassign_dw()) > 0 ||
          nrow(rv_rules_autoassign_dw()) > 0
      }),
      rules_loaded_centrally = rules_loaded_centrally,

      # Enhanced template management status
      get_template_status = reactive({
        tryCatch({
          edit_ops <- collect_edit_operations()
          ratio_configs <- collect_ratio_configurations(ratios_module)

          list(
            export_status = template_export_status(),
            loading_in_progress = template_loading_in_progress(),
            last_export = last_export_info(),
            last_import = last_import_info(),
            has_filter_config = !is.null(collect_filter_ui_state(filter_module)),
            has_imputation_config = !is.null(collect_imputation_ui_config()),
            has_edit_operations = !is.null(edit_ops) && nrow(edit_ops) > 0,
            edit_operations_count = if (!is.null(edit_ops)) nrow(edit_ops) else 0,
            edit_operations_pending = if (!is.null(edit_ops) && "Executed" %in% names(edit_ops)) sum(!edit_ops$Executed, na.rm = TRUE) else 0,
            has_ratio_configurations = !is.null(ratio_configs) && nrow(ratio_configs) > 0,
            ratio_configurations_count = if (!is.null(ratio_configs)) nrow(ratio_configs) else 0,
            module_health = module_health_status(),
            debug_level = DEBUG_LEVEL
          )
        }, error = function(e) {
          debug_log(paste("Error getting template status:", e$message), 1)
          list(
            export_status = "error",
            loading_in_progress = FALSE,
            last_export = NULL,
            last_import = NULL,
            has_filter_config = FALSE,
            has_imputation_config = FALSE,
            has_edit_operations = FALSE,
            edit_operations_count = 0,
            edit_operations_pending = 0,
            has_ratio_configurations = FALSE,
            ratio_configurations_count = 0,
            module_health = "Error",
            debug_level = DEBUG_LEVEL,
            error = e$message
          )
        })
      }),

      # Module UI state collections
      collect_batch_effects_ui_state = collect_batch_effects_ui_state,
      collect_pivot_ui_state = collect_pivot_ui_state,
      apply_pivot_ui_config = apply_pivot_ui_config,
      get_pivot_state = get_pivot_state,
      collect_merge_ui_state = collect_merge_ui_state,

      # Enhanced status with pivot details
      get_enhanced_template_status_with_pivot = reactive({
        tryCatch({
          # Get basic status
          basic_status <- get_template_status()

          # Add enhanced pivot information
          pivot_state <- get_pivot_state(pivot_module)
          basic_status$pivot_state <- pivot_state
          basic_status$pivot_type <- pivot_state$pivot_type_dw %||% "wider"
          basic_status$pivot_data <- pivot_state$pivot_data_dw %||% "primary"
          basic_status$pivot_options_count <- length(pivot_state$pivot_options %||% list())

          return(basic_status)

        }, error = function(e) {
          debug_log(paste("Error getting enhanced template status with pivot:", e$message), 1)
          # Fallback to basic status
          return(get_template_status())
        })
      }),

      # Utility functions
      create_empty_structure = create_empty_structure,
      collect_module_ui_state = collect_module_ui_state,
      validate_module_reference = validate_module_reference,
      get_module_safely = get_module_safely,

      # Enhanced error tracking and performance monitoring
      processing_errors = reactive({ processing_errors() }),
      get_processing_errors = function() { processing_errors() },
      clear_processing_errors = function() {
        tryCatch({
          processing_errors(list())
          debug_log("Processing errors cleared", 2)
        }, error = function(e) {
          debug_log(paste("Error clearing processing errors:", e$message), 1)
        })
      },

      # Enhanced performance monitoring
      get_performance_metrics = reactive({
        tryCatch({
          list(
            last_processing_time = last_processing_time(),
            processing_history = processing_history(),
            debug_level = DEBUG_LEVEL,
            module_health = module_health_status(),
            error_count = length(processing_errors())
          )
        }, error = function(e) {
          debug_log(paste("Error getting performance metrics:", e$message), 1)
          list(
            last_processing_time = NULL,
            processing_history = list(),
            debug_level = DEBUG_LEVEL,
            module_health = "Error",
            error_count = 0
          )
        })
      }),

      # Central metadata content status management
      metadata_content_ready = metadata_content_ready,
      get_metadata_content_status = get_metadata_content_status,
      check_metadata_content_available = function(metadata_df = NULL) {
        # Callers must resolve reactive metadata in their observer/reactive
        # context and pass the resulting frame to this pure boundary.
        if (is.null(metadata_df)) return(FALSE)
        check_metadata_content_available_central(metadata_df, DEBUG_LEVEL)
      }
    ))
  })
}
