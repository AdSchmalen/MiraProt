# ============================================================================
# MiraProt File Contract: modules/Data Wizard/core/datawizard_core_ui_configuration.R
# Purpose:
#   Provide the core ui configuration portion of the Data Wizard without changing public behavior.
# Architectural Role:
#   Core implementation unit loaded by the historical datawizard_core.R compatibility entry point.
# Responsibilities:
#   Define only the focused functions or composition wiring named by this file.
# Non-Responsibilities:
#   Do not redefine public APIs, create parallel state owners, or change workflow semantics.
# Main Interface:
#   Top-level functions defined here, or compatibility symbols exposed by its ordered sources.
# Dependencies:
#   MiraProt Data Wizard helpers and injected Shiny/package services used by those functions.
# State Ownership:
#   Core reactive containers or helpers explicitly created by this unit; canonical datasets remain owned by the registry/core adapters.
# Mutation Authority:
#   Only returned setters and registered lifecycle observers may mutate the core state passed to them.
# Source-Order Assumptions:
#   Source through datawizard_core.R; sibling order there supplies utility and adapter definitions before dependent factories.
# Session/Restore Implications:
#   Restore uses the unchanged core factories and state keys; this unit must not add a second restore owner.
# Important Invariants:
#   Preserve Section B symbols/returns, unchanged public APIs, one loader/Tables
#   context per module session, source-DAG acyclicity, and existing timing guards.
# ============================================================================

#' Create UI configuration management functions
#' @param ui_config_values list of UI config reactive values
#' @param core_values list of core reactive values
#' @return list of UI config management functions
create_ui_config_management_functions <- function(ui_config_values, core_values) {

  # Generic UI config setter
  set_ui_config_generic <- function(ui_config, config_type, central_config_rv,
                                    source_rv, update_in_progress_rv) {
    if (update_in_progress_rv()) {
      debug_log(paste(config_type, "config update in progress, skipping"), level = 2)
      return(TRUE)
    }

    if (is.null(ui_config)) {
      central_config_rv(NULL)
      source_rv("none")
      return(TRUE)
    }

    tryCatch({
      update_in_progress_rv(TRUE)

      if (!is.list(ui_config)) {
        debug_log(paste(config_type, "UI config is not a list"), level = 1)
        update_in_progress_rv(FALSE)
        return(FALSE)
      }

      central_config_rv(ui_config)
      source_rv("import")
      core_values$last_config_application_time(Sys.time())

      debug_log(paste(config_type, "UI config set from import"), level = 2)
      update_in_progress_rv(FALSE)
      return(TRUE)

    }, error = function(e) {
      debug_log(paste("Error setting", config_type, "UI config:", e$message), level = 1)
      update_in_progress_rv(FALSE)
      return(FALSE)
    })
  }

  # Generic UI config creator
  create_ui_config_generic <- function(config_name, central_config, rules_getter, default_config) {
    if (!is.null(central_config)) {
      debug_log(paste("Using central", config_name, "config"), level = 2)
      return(central_config)
    }

    if (!is.null(rules_getter)) {
      rules_config <- safe_module_call(rules_getter, default_return = NULL,
                                       context = paste("assign_rules", config_name, sep = "_"))
      if (!is.null(rules_config)) {
        debug_log(paste("Using", config_name, "config from assign_rules"), level = 2)
        return(rules_config)
      }
    }

    debug_log(paste("Using default", config_name, "config"), level = 2)
    return(default_config)
  }

  list(
    # Imputation UI config functions
    set_imputation_ui_config_from_import = function(ui_config) {
      if (ui_config_values$ui_config_update_in_progress()) {
        debug_log("UI config update in progress, skipping to prevent circular dependency", level = 2)
        return(TRUE)
      }

      if (is.null(ui_config)) {
        ui_config_values$central_imputation_ui_config(NULL)
        ui_config_values$ui_config_source("none")
        return(TRUE)
      }

      tryCatch({
        ui_config_values$ui_config_update_in_progress(TRUE)

        if (!is.list(ui_config)) {
          core_values$ui_config_errors(append(core_values$ui_config_errors(), "Imputation UI config is not a list"))
          ui_config_values$ui_config_update_in_progress(FALSE)
          return(FALSE)
        }

        required_fields <- c("imputation_method_select", "imputation_column_select")
        missing_fields <- setdiff(required_fields, names(ui_config))
        if (length(missing_fields) > 0) {
          core_values$ui_config_errors(append(core_values$ui_config_errors(),
                                              paste("Missing imputation config fields:", paste(missing_fields, collapse = ", "))))
          ui_config_values$ui_config_update_in_progress(FALSE)
          return(FALSE)
        }

        ui_config_values$central_imputation_ui_config(ui_config)
        ui_config_values$ui_config_source("import")
        core_values$last_config_application_time(Sys.time())

        debug_log("Imputation UI config set from import", level = 2)
        ui_config_values$ui_config_update_in_progress(FALSE)
        return(TRUE)

      }, error = function(e) {
        debug_log(paste("Error setting imputation UI config:", e$message), level = 1)
        core_values$ui_config_errors(append(core_values$ui_config_errors(), paste("Import imputation config error:", e$message)))
        ui_config_values$ui_config_update_in_progress(FALSE)
        return(FALSE)
      })
    },

    # Filtering UI config functions
    set_filtering_ui_config_from_import = function(ui_config) {
      if (ui_config_values$filtering_update_in_progress()) {
        debug_log("Filtering config update in progress, skipping to prevent circular dependency", level = 2)
        return(TRUE)
      }

      if (is.null(ui_config)) {
        ui_config_values$central_filtering_ui_config(NULL)
        ui_config_values$filtering_ui_config_source("none")
        return(TRUE)
      }

      tryCatch({
        ui_config_values$filtering_update_in_progress(TRUE)

        if (!is.list(ui_config)) {
          core_values$filtering_config_errors(append(core_values$filtering_config_errors(), "Filtering UI config is not a list"))
          ui_config_values$filtering_update_in_progress(FALSE)
          return(FALSE)
        }

        # Validate custom filters if present
        if (!is.null(ui_config$custom)) {
          if (!is.data.frame(ui_config$custom)) {
            core_values$filtering_config_errors(append(core_values$filtering_config_errors(), "Custom filters section is not a data frame"))
            ui_config_values$filtering_update_in_progress(FALSE)
            return(FALSE)
          }
        }

        ui_config_values$central_filtering_ui_config(ui_config)
        ui_config_values$filtering_ui_config_source("import")
        core_values$last_config_application_time(Sys.time())

        debug_log("Filtering UI config set from import", level = 2)
        ui_config_values$filtering_update_in_progress(FALSE)
        return(TRUE)

      }, error = function(e) {
        debug_log(paste("Error setting filtering UI config:", e$message), level = 1)
        core_values$filtering_config_errors(append(core_values$filtering_config_errors(), paste("Import filtering config error:", e$message)))
        ui_config_values$filtering_update_in_progress(FALSE)
        return(FALSE)
      })
    },

    # Generic setters for other modules
    set_ratios_ui_config_from_import = function(ui_config) {
      return(set_ui_config_generic(ui_config, "ratios", ui_config_values$central_ratios_ui_config,
                                   ui_config_values$ratios_ui_config_source, ui_config_values$ratios_update_in_progress))
    },

    set_batch_effects_ui_config_from_import = function(ui_config) {
      return(set_ui_config_generic(ui_config, "batch_effects", ui_config_values$central_batch_effects_ui_config,
                                   ui_config_values$batch_effects_ui_config_source, ui_config_values$batch_effects_update_in_progress))
    },

    set_pivot_ui_config_from_import = function(ui_config) {
      return(set_ui_config_generic(ui_config, "pivot", ui_config_values$central_pivot_ui_config,
                                   ui_config_values$pivot_ui_config_source, ui_config_values$pivot_update_in_progress))
    },

    set_merge_ui_config_from_import = function(ui_config) {
      return(set_ui_config_generic(ui_config, "merge", ui_config_values$central_merge_ui_config,
                                   ui_config_values$merge_ui_config_source, ui_config_values$merge_update_in_progress))
    },

    set_basemean_ui_config_from_import = function(ui_config) {
      return(set_ui_config_generic(
        ui_config,
        "basemean",
        ui_config_values$central_basemean_ui_config,
        ui_config_values$basemean_ui_config_source,
        ui_config_values$basemean_update_in_progress
      ))
    },

    # UI config creators
    create_imputation_ui_config = function(assign_rules_out) {
      reactive({
        central_config <- ui_config_values$central_imputation_ui_config()
        if (!is.null(central_config)) {
          debug_log(paste("Using central imputation config from", ui_config_values$ui_config_source()), level = 2)
          return(central_config)
        }

        if (!is.null(assign_rules_out$get_imputation_ui_config)) {
          rules_config <- safe_module_call(assign_rules_out$get_imputation_ui_config,
                                           default_return = NULL,
                                           context = "assign_rules_imputation")
          if (!is.null(rules_config)) {
            debug_log("Using imputation config from assign_rules", level = 2)
            return(rules_config)
          }
        }

        default_config <- list(
          imputation_method_select = "None",
          imputation_column_select = character(0)
        )
        debug_log("Using default imputation config", level = 2)
        return(default_config)
      })
    },

    create_filtering_ui_config = function(assign_rules_out) {
      reactive({
        central_config <- ui_config_values$central_filtering_ui_config()
        if (!is.null(central_config)) {
          debug_log(paste("Using central filtering config from", ui_config_values$filtering_ui_config_source()), level = 2)

          validated_config <- validate_filtering_config_structure(central_config)
          if (!is.null(validated_config)) {
            return(validated_config)
          }
        }

        if (!is.null(assign_rules_out$get_filtering_ui_config)) {
          rules_config <- safe_module_call(assign_rules_out$get_filtering_ui_config,
                                           default_return = NULL,
                                           context = "assign_rules_filtering")
          if (!is.null(rules_config)) {
            debug_log("Using filtering config from assign_rules", level = 2)

            validated_config <- validate_filtering_config_structure(rules_config)
            if (!is.null(validated_config)) {
              return(validated_config)
            }
          }
        }

        default_config <- list(
          confidence = list(
            numeric_fdr_dw = FALSE,
            string_fdr_dw = FALSE,
            numeric_input_dw_max = NULL,
            numeric_input_dw = NULL,
            string_input_dw = ""
          ),
          valid_values = list(
            valid_filtering_group_dw = "In total",
            valid_filtering_value_dw = 1
          ),
          custom = data.frame(
            Column = character(),
            Operator_1 = character(),
            Value_1 = character(),
            Logic = character(),
            Operator_2 = character(),
            Value_2 = character(),
            Empty_Filter = character(),
            Multi_Column_Logic = character(),
            stringsAsFactors = FALSE
          )
        )
        debug_log("Using default filtering config", level = 2)
        return(default_config)
      })
    },

    # Generic config creators for other modules
    create_ratios_ui_config = function(assign_rules_out) {
      reactive({
        return(create_ui_config_generic("ratios", ui_config_values$central_ratios_ui_config(),
                                        assign_rules_out$get_ratios_ui_config,
                                        list(ratio_settings = list(custom_col_sel = "Normalized Abundance",
                                                                   statistics_sel = "Limma", adjust_sel = "FDR",
                                                                   column_prefix = "Ratio_"),
                                             ratio_configurations = data.frame())))
      })
    },

    create_batch_effects_ui_config = function(assign_rules_out) {
      reactive({
        return(create_ui_config_generic("batch_effects", ui_config_values$central_batch_effects_ui_config(),
                                        assign_rules_out$get_batch_effects_ui_config,
                                        list(batch_method = "ComBat", imputation_method_batch = "None",
                                             transformation_batch = "None", remove_imputed_batch = FALSE,
                                             batch_counter = 2, batch_inputs = list())))
      })
    },

    create_pivot_ui_config = function(assign_rules_out) {
      reactive({
        # First try central config (from import)
        central_config <- ui_config_values$central_pivot_ui_config()
        if (!is.null(central_config)) {
          debug_log(paste("Using central pivot config from", ui_config_values$pivot_ui_config_source()), level = 2)
          return(central_config)
        }

        # Try assign_rules module
        if (!is.null(assign_rules_out$get_pivot_ui_config)) {
          rules_config <- safe_module_call(assign_rules_out$get_pivot_ui_config,
                                           default_return = NULL,
                                           context = "assign_rules_pivot")
          if (!is.null(rules_config)) {
            debug_log("Using pivot config from assign_rules", level = 2)
            return(rules_config)
          }
        }

        # Default config
        debug_log("Using default pivot config", level = 2)
        return(list(
          pivot_data_dw = "primary",
          pivot_type_dw = "wider",
          pivot_options = list()
        ))
      })
    },

    create_merge_ui_config = function(assign_rules_out) {
      reactive({
        return(create_ui_config_generic("merge", ui_config_values$central_merge_ui_config(),
                                        assign_rules_out$get_merge_ui_config,
                                        list(file1_col = NULL, file2_col = NULL,
                                             file2_add_col = character(0), join_type = "left")))
      })
    },

    create_basemean_ui_config = function(assign_rules_out) {
      reactive({
        central_config <- ui_config_values$central_basemean_ui_config()
        if (!is.null(central_config)) {
          debug_log(paste("Using central basemean config from", ui_config_values$basemean_ui_config_source()), level = 2)
          return(central_config)
        }

        if (!is.null(assign_rules_out$get_basemean_ui_config)) {
          rules_config <- safe_module_call(assign_rules_out$get_basemean_ui_config,
                                           default_return = NULL,
                                           context = "assign_rules_basemean")
          if (!is.null(rules_config)) {
            debug_log("Using basemean config from assign_rules", level = 2)
            return(rules_config)
          }
        }

        default_config <- list(
          abundance_type = "Normalized Abundance",
          samples = character(0),
          suffix = ""
        )
        debug_log("Using default basemean config", level = 2)
        return(default_config)
      })
    },

    create_annotation_ui_config = function(assign_rules_out) {
      reactive({ NULL })
    }

  )
}
