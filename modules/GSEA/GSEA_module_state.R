# GSEA_module_state.R
#
# Purpose:
#   Defines and initialises all reactive state for the GSEA module through
#   the init_gsea_state() factory function.
#
# Architecture:
#   This file is sourced by GSEA_module.R. init_gsea_state() is called once
#   inside GSEA_module_server() and returns a named list of reactive values.
#   The server then unpacks this list and passes it to init_gsea_observers().
#
# Structure:
#   1. init_gsea_state() - creates all reactiveVal objects and returns them
#      as a named list.
#
# Developer notes:
#   - All reactive state lives here. Do not create additional reactiveVal()
#     calls elsewhere in the module.
#   - The returned list is the single source of truth for module state.
#   - debug_log is passed in so that any initialisation-time messages are
#     consistent with the server's logger.
#   - data access reactives (data_modified, df_data_definition_post_mod, etc.)
#     are included here because they represent derived reactive state that
#     reads from the rv shared values object.

#' Initialise all reactive state for the GSEA module
#'
#' Called once from GSEA_module_server() inside moduleServer(). Returns a
#' named list of all reactive values used by the module.
#'
#' @param rv reactiveValues; the shared application state object.
#' @param debug_log Function(message, level); the server's debug logger.
#' @return Named list of reactive values and reactive expressions.
init_gsea_state <- function(rv, debug_log) {

  # ----------------------------------------------------------
  # Data access reactives
  # ----------------------------------------------------------

  data_signature <- function(data) {
    dimensions <- dim(data)
    if (!is.null(dimensions) && length(dimensions) >= 2) {
      return(paste0(dimensions[1], " rows x ", dimensions[2], " columns"))
    }

    paste0(length(data), " ", paste(class(data), collapse = "/"))
  }

  last_data_mod_signature <- NULL
  data_mod_missing_logged <- FALSE
  last_data_def_signature <- NULL
  data_def_missing_logged <- FALSE

  data_modified <- reactive({
    data_mod <- rv$data_mod

    if (!is.null(data_mod)) {
      current_signature <- data_signature(data_mod)
      if (!identical(current_signature, last_data_mod_signature)) {
        debug_log(paste("Accessing data_mod from rv", current_signature), 3)
        last_data_mod_signature <<- current_signature
      }
      data_mod_missing_logged <<- FALSE
      return(data_mod)
    }

    if (!data_mod_missing_logged) {
      debug_log("No data available", 1)
      data_mod_missing_logged <<- TRUE
      last_data_mod_signature <<- NULL
    }
    NULL
  })

  df_data_definition_post_mod <- reactive({
    data_def <- rv$data_def

    if (!is.null(data_def)) {
      current_signature <- data_signature(data_def)
      if (!identical(current_signature, last_data_def_signature)) {
        debug_log(paste("Accessing data_def from rv", current_signature), 3)
        last_data_def_signature <<- current_signature
      }
      data_def_missing_logged <<- FALSE
      return(data_def)
    }

    if (!data_def_missing_logged) {
      debug_log("No metadata available", 1)
      data_def_missing_logged <<- TRUE
      last_data_def_signature <<- NULL
    }
    NULL
  })

  abundance_validate <- reactive({
    if (!is.null(rv$ab_validate)) return(rv$ab_validate)
    FALSE
  })

  abundance_ratio_validate <- reactive({
    if (!is.null(rv$ratio_validate)) return(rv$ratio_validate)
    FALSE
  })

  imputation_list <- reactive({
    if (!is.null(rv$imputation_list)) return(rv$imputation_list)
    NULL
  })

  # ----------------------------------------------------------
  # Core analysis state
  # ----------------------------------------------------------

  # Stores the complete result wrapper list:
  # list(Results, GeneList, GeneList_FC, source, analysis_metadata)
  res_GSEA <- reactiveVal(NULL)

  # Named list: list(Ranks = ..., FC = ...) from the last ranking computation
  current_rankings <- reactiveVal(NULL)

  # Currently selected pathway name(s)
  selected_enrichment <- reactiveVal(NULL)

  # Current plot height in pixels
  plot_height <- reactiveVal(600)

  # The most recently created ggplot object
  current_plot <- reactiveVal(NULL)

  # Plot settings captured during session restore; observers consume this to
  # regenerate the plot after UI inputs and result state have been restored.
  plot_recreation_state <- reactiveVal(NULL)

  # Per-run worker diagnostics. These values describe the most recent
  # execution only and must never be reused as configuration for a later run.
  last_workers_requested <- reactiveVal(NULL)
  last_workers_effective <- reactiveVal(NULL)

  # Metadata from the last completed analysis
  analysis_metadata <- reactiveVal(NULL)

  # ----------------------------------------------------------
  # Import state
  # ----------------------------------------------------------

  imported_gsea_results <- reactiveVal(NULL)
  import_status_message <- reactiveVal("")

  # ----------------------------------------------------------
  # Return all state as a named list
  # ----------------------------------------------------------

  list(
    # Data access
    data_modified               = data_modified,
    df_data_definition_post_mod = df_data_definition_post_mod,
    abundance_validate          = abundance_validate,
    abundance_ratio_validate    = abundance_ratio_validate,
    imputation_list             = imputation_list,

    # Analysis state
    res_GSEA             = res_GSEA,
    current_rankings     = current_rankings,
    selected_enrichment  = selected_enrichment,
    plot_height          = plot_height,
    current_plot         = current_plot,
    plot_recreation_state = plot_recreation_state,
    last_workers_requested = last_workers_requested,
    last_workers_effective = last_workers_effective,
    analysis_metadata    = analysis_metadata,

    # Import state
    imported_gsea_results = imported_gsea_results,
    import_status_message = import_status_message
  )
}
