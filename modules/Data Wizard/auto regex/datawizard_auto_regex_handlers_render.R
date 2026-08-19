# ============================================================================
# Module: Data Wizard Auto Regex rendering and lifecycle registrar
# Purpose: Register presentation outputs, panel toggles, and session cleanup.
# Owns: render/download handlers, bounded diagnostics, output suspension,
# panel presentation state transitions, and the single cleanup callback.
# Does not own: source/run/transfer events, inference, or state allocation.
# ============================================================================

auto_regex_register_render_handlers <- function(context) {
  list2env(unclass(context), envir = environment())
  output$global_redundancy_control <-
    shiny::renderUI({

      shiny::div(
        title = paste(
          "Default number of safely removed regex edge units to restore",
          "after the minimal equivalent Content regex has been found.",
          "Individual Content rules can override this value after inference."
        ),
        shiny::numericInput(
          ns("redundancy"),
          "Regex redundancy:",
          value =
            auto_regex_redundancy_value(
              state$global_redundancy(),
              fallback = 0L
            ),
          min = 0,
          max = 10,
          step = 1
        )
      )
    })

  output$content_redundancy_controls <-
    shiny::renderUI({

      rules <-
        state$candidate_rules()

      table <- if (is.list(rules)) {
        rules$table
      } else {
        NULL
      }

      if (!is.data.frame(table) ||
          !nrow(table)) {
        return(NULL)
      }

      overrides <-
        state$redundancy_overrides()

      editable <- which(
        chr(table$Content) !=
          "Row Index" &
          (
            is.na(table$Priority) |
              table$Priority > 0L
          )
      )

      if (!length(editable))
        return(NULL)

      shiny::tagList(
        shiny::tags$div(
          style = paste(
            "margin-bottom: 8px;",
            "font-size: 90%;"
          ),
          shiny::tags$strong(
            "Per-regex overrides"
          ),
          shiny::tags$span(
            class = "text-muted",
            paste(
              " — Use global leaves the rule attached",
              "to the Regex redundancy setting above."
            )
          )
        ),

        lapply(
          editable,
          function(i) {

            rule_id <- chr(
              table$RuleId[[i]]
            )

            selected <-
              if (length(overrides) &&
                  !is.null(
                    names(overrides)
                  ) &&
                  rule_id %in%
                  names(overrides)) {

                as.character(
                  overrides[[rule_id]]
                )

              } else {

                "global"
              }

            choices <- c(
              "Use global" = "global",
              stats::setNames(
                as.character(0:10),
                as.character(0:10)
              )
            )

            regex_preview <- chr(
              table$Include[[i]]
            )

            if (nchar(
              regex_preview,
              type = "chars"
            ) > 140L) {

              regex_preview <- paste0(
                substr(
                  regex_preview,
                  1L,
                  137L
                ),
                "..."
              )
            }

            shiny::fluidRow(
              style = paste(
                "border-top: 1px solid #eee;",
                "padding-top: 6px;",
                "padding-bottom: 4px;"
              ),

              shiny::column(
                8,
                shiny::tags$strong(
                  chr(
                    table$Content[[i]]
                  )
                ),
                shiny::tags$div(
                  class = "text-muted",
                  style = paste(
                    "font-family: monospace;",
                    "word-break: break-all;"
                  ),
                  regex_preview
                ),
                shiny::tags$small(
                  class = "text-muted",
                  paste0(
                    "Variant: ",
                    chr(
                      table$VariantId[[i]]
                    )
                  )
                )
              ),

              shiny::column(
                4,
                shiny::selectInput(
                  ns(
                    content_redundancy_input_id(
                      rule_id
                    )
                  ),
                  label = NULL,
                  choices = choices,
                  selected = selected,
                  width = "100%"
                )
              )
            )
          }
        )
      )
    })
  output$current_metadata_readiness <- shiny::renderUI({
    current <- current_snapshot()
    switch(current$readiness,
      no_active_dataset = shiny::span(class = "text-muted", "No active dataset is available."),
      metadata_unavailable = shiny::span(class = "text-danger",
        "Metadata is unavailable. Open the metadata table and press Synchronize metadata, then retry."),
      missing_column = shiny::span(class = "text-danger",
        "Metadata is missing the required Column field. Open the metadata table and press Synchronize metadata, then retry."),
      misaligned = shiny::span(class = "text-danger",
        "Metadata does not align with the active dataset. Open the metadata table and press Synchronize metadata, then retry."),
      duplicate_column = shiny::span(class = "text-danger",
        "Metadata contains duplicate Column values. Resolve duplicate canonical keys and synchronize metadata before inference."),
      pending_synchronization = shiny::span(class = "text-warning",
        "Metadata table changes are pending synchronization. Press Synchronize metadata before inference."),
      assignments_required = shiny::span(class = "text-warning",
        "Metadata is aligned but has no meaningful Content assignments. Assign Content values, synchronize metadata, and retry."),
      ready = shiny::span(class = "auto-regex-readiness-ready",
        "Metadata is exactly aligned and contains meaningful Content assignments."))
  })
  output$active_source_summary <- shiny::renderText({
    if (identical(input$source, "excel")) paste("Excel:", state$worksheet() %||% "no worksheet selected")
    else "Current Data Wizard metadata"
  })
  output$download_metadata_template <- shiny::downloadHandler(
    filename = function() "active_dataset_metadata_template.xlsx",
    content = function(file) {
      # Freeze the dataset at initiation. No reactive value is read below.
      dataset <- shiny::isolate(injected("data"))
      auto_regex_write_metadata_template(file, dataset, logger)
    }
  )
  output$transfer_rules_control <-
    shiny::renderUI({

      payload <-
        state$candidate_payload()

      if (!is.list(payload)) {
        return(NULL)
      }

      has_rules <-
        any(
          vapply(
            payload[
              c(
                "table",
                "condition",
                "ratio"
              )
            ],
            function(value) {
              is.data.frame(value) &&
                nrow(value) > 0L
            },
            logical(1)
          )
        )

      if (!has_rules) {
        return(NULL)
      }

      shiny::div(
        title = paste(
          "Transfer the currently displayed Auto RegEx candidate to Auto-Assign.",
          "This can also restore Auto RegEx rules after manual Auto-Assign edits."
        ),
        shiny::actionButton(
          ns("transfer_rules"),
          "Transfer Rules",
          class = "btn-success",
          style = "width: 100%;"
        )
      )
    })
  output$processing_status <- shiny::renderText({

    if (state$processing()) {

      return(
        paste(
          "running:",
          state$current_processing_stage() %||% "preparing"
        )
      )
    }

    status <- state$run_status()

    warnings <- as.character(
      state$warnings()
    )

    warnings <- warnings[
      !is.na(warnings) &
        nzchar(trimws(warnings))
    ]

    if (status %in% c("complete", "transferred") &&
        length(warnings) > 0L) {

      return(
        paste(
          "Processing completed with warnings.",
          "Check the Metadata table for incorrect or incomplete rule-inference assignments.",
          "Mistakes in the metadata can interfere with Content, Condition, and Ratio rule inference.",
          sep = "\n"
        )
      )
    }

    status
  })

  # These are lightweight modal-facing state/control outputs. Keep them active
  # while the Auto-Assign modal is absent so reopening reconnects immediately
  # to the current Auto RegEx state. Expensive diagnostic DT outputs remain
  # lazily suspended.
  modal_state_outputs <- c(
    "source_control",
    "global_redundancy_control",
    "current_metadata_readiness",
    "active_source_summary",
    "transfer_rules_control",
    "processing_status"
  )

  for (output_id in modal_state_outputs) {
    shiny::outputOptions(
      output,
      output_id,
      suspendWhenHidden = FALSE
    )
  }

  current_diagnostic_record <- shiny::reactive({
    record <- state$diagnostics()
    context <- isolate(state$run_context())
    accepted_run <- identical(record$run_id, state$completed_run_id()) ||
      identical(state$run_status(), "failed")
    if (is.null(record) || is.null(record$run_id) || !accepted_run ||
        is.null(context) || !identical(record$source_fingerprint,
          context$source_fingerprint)) return(NULL)
    record
  })
  bounded_table <- function(value) {
    auto_regex_bound_diagnostics(value, diagnostic_row_limit)
  }
  table_options <- list(pageLength = 10L, lengthMenu = c(10L, 25L, 50L),
    scrollX = TRUE, deferRender = TRUE, processing = TRUE)
  register_table <- function(name, output_name, value) {
    output[[paste0(name, "_summary")]] <- shiny::renderUI({
      shiny::req(isTRUE(panel_open[[name]]))
      frame <- tryCatch(value(), error = function(e) NULL)
      total <- if (is.data.frame(frame)) nrow(frame) else 0L
      shown <- min(total, diagnostic_row_limit)
      shiny::tags$p(class = "text-muted", sprintf(
        "Summary: %s row%s available; showing %s%s.", total,
        if (total == 1L) "" else "s", shown,
        if (total > shown) sprintf(" (bounded to the first %s rows)", shown) else ""
      ))
    })
    output[[output_name]] <- DT::renderDT({
      shiny::req(isTRUE(panel_open[[name]]))
      tryCatch(
        DT::datatable(bounded_table(value()), options = table_options,
          rownames = FALSE, escape = TRUE),
        error = function(e) DT::datatable(
          data.frame(`Rendering error` = conditionMessage(e), check.names = FALSE),
          options = table_options, rownames = FALSE)
      )
    })
  }
  output$validation_summary <- shiny::renderUI({
    shiny::req(isTRUE(panel_open$validation))
    validation <- state$validation()
    total <- if (is.data.frame(validation))
      sum(toupper(as.character(validation$Severity)) %in% c("ERROR", "WARNING")) else 0L
    shiny::tags$p(class = "text-muted", sprintf(
      "Summary: %s structural issue record%s available.", total,
      if (total == 1L) "" else "s"
    ))
  })
  output$validation_text <- shiny::renderText({
    shiny::req(isTRUE(panel_open$validation))
    validation <- state$validation()
    if (!is.data.frame(validation) || !nrow(validation)) {
      return("Structural validation: 0 errors, 0 warnings.\nInformation: No structural problems detected.")
    }
    severity <- toupper(as.character(validation$Severity %||% ""))
    severity[is.na(severity) | !nzchar(severity)] <- "INFO"
    errors <- sum(severity == "ERROR")
    warnings <- sum(severity == "WARNING")
    check <- as.character(validation$Check %||% "Validation")
    check[is.na(check) | !nzchar(check)] <- "Validation"
    message <- as.character(validation$Message %||% "")
    message[is.na(message)] <- ""
    structural <- severity %in% c("ERROR", "WARNING")
    lines <- c(sprintf("[%s] %s: %s", severity[structural], check[structural],
      message[structural]), sprintf("Information: %s", message[severity == "INFO"]))
    descriptor <- state$source_descriptor()
    readiness <- auto_regex_condition_reference_summary(
      if (is.list(descriptor)) descriptor$metadata else NULL, "Options")
    readiness_line <- sprintf(paste0("Condition inference readiness: target %s; ",
      "sample-bearing content labels: %s; applicable rows: %d; rows with nonempty references: %d; ",
      "labels with no references: %s; status: %s."), readiness$target, readiness$labels,
      readiness$applicable_rows, readiness$reference_rows,
      readiness$unavailable_labels_display, readiness$status)
    paste(c(sprintf("Structural validation: %d errors, %d warnings.",
      errors, warnings), lines, readiness_line), collapse = "\n")
  })
  register_table("content_rules", "content_rules_table", function() {
    z <- state$candidate_rules(); if (is.null(z)) data.frame() else z$table
  })
  register_table("condition_rules", "condition_rules_table", function() {
    z <- state$candidate_rules(); if (is.null(z)) data.frame() else z$condition
  })
  register_table("ratio_rules", "ratio_rules_table", function() {
    z <- state$candidate_rules(); if (is.null(z)) data.frame() else z$ratio
  })
  register_table("content_diagnostics", "content_diagnostics_table", function() {
    z <- current_diagnostic_record(); if (is.null(z)) data.frame() else z$tables$content
  })
  register_table("semantic_spans", "semantic_spans_table", function() {
    z <- current_diagnostic_record(); if (is.null(z)) data.frame() else z$tables$semantic_spans
  })
  register_table("content_refinement_lineage", "content_refinement_lineage_table", function() {
    z <- current_diagnostic_record(); if (is.null(z)) data.frame() else z$tables$content_refinement_lineage
  })
  register_table("condition_diagnostics", "condition_diagnostics_table", function() {
    z <- current_diagnostic_record(); if (is.null(z)) data.frame() else z$tables$condition
  })
  register_table("ratio_diagnostics", "ratio_diagnostics_table", function() {
    z <- current_diagnostic_record(); if (is.null(z)) data.frame() else z$tables$ratio
  })
  output$run_diagnostics_details <- shiny::renderText({
    shiny::req(isTRUE(panel_open$run_diagnostics))
    tryCatch({
      record <- current_diagnostic_record()
      if (is.null(record)) return(paste(c("No current run diagnostics.",
        state$warnings(), state$errors()), collapse = "\n"))
      header <- sprintf("Run %s | source: %s | fingerprint: %s\nStarted: %s | completed: %s | elapsed: %.1f ms",
        record$run_id, record$source_mode, record$source_fingerprint,
        format(record$started_at), format(record$completed_at), record$elapsed_ms)
      paste(c(header, state$warnings(), state$errors(),
        capture.output(print(record$timings))), collapse = "\n")
    }, error = function(e) paste("Diagnostics rendering failed:", conditionMessage(e)))
  })
  output$run_diagnostics_summary <- shiny::renderUI({
    shiny::req(isTRUE(panel_open$run_diagnostics))
    shiny::tags$p(class = "text-muted",
      "Summary: run identity, warnings, errors, and stage timings.")
  })
  lapply(panel_names, function(name) {
    shiny::observeEvent(input[[paste0("toggle_", name)]], {
      panel_open[[name]] <- !isTRUE(panel_open[[name]])
      shinyjs::toggle(id = ns(paste0(name, "_content")), asis = TRUE)
      shinyjs::toggleClass(id = ns(paste0(name, "_icon")),
        class = "fa-chevron-down", asis = TRUE)
    }, ignoreInit = TRUE)
  })
  cleanup_failure_logged <- FALSE
  cleanup_auto_regex <- function() {
    tryCatch(state$cleanup(), error = function(e) {
      if (!cleanup_failure_logged) {
        cleanup_failure_logged <<- TRUE
        logger(paste("Session cleanup failed:", conditionMessage(e)), 1L)
      }
      invisible(NULL)
    })
  }
  session$onSessionEnded(cleanup_auto_regex)
  invisible(NULL)
}
