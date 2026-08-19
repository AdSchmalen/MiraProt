# Session source/workbook observer registrar.
auto_regex_register_source_handlers <- function(context) {
  list2env(unclass(context), envir = environment())
  output$source_control <- shiny::renderUI({

    selected <-
      state$source()

    if (is.null(selected) ||
        !selected %in%
        c(
          "current_metadata",
          "excel"
        )) {
      selected <- "current_metadata"
    }

    shiny::selectInput(
      ns("source"),
      "Metadata source:",
      choices = c(
        "Current MiraProt metadata" =
          "current_metadata",
        "Excel workbook" =
          "excel"
      ),
      selected = selected
    )
  })

  # A mode transition is a hard boundary: no workbook mapping or completed
  # current-metadata result is allowed to leak into the other source.
  shiny::observeEvent(
    input$source,
    {

      requested_source <-
        as.character(
          input$source %||% ""
        )[[1L]]

      if (!nzchar(requested_source)) {
        return()
      }

      previous_source <-
        shiny::isolate(
          state$source()
        )

      # Reopening the Auto-Assign modal recreates its inputs. That is UI
      # hydration, not a Data Wizard source change.
      if (identical(
        previous_source,
        requested_source
      )) {

        logger(
          sprintf(
            paste0(
              "Source control rehydrated with unchanged source '%s'; ",
              "preserving inferred rules and redundancy cache."
            ),
            requested_source
          ),
          2L
        )

        return()
      }

      logger(
        sprintf(
          "Auto RegEx source changed: '%s' -> '%s'.",
          previous_source %||% "<unset>",
          requested_source
        ),
        2L
      )

      state$reset_source_specific_state(
        requested_source
      )
    },
    ignoreInit = FALSE,
    ignoreNULL = TRUE
  )
  invisible(NULL)
}

auto_regex_register_workbook_handlers <- function(context) {
  list2env(unclass(context), envir = environment())
  shiny::observeEvent(input$excel_file, {
    file <- input$excel_file
    state$reset_workbook_state()
    if (is.null(file) || !nzchar(file$datapath)) return()
    if (!requireNamespace("readxl", quietly = TRUE)) {
      state$errors("The readxl package is required to inspect Excel workbooks.")
      state$run_status("failed")
      return()
    }
    sheets <- tryCatch(readxl::excel_sheets(file$datapath), error = function(e) {
      state$errors(conditionMessage(e)); character()
    })
    state$reset_workbook_state(
      workbook = list(path = file$datapath, name = file$name, size = file$size,
                      type = file$type), worksheets = sheets
    )
  }, ignoreInit = TRUE)

  output$excel_worksheet_controls <- shiny::renderUI({
    sheets <- state$worksheets()
    if (!length(sheets)) return(NULL)
    shiny::selectInput(ns("worksheet"), "Worksheet:", choices = sheets,
                       selected = sheets[[1L]])
  })

  shiny::observeEvent(list(input$worksheet, state$workbook()), {
    workbook <- state$workbook(); sheet <- input$worksheet
    if (is.null(workbook) || !nzchar(sheet %||% "")) return()
    frame <- tryCatch(
      as.data.frame(readxl::read_excel(workbook$path, sheet = sheet,
        .name_repair = "unique"), stringsAsFactors = FALSE, check.names = FALSE),
      error = function(e) { state$errors(conditionMessage(e)); NULL }
    )
    state$reset_worksheet_state(sheet, frame)
  }, ignoreInit = TRUE)

  output$excel_mapping_controls <- shiny::renderUI({
    frame <- state$worksheet_data()
    if (!is.data.frame(frame)) return(NULL)
    mapping_control <- function(field) {
      choices <- c("Not mapped" = "", stats::setNames(names(frame), names(frame)))
      selected <- if (field %in% names(frame)) field else ""
      shiny::selectInput(ns(paste0("map_", tolower(field))), field,
                         choices = choices, selected = selected)
    }
    shiny::tagList(
      shiny::fluidRow(lapply(mapping_fields[1:3], function(field) {
        shiny::column(4, mapping_control(field))
      })),
      shiny::fluidRow(lapply(mapping_fields[4:6], function(field) {
        shiny::column(4, mapping_control(field))
      }))
    )
  })

  # Shared mapping application logic.  Called by both the mapping-input observer

  # and the worksheet-data observer to handle cases where inputs don't re-fire
  # (e.g. a second workbook with identical column names producing identical
  # default selectInput values).
  apply_mapping_from_inputs <- function(frame) {
    if (!is.data.frame(frame)) return(invisible(NULL))
    selected <- vapply(mapping_fields, function(field)
      input[[paste0("map_", tolower(field))]] %||% "", character(1))
    mapping <- stats::setNames(selected, mapping_fields)
    keep <- nzchar(selected) & selected %in% names(frame)
    mapped <- frame[, selected[keep], drop = FALSE]
    names(mapped) <- mapping_fields[keep]
    persisted <- intersect(provenance_fields, names(frame))
    if (length(persisted)) mapped[persisted] <- frame[persisted]
    validation <- validate_metadata(mapped, names(frame),
      if ("Options" %in% names(mapped)) "Options" else "")
    descriptor <- auto_regex_source_descriptor("excel", mapped, frame,
      isolate(state$workbook()), isolate(state$worksheet()), mapping,
      list(workbook = isolate(state$workbook())$revision %||% NULL))
    previous <- isolate(state$source_descriptor())
    state$mapping(mapping)
    state$validation(validation)
    state$source_descriptor(descriptor)
    if (!is.null(previous) && !identical(previous$signature, descriptor$signature))
      auto_regex_invalidate_effective_source(state)
    invisible(mapping)
  }

  shiny::observeEvent(lapply(mapping_fields, function(field) input[[paste0("map_", tolower(field))]]), {
    apply_mapping_from_inputs(state$worksheet_data())
  }, ignoreInit = TRUE)

  # When worksheet data changes (new workbook/sheet upload), the mapping controls

  # are re-rendered via renderUI.  If the new worksheet has identical column names,
  # the mapping-input observer may not re-fire (inputs go NULL → same value with
  # ignoreNULL=TRUE).  This observer ensures a fresh mapping is always initialized
  # from the current frame even when input values are unchanged.
  shiny::observeEvent(state$worksheet_data(), {
    frame <- state$worksheet_data()
    if (!is.data.frame(frame)) return()
    # Only initialize when mapping was cleared by a reset (NULL).  If the mapping
    # observer already fired (non-NULL mapping), don't override.
    if (!is.null(isolate(state$mapping()))) return()
    apply_mapping_from_inputs(frame)
  }, ignoreInit = TRUE, ignoreNULL = TRUE)

  # Invalidate completed current-source candidates when an injected revision,
  # dataset, or metadata value changes.  Alignment and meaningfulness are
  # deliberately evaluated independently: a rebuilt skeleton can align exactly
  # but is not evidence suitable for inference.
  shiny::observeEvent(
    list(
      injected("metadata"),
      injected("data"),
      injected("revision")
    ),
    {

      if (!identical(
        isolate(input$source),
        "current_metadata"
      )) {
        return()
      }

      descriptor <-
        source_snapshot()

      previous <-
        isolate(
          state$source_descriptor()
        )

      state$source_descriptor(
        descriptor
      )

      if (!is.null(previous) &&
          !identical(
            previous$signature,
            descriptor$signature
          )) {

        reason <-
          auto_regex_source_change_reason(
            previous,
            descriptor
          )

        logger(
          sprintf(
            paste0(
              "Auto RegEx effective source changed: %s; ",
              "invalidating the completed inference result."
            ),
            reason
          ),
          2L
        )

        auto_regex_invalidate_effective_source(
          state
        )
      }
    },
    ignoreInit = TRUE
  )

  invisible(NULL)
}
