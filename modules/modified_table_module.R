# modules/modified_table_module.R

modModifiedTableUI <- function(id) {
  ns <- NS(id)

  tagList(
    fluidRow(
      column(
        width = 12,
        verbatimTextOutput(ns("mod_table_info"))
      )
    ),
    fluidRow(
      column(
        width = 12,
        tags$style(HTML(sprintf("#%s { height: 900px !important; }", ns("mod_table_content")))),
        DT::DTOutput(ns("mod_table_content"))
      )
    )
  )
}

modModifiedTableServer <- function(id, rv, debug_level = 0) {
  DEBUG_LEVEL <- suppressWarnings(as.integer(debug_level))[1]
  if (length(DEBUG_LEVEL) == 0 || !is.finite(DEBUG_LEVEL)) DEBUG_LEVEL <- 0
  moduleServer(id, function(input, output, session) {
    data_modified <- reactive({
      req(rv$data_mod)
      rv$data_mod
    })

    output$mod_table_info <- renderText({
      df <- data_modified()
      paste0("Number of rows: ", nrow(df), " — Number of columns: ", ncol(df))
    })

    output$mod_table_content <- DT::renderDT({
      df <- data_modified()
      DT::datatable(
        df,
        filter = "top",
        fillContainer = TRUE,
        options = list(
          dom = "Bftp",
          autoWidth = TRUE,
          paging = TRUE,
          pageLength = 100,
          lengthMenu = list(c(50, 100, 200, 500), c("50", "100", "200", "500")),
          scrollY = "800px"
        )
      )
    })
  })
}
