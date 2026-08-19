suppressPackageStartupMessages(library(shiny))
suppressPackageStartupMessages(library(testthat))

source("R/utils.R", local = TRUE)

test_that("one upload releases each downstream observer exactly once", {
  server <- function(input, output, session) {
    rv <- reactiveValues(
      datawizard_import_phase = "idle",
      datawizard_import_ready_revision = 0L,
      datawizard_data_revision_id = 0L,
      datawizard_metadata_revision_id = 0L
    )
    refreshes <- reactiveValues(GO = 0L, PCA = 0L, Heatmap = 0L, GSEA = 0L,
                                Volcano = 0L, Dotplot = 0L, Venn = 0L)
    for (module_name in names(reactiveValuesToList(refreshes))) local({
      name <- module_name
      observeEvent(datawizard_import_ready_signature(rv), {
        if (datawizard_import_barrier_active(rv)) return()
        refreshes[[name]] <- isolate(refreshes[[name]]) + 1L
      }, ignoreInit = TRUE)
    })
    session$userData$rv <- rv
    session$userData$refreshes <- refreshes
  }

  testServer(server, {
    rv <- session$userData$rv
    rv$datawizard_import_phase <- "reading"
    rv$datawizard_data_revision_id <- 1L
    rv$datawizard_import_phase <- "publishing_raw"
    rv$datawizard_metadata_revision_id <- 1L
    rv$datawizard_import_phase <- "creating_metadata"
    session$flushReact()
    expect_true(all(unlist(reactiveValuesToList(session$userData$refreshes)) == 0L))
    rv$datawizard_import_phase <- "ready"
    rv$datawizard_import_ready_revision <- 1L
    session$flushReact()
    expect_true(all(unlist(reactiveValuesToList(session$userData$refreshes)) == 1L))
  })
})
