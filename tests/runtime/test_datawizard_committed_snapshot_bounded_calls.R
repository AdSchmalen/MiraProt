# Runtime regression harness for the committed Data Wizard refresh boundary.
suppressPackageStartupMessages(library(shiny))

source("modules/Data Wizard/datawizard_utils.R", local = TRUE)
source("modules/Data Wizard/datawizard_core.R", local = TRUE)

testServer(function(input, output, session) {
  core <- create_core_reactive_values()
  counts <- new.env(parent = emptyenv())
  counts$data_getter <- 0L
  counts$metadata_getter <- 0L
  counts$numeric_scan <- 0L
  counts$metadata_choices <- 0L
  counts$select_update <- 0L

  data_getter <- function() {
    counts$data_getter <- counts$data_getter + 1L
    data.frame(id = c("a", "b"), abundance = c(1, 2), check.names = FALSE)
  }
  metadata_getter <- function() {
    counts$metadata_getter <- counts$metadata_getter + 1L
    data.frame(Column = c("id", "abundance"), Content = c("Identifier", "Abundance"))
  }

  descriptors <- reactiveVal(NULL)
  observeEvent(core$committed_snapshot_key_debounced(), {
    df <- isolate(data_getter())
    md <- isolate(metadata_getter())
    counts$numeric_scan <- counts$numeric_scan + 1L
    numeric <- names(df)[vapply(df, is.numeric, logical(1))]
    counts$metadata_choices <- counts$metadata_choices + 1L
    content_map <- split(as.character(md$Column), as.character(md$Content))
    descriptors(list(columns = names(df), numeric = numeric, content_map = content_map))
    counts$select_update <- counts$select_update + 1L
  }, ignoreInit = TRUE)

  # A committed upload changes several compatibility signals in one flush.
  core$import_generation_committed(1L)
  core$primary_working_revision(1L)
  core$metadata_revision(1L)
  core$metadata_content_signature("content-1")
  core$metadata_meaningful_ready(TRUE)
  core$metadata_assignment_pending(FALSE)
  core$import_phase("ready")
  session$flushReact()
  session$elapse(550)
  session$flushReact()

  stopifnot(
    counts$data_getter <= 1L,
    counts$metadata_getter <= 1L,
    counts$numeric_scan <= 1L,
    counts$metadata_choices <= 1L,
    counts$select_update <= 1L
  )
})

cat("Data Wizard committed snapshot bounded-call harness passed\n")
