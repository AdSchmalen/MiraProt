#!/usr/bin/env Rscript

library(shiny)
source("modules/Data Wizard/auto assign/datawizard_auto_assign_utils.R")

# Pure helpers receive values, never reactive handles. In particular, passing a
# reactive handle cannot accidentally dereference it outside a reactive domain.
reads <- 0L
metadata_rx <- reactive({
  reads <<- reads + 1L
  data.frame(Column = "run1", Content = "Raw Abundance")
})
stopifnot(!check_metadata_content_available_central(metadata_rx), reads == 0L)

# The module's dependency owner resolves metadata inside a reactive expression
# before invoking the pure helper.
resolved <- reactive({
  metadata <- metadata_rx()
  check_metadata_content_available_central(metadata)
})
stopifnot(isTRUE(isolate(resolved())), reads == 1L)

# Guard the architectural boundary in the server sources as well as exercising
# it above: neither pure helper contains a call to a reactive metadata value.
utils_text <- readLines("modules/Data Wizard/auto assign/datawizard_auto_assign_utils.R")
helper_start <- grep("^check_metadata_content_available_central <-", utils_text)
helper_end <- helper_start + which(utils_text[(helper_start + 1L):length(utils_text)] == "}")[1L]
helper_text <- paste(utils_text[helper_start:helper_end], collapse = "\n")
stopifnot(!grepl("metadata_def\\s*\\(", helper_text, perl = TRUE))

state_text <- paste(readLines(
  "modules/Data Wizard/auto assign/datawizard_auto_assign_reactive_state.R"
), collapse = "\n")
stopifnot(grepl("current_metadata <- if \\(is.reactive\\(metadata_skeleton\\)\\) metadata_skeleton\\(\\)",
                state_text, perl = TRUE),
          grepl("check_metadata_content_available_central\\(current_metadata, debug_level\\)",
                state_text, perl = TRUE))

cat("reactive processing boundary regression checks passed\n")
