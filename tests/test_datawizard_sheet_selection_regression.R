# Narrow source-level regression checks for the Data Wizard sheet lifecycle.
# Run with: Rscript tests/test_datawizard_sheet_selection_regression.R

read_source <- function(path) paste(readLines(path, warn = FALSE), collapse = "\n")
expect_match <- function(text, pattern, label) {
  if (!grepl(pattern, text, perl = TRUE)) stop(label, call. = FALSE)
}
expect_no_match <- function(text, pattern, label) {
  if (grepl(pattern, text, perl = TRUE)) stop(label, call. = FALSE)
}

context <- read_source("modules/Data Wizard/file_loader/datawizard_file_loader_context.R")
interactive <- read_source("modules/Data Wizard/file_loader/datawizard_file_loader_interactive.R")
restore <- read_source("modules/Data Wizard/file_loader/datawizard_file_loader_restore.R")
tables <- read_source("modules/Data Wizard/tables/datawizard_tables_observer_rendering.R")

# Full restore must protect both dropdown replays with the expected sheet name,
# rather than arming a Boolean that can swallow an unrelated later selection.
expect_match(restore, "skip_next_sheet_change_primary\\(selected1\\)",
             "primary restore replay is not target-guarded")
expect_match(restore, "skip_next_sheet_change_secondary\\(selected2\\)",
             "secondary restore replay is not target-guarded")
expect_no_match(paste(context, interactive, restore), "skip_next_cached_sheet_apply_",
                "stale Boolean cache-apply guard remains")

# A matching programmatic event is consumed, while a different (genuine) sheet
# clears the stale target and continues into the live/cache publication paths.
expect_match(interactive,
             "(?s)identical\\(as.character\\(input\\$sheetDropdown\\), as.character\\(skip_primary_sheet\\)\\).+?return\\(\\).+?skip_next_sheet_change_primary\\(NULL\\).+?publish_primary_current_sheet",
             "primary target guard no longer distinguishes programmatic and user selections")
expect_match(interactive,
             "(?s)identical\\(as.character\\(input\\$sheetDropdown2\\), as.character\\(skip_secondary_sheet\\)\\).+?return\\(\\).+?skip_next_sheet_change_secondary\\(NULL\\).+?publish_secondary_current_sheet",
             "secondary target guard no longer distinguishes programmatic and user selections")

# Keep Tables revision-driven: the corresponding data frame is read in isolate
# only after the published-vs-debounced revision gate accepts the snapshot.
expect_match(tables, "revision <- primary_working_revision_debounced\\(\\)",
             "primary table lost its debounced revision dependency")
expect_match(tables, "req\\(as.integer\\(revision\\) >= as.integer\\(published_revision\\)\\)",
             "primary table lost its published revision gate")
expect_match(tables, "df <- isolate\\(get_current_primary_df\\(\\)\\)",
             "primary table data read is no longer isolated")
expect_match(tables, "revision <- secondary_revision_debounced\\(\\)",
             "secondary table lost its debounced revision dependency")

cat("Data Wizard sheet-selection lifecycle checks passed.\n")
