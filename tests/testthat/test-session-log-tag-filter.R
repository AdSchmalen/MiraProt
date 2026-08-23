orchestration_file <- file.path(
  "R", "session_save_restore", "session_save_restore_orchestration.R"
)
if (!file.exists(orchestration_file)) {
  orchestration_file <- file.path("..", "..", orchestration_file)
}

test_that("Session log tag filter includes known tags", {
  source_lines <- readLines(orchestration_file, warn = FALSE)
  known_tags_line <- grep(
    "known_tags <- c\\(", source_lines, value = TRUE, fixed = FALSE
  )
  known_tags <- eval(parse(text = trimws(known_tags_line)), envir = new.env())

  expect_identical(
    sort(unique(known_tags), method = "radix"),
    c("FILE LOADER", "LAUNCHER", "MAIN APP")
  )
})
