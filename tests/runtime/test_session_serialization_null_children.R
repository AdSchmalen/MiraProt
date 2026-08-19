library(testthat)

source("R/session_save_restore/session_save_restore_core_helpers.R")
source("R/session_save_restore/session_save_restore_module_registration.R")

test_that("serialization sanitizer preserves genuine NULL children and their names", {
  # This four-name payload used to become a three-value `out` list when the
  # genuine NULL was assigned with `out[[i]] <- cleaned`.
  payload <- list(
    submodules = list(
      loader = list(selected = "input.csv", optional = NULL),
      imputation = NULL,
      filtering = list(enabled = TRUE),
      quarantined = function() "runtime-only"
    )
  )

  dedicated <- .sanitize_datawizard_ui_state_for_save(
    payload,
    path = "submodule_ui_states"
  )
  stripped <- .strip_closures(dedicated, "submodule_ui_states")

  fallback_used <- FALSE
  expect_warning(
    sanitized <- tryCatch(
      .sanitize_for_serialization(
        stripped,
        path = "datawizard$submodule_ui_states"
      ),
      error = function(e) {
        fallback_used <<- TRUE
        warning(
          "using dedicated submodule_ui_states sanitizer fallback",
          call. = FALSE
        )
        dedicated
      }
    ),
    NA
  )

  expect_false(fallback_used)
  expect_identical(
    names(sanitized$submodules),
    c("loader", "imputation", "filtering", "quarantined")
  )
  expect_length(sanitized$submodules, 4L)
  expect_null(sanitized$submodules[[2L]])
  expect_null(sanitized$submodules$loader$optional)
  expect_identical(sanitized$submodules$filtering, list(enabled = TRUE))
  expect_null(sanitized$submodules[[4L]])
  expect_false(any(vapply(sanitized$submodules, is.function, logical(1L))))
})
