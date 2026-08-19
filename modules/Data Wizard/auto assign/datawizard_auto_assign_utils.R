# ============================================================================

if (!exists("datawizard_known_sample_spans", mode = "function", inherits = TRUE))
  source("modules/Data Wizard/datawizard_ratio_helpers.R", local = environment())
# Sub-Script: Data Wizard Auto-Assign Utilities
# Purpose:
#   Centralize reusable helper functions for validation, parsing, extraction, and
#   resilience patterns used by auto-assign orchestration.
# Architectural Role:
#   Business-logic/helper layer with no ownership of module lifecycle.
# Responsibilities:
#   - Provide deterministic helper behavior for regex conversion, extraction, and
#     module-safe access/validation.
#   - Provide debug utility entrypoints compatible with orchestrator debug policy.
# Non-Responsibilities:
#   - Must not own UI rendering or top-level reactive graph registration.
# Allowed Dependencies:
#   - Base R, stringr, shiny helpers, and data structures passed by orchestrator.
# Interaction Boundaries:
#   - Functions are called by orchestrator and related in-scope internals only.
# Stability Guarantees:
#   - Helper function signatures used by orchestrator are kept backward compatible.
# ============================================================================


############
# Enhanced Debug System Integration

#' Auto-assign debug logging with consistent management
#' @param message debug message
#' @param level debug level (0=none, 1=critical, 2=verbose)
#' @param debug_level optional override for debug level
debug_auto_assign <- function(message, level = 1, debug_level = NULL) {
  if (is.null(debug_level)) debug_level <- as.numeric(Sys.getenv("DW_DEBUG_LEVEL", "1"))
  if (debug_level >= level) {
    timestamp <- format(Sys.time(), "%H:%M:%S")
    cat("[ AUTO ASSIGN", timestamp, "]", message, "\n")
  }
}

# Resolve the editor identity behind a Content dropdown.  Content labels are
# intentionally not unique: RuleId is the editor identity and VariantId is the
# application identity shared with condition/ratio rules.
resolve_content_rule_id <- function(rules, content, selected_rule_id = NULL) {
  if (!is.data.frame(rules) || !all(c("RuleId", "Content") %in% names(rules)) ||
      length(content) != 1L || is.na(content)) return(NULL)

  candidates <- which(!is.na(rules$Content) & rules$Content == content)
  if (!length(candidates)) return(NULL)

  selected_rule_id <- as.character(if (is.null(selected_rule_id)) "" else selected_rule_id)
  if (length(selected_rule_id) == 1L) {
    selected <- candidates[rules$RuleId[candidates] == selected_rule_id]
    if (length(selected)) return(as.character(rules$RuleId[selected[[1L]]]))
  }

  priority <- if ("Priority" %in% names(rules)) suppressWarnings(as.numeric(rules$Priority)) else
    rep(0, nrow(rules))
  priority[is.na(priority)] <- -Inf
  candidates <- candidates[order(-priority[candidates], as.character(rules$RuleId[candidates]))]
  as.character(rules$RuleId[candidates[[1L]]])
}

resolve_content_variant_id <- function(rules, content, selected_rule_id = NULL) {
  rule_id <- resolve_content_rule_id(rules, content, selected_rule_id)
  if (is.null(rule_id) || !"VariantId" %in% names(rules)) return(NULL)
  as.character(rules$VariantId[match(rule_id, rules$RuleId)])
}

resolve_variant_rule_id <- function(rules, content, variant_id,
                                    selected_rule_id = NULL) {
  if (!is.data.frame(rules) || !all(c("RuleId", "Content", "VariantId") %in% names(rules)) ||
      length(content) != 1L || length(variant_id) != 1L ||
      is.na(content) || is.na(variant_id)) return(NULL)
  candidates <- which(!is.na(rules$Content) & rules$Content == content &
                        !is.na(rules$VariantId) & rules$VariantId == variant_id)
  if (!length(candidates)) return(NULL)
  selected_rule_id <- as.character(if (is.null(selected_rule_id)) "" else selected_rule_id)
  selected <- candidates[rules$RuleId[candidates] == selected_rule_id]
  if (length(selected)) return(as.character(rules$RuleId[selected[[1L]]]))
  as.character(sort(rules$RuleId[candidates])[[1L]])
}

next_auto_assign_rule_id <- function(
    kind = c("content", "condition", "ratio"),
    variant_id,
    existing_rule_ids = character()) {

  kind <- match.arg(kind)

  variant_id <- as.character(variant_id)

  if (length(variant_id) != 1L ||
      is.na(variant_id) ||
      !nzchar(trimws(variant_id))) {
    stop(
      "A nonblank VariantId is required to create a RuleId.",
      call. = FALSE
    )
  }

  existing_rule_ids <- as.character(existing_rule_ids)

  existing_rule_ids <- existing_rule_ids[
    !is.na(existing_rule_ids) &
      nzchar(trimws(existing_rule_ids))
  ]

  # Keep the current canonical naming convention as the preferred identity.
  base_id <- stable_rule_ids(
    kind,
    variant_id
  )[[1L]]

  if (!base_id %in% existing_rule_ids) {
    return(base_id)
  }

  # stable_rule_ids() currently ends generated IDs with -r1.
  # Preserve that convention and allocate the first unused ordinal.
  stem <- sub(
    "-r[0-9]+$",
    "",
    base_id,
    perl = TRUE
  )

  ordinal <- 2L

  repeat {

    candidate <- sprintf(
      "%s-r%d",
      stem,
      ordinal
    )

    if (!candidate %in% existing_rule_ids) {
      return(candidate)
    }

    ordinal <- ordinal + 1L
  }
}

next_user_auto_assign_rule_id <- function(
    kind = c("content", "condition", "ratio"),
    existing_rule_ids = character()) {

  kind <- match.arg(kind)

  existing_rule_ids <- as.character(
    existing_rule_ids
  )

  existing_rule_ids <- existing_rule_ids[
    !is.na(existing_rule_ids) &
      nzchar(trimws(existing_rule_ids))
  ]

  stem <- paste0(
    "user-",
    kind,
    "-r"
  )

  ordinal <- 1L

  repeat {

    candidate <- paste0(
      stem,
      ordinal
    )

    if (!candidate %in% existing_rule_ids) {
      return(candidate)
    }

    ordinal <- ordinal + 1L
  }
}

# Source helper families in definition order. This order is behaviorally
# significant because later definitions intentionally override earlier ones.
source("modules/Data Wizard/auto assign/datawizard_auto_assign_ratio_pairing.R", local = environment())
source("modules/Data Wizard/auto assign/datawizard_auto_assign_sample_helpers.R", local = environment())
source("modules/Data Wizard/auto assign/datawizard_auto_assign_module_adapters.R", local = environment())
source("modules/Data Wizard/auto assign/datawizard_auto_assign_ratio_parsing.R", local = environment())
