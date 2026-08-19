# Deterministic, UI-free integration seam for the Auto RegEx module.  The
# harness deliberately uses the production source descriptor, state machine,
# inference entry point, and public rule loader shape; only external reactive
# inputs and workbook I/O are injected.
auto_regex_test_harness <- function(metadata, data, revision = 1L,
                                    loader = function(...) stop("no loader"),
                                    initial_rules = NULL) {
  empty <- coerce_contract(list())
  values <- list(
    metadata = shiny::reactiveVal(metadata),
    data = shiny::reactiveVal(data),
    revision = shiny::reactiveVal(revision),
    rules = shiny::reactiveVal(initial_rules %||% empty),
    pending = shiny::reactiveVal(NULL)
  )
  state <- auto_regex_create_state(values)
  events <- character()
  loads <- list()

  effective_metadata <- function() {
    pending <- shiny::isolate(values$pending())
    if (is.data.frame(pending)) pending else shiny::isolate(values$metadata())
  }
  readiness <- function() {
    meta <- effective_metadata()
    dat <- shiny::isolate(values$data())
    if (!metadata_matches_dataset(meta, dat)) return("misaligned")
    if (!is_meaningful_metadata(meta)) return("assignments_required")
    "ready"
  }
  public_loader <- function(payload) {
    loads[[length(loads) + 1L]] <<- payload
    values$rules(payload)
    TRUE
  }
  infer <- function(meta = effective_metadata()) {
    events <<- character()
    auto_regex_infer_rules(meta,
      condition_target = if ("Options" %in% names(meta)) "Options" else "",
      progress = function(event) events <<- c(events, event))
  }

  list(values = values, state = state, readiness = readiness,
       effective_metadata = effective_metadata, infer = infer,
       progress = function() events, load = public_loader,
       loads = function() loads, loader = loader,
       excel = function(workbook, sheet, mapping) {
         frame <- loader(workbook, sheet)
         selected <- unname(mapping[nzchar(mapping)])
         mapped <- frame[, selected, drop = FALSE]
         names(mapped) <- names(mapping)[nzchar(mapping)]
         auto_regex_source_descriptor("excel", mapped, frame, workbook, sheet,
                                      mapping, workbook$revision %||% NULL)
       })
}
