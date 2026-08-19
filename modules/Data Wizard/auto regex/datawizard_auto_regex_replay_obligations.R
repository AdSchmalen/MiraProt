# Auto Regex ratio replay obligations. Loaded after diagnostic helpers.

auto_regex_ratio_pair_header_representable <- function(
    column,
    numerator,
    denominator) {

  numerator_ranges <- auto_regex_fixed_reference_ranges(
    column,
    numerator
  )

  denominator_ranges <- auto_regex_fixed_reference_ranges(
    column,
    denominator
  )

  if (!nrow(numerator_ranges) || !nrow(denominator_ranges))
    return(FALSE)

  # Both semantic values must occupy distinct spans. This prevents a shorter
  # reference from being "found" only as a substring of its longer sibling.
  any(vapply(
    seq_len(nrow(numerator_ranges)),
    function(i) {
      any(
        numerator_ranges[i, "End"] < denominator_ranges[, "Start"] |
          denominator_ranges[, "End"] < numerator_ranges[i, "Start"]
      )
    },
    logical(1)
  ))
}

auto_regex_ratio_row_state <- function(
    metadata,
    rows = seq_len(nrow(metadata))) {

  rows <- sort(unique(as.integer(rows)))
  rows <- rows[
    !is.na(rows) &
      rows >= 1L &
      rows <= nrow(metadata)
  ]

  state <- rep("absent", length(rows))

  if (!length(rows))
    return(state)

  if (!all(c("Numerator", "Denominator") %in%
           names(metadata)))
    return(state)

  numerator <- chr(metadata$Numerator[rows])
  denominator <- chr(metadata$Denominator[rows])

  has_numerator <- nzchar(numerator)
  has_denominator <- nzchar(denominator)

  partial <- xor(
    has_numerator,
    has_denominator
  )

  state[partial] <- "partial"

  complete <-
    has_numerator &
    has_denominator

  if (!any(complete))
    return(state)

  columns <- if ("Column" %in% names(metadata)) {
    chr(metadata$Column[rows])
  } else {
    rep("", length(rows))
  }

  representable <- rep(FALSE, length(rows))

  representable[complete] <- mapply(
    auto_regex_ratio_pair_header_representable,
    columns[complete],
    numerator[complete],
    denominator[complete],
    USE.NAMES = FALSE
  )

  state[
    complete &
      representable
  ] <- "required"

  state[
    complete &
      !representable
  ] <- "nonrepresentable"

  state
}

auto_regex_references_header_representable <- function(subset) {
  required <- c("Numerator", "Denominator", "Column")

  if (!all(required %in% names(subset)) || !nrow(subset))
    return(FALSE)

  columns <- chr(subset$Column)
  numerators <- chr(subset$Numerator)
  denominators <- chr(subset$Denominator)

  complete <- nzchar(numerators) & nzchar(denominators)

  if (!all(complete))
    return(FALSE)

  all(mapply(
    auto_regex_ratio_pair_header_representable,
    columns,
    numerators,
    denominators,
    USE.NAMES = FALSE
  ))
}

auto_regex_variant_ratio_state <- function(subset) {
  states <- auto_regex_ratio_row_state(
    subset
  )

  if (any(states == "partial"))
    return("partial")

  # A variant may contain both extractable and nonextractable headers.
  # Presence of at least one required row means a ratio rule can be learned
  # from the required rows; nonrepresentable siblings are allowed to abstain.
  if (any(states == "required"))
    return("complete")

  if (all(states == "absent") &&
      auto_regex_variant_authoritative_contrast(subset))
    return("complete")

  if (any(states == "nonrepresentable"))
    return("complete_nonrepresentable")

  "absent"
}

auto_regex_ratio_obligation_rows <- function(
    metadata,
    rows,
    variant_ids) {

  rows <- sort(unique(as.integer(rows)))
  rows <- rows[
    !is.na(rows) &
      rows >= 1L &
      rows <= nrow(metadata)
  ]

  if (!length(rows))
    return(integer())

  if (length(variant_ids) != nrow(metadata))
    stop(
      "variant_ids must contain one value per metadata row.",
      call. = FALSE
    )

  variant_ids <- chr(variant_ids)

  assigned <- rows[
    nzchar(variant_ids[rows])
  ]

  if (!length(assigned))
    return(integer())

  states <- auto_regex_ratio_row_state(
    metadata,
    assigned
  )

  assigned[
    states == "required"
  ]
}

auto_regex_ratio_nonextractable_rows <- function(
    metadata,
    rows,
    variant_ids) {

  rows <- sort(unique(as.integer(rows)))
  rows <- rows[
    !is.na(rows) &
      rows >= 1L &
      rows <= nrow(metadata)
  ]

  if (!length(rows))
    return(integer())

  if (length(variant_ids) != nrow(metadata))
    stop(
      "variant_ids must contain one value per metadata row.",
      call. = FALSE
    )

  variant_ids <- chr(variant_ids)

  assigned <- rows[
    nzchar(variant_ids[rows])
  ]

  if (!length(assigned))
    return(integer())

  states <- auto_regex_ratio_row_state(
    metadata,
    assigned
  )

  assigned[
    states %in% c(
      "nonrepresentable",
      "absent"
    )
  ]
}

auto_regex_ratio_replay_contract <- function(
    applied,
    metadata,
    rows,
    variant_ids) {

  required <- auto_regex_ratio_obligation_rows(
    metadata,
    rows,
    variant_ids
  )

  nonextractable <-
    auto_regex_ratio_nonextractable_rows(
      metadata,
      rows,
      variant_ids
    )

  required_ok <-
    !length(required) ||
    all(
      applied$diagnostics$Success[
        required
      ] %in% TRUE
    )

  nonextractable_ok <-
    !length(nonextractable) ||
    all(
      !nzchar(
        chr(
          applied$metadata$Numerator[
            nonextractable
          ]
        )
      ) &
        !nzchar(
          chr(
            applied$metadata$Denominator[
              nonextractable
            ]
          )
        )
    )

  list(
    ok = required_ok &&
      nonextractable_ok,
    required_rows = required,
    nonextractable_rows =
      nonextractable,
    required_ok = required_ok,
    nonextractable_ok =
      nonextractable_ok
  )
}
