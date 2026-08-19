# Auto Regex redundancy edge analysis and ladder construction. Loaded by the legacy logic entrypoint.

auto_regex_redundancy_value <- function(
    value,
    fallback = 0L,
    maximum = 10L) {

  fallback <- suppressWarnings(
    as.integer(fallback)[[1L]]
  )

  if (is.na(fallback))
    fallback <- 0L

  fallback <- max(
    0L,
    min(as.integer(maximum), fallback)
  )

  if (is.null(value) ||
      !length(value)) {
    return(fallback)
  }

  value <- as.character(value)[[1L]]

  if (is.na(value) ||
      !nzchar(trimws(value)) ||
      identical(value, "global")) {
    return(fallback)
  }

  parsed <- suppressWarnings(
    as.integer(value)
  )

  if (is.na(parsed))
    return(fallback)

  max(
    0L,
    min(as.integer(maximum), parsed)
  )
}


auto_regex_edge_atoms <- function(
    stored_pattern) {

  runtime <- regex_from_miraprot_storage(
    stored_pattern,
    "content"
  )

  runtime <- chr(runtime)

  if (length(runtime) != 1L ||
      is.na(runtime[[1L]]) ||
      !nzchar(runtime[[1L]])) {
    return(NULL)
  }

  runtime <- runtime[[1L]]

  chars <- strsplit(
    runtime,
    "",
    fixed = TRUE
  )[[1L]]

  n <- length(chars)

  if (!n)
    return(NULL)

  text_between <- function(
    start,
    finish) {

    if (start > finish)
      return("")

    paste0(
      chars[
        seq.int(start, finish)
      ],
      collapse = ""
    )
  }

  scan_character_class <- function(start) {

    depth <- 0L
    i <- start

    while (i <= n) {

      ch <- chars[[i]]

      if (identical(ch, "\\")) {

        if (i >= n)
          return(NA_integer_)

        i <- i + 2L
        next
      }

      if (identical(ch, "[")) {

        depth <- depth + 1L

      } else if (identical(ch, "]")) {

        depth <- depth - 1L

        if (depth == 0L)
          return(i)

        if (depth < 0L)
          return(NA_integer_)
      }

      i <- i + 1L
    }

    NA_integer_
  }

  scan_group <- function(start) {

    depth <- 0L
    i <- start

    while (i <= n) {

      ch <- chars[[i]]

      if (identical(ch, "\\")) {

        if (i >= n)
          return(NA_integer_)

        i <- i + 2L
        next
      }

      if (identical(ch, "[")) {

        class_end <-
          scan_character_class(i)

        if (is.na(class_end))
          return(NA_integer_)

        i <- class_end + 1L
        next
      }

      if (identical(ch, "(")) {

        depth <- depth + 1L

      } else if (identical(ch, ")")) {

        depth <- depth - 1L

        if (depth == 0L)
          return(i)

        if (depth < 0L)
          return(NA_integer_)
      }

      i <- i + 1L
    }

    NA_integer_
  }

  attach_quantifier <- function(end) {

    if (end >= n)
      return(end)

    next_position <- end + 1L
    ch <- chars[[next_position]]

    if (ch %in% c("*", "+", "?")) {

      end <- next_position

      if (end < n &&
          chars[[end + 1L]] %in%
          c("?", "+")) {
        end <- end + 1L
      }

      return(end)
    }

    if (!identical(ch, "{"))
      return(end)

    if (next_position >= n)
      return(end)

    search_positions <-
      seq.int(
        next_position + 1L,
        n
      )

    closes <-
      search_positions[
        chars[search_positions] == "}"
      ]

    if (!length(closes))
      return(end)

    close <- closes[[1L]]

    quantifier <- text_between(
      next_position,
      close
    )

    if (!grepl(
      "^\\{[0-9]+(,[0-9]*)?\\}$",
      quantifier,
      perl = TRUE
    )) {
      return(end)
    }

    end <- close

    if (end < n &&
        chars[[end + 1L]] %in%
        c("?", "+")) {
      end <- end + 1L
    }

    end
  }

  atoms <- list()
  i <- 1L

  while (i <= n) {

    start <- i
    ch <- chars[[i]]
    type <- "literal"

    # Top-level alternation would require branching the syntax tree.
    # Compaction is optional, so retaining the proven rule is safer.
    if (identical(ch, "|"))
      return(NULL)

    if (identical(ch, "\\")) {

      if (i >= n)
        return(NULL)

      # \Q...\E has its own quoting grammar. Fail closed.
      if (chars[[i + 1L]] %in%
          c("Q", "E")) {
        return(NULL)
      }

      end <- i + 1L
      type <- "escape"

    } else if (identical(ch, "[")) {

      end <- scan_character_class(i)

      if (is.na(end))
        return(NULL)

      type <- "class"

    } else if (identical(ch, "(")) {

      end <- scan_group(i)

      if (is.na(end))
        return(NULL)

      type <- "group"

    } else if (identical(ch, ")")) {

      return(NULL)

    } else if (ch %in% c("^", "$")) {

      end <- i
      type <- "anchor"

    } else if (identical(ch, ".")) {

      end <- i
      type <- "dot"

    } else {

      end <- i
    }

    base_end <- end
    end <- attach_quantifier(end)

    atoms[[length(atoms) + 1L]] <-
      data.frame(
        Start = start,
        End = end,
        Text = text_between(
          start,
          end
        ),
        Type = type,
        Quantified = end > base_end,
        stringsAsFactors = FALSE,
        check.names = FALSE
      )

    i <- end + 1L
  }

  if (!length(atoms))
    return(NULL)

  result <- do.call(
    rbind,
    atoms
  )

  rownames(result) <- NULL

  result
}


auto_regex_edge_boundary_natural <- function(
    left_atom,
    right_atom) {

  if (is.null(left_atom) ||
      is.null(right_atom)) {
    return(TRUE)
  }

  # Regex syntax itself represents a natural structural boundary.
  if (!identical(
    left_atom$Type[[1L]],
    "literal"
  ) ||
  !identical(
    right_atom$Type[[1L]],
    "literal"
  ) ||
  isTRUE(
    left_atom$Quantified[[1L]]
  ) ||
  isTRUE(
    right_atom$Quantified[[1L]]
  )) {
    return(TRUE)
  }

  left <- left_atom$Text[[1L]]
  right <- right_atom$Text[[1L]]

  if (!nzchar(left) ||
      !nzchar(right)) {
    return(TRUE)
  }

  left_width <- nchar(
    left,
    type = "chars"
  )

  a <- substring(
    left,
    left_width,
    left_width
  )

  b <- substring(
    right,
    1L,
    1L
  )

  a_alnum <- grepl(
    "^[[:alnum:]]$",
    a,
    perl = TRUE
  )

  b_alnum <- grepl(
    "^[[:alnum:]]$",
    b,
    perl = TRUE
  )

  # Punctuation / whitespace / delimiters.
  if (!a_alnum ||
      !b_alnum) {
    return(TRUE)
  }

  # camelCase / PascalCase boundary.
  if (grepl(
    "^[[:lower:]]$",
    a,
    perl = TRUE
  ) &&
  grepl(
    "^[[:upper:]]$",
    b,
    perl = TRUE
  )) {
    return(TRUE)
  }

  a_digit <- grepl(
    "^[[:digit:]]$",
    a,
    perl = TRUE
  )

  b_digit <- grepl(
    "^[[:digit:]]$",
    b,
    perl = TRUE
  )

  a_alpha <- grepl(
    "^[[:alpha:]]$",
    a,
    perl = TRUE
  )

  b_alpha <- grepl(
    "^[[:alpha:]]$",
    b,
    perl = TRUE
  )

  # Ratio2 / 2Ratio.
  if ((a_digit && b_alpha) ||
      (a_alpha && b_digit)) {
    return(TRUE)
  }

  FALSE
}


auto_regex_range_has_natural_edges <- function(
    atoms,
    left,
    right) {

  left_ok <-
    left == 1L ||
    auto_regex_edge_boundary_natural(
      atoms[
        left - 1L,
        ,
        drop = FALSE
      ],
      atoms[
        left,
        ,
        drop = FALSE
      ]
    )

  right_ok <-
    right == nrow(atoms) ||
    auto_regex_edge_boundary_natural(
      atoms[
        right,
        ,
        drop = FALSE
      ],
      atoms[
        right + 1L,
        ,
        drop = FALSE
      ]
    )

  isTRUE(left_ok) &&
    isTRUE(right_ok)
}


auto_regex_content_replay_state <- function(
    application) {

  if (is.null(application) ||
      !is.data.frame(
        application$metadata
      )) {
    return(NULL)
  }

  metadata <- application$metadata
  n <- nrow(metadata)

  variants <- attr(
    metadata,
    "variant_id",
    exact = TRUE
  )

  if (is.null(variants) ||
      length(variants) != n) {
    variants <- rep(
      "",
      n
    )
  }

  conflicts <- rep(
    FALSE,
    n
  )

  if (is.data.frame(
    application$rows
  ) &&
  nrow(application$rows) == n &&
  "Conflict" %in%
  names(application$rows)) {

    conflicts <-
      application$rows$Conflict %in%
      TRUE
  }

  data.frame(
    Content = chr(
      metadata$Content
    ),
    VariantId = chr(
      variants
    ),
    Transformation =
      if ("Transformation" %in%
          names(metadata)) {
        chr(
          metadata$Transformation
        )
      } else {
        rep(
          "",
          n
        )
      },
    Conflict = conflicts,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}


auto_regex_semantic_match_profile <- function(
    stored_pattern,
    metadata,
    target_rows,
    semantic_spans) {

  empty <- list(
    Fraction = 0,
    MatchChars = 0L,
    SemanticChars = 0L,
    PredominantlySemantic = FALSE
  )

  if (!length(target_rows) ||
      !is.data.frame(semantic_spans) ||
      !nrow(semantic_spans) ||
      !all(
        c(
          "Row",
          "Start",
          "End"
        ) %in%
        names(semantic_spans)
      )) {
    return(empty)
  }

  spans <- semantic_spans

  # Semantic avoidance and semantic replacement are different questions.
  #
  # Any span that was confirmed by the selected downstream extractor is
  # biological/experimental information and should therefore influence
  # Content-regex minimization.
  #
  # SafeToGeneralize is intentionally NOT required here. That flag only says
  # whether the literal span may be replaced by a generalized regex atom.
  if ("ExtractionConfirmed" %in% names(spans)) {

    spans <- spans[
      spans$ExtractionConfirmed %in% TRUE,
      ,
      drop = FALSE
    ]

  } else if ("SafeToGeneralize" %in% names(spans)) {

    # Backward compatibility for older/manually constructed span tables that
    # predate ExtractionConfirmed.
    spans <- spans[
      spans$SafeToGeneralize %in% TRUE,
      ,
      drop = FALSE
    ]
  }

  spans <- spans[
    !is.na(spans$Row) &
      !is.na(spans$Start) &
      !is.na(spans$End) &
      spans$Row %in%
      target_rows,
    ,
    drop = FALSE
  ]

  if (!nrow(spans))
    return(empty)

  runtime <- regex_from_miraprot_storage(
    stored_pattern,
    "content"
  )

  runtime <- chr(runtime)

  if (length(runtime) != 1L ||
      !nzchar(runtime[[1L]])) {
    return(empty)
  }

  runtime <- runtime[[1L]]

  total_match <- 0L
  total_semantic <- 0L

  for (row in target_rows) {

    source <- chr(
      metadata$Column[[row]]
    )

    locations <- gregexpr(
      runtime,
      source,
      perl = TRUE,
      useBytes = FALSE
    )[[1L]]

    if (identical(
      locations,
      -1L
    )) {
      return(list(
        Fraction = 1,
        MatchChars = 0L,
        SemanticChars = 0L,
        PredominantlySemantic = TRUE
      ))
    }

    widths <- attr(
      locations,
      "match.length"
    )

    row_spans <- spans[
      as.integer(spans$Row) ==
        row,
      ,
      drop = FALSE
    ]

    best_fraction <- Inf
    best_match <- 0L
    best_semantic <- 0L

    for (i in seq_along(
      locations
    )) {

      start <- locations[[i]]
      width <- widths[[i]]

      if (is.na(start) ||
          is.na(width) ||
          start < 1L ||
          width <= 0L) {
        next
      }

      finish <-
        start + width - 1L

      semantic_mask <- rep(
        FALSE,
        width
      )

      if (nrow(row_spans)) {

        for (j in seq_len(
          nrow(row_spans)
        )) {

          overlap_start <- max(
            start,
            as.integer(
              row_spans$Start[[j]]
            )
          )

          overlap_end <- min(
            finish,
            as.integer(
              row_spans$End[[j]]
            )
          )

          if (overlap_start <=
              overlap_end) {

            semantic_mask[
              seq.int(
                overlap_start -
                  start + 1L,
                overlap_end -
                  start + 1L
              )
            ] <- TRUE
          }
        }
      }

      semantic_width <-
        sum(semantic_mask)

      fraction <-
        semantic_width / width

      # When the same regex occurs several times, prefer an occurrence that is
      # structural rather than one overlapping a confirmed biological span.
      if (fraction <
          best_fraction) {

        best_fraction <- fraction
        best_match <- width
        best_semantic <-
          semantic_width
      }
    }

    if (!is.finite(
      best_fraction
    )) {
      next
    }

    total_match <-
      total_match +
      best_match

    total_semantic <-
      total_semantic +
      best_semantic
  }

  if (!total_match)
    return(empty)

  fraction <-
    total_semantic /
    total_match

  list(
    Fraction = fraction,
    MatchChars =
      as.integer(
        total_match
      ),
    SemanticChars =
      as.integer(
        total_semantic
      ),
    PredominantlySemantic =
      fraction > 0.5
  )
}

auto_regex_content_redundancy_ladder <- function(
    states,
    minimal_index,
    n_atoms,
    maximum = 10L) {

  maximum <- suppressWarnings(
    as.integer(maximum)[[1L]]
  )

  if (is.na(maximum)) {
    maximum <- 10L
  }

  maximum <- max(
    0L,
    maximum
  )

  current_index <- as.integer(
    minimal_index
  )

  effective <- 0L

  state_keys <- paste0(
    states$Left,
    ":",
    states$Right
  )

  rows <- vector(
    "list",
    maximum + 1L
  )

  record_level <- function(requested) {

    data.frame(
      RequestedRedundancy = as.integer(requested),
      EffectiveRedundancy = as.integer(effective),
      StateIndex = as.integer(current_index),
      Include = chr(states$Include[[current_index]]),
      SemanticFraction = as.numeric(states$SemanticFraction[[current_index]]),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  }

  rows[[1L]] <- record_level(0L)

  if (maximum > 0L) {

    for (step in seq_len(maximum)) {

      current <- states[
        current_index,
        ,
        drop = FALSE
      ]

      neighbour_keys <- character()

      if (current$Left[[1L]] > 1L) {

        neighbour_keys <- c(
          neighbour_keys,
          paste0(
            current$Left[[1L]] - 1L,
            ":",
            current$Right[[1L]]
          )
        )
      }

      if (current$Right[[1L]] < n_atoms) {

        neighbour_keys <- c(
          neighbour_keys,
          paste0(
            current$Left[[1L]],
            ":",
            current$Right[[1L]] + 1L
          )
        )
      }

      neighbours <- match(
        neighbour_keys,
        state_keys
      )

      neighbours <- neighbours[
        !is.na(neighbours)
      ]

      if (length(neighbours)) {

        restoration_fraction <- vapply(
          neighbours,
          function(index) {

            added_match <- max(
              0,
              states$MatchChars[[index]] -
                current$MatchChars[[1L]]
            )

            added_semantic <- max(
              0,
              states$SemanticChars[[index]] -
                current$SemanticChars[[1L]]
            )

            if (added_match > 0) {

              return(
                added_semantic /
                  added_match
              )
            }

            if (states$SemanticFraction[[index]] >
                current$SemanticFraction[[1L]]) {
              return(1)
            }

            0
          },
          numeric(1)
        )

        usable <- which(
          restoration_fraction <= 0.5 &
            !states$PredominantlySemantic[neighbours]
        )

        if (length(usable)) {

          neighbours <- neighbours[usable]

          restoration_fraction <-
            restoration_fraction[usable]

          neighbour_order <- order(
            restoration_fraction,
            states$SemanticFraction[neighbours],
            -as.integer(states$NaturalEdges[neighbours]),
            states$Length[neighbours],
            states$Include[neighbours],
            method = "radix"
          )

          next_index <- neighbours[
            neighbour_order[[1L]]
          ]

          if (!identical(
            next_index,
            current_index
          )) {

            current_index <- next_index
            effective <- effective + 1L
          }
        }
      }

      # Always cache every requested level. Once no further structural state
      # can safely be restored, later requested levels simply resolve to the
      # same saturated regex/effective redundancy.
      rows[[step + 1L]] <-
        record_level(step)
    }
  }

  result <- do.call(
    rbind,
    rows
  )

  rownames(result) <- NULL

  result
}
