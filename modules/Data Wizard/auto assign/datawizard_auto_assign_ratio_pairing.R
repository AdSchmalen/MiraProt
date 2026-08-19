# Auto-Assign helper family. Loaded by datawizard_auto_assign_utils.R.

# Force case-sensitive matching for a given regex (idempotent, NA-safe)
cs_wrap <- function(x) {
  if (is.null(x) || is.na(x) || !nzchar(x)) return(x)
  if (grepl("^\\(\\?-i:", x)) return(x)  # already wrapped
  paste0("(?-i:", x, ")")
}

.ratio_pairing_tokenize <- function(header) {
  header <- as.character(header)

  if (length(header) != 1L ||
      is.na(header) ||
      !nzchar(trimws(header))) {
    return(character())
  }

  hits <- gregexpr(
    "[[:alnum:]]+",
    header,
    perl = TRUE
  )[[1L]]

  if (length(hits) == 1L && hits[[1L]] < 0L) {
    return(character())
  }

  tokens <- regmatches(
    header,
    list(hits)
  )[[1L]]

  tokens <- tokens[
    !is.na(tokens) &
      nzchar(tokens)
  ]

  tokens
}


.ratio_pairing_deletion_signatures <- function(header) {
  tokens <- .ratio_pairing_tokenize(header)

  empty <- data.frame(
    Signature = character(),
    RetainedTokens = integer(),
    RetainedChars = integer(),
    RemovedTokens = integer(),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  n <- length(tokens)

  if (n < 2L) {
    return(empty)
  }

  candidates <- list()

  add_candidate <- function(keep) {
    kept <- tokens[keep]

    # A one-token structural key is too weak for generic pairing.
    if (length(kept) < 2L) {
      return()
    }

    # Do not allow a purely numeric identity.
    if (!any(grepl("[[:alpha:]]", kept))) {
      return()
    }

    signature <- paste(
      kept,
      collapse = "_"
    )

    candidates[[length(candidates) + 1L]] <<-
      data.frame(
        Signature = signature,
        RetainedTokens = length(kept),
        RetainedChars = sum(nchar(kept, type = "chars")),
        RemovedTokens = n - length(kept),
        stringsAsFactors = FALSE,
        check.names = FALSE
      )
  }

  # No deletion is a legal structural signature.
  add_candidate(seq_len(n))

  # Remove one arbitrary contiguous token span.
  #
  # This deliberately permits spans of any length:
  #
  # Merged_Abundance_Ratio_P_Value_1
  #                    ^^^^^^^^^
  #
  # and
  #
  # Merged_Abundance_Ratio_Adj_P_Value_1
  #                    ^^^^^^^^^^^^^
  #
  # can therefore share Merged_Abundance_Ratio_1.
  for (start in seq_len(n)) {
    for (finish in seq.int(start, n)) {
      keep <- setdiff(
        seq_len(n),
        seq.int(start, finish)
      )

      add_candidate(keep)
    }
  }

  if (!length(candidates)) {
    return(empty)
  }

  result <- do.call(
    rbind,
    candidates
  )

  # Keep the strongest representation of duplicate signatures.
  result <- result[
    order(
      -result$RetainedTokens,
      -result$RetainedChars,
      result$RemovedTokens,
      result$Signature,
      method = "radix"
    ),
    ,
    drop = FALSE
  ]

  result <- result[
    !duplicated(result$Signature),
    ,
    drop = FALSE
  ]

  rownames(result) <- NULL

  result
}


.ratio_pairing_structural_groups <- function(
    columns,
    content,
    rows,
    ratio_types) {

  empty <- data.frame(
    Row = integer(),
    GroupKey = character(),
    Base = character(),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  rows <- sort(unique(as.integer(rows)))

  rows <- rows[
    !is.na(rows) &
      rows >= 1L &
      rows <= length(columns) &
      rows <= length(content)
  ]

  if (!length(rows)) {
    return(empty)
  }

  role_rows <- lapply(
    ratio_types,
    function(role) {
      rows[
        content[rows] == role
      ]
    }
  )

  # Structural triplet inference requires all three complementary roles.
  if (any(lengths(role_rows) == 0L)) {
    return(empty)
  }

  signature_rows <- list()

  for (row in rows) {
    signatures <- .ratio_pairing_deletion_signatures(
      columns[[row]]
    )

    if (!nrow(signatures)) {
      next
    }

    signatures$Row <- row
    signatures$Content <- content[[row]]

    signature_rows[[length(signature_rows) + 1L]] <-
      signatures
  }

  if (!length(signature_rows)) {
    return(empty)
  }

  signatures <- do.call(
    rbind,
    signature_rows
  )

  # For every structural signature, require EXACTLY one source row from each
  # complementary ratio Content role.
  candidate_keys <- unique(
    signatures$Signature
  )

  candidates <- list()

  for (signature in candidate_keys) {
    subset <- signatures[
      signatures$Signature == signature,
      ,
      drop = FALSE
    ]

    matched_rows <- integer()

    valid <- TRUE

    for (role in ratio_types) {
      role_matches <- unique(
        subset$Row[
          subset$Content == role
        ]
      )

      if (length(role_matches) != 1L) {
        valid <- FALSE
        break
      }

      matched_rows <- c(
        matched_rows,
        role_matches[[1L]]
      )
    }

    if (!valid ||
        length(unique(matched_rows)) != length(ratio_types)) {
      next
    }

    matched <- subset[
      subset$Row %in% matched_rows,
      ,
      drop = FALSE
    ]

    # Conservative confidence guard:
    # retain at least half the tokens of every participating header.
    source_token_counts <- vapply(
      columns[matched_rows],
      function(x) length(.ratio_pairing_tokenize(x)),
      integer(1)
    )

    retained_by_row <- vapply(
      matched_rows,
      function(row) {
        values <- matched$RetainedTokens[
          matched$Row == row
        ]

        if (!length(values)) {
          0L
        } else {
          max(values)
        }
      },
      integer(1)
    )

    if (any(
      retained_by_row <
      pmax(2L, ceiling(source_token_counts / 2))
    )) {
      next
    }

    candidates[[length(candidates) + 1L]] <-
      data.frame(
        Signature = signature,
        Row1 = matched_rows[[1L]],
        Row2 = matched_rows[[2L]],
        Row3 = matched_rows[[3L]],
        RetainedTokens = min(retained_by_row),
        RetainedChars = nchar(
          signature,
          type = "chars"
        ),
        stringsAsFactors = FALSE,
        check.names = FALSE
      )
  }

  if (!length(candidates)) {
    return(empty)
  }

  candidates <- do.call(
    rbind,
    candidates
  )

  # Strongest and most informative structural identity wins first.
  candidates <- candidates[
    order(
      -candidates$RetainedTokens,
      -candidates$RetainedChars,
      candidates$Signature,
      method = "radix"
    ),
    ,
    drop = FALSE
  ]

  used_rows <- integer()
  accepted <- list()

  for (i in seq_len(nrow(candidates))) {
    candidate_rows <- as.integer(c(
      candidates$Row1[[i]],
      candidates$Row2[[i]],
      candidates$Row3[[i]]
    ))

    # One row may belong to only one inferred triplet.
    if (any(candidate_rows %in% used_rows)) {
      next
    }

    signature <- candidates$Signature[[i]]

    accepted[[length(accepted) + 1L]] <-
      data.frame(
        Row = candidate_rows,
        GroupKey = rep(
          paste0("structural:", signature),
          length(candidate_rows)
        ),
        Base = rep(
          signature,
          length(candidate_rows)
        ),
        stringsAsFactors = FALSE,
        check.names = FALSE
      )

    used_rows <- c(
      used_rows,
      candidate_rows
    )
  }

  if (!length(accepted)) {
    return(empty)
  }

  result <- do.call(
    rbind,
    accepted
  )

  result <- result[
    order(result$Row),
    ,
    drop = FALSE
  ]

  rownames(result) <- NULL

  result
}

############
# Helper Functions

#' Fill deterministic pairing surrogates for opaque ratio triplets
#'
#' This is a post-inference compatibility enrichment. It MUST NOT be used by
#' Auto RegEx as evidence that Numerator/Denominator were extracted from a
#' header.
#'
#' Real Numerator/Denominator values are never overwritten. A synthetic pair is
#' created only when a complete, unambiguous ratio/p-value/adjusted-p-value
#' triplet can be identified from an existing ContrastId or from a shared
#' header stem.
#'
#' @param df Metadata data frame after Content and real ratio rules were applied.
#' @param eligible_rows Optional row indices allowed to receive enrichment.
#' @param debug_log Optional function(message, level).
#' @return Metadata data frame, optionally carrying ratio_pairing_diagnostics.
finalize_ratio_pairing_surrogates <- function(
    df,
    eligible_rows = NULL,
    debug_log = NULL) {

  if (!is.data.frame(df) ||
      nrow(df) == 0L ||
      !all(c("Column", "Content") %in% names(df))) {
    return(df)
  }

  ratio_types <- c(
    "Abundance Ratio",
    "Abundance Ratio p-Value",
    "Abundance Ratio Adj. p-Value"
  )

  if (!"Numerator" %in% names(df)) {
    df$Numerator <- NA_character_
  }

  if (!"Denominator" %in% names(df)) {
    df$Denominator <- NA_character_
  }

  clean_chr <- function(x) {
    x <- as.character(x)
    x[is.na(x)] <- ""
    trimws(x)
  }

  canonical_ratio_type <- function(x) {
    x <- trimws(as.character(x))

    at <- match(
      tolower(x),
      tolower(ratio_types)
    )

    if (is.na(at)) "" else ratio_types[[at]]
  }

  normalize_stem <- function(x) {
    x <- trimws(as.character(x))

    # Trim punctuation that only separates the stem from the Content phrase.
    x <- gsub(
      "^[[:space:]_.:/-]+|[[:space:]_.:/-]+$",
      "",
      x,
      perl = TRUE
    )

    # Make the surrogate stable and metadata-safe.
    x <- gsub("[[:space:]]+", "_", x, perl = TRUE)
    x <- gsub("[^[:alnum:]_.-]+", "_", x, perl = TRUE)
    x <- gsub("_+", "_", x, perl = TRUE)
    x <- gsub("^_+|_+$", "", x, perl = TRUE)

    x
  }

  header_stem <- function(column, content) {
    column <- as.character(column)[[1L]]
    content <- as.character(content)[[1L]]

    if (is.na(column) ||
        is.na(content) ||
        !nzchar(column) ||
        !nzchar(content)) {
      return("")
    }

    lower_column <- tolower(column)
    lower_content <- tolower(content)

    hits <- gregexpr(
      lower_content,
      lower_column,
      fixed = TRUE
    )[[1L]]

    hits <- hits[hits > 0L]

    # Ambiguous or absent Content occurrence is not sufficient evidence.
    if (length(hits) != 1L) {
      return("")
    }

    start <- hits[[1L]]
    finish <- start + nchar(content, type = "chars") - 1L

    left <- if (start > 1L) {
      substr(column, 1L, start - 1L)
    } else {
      ""
    }

    right <- if (finish < nchar(column, type = "chars")) {
      substr(
        column,
        finish + 1L,
        nchar(column, type = "chars")
      )
    } else {
      ""
    }

    normalize_stem(
      paste(left, right, sep = "_")
    )
  }

  content <- vapply(
    df$Content,
    canonical_ratio_type,
    character(1)
  )

  rows <- which(nzchar(content))

  if (!is.null(eligible_rows)) {
    eligible_rows <- unique(
      suppressWarnings(as.integer(eligible_rows))
    )

    eligible_rows <- eligible_rows[
      !is.na(eligible_rows) &
        eligible_rows >= 1L &
        eligible_rows <= nrow(df)
    ]

    rows <- intersect(rows, eligible_rows)
  }

  if (!length(rows)) {
    return(df)
  }

  columns <- clean_chr(df$Column)
  numerators <- clean_chr(df$Numerator)
  denominators <- clean_chr(df$Denominator)

  stems <- rep("", nrow(df))

  stems[rows] <- mapply(
    header_stem,
    columns[rows],
    content[rows],
    USE.NAMES = FALSE
  )

  contrast_ids <- if ("ContrastId" %in% names(df)) {
    clean_chr(df$ContrastId)
  } else {
    rep("", nrow(df))
  }

  group_key <- rep("", nrow(df))
  group_source <- rep("", nrow(df))

  # Prefer an existing authoritative relationship identity.
  with_contrast <- rows[
    nzchar(contrast_ids[rows])
  ]

  if (length(with_contrast)) {
    group_key[with_contrast] <- paste0(
      "contrast:",
      contrast_ids[with_contrast]
    )

    group_source[with_contrast] <- "ContrastId"
  }

  # Otherwise use literal Content removal only when it already establishes one
  # complete, unambiguous ratio-result triplet.
  #
  # An incomplete stem group must remain available to the stronger structural
  # backbone fallback below. This matters when a canonical Content label and a
  # source header differ only in presentation punctuation, for example
  # "Adj. p-Value" versus "Adj P-Value".
  fallback <- setdiff(
    rows,
    with_contrast
  )

  fallback <- fallback[
    nzchar(
      stems[fallback]
    )
  ]

  if (length(fallback)) {

    stem_groups <- split(
      fallback,
      stems[fallback],
      drop = TRUE
    )

    for (stem in names(stem_groups)) {

      stem_rows <- sort(
        unique(
          as.integer(
            stem_groups[[stem]]
          )
        )
      )

      stem_content <- content[
        stem_rows
      ]

      complete_triplet <-
        length(stem_rows) ==
        length(ratio_types) &&
        all(
          vapply(
            ratio_types,
            function(role) {
              sum(
                stem_content == role
              ) == 1L
            },
            logical(1)
          )
        )

      if (!complete_triplet) {
        next
      }

      group_key[
        stem_rows
      ] <- paste0(
        "header:",
        stem
      )

      group_source[
        stem_rows
      ] <- "header_stem"
    }
  }

  # Third-level fallback:
  # derive a role-independent structural backbone for ratio/p/q rows that could
  # not be grouped through ContrastId or literal Content removal.
  #
  # This operates only on still-unassigned ratio rows and requires a unique
  # one-row-per-role triplet before assigning any relationship identity.

  structural_rows <- rows[
    !nzchar(group_key[rows])
  ]

  if (length(structural_rows)) {

    structural_groups <-
      .ratio_pairing_structural_groups(
        columns = columns,
        content = content,
        rows = structural_rows,
        ratio_types = ratio_types
      )

    if (nrow(structural_groups)) {
      group_key[
        structural_groups$Row
      ] <- structural_groups$GroupKey

      group_source[
        structural_groups$Row
      ] <- "structural_backbone"

      # Reuse the already-established `stems` vector as the canonical synthetic
      # base consumed later in this function.
      stems[
        structural_groups$Row
      ] <- structural_groups$Base
    }
  }

  candidates <- rows[
    nzchar(group_key[rows])
  ]

  if (!length(candidates)) {
    return(df)
  }

  groups <- split(
    candidates,
    group_key[candidates],
    drop = TRUE
  )

  diagnostics <- list()

  add_diagnostic <- function(
    key,
    rows,
    action,
    reason = "",
    numerator = "",
    denominator = "") {

    diagnostics[[length(diagnostics) + 1L]] <<-
      data.frame(
        GroupKey = key,
        Rows = paste(rows, collapse = ","),
        Action = action,
        Reason = reason,
        Numerator = numerator,
        Denominator = denominator,
        stringsAsFactors = FALSE,
        check.names = FALSE
      )
  }

  for (key in names(groups)) {

    group_rows <- sort(
      as.integer(groups[[key]])
    )

    group_content <- content[group_rows]

    counts <- table(
      factor(
        group_content,
        levels = ratio_types
      )
    )

    # Require exactly:
    #   1 ratio
    #   1 raw p-value
    #   1 adjusted p-value
    if (length(group_rows) != 3L ||
        any(counts != 1L)) {

      add_diagnostic(
        key,
        group_rows,
        "abstained",
        paste(
          "group is not one complete unique",
          "ratio/p-value/adjusted-p-value triplet"
        )
      )

      next
    }

    num <- numerators[group_rows]
    den <- denominators[group_rows]

    num_present <- nzchar(num)
    den_present <- nzchar(den)

    # Half-filled biological metadata is unsafe.
    if (any(xor(num_present, den_present))) {

      add_diagnostic(
        key,
        group_rows,
        "abstained",
        "partial numerator/denominator metadata is present"
      )

      next
    }

    complete <- num_present & den_present

    real_pairs <- unique(
      paste(
        num[complete],
        den[complete],
        sep = "\034"
      )
    )

    # Different real pairs inside one candidate triplet mean our grouping
    # assumption is wrong. Never repair such a conflict automatically.
    if (length(real_pairs) > 1L) {

      add_diagnostic(
        key,
        group_rows,
        "abstained",
        "conflicting numerator/denominator pairs inside triplet"
      )

      next
    }

    blank <- !num_present & !den_present

    if (!any(blank)) {
      next
    }

    if (length(real_pairs) == 1L) {

      # One member already has real biology. Propagate that exact pair rather
      # than inventing a surrogate for its blank siblings.
      source_position <- which(complete)[[1L]]
      source_row <- group_rows[[source_position]]

      new_num <- numerators[[source_row]]
      new_den <- denominators[[source_row]]
      action <- "propagated_real_pair"

    } else {

      # Every member is blank. Derive a deterministic relationship label.
      if (all(group_source[group_rows] == "ContrastId")) {

        base <- normalize_stem(
          sub(
            "^contrast:",
            "",
            key
          )
        )

      } else {

        bases <- unique(
          stems[group_rows]
        )

        bases <- bases[nzchar(bases)]

        if (length(bases) != 1L) {

          add_diagnostic(
            key,
            group_rows,
            "abstained",
            "no unique stable header stem"
          )

          next
        }

        base <- bases[[1L]]
      }

      if (!nzchar(base)) {

        add_diagnostic(
          key,
          group_rows,
          "abstained",
          "synthetic pairing stem is empty"
        )

        next
      }

      # Deliberately recognizable as non-biological compatibility metadata.
      new_num <- paste0(base, "__PAIR_X")
      new_den <- paste0(base, "__PAIR_Y")

      action <- "synthetic_pair"
    }

    fill_rows <- group_rows[blank]

    # This assignment can only touch rows proven blank above.
    df$Numerator[fill_rows] <- new_num
    df$Denominator[fill_rows] <- new_den

    numerators[fill_rows] <- new_num
    denominators[fill_rows] <- new_den

    add_diagnostic(
      key,
      fill_rows,
      action,
      numerator = new_num,
      denominator = new_den
    )
  }

  # Final fallback:
  # any eligible ratio row that still has BOTH Numerator and Denominator blank
  # receives its own complete-header identity.
  #
  # This intentionally does not claim a relationship with any differently named
  # ratio/p/q sibling. Its only job is to keep downstream metadata consumers and
  # dropdowns from receiving unusable blank identifiers.
  #
  # Partial real values remain untouched.

  numerators <- clean_chr(df$Numerator)
  denominators <- clean_chr(df$Denominator)

  header_fallback_rows <- rows[
    !nzchar(numerators[rows]) &
      !nzchar(denominators[rows])
  ]

  if (length(header_fallback_rows)) {

    for (row in header_fallback_rows) {

      header <- trimws(columns[[row]])

      if (!nzchar(header)) {
        add_diagnostic(
          paste0("row:", row),
          row,
          "abstained",
          "complete-header fallback unavailable because Column is blank"
        )

        next
      }

      new_num <- paste0(
        header,
        "_X"
      )

      new_den <- paste0(
        header,
        "_Y"
      )

      df$Numerator[[row]] <- new_num
      df$Denominator[[row]] <- new_den

      numerators[[row]] <- new_num
      denominators[[row]] <- new_den

      add_diagnostic(
        paste0(
          "header_fallback:",
          header
        ),
        row,
        "header_fallback_pair",
        "no unambiguous triplet relationship could be established",
        numerator = new_num,
        denominator = new_den
      )
    }
  }

  if (length(diagnostics)) {

    diagnostics <- do.call(
      rbind,
      diagnostics
    )

    rownames(diagnostics) <- NULL

    attr(
      df,
      "ratio_pairing_diagnostics"
    ) <- diagnostics

    if (is.function(debug_log)) {

      enriched <- sum(
        diagnostics$Action %in%
          c(
            "synthetic_pair",
            "propagated_real_pair",
            "header_fallback_pair"
          )
      )

      abstained <- sum(
        diagnostics$Action ==
          "abstained"
      )

      debug_log(
        sprintf(
          paste0(
            "Ratio pairing enrichment: ",
            "%d group(s) enriched, ",
            "%d group(s) abstained."
          ),
          enriched,
          abstained
        ),
        2
      )
    }
  }

  df
}
