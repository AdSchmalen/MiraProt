# Auto-Assign helper family. Loaded by datawizard_auto_assign_utils.R.

#' Identify sample values that still need a name
#'
#' This helper operates only on an already-resolved vector. Missing, absent, or
#' blank values all require a generated sample name.
#' @param sample resolved vector of sample names
#' @return logical vector, or a single `TRUE` for a zero-length input
sample_name_needed <- function(sample) {
  if (!length(sample)) return(TRUE)
  sample <- as.character(sample)
  is.na(sample) | !nzchar(trimws(sample))
}

#' Identify content types that are eligible to bear sample names
#'
#' Eligibility delegates to the canonical content classifier so Auto-Assign
#' cannot drift from the definition shared by the rest of Data Wizard.
#' @param content resolved vector of content types
#' @return logical vector indicating sample-bearing content
sample_name_eligible <- function(content) {
  is_sample_bearing_content(content)
}

# Private, pure lexical helpers used by sample-name construction.  These accept
# resolved character vectors (never reactive expressions) and intentionally
# return only ordinary lists and data frames so that their results are easy to
# inspect and test outside a Shiny session.
.tokenize_source_headers <- function(headers, separators = NULL) {
  headers <- as.character(headers)
  if (is.null(separators)) separators <- NA_character_
  separators <- rep_len(as.character(separators), length(headers))

  tokenize_one <- function(source, separator) {
    empty <- data.frame(
      text = character(), kind = character(), start = integer(), end = integer(),
      left = integer(), right = integer(), shape = character(),
      stringsAsFactors = FALSE
    )
    if (is.na(source) || !nzchar(source)) return(empty)

    meaningful <- length(separator) == 1L && !is.na(separator) && nzchar(trimws(separator))
    if (meaningful) {
      hits <- gregexpr(separator, source, perl = TRUE)[[1L]]
      hit_lengths <- attr(hits, "match.length")
      if (identical(hits[1L], -1L)) {
        starts <- 1L
        ends <- nchar(source)
        kinds <- "field"
      } else {
        # Include gaps and matches: separators are first-class lexemes rather
        # than information discarded by strsplit().
        starts <- integer()
        ends <- integer()
        kinds <- character()
        cursor <- 1L
        for (i in seq_along(hits)) {
          if (hits[i] > cursor) {
            starts <- c(starts, cursor); ends <- c(ends, hits[i] - 1L)
            kinds <- c(kinds, "field")
          }
          if (hit_lengths[i] > 0L) {
            starts <- c(starts, hits[i]); ends <- c(ends, hits[i] + hit_lengths[i] - 1L)
            kinds <- c(kinds, "separator")
          }
          cursor <- max(cursor, hits[i] + hit_lengths[i])
        }
        if (cursor <= nchar(source)) {
          starts <- c(starts, cursor); ends <- c(ends, nchar(source))
          kinds <- c(kinds, "field")
        }
      }
    } else {
      hits <- gregexpr("[[:alnum:]]+(?:[-_][[:alnum:]]+)*|[^[:alnum:]]+", source,
                       perl = TRUE)[[1L]]
      starts <- as.integer(hits)
      ends <- starts + attr(hits, "match.length") - 1L
      text <- substring(source, starts, ends)
      kinds <- ifelse(grepl("^[[:alnum:]]", text), "field", "separator")
    }

    text <- substring(source, starts, ends)
    field_number <- cumsum(kinds == "field")
    field_count <- sum(kinds == "field")
    left <- ifelse(kinds == "field", field_number, NA_integer_)
    right <- ifelse(kinds == "field", field_count - field_number + 1L, NA_integer_)
    shape_one <- function(x) {
      chars <- strsplit(x, "", fixed = TRUE)[[1L]]
      classes <- ifelse(grepl("[[:upper:]]", chars), "A",
                 ifelse(grepl("[[:lower:]]", chars), "a",
                 ifelse(grepl("[[:digit:]]", chars), "9", chars)))
      paste(rle(classes)$values, collapse = "")
    }
    data.frame(text = text, kind = kinds, start = starts, end = ends,
      left = as.integer(left), right = as.integer(right),
      shape = vapply(text, shape_one, character(1)), stringsAsFactors = FALSE)
  }

  lapply(seq_along(headers), function(i) tokenize_one(headers[i], separators[i]))
}

.locate_source_conditions <- function(headers, conditions) {
  headers <- as.character(headers)
  conditions <- rep_len(as.character(conditions), length(headers))
  lapply(seq_along(headers), function(i) {
    source <- headers[i]
    condition <- conditions[i]
    matches <- data.frame(start = integer(), end = integer(), protected = logical(),
                          stringsAsFactors = FALSE)
    if (is.na(condition) || !nzchar(trimws(condition))) {
      return(list(status = "not_provided", matches = matches, protected_range = NULL))
    }
    condition <- trimws(condition)
    if (is.na(source) || nchar(condition) > nchar(source)) {
      return(list(status = "absent", matches = matches, protected_range = NULL))
    }
    candidates <- seq_len(nchar(source) - nchar(condition) + 1L)
    starts <- candidates[substring(source, candidates,
      candidates + nchar(condition) - 1L) == condition]
    matches <- data.frame(start = starts, end = starts + nchar(condition) - 1L,
                          protected = length(starts) == 1L,
                          stringsAsFactors = FALSE)
    if (length(starts) == 1L) {
      list(status = "unique", matches = matches, protected_range = matches)
    } else {
      # Ambiguity and absence are explicit.  In particular, do not choose a
      # convenient occurrence and thereby remove guessed pieces of the header.
      list(status = if (length(starts)) "ambiguous" else "absent",
           matches = matches, protected_range = NULL)
    }
  })
}

#' Build deterministic sample names from source column structure
#'
#' Tokenization and uniqueness are deliberately evaluated separately for each
#' content type.  The returned character vector carries a row-level diagnostics
#' data frame in its `diagnostics` attribute.
#' @param columns original metadata column names
#' @param conditions extracted condition labels
#' @param content_type content label for each row (or one label for all rows)
#' @param separators optional, meaningful regular expression used to split names
#' @return sample-name character vector with a `diagnostics` attribute
build_unique_sample_names <- function(columns, conditions, content_type,
                                      separators = NULL) {
  columns <- as.character(columns)
  n <- length(columns)
  conditions <- rep_len(as.character(conditions), n)
  content_type <- rep_len(as.character(content_type), n)
  separators <- rep_len(as.character(separators), n)

  # A file routinely contains the same source header in several content rows.
  # Memoize the two pure lexical operations for the duration of one build so
  # repeated headers do not repeatedly pay the regular-expression cost.
  lexical_cache <- new.env(hash = TRUE, parent = emptyenv())
  location_cache <- new.env(hash = TRUE, parent = emptyenv())
  prediction_cache <- new.env(hash = TRUE, parent = emptyenv())
  cache_stats <- new.env(parent = emptyenv())
  cache_stats$tokenization_misses <- 0L
  cache_stats$location_misses <- 0L
  cache_stats$candidate_prediction_misses <- 0L
  cache_key <- function(...) paste(vapply(list(...), function(value) {
    if (length(value) == 0L) return("0:")
    value <- ifelse(is.na(value), "<NA>", as.character(value))
    paste0(nchar(value, type = "bytes"), ":", value, collapse = "\034")
  }, character(1)), collapse = "\035")
  cached_tokenize <- function(source, separator) {
    key <- cache_key(source, separator)
    if (!exists(key, lexical_cache, inherits = FALSE)) {
      cache_stats$tokenization_misses <- cache_stats$tokenization_misses + 1L
      assign(key, .tokenize_source_headers(source, separator)[[1L]], lexical_cache)
    }
    get(key, lexical_cache, inherits = FALSE)
  }
  cached_location <- function(source, condition) {
    key <- cache_key(source, condition)
    if (!exists(key, location_cache, inherits = FALSE)) {
      cache_stats$location_misses <- cache_stats$location_misses + 1L
      assign(key, .locate_source_conditions(source, condition)[[1L]], location_cache)
    }
    get(key, location_cache, inherits = FALSE)
  }
  bounded_combinations <- function(items, size, limit) {
    out <- list()
    visit <- function(prefix, start) {
      if (length(out) >= limit) return(invisible(NULL))
      needed <- size - length(prefix)
      if (!needed) {
        out[[length(out) + 1L]] <<- prefix
        return(invisible(NULL))
      }
      last <- length(items) - needed + 1L
      if (start > last) return(invisible(NULL))
      for (index in seq.int(start, last)) {
        visit(c(prefix, items[index]), index + 1L)
        if (length(out) >= limit) break
      }
      invisible(NULL)
    }
    visit(integer(), 1L)
    out
  }

  clean_tokens <- function(x, separator, condition) {
    if (is.na(x) || !nzchar(x)) return(character())
    lexemes <- cached_tokenize(x, separator)
    fields <- lexemes$kind == "field"
    location <- cached_location(x, condition)
    if (identical(location$status, "unique")) {
      protected <- location$protected_range
      # Protect the complete condition, including a condition spanning multiple
      # lexical fields. Any intersecting field belongs to that protected range.
      wholly_protected <- lexemes$start >= protected$start & lexemes$end <= protected$end
      fields <- fields & !wholly_protected
    }
    out <- lexemes$text[fields]
    out <- trimws(gsub("^[[:punct:]]+|[[:punct:]]+$", "", out))
    out[!is.na(out) & nzchar(out)]
  }
  condition_present <- function(x) !is.na(x) && nzchar(trimws(x))
  explicit_bracket_identifier <- function(x) {
    if (is.na(x) || !nzchar(x)) return(NA_character_)
    hit <- regexec("^\\s*\\[([^][]+)\\]", x, perl = TRUE)
    pieces <- regmatches(x, hit)[[1L]]
    if (length(pieces) == 2L && nzchar(trimws(pieces[[2L]])))
      trimws(pieces[[2L]]) else NA_character_
  }

  samples <- rep(NA_character_, n)
  diagnostics <- data.frame(
    original_column = columns, extracted_condition = conditions,
    condition_match_status = rep("", n), candidate_tokens = rep("", n),
    discarded_invariant_tokens = rep("", n),
    selected_discriminator_tokens = rep("", n), final_sample = rep("", n),
    unique_within_content = rep(FALSE, n), fallback_reason = rep("", n),
    condition_start = rep(NA_integer_, n), condition_end = rep(NA_integer_, n),
    condition_location_status = rep("", n), candidate_position = rep("", n),
    candidate_direction = rep("", n), source_token = rep("", n),
    normalization = rep("", n), candidate_tier = rep(NA_integer_, n),
    uniqueness_scope = rep("", n), rejected_candidate_count = rep(0L, n),
    selection_reason = rep("", n),
    stringsAsFactors = FALSE
  )

  group_key <- ifelse(is.na(content_type), "<NA>", content_type)
  for (group in unique(group_key)) {
    rows <- which(group_key == group)
    locations <- lapply(rows, function(i) cached_location(columns[i], conditions[i]))
    diagnostics$condition_match_status[rows] <- vapply(locations, `[[`, "", "status")
    diagnostics$condition_location_status[rows] <- diagnostics$condition_match_status[rows]
    diagnostics$condition_start[rows] <- vapply(locations, function(location) {
      if (is.null(location$protected_range)) NA_integer_ else location$protected_range$start[[1L]]
    }, integer(1))
    diagnostics$condition_end[rows] <- vapply(locations, function(location) {
      if (is.null(location$protected_range)) NA_integer_ else location$protected_range$end[[1L]]
    }, integer(1))
    diagnostics$uniqueness_scope[rows] <- if (identical(group, "<NA>")) "<NA>" else group
    tokens <- lapply(rows, function(i) clean_tokens(columns[i], separators[i], conditions[i]))
    width <- max(c(0L, lengths(tokens)))
    mat <- matrix(NA_character_, nrow = length(rows), ncol = width)
    if (width) for (i in seq_along(tokens)) mat[i, seq_along(tokens[[i]])] <- tokens[[i]]

    invariant <- if (width) vapply(seq_len(width), function(j) {
      values <- unique(mat[, j][!is.na(mat[, j]) & nzchar(mat[, j])])
      length(values) == 1L && all(!is.na(mat[, j]))
    }, logical(1)) else logical()
    varying <- if (width) which(vapply(seq_len(width), function(j) {
      length(unique(mat[, j])) > 1L
    }, logical(1)) & !invariant) else integer()
    discriminator_positions <- varying
    for (i in seq_along(rows)) {
      diagnostics$candidate_tokens[rows[i]] <- paste(tokens[[i]], collapse = " | ")
      diagnostics$discarded_invariant_tokens[rows[i]] <- paste(
        mat[i, which(invariant)], collapse = " | ")
    }

    # A representation is ranked only after its value has been combined with
    # the complete condition.  This is important: an identifier may repeat
    # between sample families while identifier_condition remains unambiguous.
    append_condition <- function(values) vapply(seq_along(values), function(i) {
      bits <- values[[i]]
      if (condition_present(conditions[rows[i]]))
        bits <- c(bits, trimws(conditions[rows[i]]))
      bits <- bits[!is.na(bits) & nzchar(bits)]
      if (length(bits)) paste(bits, collapse = "_") else NA_character_
    }, character(1))
    add_candidate <- function(store, values, safety, positions, label) {
      if (is.matrix(values)) values <- apply(values, 1L, paste, collapse = "_")
      values <- as.character(values)
      if (length(values) != length(rows) || any(is.na(values) | !nzchar(values))) return(store)
      composed <- append_condition(as.list(values))
      # Different structural paths can predict the same names.  Retaining one
      # canonical prediction bounds both ranking and collision refinement.
      prediction_key <- cache_key(group, composed)
      if (exists(prediction_key, prediction_cache, inherits = FALSE)) return(store)
      assign(prediction_key, TRUE, prediction_cache)
      cache_stats$candidate_prediction_misses <-
        cache_stats$candidate_prediction_misses + 1L
      distance <- if (length(positions)) min(width - positions) else width + 1L
      store[[length(store) + 1L]] <- list(
        values = values, composed = composed, safety = safety,
        components = max(1L, length(positions)), length = sum(nchar(values)),
        stability = -sum(!is.na(mat[, positions, drop = FALSE])),
        proximity = distance, position = paste(sprintf("%05d", positions), collapse = ","),
        label = label, unique = !anyDuplicated(composed))
      store
    }

    candidates <- list()
    bracket_ids <- vapply(columns[rows], explicit_bracket_identifier, character(1))
    bracket_safety <- if (all(!is.na(bracket_ids)) &&
        all(grepl("^[[:digit:]]+$", bracket_ids))) 0L else 3L
    candidates <- add_candidate(candidates, bracket_ids, bracket_safety, integer(),
                                "bracket identifier")
    if (length(discriminator_positions)) {
      # Singles include conservative normalizations.  A complete numeric field
      # is safest; a tail is admitted only when every value has the same
      # decoration and the normalized candidate is collision-free in scope.
      for (position in discriminator_positions) {
        values <- mat[, position]
        if (all(grepl("^[[:digit:]]+$", values)))
          candidates <- add_candidate(candidates, values, 0L, position, "numeric field")
        tail_matches <- regexec("^([^[:digit:]]+)([[:digit:]]+)$", values, perl = TRUE)
        tail_parts <- regmatches(values, tail_matches)
        if (all(lengths(tail_parts) == 3L) &&
            length(unique(vapply(tail_parts, `[`, "", 2L))) == 1L) {
          tails <- vapply(tail_parts, `[`, "", 3L)
          candidate <- add_candidate(list(), tails, 1L, position, "numeric tail")
          if (length(candidate) && candidate[[1L]]$unique)
            candidates <- c(candidates, candidate)
        }
        decorated <- !all(grepl("^[[:alnum:]]+$", values)) ||
          any(grepl("[[:alpha:]]", values) & grepl("[[:digit:]]", values))
        candidates <- add_candidate(candidates, values, if (decorated) 3L else 2L,
                                    position, "field")
      }
      # Multi-component representations are raw by design: normalizing a
      # compound identifier is less safe than selecting a proven numeric part.
      if (length(discriminator_positions) > 1L) {
        # Search by increasing width and stop once that width provides a
        # collision-free representation.  Cap exploration for pathological
        # headers; the source-derived fallback below remains deterministic and
        # collision-free when no compact field representation exists.
        combination_budget <- 512L
        generated <- 0L
        for (size in 2:length(discriminator_positions)) {
          before <- length(candidates)
          position_sets <- bounded_combinations(
            discriminator_positions, size, combination_budget - generated)
          for (positions in position_sets) {
            if (generated >= combination_budget) break
            candidates <- add_candidate(candidates, mat[, positions, drop = FALSE],
                                        4L, positions, "field combination")
            generated <- generated + 1L
          }
          added <- if (length(candidates) > before) candidates[(before + 1L):length(candidates)] else list()
          if (any(vapply(added, `[[`, logical(1), "unique")) ||
              generated >= combination_budget) break
        }
      }
    }
    if (!length(candidates))
      candidates <- add_candidate(candidates, rep("sample", length(rows)), 9L,
                                  integer(), "defensive base")
    rank_candidates <- function(pool) order(
      vapply(pool, `[[`, integer(1), "safety"),
      vapply(pool, `[[`, integer(1), "components"),
      vapply(pool, `[[`, integer(1), "length"),
      vapply(pool, `[[`, integer(1), "stability"),
      vapply(pool, `[[`, integer(1), "proximity"),
      vapply(pool, `[[`, character(1), "position"),
      vapply(pool, `[[`, character(1), "label"))
    eligible <- which(vapply(candidates, `[[`, logical(1), "unique"))
    chosen_index <- if (length(eligible)) eligible[rank_candidates(candidates[eligible])[1L]] else
      rank_candidates(candidates)[1L]
    chosen <- candidates[[chosen_index]]
    selected <- lapply(chosen$values, function(value) value)
    group_samples <- chosen$composed
    diagnostics$candidate_position[rows] <- chosen$position
    diagnostics$candidate_direction[rows] <- if (!length(chosen$position) ||
        !nzchar(chosen$position)) "none" else {
      chosen_positions <- as.integer(strsplit(chosen$position, ",", fixed = TRUE)[[1L]])
      if (min(chosen_positions) - 1L <= width - max(chosen_positions)) "from_left" else "from_right"
    }
    diagnostics$source_token[rows] <- chosen$values
    diagnostics$normalization[rows] <- switch(
      chosen$label, `numeric tail` = "numeric_tail", `defensive base` = "synthetic",
      "none"
    )
    diagnostics$candidate_tier[rows] <- chosen$safety
    diagnostics$rejected_candidate_count[rows] <- length(candidates) - 1L
    diagnostics$selection_reason[rows] <- paste0(
      chosen$label, if (isTRUE(chosen$unique)) "; unique after condition" else
        "; best-ranked candidate required refinement")

    # If no one representation serves every row, refine collision sets only.
    # Non-colliding names are deliberately left byte-for-byte unchanged.
    duplicates <- which(!is.na(group_samples) & (duplicated(group_samples) |
      duplicated(group_samples, fromLast = TRUE)))
    if (length(duplicates)) {
      collision_sets <- split(duplicates, group_samples[duplicates])
      for (set in collision_sets) {
        refiners <- candidates[vapply(candidates, function(candidate)
          !anyDuplicated(candidate$values[set]), logical(1))]
        if (length(refiners)) {
          refiner <- refiners[[rank_candidates(refiners)[1L]]]
          group_samples[set] <- paste(group_samples[set], refiner$values[set], sep = "_")
          selected[set] <- Map(function(old, extra) c(old, extra), selected[set], refiner$values[set])
          diagnostics$fallback_reason[rows[set]] <- "per-collision candidate refinement"
          diagnostics$selection_reason[rows[set]] <- paste0(
            diagnostics$selection_reason[rows[set]], "; per-collision candidate refinement")
          next
        }
        sources <- columns[rows[set]]
        distinguishing <- NULL
        if (length(unique(sources)) == length(sources)) {
          source_lexemes <- lapply(rows[set], function(i)
            cached_tokenize(columns[i], separators[i]))
          differing <- vapply(seq_along(sources), function(i) {
            source <- sources[i]
            others <- sources[-i]
            for (size in seq_len(nchar(source))) {
              starts <- seq_len(nchar(source) - size + 1L)
              pieces <- substring(source, starts, starts + size - 1L)
              unique_piece <- pieces[!vapply(pieces, function(piece) {
                any(grepl(piece, others, fixed = TRUE))
              }, logical(1))]
              if (length(unique_piece)) return(unique_piece[1L])
            }
            source
          }, character(1))

          # A coincidental character difference is not adequate evidence for a
          # sample identifier.  Retain the legacy shortest-substring result only
          # when each value is wholly contained in a lexical field (and hence in
          # a stable, source-recorded range).  Separators and boundary-spanning
          # fragments are deliberately rejected.
          in_field_range <- function(value, source, lexemes) {
            hits <- gregexpr(value, source, fixed = TRUE)[[1L]]
            if (identical(hits[1L], -1L)) return(FALSE)
            ends <- hits + nchar(value) - 1L
            any(vapply(seq_along(hits), function(j) any(
              lexemes$kind == "field" & lexemes$start <= hits[j] &
                lexemes$end >= ends[j]), logical(1)))
          }
          stable <- vapply(seq_along(differing), function(i)
            in_field_range(differing[i], sources[i], source_lexemes[[i]]), logical(1))
          proposed <- paste(group_samples[set], differing, sep = "_")
          trial <- group_samples
          trial[set] <- proposed
          if (all(stable) && !anyDuplicated(trial)) distinguishing <- differing

          # Distinct headers always provide deterministic evidence even when
          # their shortest difference is an unstable separator fragment.  The
          # complete source is the final stable range; unlike a row number it is
          # reproducible if rows are reordered.
          if (is.null(distinguishing)) {
            proposed <- paste(group_samples[set], sources, sep = "_")
            trial <- group_samples
            trial[set] <- proposed
            if (!anyDuplicated(trial)) distinguishing <- sources
          }
        }
        if (!is.null(distinguishing)) {
          suffix <- make.unique(distinguishing, sep = "_")
          group_samples[set] <- paste(group_samples[set], suffix, sep = "_")
          diagnostics$fallback_reason[rows[set]] <- "source-derived differing substring"
          diagnostics$selection_reason[rows[set]] <- paste0(
            diagnostics$selection_reason[rows[set]], "; source-derived differing substring")
        } else {
          warning("Identical source columns could not be distinguished structurally; using row index as a defensive fallback")
          group_samples[set] <- paste(group_samples[set], rows[set], sep = "_")
          diagnostics$fallback_reason[rows[set]] <- "defensive row-index fallback; unresolved source uniqueness"
          diagnostics$selection_reason[rows[set]] <- paste0(
            diagnostics$selection_reason[rows[set]], "; defensive row-index fallback")
        }
      }
    }

    samples[rows] <- group_samples
    diagnostics$selected_discriminator_tokens[rows] <- vapply(selected, paste, "", collapse = " | ")
    diagnostics$final_sample[rows] <- ifelse(is.na(group_samples), "", group_samples)
    diagnostics$unique_within_content[rows] <- is.na(group_samples) |
      !(duplicated(group_samples) | duplicated(group_samples, fromLast = TRUE))
  }
  attr(samples, "diagnostics") <- diagnostics
  attr(samples, "cache_stats") <- c(
    tokenizations = cache_stats$tokenization_misses,
    condition_locations = cache_stats$location_misses,
    candidate_predictions = cache_stats$candidate_prediction_misses
  )
  samples
}
