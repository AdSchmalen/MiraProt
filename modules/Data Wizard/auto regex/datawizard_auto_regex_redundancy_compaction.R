# Auto Regex content-table compaction. Loaded by the legacy logic entrypoint.

auto_regex_compact_content_table <- function(
    metadata,
    table,
    semantic_spans = data.frame(),
    redundancy = 0L,
    redundancy_overrides = NULL,
    logger = function(...) invisible(NULL),
    search_limit = 512L) {

  empty_lineage <- data.frame(
    RuleId = character(),
    Content = character(),
    VariantId = character(),
    OriginalInclude = character(),
    MinimalInclude = character(),
    FinalInclude = character(),
    GlobalRedundancy = integer(),
    OverrideRedundancy = integer(),
    RequestedRedundancy = integer(),
    EffectiveRedundancy = integer(),
    MinimalSemanticFraction = numeric(),
    FinalSemanticFraction = numeric(),
    ExploredStates = integer(),
    Accepted = logical(),
    Reason = character(),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  if (!is.data.frame(table) ||
      !nrow(table)) {
    return(list(
      table = table,
      lineage = empty_lineage
    ))
  }

  global_requested <-
    auto_regex_redundancy_value(
      redundancy,
      fallback = 0L
    )

  if (is.list(
    redundancy_overrides
  )) {
    redundancy_overrides <-
      unlist(
        redundancy_overrides,
        use.names = TRUE
      )
  }

  if (is.null(
    redundancy_overrides
  )) {
    redundancy_overrides <-
      integer()
  }

  resolve_requested <- function(
    rule_id) {

    override <- NA_integer_

    if (length(
      redundancy_overrides
    ) &&
    !is.null(
      names(
        redundancy_overrides
      )
    ) &&
    rule_id %in%
    names(
      redundancy_overrides
    )) {

      raw <-
        redundancy_overrides[[rule_id]]

      parsed <-
        suppressWarnings(
          as.integer(
            as.character(raw)
          )
        )

      if (!is.na(parsed)) {
        override <-
          auto_regex_redundancy_value(
            parsed,
            fallback =
              global_requested
          )
      }
    }

    list(
      global =
        global_requested,
      override =
        override,
      requested =
        if (is.na(override)) {
          global_requested
        } else {
          override
        }
    )
  }

  out <- table

  baseline_application <-
    apply_content_table(
      metadata,
      out
    )

  baseline_state <-
    auto_regex_content_replay_state(
      baseline_application
    )

  if (is.null(
    baseline_state
  )) {
    return(list(
      table = table,
      lineage = empty_lineage
    ))
  }

  variants <- attr(
    baseline_application$metadata,
    "variant_id",
    exact = TRUE
  )

  if (is.null(variants) ||
      length(variants) !=
      nrow(metadata)) {
    return(list(
      table = table,
      lineage = empty_lineage
    ))
  }

  same_replay <- function(
    candidate_table) {

    application <- tryCatch(
      apply_content_table(
        metadata,
        candidate_table
      ),
      error = function(e) NULL
    )

    if (is.null(application))
      return(FALSE)

    identical(
      auto_regex_content_replay_state(
        application
      ),
      baseline_state
    )
  }

  lineage <- list()

  redundancy_cache_rules <- list()

  for (rule_index in
       seq_len(nrow(out))) {

    rule_id <- chr(
      out$RuleId[[rule_index]]
    )

    label <- chr(
      out$Content[[rule_index]]
    )

    variant_id <- chr(
      out$VariantId[[rule_index]]
    )

    original_include <- chr(
      out$Include[[rule_index]]
    )

    priority <- suppressWarnings(
      as.integer(
        out$Priority[[rule_index]]
      )
    )

    redundancy_values <-
      resolve_requested(
        rule_id
      )

    requested <-
      redundancy_values$requested

    target_rows <- which(
      baseline_state$Content ==
        label &
        baseline_state$VariantId ==
        variant_id
    )

    singleton_exact_seed <- FALSE

    if (length(target_rows) == 1L) {

      expected_singleton <-
        auto_regex_exact_content_selector(
          metadata$Column[
            target_rows[[1L]]
          ]
        )

      singleton_exact_seed <-
        nzchar(expected_singleton) &&
        identical(
          original_include,
          expected_singleton
        )
    }

    add_lineage <- function(
    minimal_include,
    final_include,
    minimal_fraction,
    final_fraction,
    explored,
    accepted,
    effective,
    reason) {

      lineage[[length(lineage) + 1L]] <<-
        data.frame(
          RuleId = rule_id,
          Content = label,
          VariantId = variant_id,
          OriginalInclude =
            original_include,
          MinimalInclude =
            minimal_include,
          FinalInclude =
            final_include,
          GlobalRedundancy =
            redundancy_values$global,
          OverrideRedundancy =
            redundancy_values$override,
          RequestedRedundancy =
            requested,
          EffectiveRedundancy =
            as.integer(effective),
          MinimalSemanticFraction =
            minimal_fraction,
          FinalSemanticFraction =
            final_fraction,
          ExploredStates =
            as.integer(explored),
          Accepted =
            isTRUE(accepted),
          Reason = reason,
          stringsAsFactors = FALSE,
          check.names = FALSE
        )
    }

    # Protocol/fallback rules remain protected.
    #
    # Singleton Content rules are compactable only when their starting rule is
    # the exact full-header selector created by the explicit singleton fallback.
    # Arbitrary one-row structural/generalized rules remain protected.
    if (identical(
      label,
      "Row Index"
    ) ||
    (!is.na(priority) &&
     priority <= 0L) ||
    (
      length(target_rows) < 2L &&
      !singleton_exact_seed
    )) {

      add_lineage(
        original_include,
        original_include,
        NA_real_,
        NA_real_,
        1L,
        FALSE,
        0L,
        paste(
          "protected rule or",
          "insufficient repeated evidence"
        )
      )

      next
    }

    atoms <-
      auto_regex_edge_atoms(
        original_include
      )

    if (is.null(atoms) ||
        nrow(atoms) < 2L) {

      add_lineage(
        original_include,
        original_include,
        NA_real_,
        NA_real_,
        1L,
        FALSE,
        0L,
        paste(
          "regex could not be safely",
          "decomposed into top-level",
          "edge atoms"
        )
      )

      next
    }

    n_atoms <- nrow(atoms)

    make_pattern <- function(
    left,
    right) {

      if (left < 1L ||
          right > n_atoms ||
          left > right) {
        return(NULL)
      }

      runtime <- paste0(
        atoms$Text[
          seq.int(left, right)
        ],
        collapse = ""
      )

      if (!nzchar(runtime))
        return(NULL)

      validation <-
        validate_pcre(runtime)

      if (!isTRUE(
        validation$valid
      )) {
        return(NULL)
      }

      regex_to_miraprot_storage(
        runtime,
        "content"
      )
    }

    original_profile <-
      auto_regex_semantic_match_profile(
        original_include,
        metadata,
        target_rows,
        semantic_spans
      )

    states <- data.frame(
      Left = 1L,
      Right = n_atoms,
      Include = original_include,
      SemanticFraction =
        original_profile$Fraction,
      MatchChars =
        original_profile$MatchChars,
      SemanticChars =
        original_profile$SemanticChars,
      PredominantlySemantic =
        original_profile$
        PredominantlySemantic,
      NaturalEdges = TRUE,
      Length = nchar(
        original_include,
        type = "chars"
      ),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )

    frontier <- 1L

    seen <- paste0(
      "1:",
      n_atoms
    )

    while (length(frontier) &&
           nrow(states) <
           as.integer(
             search_limit
           )) {

      current_index <-
        frontier[[1L]]

      frontier <-
        frontier[-1L]

      current <-
        states[
          current_index,
          ,
          drop = FALSE
        ]

      proposals <- list()

      if (current$Left[[1L]] <
          current$Right[[1L]]) {

        proposals[[1L]] <- c(
          current$Left[[1L]] + 1L,
          current$Right[[1L]]
        )

        proposals[[2L]] <- c(
          current$Left[[1L]],
          current$Right[[1L]] - 1L
        )
      }

      for (range in proposals) {

        left <- range[[1L]]
        right <- range[[2L]]

        key <- paste0(
          left,
          ":",
          right
        )

        if (key %in% seen)
          next

        seen <- c(
          seen,
          key
        )

        candidate_include <-
          make_pattern(
            left,
            right
          )

        if (is.null(
          candidate_include
        )) {
          next
        }

        trial <- table

        trial$Include[[rule_index]] <-
          candidate_include

        # This is a minimization pass, not inference:
        # no change in assignment is tolerated.
        if (!same_replay(trial))
          next

        profile <-
          auto_regex_semantic_match_profile(
            candidate_include,
            metadata,
            target_rows,
            semantic_spans
          )

        states <- rbind(
          states,
          data.frame(
            Left = left,
            Right = right,
            Include =
              candidate_include,
            SemanticFraction =
              profile$Fraction,
            MatchChars =
              profile$MatchChars,
            SemanticChars =
              profile$SemanticChars,
            PredominantlySemantic =
              profile$
              PredominantlySemantic,
            NaturalEdges =
              auto_regex_range_has_natural_edges(
                atoms,
                left,
                right
              ),
            Length = nchar(
              candidate_include,
              type = "chars"
            ),
            stringsAsFactors = FALSE,
            check.names = FALSE
          )
        )

        frontier <- c(
          frontier,
          nrow(states)
        )
      }
    }

    clean <- which(
      !states$PredominantlySemantic
    )

    original_length <- states$Length[[1L]]

    # First preference:
    # keep at least one edge of the original proven regex.
    #
    # This is important because otherwise an internal island such as
    #
    #   Ratio_
    #   Adj
    #   Identified
    #
    # can beat a more reusable structural prefix/suffix simply because the
    # internal island also happens to replay exactly on the training table.
    keeps_original_edge <-
      states$Left == 1L |
      states$Right == n_atoms

    edge_clean <- clean[keeps_original_edge[clean]]

    natural_edge_clean <- edge_clean[states$NaturalEdges[edge_clean]]

    natural_edge_shorter <- natural_edge_clean[states$Length[natural_edge_clean] < original_length]

    edge_shorter <- edge_clean[states$Length[edge_clean] < original_length]

    natural_clean <- clean[states$NaturalEdges[clean]]

    natural_shorter <- natural_clean[states$Length[natural_clean] < original_length]

    # Preference hierarchy:
    #
    # 1. natural boundary + one original edge
    # 2. one original edge
    # 3. natural internal range
    # 4. any exact-replay clean range
    #
    # The last two cases remain deliberate fallbacks for unusual headers where
    # there is genuinely no useful delimiter/case/digit boundary.
    selectable <-
      if (length(natural_edge_shorter)) {

        natural_edge_shorter

      } else if (length(edge_shorter)) {

        edge_shorter

      } else if (length(natural_shorter)) {

        natural_shorter

      } else {

        clean
      }

    minimal_index <- 1L

    if (length(selectable)) {

      ordering <- order(
        states$SemanticFraction[selectable],
        states$Length[selectable],
        states$Include[selectable],
        method = "radix"
      )

      candidate_index <- selectable[
        ordering[[1L]]
      ]

      if (states$Length[[candidate_index]] <
          original_length) {
        minimal_index <- candidate_index
      }
    }

    ladder <-
      auto_regex_content_redundancy_ladder(
        states = states,
        minimal_index = minimal_index,
        n_atoms = n_atoms,
        maximum = 10L
      )

    selected_level <- match(
      requested,
      ladder$RequestedRedundancy
    )

    if (is.na(selected_level)) {
      selected_level <- nrow(ladder)
    }

    final_index <-
      ladder$StateIndex[[selected_level]]

    effective <-
      ladder$EffectiveRedundancy[[selected_level]]

    if (nzchar(rule_id)) {

      redundancy_cache_rules[[rule_id]] <- list(
        RuleId = rule_id,
        Content = label,
        VariantId = variant_id,
        OriginalInclude = original_include,
        MinimalInclude = states$Include[[minimal_index]],
        MinimalSemanticFraction =
          states$SemanticFraction[[minimal_index]],
        ExploredStates = as.integer(nrow(states)),
        Ladder = ladder
      )
    }

    out$Include[[rule_index]] <-
      states$Include[[final_index]]

    if (!same_replay(out)) {
      stop(
        paste(
          "Content-regex compaction",
          "changed complete-table",
          "assignment."
        ),
        call. = FALSE
      )
    }

    accepted <-
      !identical(
        original_include,
        states$Include[[minimal_index]]
      )

    add_lineage(
      states$Include[[minimal_index]],
      states$Include[[final_index]],
      states$SemanticFraction[[minimal_index]],
      states$SemanticFraction[[final_index]],
      nrow(states),
      accepted,
      effective,
      if (!accepted) {

        paste(
          "no simpler structurally safe",
          "equivalent rule found"
        )

      } else if (effective <
                 requested) {

        paste(
          "minimal equivalent rule found;",
          "redundancy stopped before",
          "unsafe or predominantly",
          "semantic context"
        )

      } else {

        paste(
          "minimal equivalent rule found;",
          "requested safe redundancy restored"
        )
      }
    )

    if (accepted) {

      logger(
        "debug",
        "content",
        "compact",
        sprintf(
          paste0(
            "%s [%s]: '%s' -> '%s' -> '%s' ",
            "(redundancy %d/%d)."
          ),
          label,
          variant_id,
          auto_regex_safe_value(
            original_include,
            100L
          ),
          auto_regex_safe_value(
            states$Include[[minimal_index]],
            100L
          ),
          auto_regex_safe_value(
            states$Include[[final_index]],
            100L
          ),
          effective,
          requested
        )
      )
    }
  }

  final_application <-
    apply_content_table(
      metadata,
      out
    )

  if (!identical(
    auto_regex_content_replay_state(
      final_application
    ),
    baseline_state
  )) {
    stop(
      paste(
        "Final Content-regex",
        "compaction failed exact replay."
      ),
      call. = FALSE
    )
  }

  list(
    table = out,
    lineage =
      if (length(lineage)) {
        do.call(
          rbind,
          lineage
        )
      } else {
        empty_lineage
      },
    cache = list(
      baseline_state = baseline_state,
      rule_ids = chr(table$RuleId),
      original_includes = chr(table$Include),
      rules = redundancy_cache_rules
    )
  )
}
