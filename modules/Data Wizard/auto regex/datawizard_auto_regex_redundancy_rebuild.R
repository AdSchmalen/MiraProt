# Auto Regex cached redundancy reconstruction. Loaded by the legacy logic entrypoint.

auto_regex_apply_cached_content_redundancy <- function(
    metadata,
    table,
    cache,
    redundancy = 0L,
    redundancy_overrides = NULL) {

  if (!is.data.frame(table) ||
      !nrow(table)) {

    return(list(
      table = table,
      lineage = data.frame()
    ))
  }

  if (!is.list(cache) ||
      !is.list(cache$rules) ||
      !identical(
        chr(cache$rule_ids),
        chr(table$RuleId)
      ) ||
      !identical(
        chr(cache$original_includes),
        chr(table$Include)
      )) {

    stop(
      "Content redundancy cache does not match the immutable base rule table.",
      call. = FALSE
    )
  }

  baseline_application <-
    apply_content_table(
      metadata,
      table
    )

  baseline_state <-
    auto_regex_content_replay_state(
      baseline_application
    )

  if (!identical(
    baseline_state,
    cache$baseline_state
  )) {

    stop(
      "Content redundancy cache does not match the current metadata replay state.",
      call. = FALSE
    )
  }

  global_requested <-
    auto_regex_redundancy_value(
      redundancy,
      fallback = 0L
    )

  if (is.list(redundancy_overrides)) {

    redundancy_overrides <- unlist(
      redundancy_overrides,
      use.names = TRUE
    )
  }

  if (is.null(redundancy_overrides)) {
    redundancy_overrides <- integer()
  }

  resolve_requested <- function(rule_id) {

    override <- NA_integer_

    if (length(redundancy_overrides) &&
        !is.null(names(redundancy_overrides)) &&
        rule_id %in% names(redundancy_overrides)) {

      parsed <- suppressWarnings(
        as.integer(
          redundancy_overrides[[rule_id]]
        )
      )

      if (!is.na(parsed)) {

        override <-
          auto_regex_redundancy_value(
            parsed,
            fallback = global_requested
          )
      }
    }

    list(
      global = global_requested,
      override = override,
      requested =
        if (is.na(override)) {
          global_requested
        } else {
          override
        }
    )
  }

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

  out <- table
  lineage <- list()

  for (rule_index in seq_len(nrow(out))) {

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
      table$Include[[rule_index]]
    )

    requested_values <-
      resolve_requested(
        rule_id
      )

    requested <-
      requested_values$requested

    entry <-
      if (nzchar(rule_id) &&
          rule_id %in% names(cache$rules)) {
        cache$rules[[rule_id]]
      } else {
        NULL
      }

    if (is.null(entry)) {

      lineage[[length(lineage) + 1L]] <-
        data.frame(
          RuleId = rule_id,
          Content = label,
          VariantId = variant_id,
          OriginalInclude = original_include,
          MinimalInclude = original_include,
          FinalInclude = original_include,
          GlobalRedundancy = requested_values$global,
          OverrideRedundancy = requested_values$override,
          RequestedRedundancy = requested,
          EffectiveRedundancy = 0L,
          MinimalSemanticFraction = NA_real_,
          FinalSemanticFraction = NA_real_,
          ExploredStates = 1L,
          Accepted = FALSE,
          Reason = "protected rule or no reusable compaction analysis",
          stringsAsFactors = FALSE,
          check.names = FALSE
        )

      next
    }

    if (!identical(
      chr(entry$Content),
      label
    ) ||
    !identical(
      chr(entry$VariantId),
      variant_id
    ) ||
    !identical(
      chr(entry$OriginalInclude),
      original_include
    ) ||
    !is.data.frame(entry$Ladder) ||
    !nrow(entry$Ladder)) {

      stop(
        sprintf(
          "Cached Content redundancy analysis is inconsistent for RuleId '%s'.",
          rule_id
        ),
        call. = FALSE
      )
    }

    selected_level <- match(
      requested,
      entry$Ladder$RequestedRedundancy
    )

    if (is.na(selected_level)) {
      selected_level <- nrow(entry$Ladder)
    }

    selected <-
      entry$Ladder[
        selected_level,
        ,
        drop = FALSE
      ]

    final_include <-
      chr(
        selected$Include[[1L]]
      )

    effective <-
      as.integer(
        selected$EffectiveRedundancy[[1L]]
      )

    out$Include[[rule_index]] <-
      final_include

    accepted <-
      !identical(
        original_include,
        chr(entry$MinimalInclude)
      )

    reason <-
      if (!accepted) {

        paste(
          "no simpler structurally safe",
          "equivalent rule found"
        )

      } else if (effective < requested) {

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

    lineage[[length(lineage) + 1L]] <-
      data.frame(
        RuleId = rule_id,
        Content = label,
        VariantId = variant_id,
        OriginalInclude = original_include,
        MinimalInclude = chr(entry$MinimalInclude),
        FinalInclude = final_include,
        GlobalRedundancy = requested_values$global,
        OverrideRedundancy = requested_values$override,
        RequestedRedundancy = requested,
        EffectiveRedundancy = effective,
        MinimalSemanticFraction =
          as.numeric(entry$MinimalSemanticFraction),
        FinalSemanticFraction =
          as.numeric(selected$SemanticFraction[[1L]]),
        ExploredStates =
          as.integer(entry$ExploredStates),
        Accepted = accepted,
        Reason = reason,
        stringsAsFactors = FALSE,
        check.names = FALSE
      )
  }

  final_application <-
    apply_content_table(
      metadata,
      out
    )

  final_state <-
    auto_regex_content_replay_state(
      final_application
    )

  # Arbitrary combinations of per-rule overrides are allowed only if the
  # combination as a whole still has the exact original assignment semantics.
  if (!identical(
    final_state,
    baseline_state
  )) {

    stop(
      paste(
        "Cached Content redundancy combination",
        "changed complete-table assignment."
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
    cache = cache
  )
}
