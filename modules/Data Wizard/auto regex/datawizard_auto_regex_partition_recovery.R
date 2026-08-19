# Bounded Auto Regex partition recovery and atomic publication.

AUTO_REGEX_PARTITION_LIMITS <- list(
  candidates = 96L,
  variants = 6L,
  nodes = 1500L,
  beam = 48L,
  memory_bytes = 4L * 1024L * 1024L,
  elapsed_seconds = 8
)

# Transfer evidence policy (grouped-inference release 1).  Classification and
# serialization gates are necessary but cannot turn memorization into evidence:
# every authoritative variant needs two independently named source headers.
auto_regex_partition_recovery <- function(metadata, label, condition_target="Options",
    logger=function(...) invisible(NULL), limits=AUTO_REGEX_PARTITION_LIMITS) {
  started <- proc.time()[["elapsed"]]; nodes <- 0L; memory <- 0L
  generated_candidates<-0L;deduplicated_candidates<-0L;replay_ms<-0
  exhausted <- FALSE
  # Downstream inference (infer_conditions, infer_ratios) may legitimately take
  # longer than the search budget allows.  Exclude that time from the elapsed-
  # seconds check so a valid merged-family ratio candidate is not rejected just
  # because ratio inference is slow.  Only the pattern-search phase is bounded.
  downstream_offset <- 0
  pause_search_clock <- function() proc.time()[["elapsed"]]
  resume_search_clock <- function(t0) downstream_offset <<- downstream_offset +
    proc.time()[["elapsed"]] - t0
  tick <- function(n=1L, bytes=0L) {
    nodes <<- nodes + n; memory <<- memory + bytes
    exhausted <<- exhausted || nodes > limits$nodes || memory > limits$memory_bytes ||
      proc.time()[["elapsed"]] - started - downstream_offset > limits$elapsed_seconds
    !exhausted
  }
  rows <- which(chr(metadata$Content)==label); other <- which(chr(metadata$Content)!=label)
  empty <- list(ok=FALSE, exhausted=FALSE, content=empty_content(),
    condition=empty_condition(), ratio=empty_ratio(), diagnostics=data.frame(),
    performance=list(generated_candidates=0L,deduplicated_candidates=0L,
      replay_ms=0,pruning_reason="no_complete_partition"))
  if(length(rows)<2L || !length(other)) return(empty)
  signatures <- auto_regex_family_signature(
    metadata,
    rows
  )

  family_keys <- ifelse(
    nzchar(signatures),
    signatures,
    paste0("<row:", rows, ">")
  )

  groups <- split(
    rows,
    family_keys,
    drop = TRUE
  )

  # Variant numbering follows source-family order rather than the lexical value
  # of the structural signature.  Search-order changes therefore cannot rename
  # an otherwise identical family.
  groups <- groups[
    order(
      vapply(
        groups,
        min,
        integer(1)
      )
    )
  ]
  # Candidates are learned per structural family.  Rows belonging to another
  # positive family are opposing examples for this local discriminator, not
  # additional positives which a convenient broad fragment may absorb.
  masks <- head(lapply(seq_along(groups),function(i)i),limits$candidates)
  candidates <- list()
  seen <- character()
  candidate_diagnostics <- data.frame()

  for (family_index in seq_along(groups)) {

    seed <- sort(unique(as.integer(
      groups[[family_index]]
    )))

    if (!length(seed))
      next

    # `seed` defines the rows this Content variant is allowed to classify.
    #
    # A singleton canonical ratio-result family may nevertheless have
    # independent structural support from its sibling ratio result roles
    # (Ratio / p-Value / adjusted p-Value).  Use that support only as evidence;
    # it must never broaden the selector's target rows.
    evidence_rows <-
      auto_regex_family_evidence_rows(
        metadata,
        seed
      )

    selectors <- auto_regex_exact_family_selectors(
      metadata,
      seed,
      limit = limits$candidates,
      evidence_rows = evidence_rows
    )

    generated_candidates <-
      generated_candidates + nrow(selectors)

    deduplicated_candidates <-
      deduplicated_candidates + nrow(selectors)

    if (!nrow(selectors))
      next

    family_key <- paste(seed, collapse = ",")

    # Once one exact selector for a structural family has survived downstream
    # replay, alternatives with identical training coverage are redundant.
    if (family_key %in% seen)
      next

    for (selector_index in seq_len(nrow(selectors))) {

      pattern <- selectors$Pattern[[selector_index]]
      family <- selectors$Family[[selector_index]]

      if (length(candidates) >= limits$candidates ||
          !tick(bytes = nchar(pattern, type = "bytes"))) {
        exhausted <- TRUE
        break
      }

      # The selector helper has already proved these facts against the complete
      # metadata table.  Re-evaluate them here as a defensive invariant because
      # this function owns publication candidates.
      hit <- safe_grepl(
        pattern,
        chr(metadata$Column)
      )

      covered <- rows[hit[rows]]
      outside <- setdiff(
        seq_len(nrow(metadata)),
        seed
      )

      complete_family_coverage <-
        identical(
          sort(unique(covered)),
          seed
        )

      outside_matches <-
        any(hit[outside])

      evidence <- auto_regex_content_evidence(
        family,
        chr(
          metadata$Column[
            evidence_rows
          ]
        )
      )

      candidate_diagnostics <- rbind(
        candidate_diagnostics,
        data.frame(
          Content = label,
          CandidateFamily = family,
          Pattern = pattern,
          SupportingSourceRows = evidence$rows,
          DistinctSourceNames = evidence$names,
          CoveredExamples =
            auto_regex_example_list(
              metadata$Column[seed]
            ),
          UncoveredExamples =
            auto_regex_example_list(
              metadata$Column[
                setdiff(rows, seed)
              ]
            ),
          FalsePositiveCount =
            sum(hit[
              chr(metadata$Content) != label
            ]),
          StructuralFamilyLeakCount =
            length(
              setdiff(covered, seed)
            ),
          DownstreamExactMatchCount = 0L,
          ValidWithoutWholeHeaderMemorization =
            !evidence$literal_only,
          EvidenceThresholdPassed =
            evidence$passes,
          Authoritative = FALSE,
          EvidenceStatus =
            if (complete_family_coverage &&
                !outside_matches &&
                evidence$passes)
              "exact_family_selector"
          else
            "rejected_family_selector",
          stringsAsFactors = FALSE,
          check.names = FALSE
        )
      )

      diagnostic_index <- nrow(
        candidate_diagnostics
      )

      if (!complete_family_coverage ||
          outside_matches ||
          !evidence$passes)
        next

      # The VariantId belongs to the STRUCTURAL FAMILY, not to a regex attempt.
      # Every alternative selector for family_index therefore receives the same
      # identity.
      family_variant_ids <- stable_variant_ids(
        rep(label, length(groups))
      )

      variant_id <- family_variant_ids[[family_index]]

      transformation <- infer_content_transformation(
        metadata[seed, , drop = FALSE],
        label
      )

      rule <- canonical_rule_row(
        "content",
        Content = label,
        VariantId = variant_id,
        Priority = family_index,
        Include = pattern,
        Exclude = "",
        Transformation = transformation
      )

      # Defensive complete-table replay.  A local candidate is not allowed to
      # redefine its structural family.
      content_application <- apply_content_table(
        metadata,
        rule
      )

      applied_variants <- attr(
        content_application$metadata,
        "variant_id",
        exact = TRUE
      )

      if (is.null(applied_variants))
        applied_variants <- rep(
          "",
          nrow(metadata)
        )

      replay_covered <- which(
        chr(content_application$metadata$Content) ==
          label &
          chr(applied_variants) ==
          variant_id
      )

      if (!identical(
        sort(replay_covered),
        seed
      ))
        next

      subset <- metadata[
        seed,
        ,
        drop = FALSE
      ]

      condition_state <-
        auto_regex_variant_condition_state(
          subset,
          condition_target
        )

      ratio_state <-
        auto_regex_variant_ratio_state(
          subset
        )

      # Partial references are genuinely unsafe.  They are not the same thing
      # as complete-but-header-nonrepresentable references.
      if (identical(condition_state, "partial") ||
          identical(ratio_state, "partial"))
        next

      condition_applicable <-
        identical(
          condition_state,
          "complete"
        )

      ratio_applicable <-
        identical(
          ratio_state,
          "complete"
        )

      downstream_t0 <- pause_search_clock()

      condition <- if (condition_applicable) {
        infer_conditions(
          subset,
          condition_target,
          logger = logger
        )
      } else {
        list(
          table = empty_condition(),
          diagnostics = data.frame()
        )
      }

      if (nrow(condition$table)) {
        condition$table$VariantId <-
          variant_id

        condition$table$RuleId <-
          stable_rule_ids(
            "condition",
            condition$table$VariantId,
            rep("", nrow(condition$table))
          )
      }

      ratio <- if (ratio_applicable) {
        infer_ratios(
          subset,
          logger = logger,
          content_rules = rule,
          condition_rules = condition$table
        )
      } else {
        list(
          table = empty_ratio(),
          diagnostics = data.frame()
        )
      }

      resume_search_clock(
        downstream_t0
      )

      if (nrow(ratio$table)) {
        ratio$table$VariantId <-
          variant_id

        ratio$table$RuleId <-
          stable_rule_ids(
            "ratio",
            ratio$table$VariantId,
            rep("", nrow(ratio$table))
          )
      }

      condition_ok <- TRUE

      if (condition_applicable) {
        selected_condition <-
          auto_regex_selected_candidate_diagnostics(
            condition$diagnostics
          )

        applicable_condition <- selected_condition[
          selected_condition$ReferenceAvailable %in% TRUE,
          ,
          drop = FALSE
        ]

        condition_ok <-
          label %in% chr(condition$table$Content) &&
          nrow(applicable_condition) == nrow(subset) &&
          all(applicable_condition$ExactMatch %in% TRUE)
      }

      ratio_ok <- TRUE

      if (ratio_applicable) {
        selected_ratio <-
          auto_regex_selected_candidate_diagnostics(
            ratio$diagnostics
          )

        applicable_ratio <- selected_ratio[
          selected_ratio$Applicable %in% TRUE,
          ,
          drop = FALSE
        ]

        required_ratio_rows <- which(
          auto_regex_ratio_row_state(subset) ==
            "required"
        )

        ratio_ok <-
          label %in% chr(ratio$table$Content) &&
          nrow(applicable_ratio) ==
          length(required_ratio_rows) &&
          all(applicable_ratio$Success %in% TRUE)
      }

      if (!condition_ok ||
          !ratio_ok)
        next

      downstream_exact <-
        if (condition_applicable)
          sum(
            condition$diagnostics$ExactMatch %in% TRUE,
            na.rm = TRUE
          )
      else
        0L

      downstream_exact <-
        downstream_exact +
        if (ratio_applicable)
          sum(
            ratio$diagnostics$Success %in% TRUE,
            na.rm = TRUE
          )
      else
        0L

      candidate_diagnostics$
        DownstreamExactMatchCount[[diagnostic_index]] <- downstream_exact

      # This family has now been proved end-to-end.  No second regex describing
      # the same observed family is needed in the partition.
      seen <- c(
        seen,
        family_key
      )

      candidates[[length(candidates) + 1L]] <- list(
        rows = seed,
        family_index = family_index,
        signature = names(groups)[[family_index]],
        rule = rule,
        condition = condition$table,
        ratio = ratio$table,
        condition_applicable =
          condition_applicable,
        ratio_applicable =
          ratio_applicable,
        evidence = length(seed),
        complexity =
          selectors$Complexity[[selector_index]],
        lexical = pattern,
        diagnostic_index =
          diagnostic_index
      )

      # The selectors are sorted strongest-first and every accepted selector has
      # identical complete-family coverage.  Once one survives downstream replay,
      # later alternatives cannot add training coverage.
      break
    }

    if (exhausted)
      break
  }
  empty$diagnostics<-candidate_diagnostics
  if(exhausted) { empty$exhausted<-TRUE; return(empty) }
  if(!length(candidates)) return(empty)
  candidate_family_indices <- sort(unique(
    vapply(
      candidates,
      `[[`,
      integer(1),
      "family_index"
    )
  ))

  expected_family_indices <- seq_along(
    groups
  )

  # Grouped recovery is all-or-nothing.  A valid rule for the 15-row merged
  # family is not yet a publishable partition if the sibling structural family
  # has no Content selector.
  if (!identical(
    candidate_family_indices,
    expected_family_indices
  )) {

    logger(
      "debug",
      "content",
      "partition",
      sprintf(
        paste0(
          "%s: partition recovery found downstream-valid candidates for ",
          "%d/%d structural families; missing family index/indices: %s."
        ),
        label,
        length(candidate_family_indices),
        length(expected_family_indices),
        paste(
          setdiff(
            expected_family_indices,
            candidate_family_indices
          ),
          collapse = ","
        )
      )
    )

    return(empty)
  }
  # Strong candidates first seeds the beam with the deterministic greedy path.
  ord<-order(-vapply(candidates,`[[`,integer(1),"evidence"),
    vapply(candidates,`[[`,integer(1),"complexity"),
    vapply(candidates,`[[`,character(1),"lexical"),method="radix")
  candidates<-candidates[ord]
  beam<-list(integer()); complete<-list()
  for(depth in seq_len(limits$variants)) {
    next_beam<-list()
    for(state in beam) for(i in seq_along(candidates)) {
      if(length(state) && i<=tail(state,1L)) next
      if(!tick()) break
      z<-c(state,i); covered<-sort(unique(unlist(lapply(candidates[z],`[[`,"rows"))))
      next_beam[[length(next_beam)+1L]]<-z
      if(identical(covered,rows)) complete[[length(complete)+1L]]<-z
    }
    if(exhausted) break
    if(!length(next_beam)) break
    score<-vapply(next_beam,function(z)length(unique(unlist(lapply(candidates[z],`[[`,"rows")))),integer(1))
    lexical<-vapply(next_beam,function(z)paste(vapply(candidates[z],`[[`,character(1),"lexical"),collapse="\034"),character(1))
    beam<-head(next_beam[order(-score,lengths(next_beam),lexical,method="radix")],limits$beam)
  }
  if(exhausted) { empty$exhausted<-TRUE; return(empty) }
  if(!length(complete)) return(empty)
  verify <- function(state) {
    replay_started<-proc.time()[["elapsed"]]
    on.exit(replay_ms<<-replay_ms+(proc.time()[["elapsed"]]-replay_started)*1000,add=TRUE)
    parts<-candidates[state]
    content<-do.call(rbind,lapply(parts,`[[`,"rule"))
    # Duplicate downstream rules that serialize identically collapse safely;
    # differing rules remain eligible only if aggregate replay below is exact.
    bind_unique<-function(field,template) {
      blocks<-lapply(parts,`[[`,field);blocks<-blocks[lengths(blocks)>0L]
      if(!length(blocks))return(template)
      unique(do.call(rbind,blocks))
    }
    condition<-bind_unique("condition",empty_condition());ratio<-bind_unique("ratio",empty_ratio())
    persisted<-coerce_contract(unserialize(serialize(list(table=content,
      condition=condition,ratio=ratio),NULL,version=2L)))
    ca<-apply_content_table(metadata,persisted$table)
    cross<-any(chr(ca$metadata$Content[other])==label)
    aggregate<-all(chr(ca$metadata$Content[rows])==label &
      ca$rows$TransformationMatch[rows])
    ci<-ca$metadata;if("Options"%in%names(ci))ci$Options[rows]<-""
    cr<-apply_condition_table(ci,persisted$condition,
      if(condition_target%in%names(metadata))chr(metadata[[condition_target]])else character())
    rr<-apply_ratio_table(cr$metadata,persisted$ratio,
      if("Numerator"%in%names(metadata))chr(metadata$Numerator)else character(),
      if("Denominator"%in%names(metadata))chr(metadata$Denominator)else character())
    condition_rows<-if(condition_target%in%names(metadata))
      rows[is_sample_bearing_content(metadata$Content[rows]) &
        nzchar(chr(metadata[[condition_target]])[rows])] else integer()
    applied_variants <- attr(
      ca$metadata,
      "variant_id",
      exact = TRUE
    )

    if (is.null(applied_variants))
      applied_variants <- rep("", nrow(metadata))

    ratio_contract <-
      auto_regex_ratio_replay_contract(
        applied = rr,
        metadata = metadata,
        rows = rows,
        variant_ids =
          applied_variants
      )

    downstream <-
      all(
        cr$diagnostics$ExactMatch[
          condition_rows
        ] %in% TRUE
      ) &&
      ratio_contract$ok
    list(ok=!cross&&aggregate&&downstream,rules=persisted,state=state,
      covered=sum(ca$rows$Match[rows]&ca$rows$TransformationMatch[rows]),
      cross=cross,downstream=downstream,aggregate=aggregate,variants=length(state),
      rule_count=nrow(content)+nrow(condition)+nrow(ratio),
      evidence=sum(vapply(parts,`[[`,integer(1),"evidence")),
      complexity=sum(vapply(parts,`[[`,integer(1),"complexity")),
      lexical=paste(vapply(parts,`[[`,character(1),"lexical"),collapse="\034"))
  }
  verified<-lapply(complete,verify);verified<-verified[vapply(verified,`[[`,logical(1),"ok")]
  if(!length(verified))return(empty)
  o<-do.call(order,list(-vapply(verified,`[[`,integer(1),"covered"),
    vapply(verified,`[[`,logical(1),"cross"),!vapply(verified,`[[`,logical(1),"downstream"),
    !vapply(verified,`[[`,logical(1),"aggregate"),vapply(verified,`[[`,integer(1),"variants"),
    vapply(verified,`[[`,integer(1),"rule_count"),-vapply(verified,`[[`,integer(1),"evidence"),
    vapply(verified,`[[`,integer(1),"complexity"),vapply(verified,`[[`,character(1),"lexical"),
    method="radix"))
  best<-verified[[o[[1L]]]]
  diagnostic_rows<-vapply(candidates[best$state],`[[`,integer(1),"diagnostic_index")
  candidate_diagnostics$Authoritative[diagnostic_rows]<-TRUE
  list(ok=TRUE,exhausted=FALSE,content=best$rules$table,
    condition=best$rules$condition,ratio=best$rules$ratio,
    diagnostics=cbind(candidate_diagnostics,data.frame(Variants=best$variants,
      SelectedVariantCount=best$variants,
      AggregateRuleCount=best$rule_count,SearchNodes=nodes,MemoryBytes=memory,
      CompleteReplay=TRUE,stringsAsFactors=FALSE)),
    performance=list(generated_candidates=generated_candidates,
      deduplicated_candidates=deduplicated_candidates,
      include_exclude_combinations=length(complete),replay_ms=replay_ms,
      elapsed_ms=(proc.time()[["elapsed"]]-started)*1000,
      pruning_reason="deterministic_complete_partition"))
}

# Merge a recovered label into the complete legacy contract, serialize it, and
# replay that exact persisted object before it can replace any legacy rule.  The
# target must become exact, while every non-target row must retain the legacy
# content/condition/ratio result.  Thus a grouped retry is transactional even
# when another label was already (honestly) unresolved by the first pass.
auto_regex_partition_publish <- function(metadata, label, legacy, recovered,
    condition_target="Options") {
  legacy_target <- which(chr(legacy$table$Content)==label)
  insertion_priority <- if(length(legacy_target))
    min(legacy$table$Priority[legacy_target]) else if(nrow(legacy$table))
    max(legacy$table$Priority)+1L else 0L
  recovered$content <- recovered$content[
    order(recovered$content$Priority,recovered$content$VariantId,method="radix"),,
    drop=FALSE]
  # Replacing one legacy selector with several variants must not reuse a
  # Priority owned by an unaffected selector.  Keep the original selector's
  # position and move only its following siblings far enough to make room.
  extra_priorities <- max(0L,nrow(recovered$content)-length(legacy_target))
  unaffected <- legacy$table[chr(legacy$table$Content)!=label,,drop=FALSE]
  if(extra_priorities)
    unaffected$Priority[unaffected$Priority>insertion_priority] <-
      unaffected$Priority[unaffected$Priority>insertion_priority]+extra_priorities
  recovered$content$Priority <- insertion_priority+seq_len(nrow(recovered$content))-1L
  proposed <- list(
    table=rbind(unaffected,recovered$content),
    condition=rbind(legacy$condition[chr(legacy$condition$Content)!=label,,drop=FALSE],
      recovered$condition),
    ratio=rbind(legacy$ratio[chr(legacy$ratio$Content)!=label,,drop=FALSE],
      recovered$ratio))
  persisted <- tryCatch(coerce_contract(unserialize(serialize(proposed,NULL,version=2L))),
    error=function(e) NULL)
  if(is.null(persisted)) return(list(ok=FALSE,rules=legacy))
  target <- which(chr(metadata$Content)==label)
  other <- which(chr(metadata$Content)!=label)
  replay <- function(contract) {
    ca <- apply_content_table(metadata,contract$table)
    ci <- ca$metadata
    if(condition_target%in%names(ci)) ci[[condition_target]] <- ""
    cr <- apply_condition_table(ci,contract$condition,
      if(condition_target%in%names(metadata))chr(metadata[[condition_target]])else character())
    rr <- apply_ratio_table(cr$metadata,contract$ratio,
      if("Numerator"%in%names(metadata))chr(metadata$Numerator)else character(),
      if("Denominator"%in%names(metadata))chr(metadata$Denominator)else character())
    list(content=ca,condition=cr,ratio=rr)
  }
  before <- replay(legacy); after <- replay(persisted)
  condition_values <- function(result) if(condition_target%in%names(result$metadata))
    chr(result$metadata[[condition_target]]) else rep("",nrow(metadata))
  exact_target <- length(target)>0L &&
    all(chr(after$content$metadata$Content[target])==label) &&
    all(after$content$rows$TransformationMatch[target])
  no_cross_label_fp <- !any(chr(after$content$metadata$Content[other])==label)
  target_variants <- attr(after$content$metadata,"variant_id",exact=TRUE)
  if(is.null(target_variants)) target_variants <- rep("",nrow(metadata))
  reference_states <- vapply(split(target,target_variants[target]),function(rows) {
    subset <- metadata[rows,,drop=FALSE]
    c(condition=auto_regex_variant_condition_state(subset,condition_target),
      ratio=auto_regex_variant_ratio_state(subset))
  },character(2L))
  references_safe <- !length(reference_states) || !any(reference_states=="partial")
  unchanged_other <- identical(chr(after$content$metadata$Content[other]),
      chr(before$content$metadata$Content[other])) &&
    identical(condition_values(after$condition)[other],
      condition_values(before$condition)[other]) &&
    identical(chr(after$ratio$metadata$Numerator[other]),
      chr(before$ratio$metadata$Numerator[other])) &&
    identical(chr(after$ratio$metadata$Denominator[other]),
      chr(before$ratio$metadata$Denominator[other]))
  condition_rows <- if(condition_target%in%names(metadata))
    target[is_sample_bearing_content(metadata$Content[target]) &
      nzchar(chr(metadata[[condition_target]])[target])] else integer()
  ratio_contract <-
    auto_regex_ratio_replay_contract(
      applied = after$ratio,
      metadata = metadata,
      rows = target,
      variant_ids =
        target_variants
    )

  downstream_exact <-
    all(
      after$condition$diagnostics$
        ExactMatch[
          condition_rows
        ] %in% TRUE
    ) &&
    ratio_contract$ok
  publish_ok <- exact_target && no_cross_label_fp && references_safe &&
    unchanged_other && downstream_exact
  list(ok=publish_ok,
    rules=if(publish_ok)
      persisted else legacy)
}

# ============================================================================
# Final Content-regex minimization
#
# This is a representation-only pass over already-proven Content rules.
#
# It removes complete top-level regex atoms from the left/right and accepts a
# candidate only when complete-table Content / VariantId / Transformation /
# conflict replay remains identical.
#
# Confirmed condition/numerator/denominator spans are used as negative evidence:
# structural candidates are preferred over dataset-specific biological text.
#
# Regex redundancy walks back toward the original proven rule one safely
# removed regex atom at a time. Per-rule overrides take precedence over the
# global redundancy value.
# ============================================================================
