# Auto-regex inference pipeline.
#
# Invariant stage order: provenance preparation; content inference/application;
# condition inference/application; ratio inference/application; semantic content
# refinement; downstream reconciliation/replay; redundancy compaction/rebuild;
# contract coercion; and payload validation. Prerequisite stages execute exactly
# once. Keep progress events and diagnostics/timings aligned with this order.

auto_regex_infer_rules <- function(
    metadata,
    condition_target = "Options",
    redundancy = 0L,
    redundancy_overrides = NULL,
    debug_log = NULL,
    progress = function(event) invisible(NULL),
    .stage_functions = NULL,
    provenance = NULL,
    .partition_recovery =
      auto_regex_partition_recovery) {
  if (!is.function(progress)) stop("progress must be a function(event).", call. = FALSE)
  if (!is.function(.partition_recovery))
    stop(".partition_recovery must be a function.", call. = FALSE)
  provenance_prepared <- auto_regex_prepare_provenance(metadata, provenance)
  metadata <- provenance_prepared$metadata
  total_started <- proc.time()[["elapsed"]]
  warnings <- character()
  unsafe_provenance <- provenance_prepared$diagnostics$Outcome %in% c(
    "ambiguous", "stale lineage", "collision", "missing source",
    "incomplete generated family")
  if (any(unsafe_provenance)) warnings <- sprintf(
    "Provenance row %d: %s; using only safe generic inference or abstention.",
    provenance_prepared$diagnostics$Row[unsafe_provenance],
    provenance_prepared$diagnostics$Outcome[unsafe_provenance])
  errors <- character()
  diagnostics <- list(content=data.frame(), condition=data.frame(), ratio=data.frame(),
    semantic_spans=data.frame(), content_refinement_lineage=data.frame(),
    identifier_fallback=data.frame(), technical_singleton_fallback=data.frame(),
    content_redundancy=data.frame(),
    provenance=provenance_prepared$diagnostics)
  statuses <- list(content=data.frame(), condition=data.frame(), ratio=data.frame())
  timings <- c(preprocessing=0, content=0, condition=0, ratio=0,
               prerequisite_application=0, candidate_construction=0,
               candidate_scoring=0, completion_gate_replay=0,
               semantic_span_analysis=0, targeted_content_refinement=0,
               downstream_replay=0,
               payload_validation=0, total=0)
  rules <- list(table=empty_content(), condition=empty_condition(), ratio=empty_ratio())
  stage_outcomes <- setNames(rep("skipped", 4L),
    c("preprocessing", "content", "condition", "ratio"))

  logger <- auto_regex_debug_logger(debug_log)
  defaults <- list(content=infer_content, condition=infer_conditions, ratio=infer_ratios)
  if (!is.null(.stage_functions)) {
    if (!is.list(.stage_functions) || is.null(names(.stage_functions)) ||
        any(!names(.stage_functions) %in% names(defaults)) ||
        any(!vapply(.stage_functions, is.function, logical(1))))
      stop(".stage_functions must be a named list of content, condition, or ratio functions.",
        call. = FALSE)
    defaults[names(.stage_functions)] <- .stage_functions
  }

  run_stage <- function(name, phase, expression, skip_reason = NULL) {
    started <- proc.time()[["elapsed"]]
    progress(paste0(name, "_start"))
    on.exit({
      timings[[name]] <<- (proc.time()[["elapsed"]] - started) * 1000
      logger("trace", "pipeline", name,
        sprintf("%s elapsed time: %.1f ms.", phase, timings[[name]]))
      progress(paste0(name, "_complete"))
    }, add = TRUE)
    if (!is.null(skip_reason)) {
      stage_outcomes[[name]] <<- "skipped"
      logger("info", "pipeline", name,
        sprintf("%s skipped: %s.", phase, skip_reason))
      return(list(outcome="skipped", value=NULL))
    }
    logger("info", "pipeline", name, sprintf("%s started.", phase))
    tryCatch({
      value <- force(expression)
      stage_outcomes[[name]] <<- "success"
      logger("info", "pipeline", name, sprintf("%s completed.", phase))
      list(outcome="success", value=value)
    }, error=function(e) {
      concise <- auto_regex_safe_value(conditionMessage(e))
      structured <- sprintf("stage=%s | outcome=failure | error=%s", name, concise)
      errors <<- c(errors, structured)
      stage_outcomes[[name]] <<- "failure"
      logger("error", "pipeline", name,
        sprintf("%s failed: %s", phase, concise))
      list(outcome="failure", value=NULL, error=structured)
    })
  }

  preprocessing <- run_stage("preprocessing", "Preprocessing and validation",
    validate_metadata(metadata, names(metadata), condition_target))
  validation <- preprocessing$value
  if (!identical(preprocessing$outcome, "success")) validation <- data.frame()
  if (!nrow(validation)) {
    timings[["total"]] <- (proc.time()[["elapsed"]] - total_started) * 1000
    return(list(rules=coerce_contract(rules), statuses=statuses, diagnostics=diagnostics,
      warnings=unique(warnings), errors=unique(errors), timings=timings,
      stage_outcomes=stage_outcomes))
  }
  warnings <- c(warnings, validation$Message[validation$Severity == "Warning"])
  errors <- c(errors, validation$Message[validation$Severity == "Error"])
  if (length(errors)) {
    timings[["total"]] <- (proc.time()[["elapsed"]] - total_started) * 1000
    return(list(rules=coerce_contract(rules), statuses=statuses, diagnostics=diagnostics,
      warnings=unique(warnings), errors=unique(errors), timings=timings,
      stage_outcomes=stage_outcomes))
  }

  content_run <- run_stage(
    "content",
    "Content inference",
    defaults$content(
      metadata,
      redundancy = 0L,
      logger = logger
    )
  )
  content <- content_run$value
  if (!is.null(content)) {
    rules$table <- content$table
    statuses$content <- content$status
    diagnostics$content <- content$metrics
    diagnostics$content_performance <- content$performance
    diagnostics$content_candidate_evidence <- content$candidate_evidence
    diagnostics$content_redundancy <- content$redundancy_history
    warnings <- c(warnings, content$warnings)
  }
  condition_run <- run_stage("condition", "Condition inference",
    defaults$condition(metadata, condition_target, logger=logger))
  condition <- condition_run$value
  if (!is.null(condition)) {
    rules$condition <- condition$table
    statuses$condition <- condition$status
    diagnostics$condition <- condition$diagnostics
    warnings <- c(warnings, condition$warnings)
  }
  # Never retry a failed prerequisite phase inside ratio inference.  Successful
  # (including canonically empty) results are injected so each phase executes
  # exactly once in an integrated run.
  ratio_run <- if (!identical(content_run$outcome, "success") ||
      !identical(condition_run$outcome, "success"))
    run_stage("ratio", "Ratio inference", NULL,
      "a prerequisite inference phase failed") else
    run_stage("ratio", "Ratio inference", defaults$ratio(metadata, logger=logger,
      content_rules=content$table, condition_rules=condition$table))
  ratio <- ratio_run$value
  if (!is.null(ratio)) {
    rules$ratio <- ratio$table
    statuses$ratio <- ratio$status
    diagnostics$ratio <- ratio$diagnostics
    warnings <- c(warnings, ratio$warnings)
    timings[names(ratio$timings)] <- ratio$timings
  }
  if(!is.null(content) && !is.null(condition) && !is.null(ratio) &&
      nrow(diagnostics$content_candidate_evidence)) {
    for(i in seq_len(nrow(diagnostics$content_candidate_evidence))) {
      label<-diagnostics$content_candidate_evidence$Content[[i]]
      condition_exact<-if(nrow(condition$diagnostics) &&
          all(c("Content","ExactMatch")%in%names(condition$diagnostics)))
        sum(condition$diagnostics$Content==label & condition$diagnostics$ExactMatch%in%TRUE,na.rm=TRUE) else 0L
      ratio_exact<-if(nrow(ratio$diagnostics) &&
          all(c("Content","Success")%in%names(ratio$diagnostics)))
        sum(ratio$diagnostics$Content==label & ratio$diagnostics$Success%in%TRUE,na.rm=TRUE) else 0L
      diagnostics$content_candidate_evidence$DownstreamExactMatchCount[[i]]<-
        condition_exact+ratio_exact
    }
  }
  # Diagnose the completed legacy pass before any later fallback/refinement.
  # In particular, eligibility is based on row-level stage failures, not merely
  # on the presence of two differently shaped examples bearing one label.
  if (!is.null(content) && !is.null(condition) && !is.null(ratio)) {
    failure_diagnostic <- auto_regex_failure_diagnostic(metadata,content,condition,
      ratio,condition_target)
    diagnostics$failure_by_content <- failure_diagnostic$by_content
    diagnostics$failure_by_row <- failure_diagnostic$by_row
    diagnostics$partition_search <- data.frame()
    if(nrow(failure_diagnostic$by_content)) for(i in seq_len(nrow(failure_diagnostic$by_content))) {
      fd <- failure_diagnostic$by_content[i,,drop=FALSE]
      label <- chr(fd$Content)[[1L]]
      recovery_status <- "rejected"
      recovery_detail <- "eligibility gate failed"
      if(isTRUE(fd$GroupedFallbackEligible)) {
        recovery_status <- "started"
        # This call is intentionally below the completed legacy replay and its
        # per-label failure gate.  The injected hook exists so tests can prove
        # that a successful one-rule label never enters partition search.
        recovered <- .partition_recovery(metadata,label,condition_target,logger)
        if(nrow(recovered$diagnostics)) diagnostics$partition_search<-rbind(
          diagnostics$partition_search,recovered$diagnostics)
        if(isTRUE(recovered$exhausted)) {
          recovery_status <- "limit_exhausted"
          recovery_detail <- "bounded search ended without publishing a partial alternative"
          warnings <- c(warnings,sprintf(
            "%s partition search limit exhausted; inference abstained without publishing a partial alternative.",label))
        } else if(!isTRUE(recovered$ok)) {
          recovery_detail <- "no complete bounded partition passed replay"
        } else {
          published <- auto_regex_partition_publish(metadata,label,list(table=content$table,
            condition=condition$table,ratio=ratio$table),recovered,condition_target)
          if(!isTRUE(published$ok)) {
            recovery_detail <- "complete partition rejected by aggregate publish replay"
          } else {
            content$table <- published$rules$table
            condition$table <- published$rules$condition
            ratio$table <- published$rules$ratio
            rules$table<-content$table;rules$condition<-condition$table;rules$ratio<-ratio$table
            warnings<-warnings[!startsWith(warnings,paste0(label," ")) &
              !startsWith(warnings,paste0(label,":"))]
            recovery_detail <- sprintf("accepted %d variants after aggregate downstream replay",
              recovered$diagnostics$Variants[[1L]])
          }
        }
      }
      logger(if(isTRUE(fd$GroupedFallbackEligible)) "debug" else "trace",
        "content","failure_diagnostic",
        auto_regex_failure_log_message(fd,recovery_status,recovery_detail))
    }
  }
  # Conditions and ratios identify which source spans are verified semantics.
  # Only now may the content second pass run; its complete final table is then
  # replayed onto metadata before construction of the exported payload.
  content_lineage <- data.frame()
  final_content_application <- NULL

  # Fully inferred, semantically refined Content rules before optional
  # representation compaction. This becomes the fast-rebuild cache authority.
  content_redundancy_base <- NULL
  content_redundancy_analysis_cache <- NULL

  # Defined outside the guarded block so the final return can construct a cache
  # even without reaching into local block state.
  semantic_spans <- data.frame()
  if (!is.null(content) && !is.null(condition) && !is.null(ratio)) {
    progress("semantic_refinement_start")
    semantic_started <- proc.time()[["elapsed"]]
    semantic_spans <- auto_regex_semantic_spans(metadata,condition$table,
      condition$diagnostics,ratio$table,ratio$diagnostics)
    timings[["semantic_span_analysis"]] <-
      (proc.time()[["elapsed"]] - semantic_started) * 1000

    refinement_started <- proc.time()[["elapsed"]]
    refined <- refine_content_with_semantic_spans(metadata,rules$table,semantic_spans)

    # Gate ordinary semantic refinements first.  The broad Identifier fallback
    # is considered only against rows left unassigned by that complete table.
    baseline_table <- rules$table
    replay_started <- proc.time()[["elapsed"]]
    gated <- auto_regex_gate_content_refinements(metadata,baseline_table,refined,
      condition$table,ratio$table,condition_target)
    timings[["downstream_replay"]] <-
      (proc.time()[["elapsed"]] - replay_started) * 1000
    normal_table <- gated$table
    normal_application <- gated$application

    # A closed vendor-protocol exception is evaluated after all ordinary rules
    # and only for their unresolved rows.  Unlike partition recovery, this is
    # neither grouped inference nor evidence transfer from source examples.
    technical_pattern <- technical_description_fallback_pattern()
    technical_hit <- safe_grepl(technical_pattern,chr(metadata$Column))
    technical_unassigned <- !nzchar(chr(normal_application$metadata$Content))
    technical_expected <- chr(metadata$Content)
    technical_wrong_truth <- technical_hit & nzchar(technical_expected) &
      technical_expected!="Description"
    technical_publishable <- technical_hit & technical_unassigned
    technical_applied <- FALSE
    if(any(technical_publishable) && !any(technical_wrong_truth)) {
      technical_rule <- canonical_rule_row("content",Content="Description",
        VariantId=stable_variant_ids("Description technical singleton"),
        Priority=if(nrow(normal_table)) max(normal_table$Priority)+1L else 0L,
        Include=technical_pattern,Exclude="",Transformation=NA_character_)
      persisted_rule <- unserialize(serialize(technical_rule,NULL,version=2L))
      if(!identical(technical_rule,persisted_rule))
        stop("Technical Description fallback serialization changed its rule.",call.=FALSE)
      candidate_table <- rbind(normal_table,persisted_rule)
      candidate_application <- apply_content_table(metadata,candidate_table)
      assigned <- !technical_unassigned
      if(!identical(chr(candidate_application$metadata$Content[assigned]),
          chr(normal_application$metadata$Content[assigned])))
        stop("Technical Description fallback changed an existing inferred assignment.",call.=FALSE)
      replay_application <- apply_content_table(metadata,
        unserialize(serialize(candidate_table,NULL,version=2L)))
      if(!identical(serialize(candidate_application$metadata,NULL,version=2L),
          serialize(replay_application$metadata,NULL,version=2L)))
        stop("Technical Description fallback serialized replay was not identical.",call.=FALSE)
      normal_table <- candidate_table
      normal_application <- candidate_application
      technical_applied <- TRUE
    }
    diagnostics$technical_singleton_fallback <- data.frame(
      Row=seq_len(nrow(metadata)),Column=chr(metadata$Column),
      FallbackType="technical_singleton_fallback",Grouped=FALSE,
      EvidenceBased=FALSE,PatternMatched=technical_hit,
      PreviouslyUnassigned=technical_unassigned,
      RejectedByAuthoritativeNonDescription=technical_wrong_truth,
      Applied=technical_applied & technical_publishable,
      stringsAsFactors=FALSE,check.names=FALSE)
    final_table <- normal_table
    fallback_diagnostics <- data.frame()
    identifier_fallback_applied <- FALSE
    if (!"Identifier" %in% chr(normal_table$Content)) {
      fallback_pattern <- identifier_fallback_pattern()
      fallback_hit <- safe_grepl(
        regex_from_miraprot_storage(fallback_pattern, "content"), chr(metadata$Column))
      previously_unassigned <- !nzchar(chr(normal_application$metadata$Content))
      expected_content <- chr(metadata$Content)
      authoritative <- nzchar(expected_content)
      authoritative_identifier <- authoritative & expected_content == "Identifier"
      rejected_by_non_identifier <- fallback_hit & authoritative &
        expected_content != "Identifier"
      fallback_rejected <- any(rejected_by_non_identifier)
      truth_supported <- any(fallback_hit & authoritative_identifier)
      overridden_by_specific <- fallback_hit & !previously_unassigned
      publishable_hit <- fallback_hit & previously_unassigned
      if (truth_supported && !fallback_rejected && any(publishable_hit)) {
        identifier_fallback_applied <- TRUE
        fallback <- canonical_rule_row("content",Content="Identifier",
          VariantId=stable_variant_ids("Identifier"),Priority=0L,
          Include=fallback_pattern,Exclude="",Transformation=NA_character_)
        final_table <- rbind(fallback, normal_table)
        final_application <- apply_content_table(metadata, final_table)
        normally_assigned <- !previously_unassigned
        changed <- sum(chr(final_application$metadata$Content[normally_assigned]) !=
          chr(normal_application$metadata$Content[normally_assigned]))
        if (changed != 0L)
          stop("Identifier fallback changed an existing inferred assignment.", call. = FALSE)
        matched_rows <- which(fallback_hit & previously_unassigned)
        logger("debug","semantic_refinement","identifier_fallback",sprintf(
          "Identifier fallback added | matched currently unassigned rows: %d | existing inferred assignments changed: 0",
          length(matched_rows)))
        examples <- head(matched_rows, MIRAPROT_REPRESENTATIVE_EXAMPLE_LIMIT)
        logger("trace","semantic_refinement","identifier_fallback",sprintf(
          "Identifier fallback matched row IDs: %s | representative columns: %s",
          paste(examples, collapse=","),
          paste(chr(metadata$Column[examples]), collapse=" | ")))
      }
      # Record truth and precedence independently, including vetoed attempts.
      # A single authoritative non-Identifier hit rejects the complete rule.
      diagnostic_final <- if (identifier_fallback_applied)
        chr(final_application$metadata$Content) else
        chr(normal_application$metadata$Content)
      fallback_diagnostics <- data.frame(
        Row=seq_len(nrow(metadata)), Column=chr(metadata$Column),
        PatternMatched=fallback_hit, ExpectedContent=expected_content,
        PreviouslyUnassigned=previously_unassigned,
        OverriddenBySpecificRule=overridden_by_specific,
        RejectedByAuthoritativeNonIdentifier=rejected_by_non_identifier,
        FallbackRejected=fallback_rejected,
        FinalContent=diagnostic_final,
        stringsAsFactors=FALSE, check.names=FALSE)
      if (fallback_rejected) logger("debug","semantic_refinement",
        "identifier_fallback",sprintf(
          "Identifier fallback rejected | authoritative non-Identifier matches: %d",
          sum(rejected_by_non_identifier)))
    }

    # -----------------------------------------------------------------------
    # Representation-only final compaction
    #
    # `final_table` is already the complete proven Content table, including
    # partition recovery, semantic refinement, technical fallback and optional
    # Identifier fallback. Preserve it untouched as the cache authority.
    # -----------------------------------------------------------------------

    content_redundancy_base <-
      final_table

    compacted_content <-
      auto_regex_compact_content_table(
        metadata = metadata,
        table =
          content_redundancy_base,
        semantic_spans =
          semantic_spans,
        redundancy =
          redundancy,
        redundancy_overrides =
          redundancy_overrides,
        logger = logger
      )

    content_redundancy_analysis_cache <-
      compacted_content$cache

    final_table <-
      compacted_content$table

    # Replace the old first-pass redundancy diagnostic with the authoritative
    # final shrink/restore lineage.
    diagnostics$content_redundancy <-
      compacted_content$lineage

    rules$table <- final_table
    content_lineage <- gated$lineage
    final_content_application <- apply_content_table(metadata, final_table)
    # Overlap with the prepended fallback is intentional precedence.  Preserve
    # only conflicts already present between ordinary inferred rules.
    final_content_application$rows$Conflict <- normal_application$rows$Conflict
    final_content_application$conflicts <-
      final_content_application$rows[final_content_application$rows$Conflict,,drop=FALSE]
    diagnostics$semantic_spans <- semantic_spans
    diagnostics$content_refinement_lineage <- content_lineage
    diagnostics$content_application <- final_content_application$rows
    diagnostics$identifier_fallback <- fallback_diagnostics
    if (technical_applied && !is.null(content$unresolved_reasons) &&
        nrow(content$unresolved_reasons)) {
      # A successfully replayed technical Description singleton is neutral
      # fallback information. Only labels that remain unresolved after the
      # integrated fallback are warning-worthy.
      resolved_description_warnings <- paste0(
        "Description: ",
        chr(content$unresolved_reasons$Reason[
          content$unresolved_reasons$Content == "Description"
        ])
      )
      warnings <- setdiff(warnings, resolved_description_warnings)
    }
    if (identifier_fallback_applied && !is.null(content$unresolved_reasons) &&
        nrow(content$unresolved_reasons)) {
      resolved_identifier_warnings <- paste0(
        "Identifier: ",
        chr(content$unresolved_reasons$Reason[
          content$unresolved_reasons$Content == "Identifier"
        ])
      )
      warnings <- setdiff(warnings, resolved_identifier_warnings)
    }
    timings[["targeted_content_refinement"]] <-
      (proc.time()[["elapsed"]] - refinement_started) * 1000
    diagnostics$downstream_replay <- gated$summary
    diagnostics$condition_replay <- gated$condition$diagnostics
    diagnostics$ratio_replay <- gated$ratio$diagnostics
    if(length(gated$remaining_violations)) errors <- c(errors,
      sprintf("Downstream replay failed for metadata row(s): %s.",
        paste(gated$remaining_violations,collapse=",")))
    affected <- unique(chr(semantic_spans$Content[!is.na(semantic_spans$SafeToGeneralize) &
      semantic_spans$SafeToGeneralize]))
    affected <- affected[nzchar(affected)]
    accepted <- sum(content_lineage$Accepted,na.rm=TRUE)
    retained <- max(0L,length(affected)-accepted)
    diagnostics$refinement_counts <- data.frame(AffectedLabels=length(affected),
      AcceptedRefinements=accepted,RetainedUnchanged=retained,stringsAsFactors=FALSE)
    logger("debug","semantic_refinement","summary",sprintf(
      "Generalization summary: affected labels=%d; accepted refinements=%d; retained unchanged=%d.",
      length(affected),accepted,retained))
    detail <- content_lineage[content_lineage$Content %in% affected,,drop=FALSE]
    for(i in head(seq_len(nrow(detail)),MIRAPROT_REPRESENTATIVE_EXAMPLE_LIMIT))
      logger("trace","semantic_refinement","candidate",sprintf(
        "%s: %s; removed dataset-specific values: %s.",detail$Content[[i]],
        if(isTRUE(detail$Accepted[[i]]))"accepted" else "retained unchanged",
        if(nzchar(detail$RemovedValues[[i]]))detail$RemovedValues[[i]] else "none"))
    progress("semantic_refinement_complete")
  }

  # -------------------------------------------------------------------------
  # Final parent/child reconciliation
  #
  # Condition and ratio inference deliberately run independently from Content
  # inference so their evidence is still available to partition recovery and
  # semantic refinement. Once the final Content identities are known, however,
  # only downstream rules with an exact (Content, VariantId) parent may be
  # published.
  #
  # Do not weaken validate_export(): an externally supplied orphan rule remains
  # an invalid contract. This step only reconciles internally inferred rules.
  # -------------------------------------------------------------------------

  parent_reconciliation <-
    auto_regex_reconcile_downstream_rules(
      rules
    )

  rules <-
    parent_reconciliation$rules

  diagnostics$downstream_parent_reconciliation <-
    parent_reconciliation$removed

  if (nrow(
    parent_reconciliation$removed
  )) {

    for (i in seq_len(
      nrow(
        parent_reconciliation$removed
      )
    )) {

      orphan <-
        parent_reconciliation$removed[
          i,
          ,
          drop = FALSE
        ]

      message <- sprintf(
        paste0(
          "%s rule '%s' for %s [%s] was not published because ",
          "its matching Content rule was unresolved or not published."
        ),
        tools::toTitleCase(
          orphan$Component[[1L]]
        ),
        orphan$RuleId[[1L]],
        orphan$Content[[1L]],
        orphan$VariantId[[1L]]
      )

      warnings <- c(
        warnings,
        message
      )

      logger(
        "warning",
        "pipeline",
        "downstream_parent_reconciliation",
        message
      )
    }
  }

  # Keep the local stage objects consistent with the contract which will be
  # returned. Their diagnostics remain untouched so the UI can still explain
  # that extraction itself succeeded before the missing parent prevented
  # publication.
  if (!is.null(condition)) {
    condition$table <-
      rules$condition
  }

  if (!is.null(ratio)) {
    ratio$table <-
      rules$ratio
  }

  redundancy_base <- NULL

  if (is.data.frame(
    content_redundancy_base
  ) &&
  nrow(
    content_redundancy_base
  )) {

    base_rules <- rules

    base_rules$table <-
      content_redundancy_base

    base_rules <-
      coerce_contract(
        base_rules
      )

    redundancy_base <- list(
      rules = base_rules,
      semantic_spans =
        if (is.data.frame(
          semantic_spans
        )) {
          semantic_spans
        } else {
          data.frame()
        },
      analysis_cache =
        content_redundancy_analysis_cache
    )
  }

  rules <- coerce_contract(rules)
  payload_started <- proc.time()[["elapsed"]]
  progress("payload_validation_start")
  errors <- c(errors, validate_export(rules, metadata, logger=logger))
  timings[["payload_validation"]] <-
    (proc.time()[["elapsed"]] - payload_started) * 1000
  logger("trace", "pipeline", "payload_validation", sprintf(
    "Payload validation completed in %.1f ms.", timings[["payload_validation"]]))
  progress("payload_validation_complete")
  timings[["total"]] <- (proc.time()[["elapsed"]] - total_started) * 1000
  list(
    rules = rules,
    statuses = statuses,
    diagnostics = diagnostics,
    warnings =
      unique(
        warnings[
          nzchar(warnings)
        ]
      ),
    errors =
      unique(
        errors[
          nzchar(errors)
        ]
      ),
    timings = timings,
    stage_outcomes =
      stage_outcomes,

    # Private Shiny fast-rebuild material; this is not part of the exported
    # Auto-Assign rule contract.
    redundancy_base =
      redundancy_base
  )
}
