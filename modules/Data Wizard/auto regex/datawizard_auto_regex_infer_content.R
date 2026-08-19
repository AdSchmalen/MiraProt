infer_content <- function(df, redundancy=0L, logger=function(...) invisible(NULL)) {
  started<-proc.time()[["elapsed"]]
  logger("debug","content","infer",sprintf("Inferring content rules from %d rows.",nrow(df)),
    details=list(examples=head(chr(df$Column),MIRAPROT_REPRESENTATIVE_EXAMPLE_LIMIT)))
  x<-chr(df$Column);y<-chr(df$Content);row_id<-seq_along(x)
  token_cache<-tokens(x)
  labels<-unique(y[nzchar(trimws(y))]) # table order is evidence; never sort it
  rules<-empty_content();metrics<-data.frame();statuses<-data.frame();refinement_history<-list()
  redundancy_history<-data.frame();unresolved<-data.frame();representatives<-data.frame();notes<-character()
  candidate_evidence<-data.frame()
  performance<-data.frame(Content=labels,GeneratedCandidates=0L,
    DeduplicatedCandidates=0L,IncludeExcludeCombinations=0L,ScoringMs=0,
    ReplayMs=0,PruningReason="not_searched",ElapsedMs=0,stringsAsFactors=FALSE)
  add_unresolved<-function(label,code,reason) {
    unresolved<<-rbind(unresolved,data.frame(Content=label,Code=code,Reason=reason,stringsAsFactors=FALSE))
    if(!label%in%statuses$Content)statuses<<-rbind(statuses,data.frame(Content=label,Status="unresolved",
      Reliable=FALSE,PositiveExamples=sum(y==label),NegativeExamples=sum(y!=label),stringsAsFactors=FALSE))
    notes<<-c(notes,paste0(label,": ",reason))
  }
  for(label in labels) {
    label_started<-proc.time()[["elapsed"]];scoring_ms<-0;replay_ms<-0
    truth<-y==label;pos<-x[truth];neg<-x[!truth]
    transformation_details<-content_transformation_details(df,label)
    transformation<-if(transformation_details$source %in% c("conflict","unsupported"))
      NA_character_ else infer_content_transformation(df,label)
    transformation_source<-transformation_details$source
    transformation_message<-transformation_details$message
    if(transformation_source %in% c("conflict","unsupported")) {
      notes<-c(notes,transformation_message)
      unresolved<-rbind(unresolved,data.frame(Content=label,
        Code=AUTO_REGEX_FAILURE_CODES[["transformation_conflict"]],Reason=transformation_message,stringsAsFactors=FALSE))
      logger("warning","content","conflicts",transformation_message,
        details=list(content=label,values=transformation_details$values,row_ids=transformation_details$row_ids))
    } else logger("debug","content","select",sprintf("%s: Transformation=%s; source=%s.",label,
      if(is.na(transformation))"NA" else transformation,transformation_source))
    logger("trace","content","tokenize",sprintf("%s: representative tokenizations prepared for row IDs %s.",
      label,paste(head(row_id[truth],3L),collapse=",")),details=list(examples=head(unique(chr(pos)), MIRAPROT_REPRESENTATIVE_EXAMPLE_LIMIT)))
    special<-unique(token_cache$Text[token_cache$Source%in%row_id[truth]])
    special<-special[grepl("^[^[:alnum:][:space:]]$",special)]
    formatted_special <- vapply(head(special,3L), .format_special_character, character(1))
    logger("trace","content","special-characters",sprintf("%s: detected %d distinct special character(s); values: %s",
      label,length(special),paste(formatted_special,collapse=", ")))
    # Row Index is a Data Wizard protocol rule rather than an inferred content
    # class.  Preserve it once, canonically, regardless of its sample count.
    if(identical(label,"Row Index")) {
      statuses<-rbind(statuses,data.frame(Content=label,Status="reliable",Reliable=TRUE,
                                          PositiveExamples=length(pos),NegativeExamples=length(neg),stringsAsFactors=FALSE))
      next
    }

    if(any(pos%in%neg)){
      add_unresolved(
        label,
        "conflicting_labels",
        "identical source text occurs under another label"
      )
      next
    }

    # A Content represented by exactly one source row has no repeated
    # structural evidence from which to learn a generalized selector.
    #
    # Start instead from the exact full header. This is not inferred
    # generalization: the complete-table replay below must prove TP=1, FP=0,
    # FN=0 after persistence. The final Content compactor may subsequently
    # remove unnecessary edge atoms while preserving the exact full-table
    # assignment state.
    if (length(pos) == 1L &&
        !transformation_source %in%
        c("conflict", "unsupported")) {

      singleton_include <-
        auto_regex_exact_content_selector(
          pos[[1L]]
        )

      if (nzchar(singleton_include)) {

        singleton_score_started <-
          proc.time()[["elapsed"]]

        singleton_metric <-
          score_pattern(
            singleton_include,
            x,
            truth,
            "",
            constraint_count = 1L
          )

        scoring_ms <-
          scoring_ms +
          (
            proc.time()[["elapsed"]] -
              singleton_score_started
          ) * 1000

        singleton_replay_started <-
          proc.time()[["elapsed"]]

        persisted_include <- tryCatch(
          regex_to_miraprot_storage(
            regex_from_miraprot_storage(
              singleton_include,
              "content"
            ),
            "content"
          ),
          error = function(e) ""
        )

        persisted_metric <-
          if (nzchar(persisted_include)) {

            score_pattern(
              persisted_include,
              x,
              truth,
              "",
              constraint_count = 1L
            )

          } else {

            NULL
          }

        replay_ms <-
          replay_ms +
          (
            proc.time()[["elapsed"]] -
              singleton_replay_started
          ) * 1000

        singleton_replay_ok <-
          !is.null(persisted_metric) &&
          identical(
            as.integer(
              singleton_metric[
                1L,
                c(
                  "TP",
                  "FP",
                  "FN"
                )
              ]
            ),
            as.integer(
              persisted_metric[
                1L,
                c(
                  "TP",
                  "FP",
                  "FN"
                )
              ]
            )
          )

        singleton_reliable <-
          singleton_replay_ok &&
          persisted_metric$TP[[1L]] == 1L &&
          persisted_metric$FP[[1L]] == 0L &&
          persisted_metric$FN[[1L]] == 0L

        if (singleton_reliable) {

          rules <- rbind(
            rules,
            canonical_rule_row(
              "content",
              Content = label,
              VariantId =
                stable_variant_ids(
                  label
                ),
              Priority =
                nrow(rules) + 1L,
              Include =
                persisted_include,
              Exclude = "",
              Transformation =
                transformation[[1L]]
            )
          )

          statuses <- rbind(
            statuses,
            data.frame(
              Content = label,
              Status = "reliable",
              Reliable = TRUE,
              PositiveExamples = 1L,
              NegativeExamples =
                length(neg),
              stringsAsFactors = FALSE
            )
          )

          metrics <- rbind(
            metrics,
            cbind(
              data.frame(
                Content = label,
                Include =
                  persisted_include,
                Exclude = "",
                stringsAsFactors = FALSE
              ),
              persisted_metric
            )
          )

          candidate_evidence <- rbind(
            candidate_evidence,
            data.frame(
              Content = label,
              CandidateFamily =
                "singleton_exact_fallback",
              Pattern =
                persisted_include,
              SupportingSourceRows = 1L,
              DistinctSourceNames = 1L,
              CoveredExamples =
                auto_regex_example_list(
                  pos
                ),
              UncoveredExamples = "",
              FalsePositiveCount = 0L,
              DownstreamExactMatchCount = 0L,
              ValidWithoutWholeHeaderMemorization =
                FALSE,
              EvidenceThresholdPassed =
                TRUE,
              Authoritative =
                TRUE,
              EvidenceStatus =
                "singleton_exact_fallback",
              stringsAsFactors = FALSE,
              check.names = FALSE
            )
          )

          performance[
            performance$Content == label,
            c(
              "GeneratedCandidates",
              "DeduplicatedCandidates",
              "IncludeExcludeCombinations",
              "ScoringMs",
              "ReplayMs",
              "PruningReason",
              "ElapsedMs"
            )
          ] <- list(
            1L,
            1L,
            1L,
            scoring_ms,
            replay_ms,
            "singleton_exact_fallback",
            (
              proc.time()[["elapsed"]] -
                label_started
            ) * 1000
          )

          logger(
            "debug",
            "content",
            "select",
            sprintf(
              paste0(
                "%s: singleton exact fallback accepted; ",
                "initial include='%s' (TP=1, FP=0, FN=0). ",
                "Final minimization is delegated to Content compaction."
              ),
              label,
              auto_regex_safe_value(
                persisted_include
              )
            )
          )

          next
        }
      }
    }

    raw<-candidate_fragments(pos,neg)
    fragment_stats<-attr(raw,"candidate_stats",exact=TRUE)
    # Normalize the new record-shaped candidate object as well as legacy vectors.
    records<-if(is.data.frame(raw))raw else if(is.list(raw)&&!is.null(raw$candidates))raw$candidates else {
      families<-attr(raw,"candidate_families",exact=TRUE)
      if(is.null(families)||length(families)!=length(raw))families<-rep("legacy",length(raw))
      data.frame(Pattern=chr(raw),Family=chr(families),stringsAsFactors=FALSE)
    }
    performance[performance$Content==label,c("GeneratedCandidates","DeduplicatedCandidates",
      "PruningReason","ElapsedMs")]<-list(
        if(is.null(fragment_stats))nrow(records)else fragment_stats$generated,
        if(is.null(fragment_stats))nrow(records)else fragment_stats$deduplicated,
        if(nrow(records))"candidate_filter"else"no_candidates",
        (proc.time()[["elapsed"]]-label_started)*1000)
    logger("trace","content","candidates",sprintf("%s: generated %d bounded candidates; trace: %s.",label,nrow(records),
      paste(vapply(head(records$Pattern,3L),auto_regex_safe_value,character(1),limit=80L),collapse=" | ")))
    logger("trace","content","abstract",sprintf("%s: candidate families abstracted %d positive and %d negative examples without retaining source frames.",label,length(pos),length(neg)))
    if(!nrow(records)){
      add_unresolved(label,AUTO_REGEX_FAILURE_CODES[["no_reliable_candidate"]],"no stable discriminating candidate was generated");next}
    # Candidate reliability is decided on the complete metadata table.
    #
    # Start/end anchors are positional transformations rather than new content
    # evidence.  Allow the existing evidence-backed candidate to acquire a safe
    # anchor before reliability is decided. infer_anchors() accepts an anchor
    # only when positive coverage is preserved and classification quality does
    # not decrease.

    base_inc <- unique(
      records$Pattern
    )

    base_inc <- base_inc[
      vapply(
        base_inc,
        function(pattern) {
          all(
            safe_grepl(
              pattern,
              pos
            )
          )
        },
        logical(1)
      )
    ]

    if (!length(base_inc)) {

      add_unresolved(
        label,
        AUTO_REGEX_FAILURE_CODES[["no_reliable_candidate"]],
        paste(
          "no candidate matches every",
          "applicable positive"
        )
      )

      next
    }

    # Preserve the candidate family when its representation is changed only by
    # a justified positional anchor. This keeps evidence qualification exactly
    # the same as for the underlying unanchored candidate.
    inc_records <- records[
      match(
        base_inc,
        records$Pattern
      ),
      c(
        "Pattern",
        "Family"
      ),
      drop = FALSE
    ]

    anchored_records <- inc_records

    anchored_records$Pattern <- vapply(
      inc_records$Pattern,
      function(pattern) {

        infer_anchors(
          pattern,
          measured = pos,
          opposing = neg
        )

      },
      character(1)
    )

    # Be defensive even though infer_anchors() itself requires unchanged
    # positive coverage.
    anchored_records <- anchored_records[
      nzchar(
        anchored_records$Pattern
      ) &
        vapply(
          anchored_records$Pattern,
          function(pattern) {

            all(
              safe_grepl(
                pattern,
                pos
              )
            )

          },
          logical(1)
        ),
      ,
      drop = FALSE
    ]

    inc_records <- unique(
      rbind(
        inc_records,
        anchored_records
      )
    )

    inc <- unique(
      inc_records$Pattern
    )

    exc_raw <- records$Pattern[
      vapply(
        records$Pattern,
        function(pattern) {

          any(
            safe_grepl(
              pattern,
              neg
            )
          ) &&
            !any(
              safe_grepl(
                pattern,
                pos
              )
            )

        },
        logical(1)
      )
    ]

    exc <- unique(
      exc_raw
    )

    family_for <- function(pattern) {

      matched <- match(
        pattern,
        inc_records$Pattern
      )

      if (is.na(matched)) {
        return("concrete")
      }

      chr(
        inc_records$Family[[matched]]
      )
    }
    score_cache<-new.env(hash=TRUE,parent=emptyenv())
    cached_score<-function(include,exclude,constraints) {
      key<-paste(include,exclude,constraints,sep="\034")
      if(exists(key,score_cache,inherits=FALSE))return(get(key,score_cache,inherits=FALSE))
      at<-proc.time()[["elapsed"]];value<-score_pattern(include,x,truth,exclude,constraint_count=constraints)
      scoring_ms<<-scoring_ms+(proc.time()[["elapsed"]]-at)*1000;assign(key,value,score_cache);value
    }
    # Pre-compute include-only scores.  When any include alone achieves FP=0
    # (no false positives), the expensive include×exclude cross-product is
    # unnecessary because the include-only candidate is already reliable.
    inc_only_scores<-do.call(rbind,lapply(inc,function(p)cached_score(p,"",1L)))
    has_perfect_include<-any(inc_only_scores$TP==length(pos)&inc_only_scores$FP==0L&inc_only_scores$FN==0L)
    if(has_perfect_include) {
      sets<-lapply(inc,function(i)list(include=i,exclude=""))
    } else {
      # Limit cross-product: only top-K includes (by ascending FP) participate
      # in the expensive include×exclude expansion.  Include-only candidates are
      # still evaluated for all patterns.
      inc_fp<-inc_only_scores$FP; top_k<-min(length(inc),8L)
      inc_for_exc<-inc[order(inc_fp)[seq_len(top_k)]]
      sets<-c(lapply(inc,function(i)list(include=i,exclude="")),
        unlist(lapply(inc_for_exc,function(i)lapply(exc,function(e)list(include=i,exclude=e))),recursive=FALSE))
    }
    # Stable, cross-row includes and exclusion-free rules are evaluated first.
    set_order<-order(vapply(sets,function(z)nzchar(z$exclude),logical(1)),
      vapply(sets,function(z)nchar(z$include)+nchar(z$exclude),integer(1)),
      vapply(sets,`[[`,character(1),"include"),vapply(sets,`[[`,character(1),"exclude"),method="radix")
    sets<-sets[set_order]
    performance[performance$Content==label,"IncludeExcludeCombinations"]<-length(sets)
    evaluated<-do.call(rbind,lapply(sets,function(z)cbind(data.frame(Include=z$include,Exclude=z$exclude,
      Family=family_for(z$include),BaseConstraints=1L+nzchar(z$exclude),stringsAsFactors=FALSE),
      cached_score(z$include,z$exclude,1L+nzchar(z$exclude)))))
    performance[performance$Content==label,c("ScoringMs","PruningReason","ElapsedMs")]<-
      list(scoring_ms,"replay_gate",(proc.time()[["elapsed"]]-label_started)*1000)
    logger("trace","content","score",sprintf("%s: scored %d include/exclude combinations; top bounded evidence FP/FN: %s.",label,nrow(evaluated),
      paste(head(paste0("#",seq_len(nrow(evaluated)),"=",evaluated$FP,"/",evaluated$FN),3L),collapse=", ")))
    replay_at<-proc.time()[["elapsed"]]
    # Only replay candidates that could be reliable (perfect score).
    # Non-reliable candidates (FP>0 or FN>0) can never pass the reliable filter,
    # so verifying their serialization round-trip is wasted work.
    could_be_reliable<-evaluated$TP==length(pos)&evaluated$FN==0L&evaluated$FP==0L
    replay_ok<-rep(FALSE, nrow(evaluated))
    need_replay<-which(could_be_reliable)
    if(length(need_replay)) {
      replay_results<-vapply(need_replay,function(i) {
        include<-regex_to_miraprot_storage(regex_from_miraprot_storage(evaluated$Include[[i]],"content"),"content")
        exclude<-regex_to_miraprot_storage(regex_from_miraprot_storage(evaluated$Exclude[[i]],"content"),"content")
        replay<-score_pattern(include,x,truth,exclude,constraint_count=evaluated$BaseConstraints[[i]])
        identical(as.integer(evaluated[i,c("TP","FP","FN")]),as.integer(replay[1L,c("TP","FP","FN")]))
      },logical(1))
      replay_ok[need_replay]<-replay_results
    }
    replay_ms<-replay_ms+(proc.time()[["elapsed"]]-replay_at)*1000
    evidence <- lapply(
      evaluated$Family,
      function(family) {

        auto_regex_content_evidence(
          family,
          pos,
          allow_singleton_exact =
            length(pos) == 1L
        )
      }
    )

    evidence_pass <- vapply(
      evidence,
      `[[`,
      logical(1),
      "passes"
    )

    literal_only <- vapply(
      evidence,
      `[[`,
      logical(1),
      "literal_only"
    )

    singleton_exact <- vapply(
      evidence,
      `[[`,
      logical(1),
      "singleton_exact"
    )
    # Only compute expensive examples for candidates that could be reliable;
    # non-reliable ones are diagnostic-only and get a placeholder.
    covered_examples<-character(nrow(evaluated))
    uncovered_examples<-character(nrow(evaluated))
    diag_idx<-which(could_be_reliable)
    for(i in diag_idx) {
      hit<-safe_grepl(evaluated$Include[[i]],pos)
      if(nzchar(evaluated$Exclude[[i]]))hit<-hit&!safe_grepl(evaluated$Exclude[[i]],pos)
      covered_examples[[i]]<-auto_regex_example_list(pos[hit])
      uncovered_examples[[i]]<-auto_regex_example_list(pos[!hit])
    }
    candidate_evidence<-rbind(candidate_evidence,data.frame(Content=label,
      CandidateFamily=chr(evaluated$Family),Pattern=chr(evaluated$Include),
      SupportingSourceRows=as.integer(evaluated$TP),
      DistinctSourceNames=length(unique(pos)),CoveredExamples=covered_examples,
      UncoveredExamples=uncovered_examples,
      FalsePositiveCount=as.integer(evaluated$FP),DownstreamExactMatchCount=0L,
      ValidWithoutWholeHeaderMemorization =
        !literal_only,
      EvidenceThresholdPassed =
        evidence_pass,
      Authoritative =
        FALSE,
      EvidenceStatus =
        ifelse(
          singleton_exact,
          "singleton_exact_replay",
          ifelse(
            evidence_pass,
            "threshold_passed",
            ifelse(
              literal_only,
              "literal_only_suggestion",
              "insufficient_independent_support"
            )
          )
        ),
      stringsAsFactors=FALSE,check.names=FALSE))
    reliable<-evaluated$TP==length(pos)&evaluated$FN==0L&evaluated$FP==0L&replay_ok&evidence_pass
    if(!any(reliable)) {
      add_unresolved(label,"no_reliable_candidate","no candidate passed complete-table coverage, false-positive, conflict, and persisted-replay gates")
      next
    }
    pool<-evaluated[reliable,,drop=FALSE]
    pool$UnanchoredLength<-nchar(vapply(pool$Include,content_unanchor,character(1)),type="chars")
    pool$ExcludeLength<-nchar(pool$Exclude,type="chars")
    # Family rank is deliberately not present: classification-equivalent
    # reliable candidates are shortest-first, with stable lexical tie breaks.
    pool<-pool[order(pool$FN,pool$FP,pool$UnanchoredLength,pool$ExcludeLength,
      pool$BaseConstraints,pool$Complexity,pool$Include,pool$Exclude,method="radix"),,drop=FALSE]
    base<-pool[1L,,drop=FALSE];base_unanchored<-content_unanchor(base$Include)
    selected_evidence<-candidate_evidence$Content==label &
      candidate_evidence$Pattern==base$Include & candidate_evidence$EvidenceThresholdPassed
    if(any(selected_evidence))candidate_evidence$Authoritative[which(selected_evidence)[[1L]]]<-TRUE
    excludes<-base$Exclude[nzchar(base$Exclude)]
    refinement<-refine_pattern_search(base$Include,x,truth,base$Exclude)
    scoring_ms<-scoring_ms+unname(refinement$timings[["scoring_ms"]])
    logger("trace","content","refine",sprintf("%s: refinement scored %d candidate(s), pruned %d; stop=%s.",label,
      nrow(refinement$candidates),sum(refinement$audit$Disposition=="rejected"),refinement$stop_reason))
    selected_anchor<-infer_anchors(list(pattern=base_unanchored),pos,neg)
    flags<-content_anchor_flags(selected_anchor$pattern)
    if(length(excludes)) excludes<-vapply(excludes,infer_anchors,character(1),measured=neg,opposing=pos)
    refinement_history[[label]]<-list(candidate_anchors=list(selected_anchor$anchor_history),refinement=refinement)
    current_unanchored<-base_unanchored; requested<-max(0L,as.integer(redundancy)); effective<-0L
    append_history<-function(step,left="",right="") {
      anchored_pattern<-content_apply_anchor_flags(current_unanchored,flags)
      metric<-score_pattern(anchored_pattern,x,truth,if(length(excludes))excludes[[1L]]else"",
        constraint_count=1L+length(excludes))
      redundancy_history<<-rbind(redundancy_history,data.frame(Content=label,Step=step,
        RequestedRedundancy=requested,EffectiveRedundancy=effective,
        UnanchoredBaseRegex=base_unanchored,FinalAnchoredRegex=anchored_pattern,
        BaseLength=nchar(base_unanchored,type="chars"),FinalUnanchoredLength=nchar(current_unanchored,type="chars"),
        AddedLeftCharacter=left,AddedRightCharacter=right,TP=metric$TP,FP=metric$FP,FN=metric$FN,
        stringsAsFactors=FALSE))
    }
    append_history(0L)
    if(requested>0L)for(step in seq_len(requested)) {
      contexts<-content_context_extensions(current_unanchored,pos)
      if(!nrow(contexts))break
      proposals<-lapply(seq_len(nrow(contexts)),function(i) {
        atom<-regex_escape_literal(contexts$Character[[i]])
        # A ladder unit must add exactly one representation character too;
        # escaped metacharacters are not silently counted as one.
        if(nchar(atom,type="chars")!=1L)return(NULL)
        candidate<-if(contexts$Side[[i]]=="left")paste0(atom,current_unanchored)else paste0(current_unanchored,atom)
        anchored<-content_apply_anchor_flags(candidate,flags)
        m<-score_pattern(anchored,x,truth,if(length(excludes))excludes[[1L]]else"",constraint_count=1L+length(excludes))
        stored<-regex_to_miraprot_storage(regex_from_miraprot_storage(anchored,"content"),"content")
        replay<-score_pattern(stored,x,truth,if(length(excludes))excludes[[1L]]else"",constraint_count=1L+length(excludes))
        if(m$TP!=length(pos)||m$FP!=0L||m$FN!=0L||!identical(as.integer(m[1L,c("TP","FP","FN")]),as.integer(replay[1L,c("TP","FP","FN")])))return(NULL)
        list(pattern=candidate,side=contexts$Side[[i]],character=contexts$Character[[i]])
      })
      proposals<-Filter(Negate(is.null),proposals);if(!length(proposals))break
      o<-order(vapply(proposals,`[[`,character(1),"pattern"),vapply(proposals,`[[`,character(1),"side"),method="radix")
      chosen<-proposals[[o[[1L]]]];current_unanchored<-chosen$pattern;effective<-effective+1L
      append_history(step,if(chosen$side=="left")chosen$character else "",if(chosen$side=="right")chosen$character else "")
    }
    if(effective<requested)notes<-c(notes,sprintf("%s: requested redundancy %d; effective redundancy %d because no shared reliable one-character context remained.",label,requested,effective))
    core<-content_apply_anchor_flags(current_unanchored,flags)
    exclude<-if(length(excludes))paste0("(?:",paste(excludes,collapse="|"),")")else ""
    m<-score_pattern(core,x,truth,exclude,constraint_count=1L+length(excludes))
    # One positive is sufficient only when the persisted form replays with the
    # same complete-table TP/FP result.
    normalized_core<-regex_to_miraprot_storage(regex_from_miraprot_storage(core,"content"),"content")
    normalized_exclude<-regex_to_miraprot_storage(regex_from_miraprot_storage(exclude,"content"),"content")
    replay<-score_pattern(normalized_core,x,truth,normalized_exclude,
      constraint_count=1L+length(excludes))
    replay_ok<-identical(as.integer(m[1L,c("TP","FP","FN")]),
      as.integer(replay[1L,c("TP","FP","FN")]))
    status<-if(replay_ok).refinement_tier(replay,FALSE)else"unresolved"
    core<-normalized_core;exclude<-normalized_exclude;m<-replay
    is_reliable<-status=="reliable"
    logger("debug","content","select",sprintf("%s: %s rule; include='%s', exclude='%s' (TP=%d, FP=%d, FN=%d); reason=%s.",
      label,status,auto_regex_safe_value(core),auto_regex_safe_value(exclude),m$TP,m$FP,m$FN,
      if(is_reliable)"quality thresholds met" else "quality thresholds not met"))
    statuses<-rbind(statuses,data.frame(Content=label,Status=status,Reliable=is_reliable,PositiveExamples=length(pos),NegativeExamples=length(neg),stringsAsFactors=FALSE))
    metrics<-rbind(metrics,cbind(data.frame(Content=label,Include=core,Exclude=exclude,stringsAsFactors=FALSE),m))
    hit<-safe_grepl(core,x);if(nzchar(exclude))hit<-hit&!safe_grepl(exclude,x)
    error_kinds<-c(rep("FP",min(3L,sum(hit&!truth))),rep("FN",min(3L,sum(!hit&truth))))
    error_rows<-c(head(row_id[hit&!truth],3L),head(row_id[!hit&truth],3L))
    if(length(error_rows))representatives<-rbind(representatives,data.frame(Content=rep(label,length(error_rows)),
      Kind=error_kinds,RowID=error_rows,stringsAsFactors=FALSE))
    if(is_reliable)rules<-rbind(rules,canonical_rule_row("content",Content=label,
      VariantId=stable_variant_ids(label),Priority=nrow(rules)+1L,Include=core,
      Exclude=exclude,Transformation=transformation[[1L]]))
    else add_unresolved(label,AUTO_REGEX_FAILURE_CODES[["no_reliable_candidate"]],"best rule did not meet reliable precision/recall requirements")
    performance[performance$Content==label,c("GeneratedCandidates","DeduplicatedCandidates",
      "IncludeExcludeCombinations","ScoringMs","ReplayMs","PruningReason","ElapsedMs")]<-
      list(if(is.null(fragment_stats))nrow(records)else fragment_stats$generated,
        if(is.null(fragment_stats))nrow(records)else fragment_stats$deduplicated,
        length(sets),scoring_ms,replay_ms,refinement$stop_reason,
        (proc.time()[["elapsed"]]-label_started)*1000)
  }
  rules<-rules[rules$Content!="Row Index"&rules$Include!="Row Index",,drop=FALSE]
  rules<-rbind(rules,canonical_rule_row("content",Content="Row Index",
    VariantId=stable_variant_ids("Row Index"),Priority=nrow(rules)+1L,
    Include="Row Index",Exclude="",Transformation=NA_character_))
  applied<-apply_content_table(df,rules)
  if(nrow(candidate_evidence)&&nrow(rules)) for(i in seq_len(nrow(rules))) {
    match_row<-candidate_evidence$Content==rules$Content[[i]] &
      candidate_evidence$Pattern==rules$Include[[i]]
    if(any(match_row))candidate_evidence$Authoritative[which(match_row)[[1L]]]<-TRUE
  }
  # Metrics must cover every expected class, including honest failures.  This
  # prevents aggregate diagnostics from silently treating omitted classes as
  # zero false negatives.
  missing_metrics<-setdiff(labels,metrics$Content)
  for(label in missing_metrics) {
    truth<-y==label;pattern<-if(identical(label,"Row Index"))"Row Index"else"(?!)"
    m<-score_pattern(pattern,x,truth)
    metrics<-rbind(metrics,cbind(data.frame(Content=label,
      Include=if(identical(label,"Row Index"))pattern else "",Exclude="",
      stringsAsFactors=FALSE),m))
  }
  pairwise<-data.frame()
  if(nrow(rules)>1L){hits<-sapply(seq_len(nrow(rules)),function(i)safe_grepl(rules$Include[i],x)&(!nzchar(rules$Exclude[i])|!safe_grepl(rules$Exclude[i],x)))
    for(i in seq_len(nrow(rules)-1L))for(j in (i+1L):nrow(rules)){rows<-which(hits[,i]&hits[,j]);if(length(rows))pairwise<-rbind(pairwise,data.frame(EarlierRule=i,LaterRule=j,EarlierContent=rules$Content[i],LaterContent=rules$Content[j],Rows=paste(rows,collapse=","),Count=length(rows),Winner=rules$Content[j],stringsAsFactors=FALSE))}}
  conflicts<-list(pairwise=pairwise,rows=applied$conflicts,application=applied$rows)
  final_summary<-content_assignment_summary(applied$rows,rules)
  logger("debug","content","conflicts",sprintf("Content application found %d overlapping row(s) across %d rule pair(s).",nrow(applied$conflicts),nrow(pairwise)))
  logger("trace","content","infer",sprintf("Content inference summary: %d rules; assigned correctly=%d, false positives=%d, false negatives=%d, unresolved rows=%d, conflicts=%d, labels with no selected rule=%d.",
    nrow(rules),final_summary$assigned_correctly,final_summary$false_positives,
    final_summary$false_negatives,final_summary$unresolved_rows,final_summary$conflicts,
    final_summary$labels_with_no_selected_rule),details=list(elapsed_ms=(proc.time()[["elapsed"]]-started)*1000))
  if(nrow(statuses)) statuses$FailureCode<-vapply(statuses$Content,function(label) {
    z<-unresolved$Code[unresolved$Content==label];if(length(z))z[[1L]]else""
  },character(1))
  list(table=rules,status=statuses,metrics=metrics,refinement_history=refinement_history,
    redundancy_history=redundancy_history,conflicts=conflicts,unresolved_reasons=unresolved,
    representative_errors=representatives,summary=final_summary,warnings=unique(notes),
    candidate_evidence=candidate_evidence,performance=performance,
    failure_codes=unique(chr(unresolved$Code[nzchar(chr(unresolved$Code))])),
    # These immutable pass-one values avoid reconstructing the full evidence
    # vectors during the bounded semantic pass.
    cache=list(columns=x,labels=y,tokenizations=token_cache,scores=metrics))
}
