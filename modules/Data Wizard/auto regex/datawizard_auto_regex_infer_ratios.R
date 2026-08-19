# ---- ratio inference --------------------------------------------------------
infer_ratios <- function(df,logger=function(...) invisible(NULL),
                         content_rules=NULL, condition_rules=NULL) {
  started<-proc.time()[["elapsed"]]
  phase_timings<-c(prerequisite_application=0,candidate_construction=0,
    candidate_scoring=0,completion_gate_replay=0)
  token_cache<-new.env(parent=emptyenv());token_cache$hits<-0L
  old_token_cache<-getOption("miraprot.ratio_token_cache",NULL)
  options(miraprot.ratio_token_cache=token_cache)
  on.exit(options(miraprot.ratio_token_cache=old_token_cache),add=TRUE)
  logger("debug", "ratio", "infer", sprintf("Inferring ratio rules from %d rows.", nrow(df)),
         details=list(examples=head(chr(df$Column), MIRAPROT_REPRESENTATIVE_EXAMPLE_LIMIT)))
  out<-empty_ratio();diag_blocks<-list();statuses<-data.frame();warnings<-character();if(!all(c("Numerator","Denominator")%in%names(df)))return(list(table=out,status=statuses,diagnostics=data.frame(),failure_codes=AUTO_REGEX_FAILURE_CODES[["partial_reference"]],warnings="Ratio targets unavailable.",timings=phase_timings))
  valid<-nzchar(chr(df$Numerator))&nzchar(chr(df$Denominator))
  # Exclude rows whose Numerator/Denominator values are not structurally
  # encoded in the column header.  These "complete_nonrepresentable" rows
  # carry training annotations that regex/header inference cannot reproduce.
  # Including them forces every candidate rule to handle impossible cases,
  # causing the whole Content family to be rejected even when the header-
  # representable sibling family is perfectly inferrable.
  if ("Column" %in% names(df) && any(valid)) {
    cols_v <- chr(df$Column); nums_v <- chr(df$Numerator); dens_v <- chr(df$Denominator)
    representable_mask <- vapply(
      which(valid),
      function(i) {
        auto_regex_ratio_pair_header_representable(
          cols_v[[i]],
          nums_v[[i]],
          dens_v[[i]]
        )
      },
      logical(1)
    )
    valid[which(valid)[!representable_mask]] <- FALSE
  }
  partial_references<-xor(nzchar(chr(df$Numerator)),nzchar(chr(df$Denominator)))
  # Construct the same precursor state used by application.  Reference Content
  # is retained separately below only for the isolated extraction check.
  content_rules<-if(is.null(content_rules)) {
    if("Content"%in%names(df))infer_content(df)$table else empty_content()
  } else canonical_prerequisite_rules(content_rules,"content")
  condition_rules<-if(is.null(condition_rules)) {
    if("Options"%in%names(df))infer_conditions(df,"Options")$table else empty_condition()
  } else canonical_prerequisite_rules(condition_rules,"condition")
  parent_variant_for_label <- function(label) {
    ids <- unique(
      chr(
        content_rules$VariantId[
          chr(content_rules$Content) == label
        ]
      )
    )

    ids <- ids[nzchar(ids)]

    # A ratio rule can only be published when it has one unambiguous
    # parent Content variant. Variant-local grouped recovery satisfies
    # this naturally because it injects exactly one Content rule.
    if (length(ids) == 1L)
      ids[[1L]]
    else
      ""
  }
  bind_ratio_replay_context <- function(data, label, rule) {
    if (!is.data.frame(data))
      stop("Ratio replay context requires a data frame.", call. = FALSE)

    if (!is.data.frame(rule) || nrow(rule) != 1L ||
        !"VariantId" %in% names(rule))
      stop("Ratio replay context requires exactly one canonical ratio rule.",
           call. = FALSE)

    variant_id <- chr(rule$VariantId)

    if (length(variant_id) != 1L || !nzchar(variant_id[[1L]]))
      stop("Ratio replay context requires a nonblank VariantId.",
           call. = FALSE)

    out <- data

    # Isolated replay deliberately asserts that these rows belong to the
    # candidate's parent Content variant. This mirrors the state produced by
    # apply_content_table() in the complete pipeline.
    out$Content <- rep(label, nrow(out))
    attr(out, "variant_id") <- rep(variant_id[[1L]], nrow(out))

    out
  }
  prerequisite_started<-proc.time()[["elapsed"]]
  pipeline_metadata<-df
  if("Content"%in%names(df))pipeline_metadata<-apply_content_table(pipeline_metadata,content_rules)$metadata
  pipeline_metadata<-apply_condition_table(pipeline_metadata,condition_rules)$metadata
  known<-known_samples_after_conditions(pipeline_metadata)
  phase_timings[["prerequisite_application"]]<-(proc.time()[["elapsed"]]-prerequisite_started)*1000
  # Stage 1 deliberately uses the mapped reference condition values.  It asks
  # whether the persisted ratio rule itself works when its Content target is
  # correct, independently of content/condition inference.
  replay_known<-known_samples_after_conditions(df)
  blank_rule <- function(
    label,
    method,
    sep = NA_character_,
    inv = FALSE,
    nb = NA_character_,
    na = NA_character_,
    db = NA_character_,
    da = NA_character_,
    np = NA_integer_,
    dp = NA_integer_) {

    parent_variant <- parent_variant_for_label(label)

    # The fallback ID is allowed only while evaluating a candidate in
    # isolation. It must never be exported unless a parent Content rule exists.
    candidate_variant <- if (nzchar(parent_variant))
      parent_variant
    else
      stable_variant_ids(label)[[1L]]

    canonical_rule_row(
      "ratio",
      Content = label,
      VariantId = candidate_variant,
      Method = method,
      Separators = sep,
      Invert = inv,
      NumBefore = nb,
      NumAfter = na,
      DenBefore = db,
      DenAfter = da,
      NumPos = np,
      DenPos = dp
    )
  }
  for(label in sort(unique(chr(df$Content[valid])))){
    label_started<-proc.time()[["elapsed"]]
    construction_started<-proc.time()[["elapsed"]]
    idx<-valid&chr(df$Content)==label;rows<-which(idx);xs<-chr(df$Column[idx]);ns<-chr(df$Numerator[idx]);ds<-chr(df$Denominator[idx]);candidates<-list();candidate_meta<-list();generated<-0L;pruned<-0L;pruning_reasons<-character()
    prediction_keys<-character()
    note_pruning<-function(reason,count){if(count>0L){pruned<<-pruned+count;pruning_reasons<<-c(pruning_reasons,sprintf("%s (%d)",reason,count))}}
    add<-function(rule,origin="general",reserved=FALSE){
      generated<<-generated+1L
      got<-lapply(xs,ratio_extract,rule=rule,known=replay_known)
      key<-paste(rule$Method,paste(vapply(got,function(z)if(is.null(z))"<none>"else paste(z$numerator,z$denominator,sep="\035"),character(1)),collapse="\034"),sep="\036")
      if(key%in%prediction_keys){note_pruning("duplicate prediction",1L);return(invisible(FALSE))}
      prediction_keys<<-c(prediction_keys,key);candidates[[length(candidates)+1L]]<<-rule
      candidate_meta[[length(candidate_meta)+1L]]<<-list(origin=origin,reserved=isTRUE(reserved));invisible(TRUE)
    }
    observed<-condition_separators(xs);fallback<-"\\s+|\\(|\\)|\\[|\\]|\\{|\\}|\\/|_|-"
    seps<-unique(c(observed,if(length(observed))paste(unique(unlist(strsplit(observed,"|",fixed=TRUE))),collapse="|")else character(),fallback))
    seps<-seps[order(nchar(seps),seps,method="radix")]
    # Pattern Recognition is only useful with mapped samples (or the documented
    # parentheses fallback); replay below decides whether either claim is true.
    for(s in seps)for(inv in c(FALSE,TRUE))add(blank_rule(label,"Pattern Recognition",s,inv))
    # Position rules use only separator alternations which can actually be
    # persisted and replayed, and positions are one based.
    for(s in seps){toks<-lapply(xs,ratio_tokenize_exact,separators=s);width<-max(0L,lengths(toks));if(width)for(inv in c(FALSE,TRUE)){
      raw_n<-if(inv)ds else ns;raw_d<-if(inv)ns else ds
      capable<-function(target)which(vapply(seq_len(width),function(p)all(vapply(seq_along(toks),function(i)length(toks[[i]])>=p&&identical(toks[[i]][p],target[i]),logical(1))),logical(1)))
      npos<-capable(raw_n);dpos<-capable(raw_d)
      note_pruning("position pair could not replay references",width*width-length(npos)*length(dpos))
      for(np in npos)for(dp in dpos)add(blank_rule(label,"Position in String",s,inv,np=np,dp=dp))
    }}
    simple_complete <- which(vapply(candidates, function(rule) {
      d <- ratio_diagnostics(
        label, rows, xs, ns, ds, rule, replay_known
      )

      if (!all(d$Success))
        return(FALSE)

      normalized <- coerce_contract(list(
        table = empty_content(),
        condition = empty_condition(),
        ratio = rule
      ))$ratio

      direct_metadata <- bind_ratio_replay_context(
        df[idx, , drop = FALSE],
        label,
        normalized
      )

      direct <- apply_ratio_table(
        direct_metadata,
        normalized,
        ns,
        ds
      )

      all(direct$diagnostics$Success)
    }, logical(1)))
    preferred<-simple_complete[vapply(candidates[simple_complete],function(r)r$Method=="Position in String",logical(1))]
    stop_before_regex<-length(preferred)>0L
    # A structural boundary is evidence shared by every referenced header, not
    # a short fragment sampled from one particular header.  Reserve it before
    # bounded context products so dataset-specific fragments cannot consume its
    # budget.  The literal prefixes include the three merged-column families;
    # slash delimiters are deliberately generalized for optional whitespace.
    structural_boundary<-function(raw_n,raw_d){
      spans<-lapply(seq_along(xs),function(i){
        nh<-gregexpr(raw_n[i],xs[i],fixed=TRUE)[[1L]];dh<-gregexpr(raw_d[i],xs[i],fixed=TRUE)[[1L]]
        nh<-nh[nh>0L];dh<-dh[dh>0L]
        choices<-expand.grid(n=nh,d=dh)
        choices<-choices[choices$d>choices$n+nchar(raw_n[i])-1L,,drop=FALSE]
        if(nrow(choices)!=1L)return(NULL)
        n<-choices$n;d<-choices$d
        list(prefix=substr(xs[i],1L,n-1L),delimiter=substr(xs[i],n+nchar(raw_n[i]),d-1L),suffix=substr(xs[i],d+nchar(raw_d[i]),nchar(xs[i])))
      })
      if(any(vapply(spans,is.null,logical(1))))return(NULL)
      stable<-function(field){z<-vapply(spans,`[[`,character(1),field);if(length(unique(z))==1L)z[[1L]]else NA_character_}
      prefix<-stable("prefix");delimiter<-stable("delimiter");suffix<-stable("suffix")
      if(is.na(prefix)||!nzchar(prefix)||is.na(delimiter)||!nzchar(delimiter)||is.na(suffix)||nzchar(suffix))return(NULL)
      delimiter_pattern<-if(trimws(delimiter)=="/")"\\s*/\\s*"else regex_atom_for_token(delimiter)
      list(nb=regex_to_miraprot_storage(regex_atom_for_token(prefix),"ratio_boundary"),
        boundary=regex_to_miraprot_storage(delimiter_pattern,"ratio_boundary"))
    }
    if(!stop_before_regex)for(inv in c(FALSE,TRUE)){
      raw_n<-if(inv)ds else ns;raw_d<-if(inv)ns else ds;structural<-structural_boundary(raw_n,raw_d)
      if(!is.null(structural))add(blank_rule(label,"Regular Expressions",inv=inv,
        nb=structural$nb,na=structural$boundary,db=structural$boundary,da=NA_character_),
        origin="cross-row structural boundary",reserved=TRUE)
    }
    # Infer regex boundaries from the aligned context of the raw (pre-invert)
    # component.  Candidate pairs must reproduce that component on every row.
    contexts<-function(target,side){z<-NA_character_;for(i in seq_along(xs)){hits<-gregexpr(target[i],xs[i],fixed=TRUE)[[1L]];hits<-hits[hits>0L];if(length(hits)==1L){raw<-if(side=="before")substr(xs[i],1L,hits-1L)else substr(xs[i],hits+nchar(target[i]),nchar(xs[i]));if(nzchar(raw)){w<-seq_len(min(RATIO_CONTEXT_SEARCH_WIDTH,nchar(raw)));fr<-if(side=="before")substring(raw,nchar(raw)-w+1L)else substring(raw,1L,w);z<-c(z,regex_to_miraprot_storage(regex_atom_for_token(fr),"ratio_boundary"))}}};z<-unique(z);z<-z[order(nchar(ifelse(is.na(z),"",z)),ifelse(is.na(z),"",z),na.last=TRUE)];head(z,RATIO_CONTEXT_LIMIT)}
    pairs<-function(target){bs<-contexts(target,"before");as<-contexts(target,"after");allp<-unlist(lapply(bs,function(b)lapply(as,function(a)list(b,a))),recursive=FALSE);if(length(allp)>RATIO_CONTEXT_PAIR_LIMIT){note_pruning("context pair limit",length(allp)-RATIO_CONTEXT_PAIR_LIMIT);allp<-head(allp,RATIO_CONTEXT_PAIR_LIMIT)};Filter(function(p){v<-mapply(ratio_between,xs,MoreArgs=list(before=p[[1]],after=p[[2]]));all(!is.na(v)&v==target)},allp)}
    if(!stop_before_regex)for(inv in c(FALSE,TRUE)){raw_n<-if(inv)ds else ns;raw_d<-if(inv)ns else ds;npairs<-pairs(raw_n);dpairs<-pairs(raw_d);product_count<-length(npairs)*length(dpairs);limit<-min(product_count,RATIO_CONTEXT_PAIR_LIMIT);if(product_count>limit)note_pruning("context product limit",product_count-limit);k<-0L;for(nq in npairs)for(dq in dpairs){k<-k+1L;if(k>limit)break;z<-ratio_normalize_boundaries(nq[[1]],nq[[2]],dq[[1]],dq[[2]]);if(!isTRUE(z$skip))add(blank_rule(label,"Regular Expressions",inv=inv,nb=nq[[1]],na=nq[[2]],db=dq[[1]],da=dq[[2]]),origin="general context product")}}
    phase_timings[["candidate_construction"]]<-phase_timings[["candidate_construction"]]+(proc.time()[["elapsed"]]-construction_started)*1000
    scoring_started<-proc.time()[["elapsed"]]
    evaluated<-lapply(seq_along(candidates),function(i){d<-ratio_diagnostics(label,rows,xs,ns,ds,candidates[[i]],replay_known);d$Candidate<-i;d$CandidateOrigin<-candidate_meta[[i]]$origin;d$PruningReason<-if(length(pruning_reasons))paste(unique(pruning_reasons),collapse="; ")else"";d$StructuralReservedBeforeLimit<-candidate_meta[[i]]$reserved;d})
    method_counts<-table(vapply(candidates,function(r)r$Method,character(1)))
    logger("trace","ratio","components",sprintf("%s: candidates generated=%d; pruned before scoring=%d; candidates scored=%d; early stop=%s; cache hits=%d; methods: %s; row IDs %s.",label,
      generated,pruned,length(candidates),if(stop_before_regex)"complete replaying Position in String tier established"else"regular-expression fallback required",token_cache$hits,paste(names(method_counts),method_counts,sep="=",collapse=", "),paste(head(rows,3L),collapse=",")))
    scores<-vapply(evaluated,function(d)sum(d$Success),integer(1));complete<-length(rows)>=1L&scores==length(rows)
    complexity<-vapply(candidates,function(r)sum(nchar(chr(unlist(r[c("Separators","NumBefore","NumAfter","DenBefore","DenAfter")]))))+sum(c(r$NumPos,r$DenPos),na.rm=TRUE),numeric(1))
    # Equal complete fits favour methods which do not depend on condition
    # context.  Pattern Recognition remains available, but carries replay risk
    # that positional and boundary rules do not.
    simplicity<-match(vapply(candidates,function(r)r$Method,character(1)),c("Position in String","Regular Expressions","Pattern Recognition"))
    stored_value<-function(r,n){z<-chr(r[[n]])[1L];if(is.na(z))"" else z}
    # Rank only by persisted values after method simplicity and complexity.  The
    # final candidate number makes otherwise identical rules stable.
    ord<-do.call(order,c(list(
      !complete,simplicity,complexity,
      vapply(candidates,stored_value,character(1),"Separators"),
      vapply(candidates,stored_value,character(1),"NumBefore"),
      vapply(candidates,stored_value,character(1),"NumAfter"),
      vapply(candidates,stored_value,character(1),"DenBefore"),
      vapply(candidates,stored_value,character(1),"DenAfter"),
      vapply(candidates,function(r)ifelse(is.na(r$NumPos),.Machine$integer.max,r$NumPos),integer(1)),
      vapply(candidates,function(r)ifelse(is.na(r$DenPos),.Machine$integer.max,r$DenPos),integer(1)),
      vapply(candidates,function(r)isTRUE(r$Invert),logical(1)),seq_along(candidates)),
      list(na.last=TRUE,method="radix")))
    rank<-integer(length(ord));rank[ord]<-seq_along(ord)
    for(i in seq_along(evaluated)){evaluated[[i]]$CandidateRank<-rank[i];evaluated[[i]]$Selected<-FALSE}
    phase_timings[["candidate_scoring"]]<-phase_timings[["candidate_scoring"]]+(proc.time()[["elapsed"]]-scoring_started)*1000
    replay_candidate <- function(i) {
      original <- candidates[[i]]

      direct_metadata <- bind_ratio_replay_context(
        df[idx, , drop = FALSE],
        label,
        original
      )

      direct_original <- tryCatch(
        apply_ratio_table(
          direct_metadata,
          original,
          ns,
          ds
        ),
        error = function(e) e
      )
      if(inherits(direct_original,"error")||!all(direct_original$diagnostics$Success))return(list(ok=FALSE,failure_code=AUTO_REGEX_FAILURE_CODES[["replay_instability"]],reason="Ratio rule direct replay failed before coercion"))
      normalized<-tryCatch(coerce_contract(list(table=empty_content(),condition=empty_condition(),ratio=original))$ratio,error=function(e)e)
      if(inherits(normalized,"error"))return(list(ok=FALSE,failure_code=AUTO_REGEX_FAILURE_CODES[["serialization_changed"]],reason=paste("method-specific fields were changed during coercion:",conditionMessage(normalized))))
      fields<-setdiff(RATIO_FIELDS,"Content")
      changed<-fields[!vapply(fields,function(n)identical(original[[n]],normalized[[n]]),logical(1))]
      if(length(changed))return(list(ok=FALSE,failure_code=AUTO_REGEX_FAILURE_CODES[["serialization_changed"]],reason=sprintf("method-specific fields were changed during coercion: %s",paste(changed,collapse=", "))))
      replay_rule<-unserialize(serialize(normalized,NULL,version=2L))
      # Stage 1: replay the normalized, serialized candidate on only the
      # reference rows for this Content.  This is the sole acceptance gate.
      direct<-tryCatch(apply_ratio_table(direct_metadata,replay_rule,ns,ds),error=function(e)e)
      if(inherits(direct,"error"))return(list(ok=FALSE,failure_code=AUTO_REGEX_FAILURE_CODES[["replay_instability"]],reason=paste("Ratio rule replay failed:",conditionMessage(direct))))
      d<-direct$diagnostics
      bad_n<-which(d$PredictedNumerator!=d$ExpectedNumerator)
      bad_d<-which(d$PredictedDenominator!=d$ExpectedDenominator)
      if(length(bad_n)){
        return(list(ok=FALSE,failure_code=AUTO_REGEX_FAILURE_CODES[["replay_instability"]],reason="Ratio rule replay failed",diagnostics=d,targeted=rep(TRUE,nrow(d))))
      }
      if(length(bad_d)){
        return(list(ok=FALSE,failure_code=AUTO_REGEX_FAILURE_CODES[["replay_instability"]],reason="Ratio rule replay failed",diagnostics=d,targeted=rep(TRUE,nrow(d))))
      }
      if(!all(d$Success))return(list(ok=FALSE,failure_code=AUTO_REGEX_FAILURE_CODES[["replay_instability"]],reason="Ratio rule replay failed",diagnostics=d,targeted=rep(TRUE,nrow(d))))

      # Stage 2: diagnose the whole pipeline without changing acceptance.
      pipeline <- tryCatch(
        apply_ratio_table(
          pipeline_metadata,
          replay_rule,
          chr(df$Numerator),
          chr(df$Denominator)
        ),
        error = function(e) e
      )

      pipeline_variants <- attr(
        pipeline_metadata,
        "variant_id",
        exact = TRUE
      )

      if (is.null(pipeline_variants)) {
        pipeline_variants <- vapply(
          chr(pipeline_metadata$Content),
          function(content)
            stable_variant_ids(content)[[1L]],
          character(1)
        )
      }

      candidate_variant <-
        chr(
          replay_rule$VariantId
        )[[1L]]

      # A ratio rule learned from header-representable rows must not emit
      # arbitrary components on nonrepresentable siblings sharing its current
      # Content/VariantId. This is a publication safety gate; grouped Content
      # recovery may subsequently split the families and publish the rule under
      # the correct narrower parent variant.
      if (!inherits(
        pipeline,
        "error"
      )) {

        variant_rows <- which(
          chr(
            pipeline_metadata$Content
          ) == label &
            chr(
              pipeline_variants
            ) == candidate_variant
        )

        variant_contract <-
          auto_regex_ratio_replay_contract(
            applied = pipeline,
            metadata = df,
            rows = variant_rows,
            variant_ids =
              pipeline_variants
          )

        if (!isTRUE(
          variant_contract$
          nonextractable_ok
        )) {

          return(
            list(
              ok = FALSE,
              failure_code =
                AUTO_REGEX_FAILURE_CODES[["no_reliable_candidate"]],
              reason = paste(
                "Ratio rule extracted components",
                "from a header-nonrepresentable",
                "sibling row sharing the same",
                "Content/VariantId; the structural",
                "families must be split before",
                "the rule can be published."
              ),
              diagnostics = d,
              targeted = rep(
                TRUE,
                nrow(d)
              )
            )
          )
        }
      }

      targeted <-
        chr(pipeline_metadata$Content[idx]) == label &
        chr(pipeline_variants[idx]) == candidate_variant
      warning<-""
      reachability<-if(all(targeted))"reached" else "not_reached"
      content_regex<-""
      reachability_reason<-"Content assignment targeted every applicable reference row."
      if(any(!targeted)) {
        selected_content <- content_rules[
          chr(content_rules$Content) == label &
            chr(content_rules$VariantId) == candidate_variant,
          ,
          drop = FALSE
        ]

        if (nrow(selected_content)) {
          selected_content <- selected_content[1L, , drop = FALSE]
          content_regex<-selected_content$Include
          missed_sources<-chr(df$Column[idx])[!targeted]
          include_miss<-!safe_grepl(selected_content$Include,missed_sources)
          excluded<-if(nzchar(selected_content$Exclude))safe_grepl(selected_content$Exclude,missed_sources)else rep(FALSE,length(missed_sources))
          detail<-if(any(include_miss))"the include regex did not match" else if(any(excluded))"the exclude regex matched" else "a later content rule reassigned the row"
          reachability_reason<-sprintf("Selected content regex '%s' missed source '%s': %s.",
            selected_content$Include,paste(missed_sources,collapse=" | "),detail)
        } else {
          reachability_reason<-sprintf("No content rule was selected for '%s'; source '%s' was therefore not assigned.",
            label,paste(chr(df$Column[idx])[!targeted],collapse=" | "))
        }
        warning<-paste("Ratio rule valid;",reachability_reason)
      } else if(original$Method=="Pattern Recognition" &&
          (inherits(pipeline,"error") || !all(pipeline$diagnostics$Success[idx]))) {
        warning<-"Ratio rule valid; Pattern Recognition context was unavailable after condition assignment"
      }
      list(ok=TRUE,failure_code="",reason="",rule=replay_rule,diagnostics=d,targeted=targeted,warning=warning,
        reachability=reachability,content_regex=content_regex,reachability_reason=reachability_reason)
    }
    complete_order<-ord[complete[ord]];best<-if(length(complete_order))complete_order[1L]else ord[1L]
    selected<-NA_integer_;gate_reason<-"";gate_code<-"";replay_failures<-character();pipeline_warning<-""
    pipeline_reachability<-"not_evaluated";selected_content_regex<-"";pipeline_reason<-"Ratio rule was not accepted for pipeline replay."
    gate_started<-proc.time()[["elapsed"]]
    for(i in complete_order){
      replay_result<-replay_candidate(i)
      if(!is.null(replay_result$diagnostics)){
        evaluated[[i]]$ReplayNumerator<-replay_result$diagnostics$PredictedNumerator
        evaluated[[i]]$ReplayDenominator<-replay_result$diagnostics$PredictedDenominator
        evaluated[[i]]$ReplaySuccess<-replay_result$diagnostics$Success
        evaluated[[i]]$ApplicableContentTargeted<-replay_result$targeted
      }
      evaluated[[i]]$ReplayFailureReason<-if(isTRUE(replay_result$ok))"" else replay_result$reason
      if(isTRUE(replay_result$ok)){selected<-i;candidates[[i]]<-replay_result$rule
        pipeline_reachability<-replay_result$reachability
        selected_content_regex<-replay_result$content_regex
        pipeline_reason<-replay_result$reachability_reason
        if(nzchar(replay_result$warning)){pipeline_warning<-replay_result$warning
          warnings<-c(warnings,sprintf("%s: %s.",label,pipeline_warning))}
        break}
      replay_failures<-c(replay_failures,sprintf("candidate %d: %s",i,replay_result$reason))
      gate_code<-replay_result$failure_code
      evaluated[[i]]$FailureReason[!nzchar(evaluated[[i]]$FailureReason)]<-replay_result$reason
    }
    phase_timings[["completion_gate_replay"]]<-phase_timings[["completion_gate_replay"]]+(proc.time()[["elapsed"]]-gate_started)*1000
    reliable<-!is.na(selected)
    if(reliable)best<-selected else if(length(replay_failures))gate_reason<-paste(replay_failures,collapse="; ")
    status<-if(reliable)"reliable"else"unresolved"
    logger("trace","ratio","score",sprintf("%s tie-break: success=%d/%d, method rank=%d, complexity=%d, candidate rank=%d.",
      label,scores[best],length(rows),simplicity[best],complexity[best],rank[best]))
    logger("debug","ratio","select",sprintf("%s: %s method '%s'; reason=%s; elapsed=%.1f ms.",label,status,candidates[[best]]$Method,
      if(reliable)"all applicable components round-tripped" else if(nzchar(gate_reason))gate_reason else "candidate did not reproduce every applicable row",
      (proc.time()[["elapsed"]]-label_started)*1000),details=list(selected_method=if(reliable)candidates[[best]]$Method else "",elapsed_ms=(proc.time()[["elapsed"]]-label_started)*1000))
    if(reliable&&length(rows)==1L)logger("trace","ratio","evidence","Neutral diagnostic: exact ratio rule was validated against one applicable row.")
    if (reliable)
      evaluated[[best]]$Selected <- TRUE

    diag_blocks <- c(diag_blocks, evaluated)

    parent_variant <- parent_variant_for_label(label)

    if (reliable && nzchar(parent_variant)) {
      selected_rule <- candidates[[best]]

      # Enforce the parent identity even if the candidate was originally
      # constructed with an isolated diagnostic VariantId.
      selected_rule$VariantId <- parent_variant
      selected_rule$RuleId <- stable_rule_ids(
        "ratio",
        parent_variant
      )[[1L]]

      out <- rbind(out, selected_rule)

    } else if (reliable) {
      # Extraction may be demonstrably valid in isolation, but downstream
      # rules are foreign-key children of Content variants. Let grouped
      # Content recovery publish it later with the correct parent.
      warnings <- c(
        warnings,
        sprintf(
          paste0(
            "%s: ratio extraction was validated on header-representable rows ",
            "but was not published because no unique parent Content/VariantId ",
            "exists yet; grouped Content recovery must bind the rule first."
          ),
          label
        )
      )

    } else {
      warnings <- c(
        warnings,
        if (nzchar(gate_reason))
          sprintf(
            "%s ratio extraction unresolved: %s.",
            label,
            gate_reason
          )
        else
          sprintf(
            "%s ratio extraction %s: best candidate reproduced %d/%d applicable rows.",
            label,
            status,
            scores[best],
            length(rows)
          )
      )
    }
    statuses<-rbind(statuses,data.frame(Content=label,SelectedMethod=if(reliable)candidates[[best]]$Method else "",
      Status=status,RatioRuleStatus=status,PipelineReachabilityStatus=pipeline_reachability,
      SelectedContentRegex=selected_content_regex,PipelineReachabilityReason=pipeline_reason,
      SuccessfulRows=scores[best],ApplicableRows=length(rows),
      FailureCode=if(reliable)""else if(nzchar(gate_code))gate_code else AUTO_REGEX_FAILURE_CODES[["no_reliable_candidate"]],
      CompletionGateReplay=reliable,
      Reason=if(reliable)pipeline_warning else tail(warnings,1L),stringsAsFactors=FALSE))
  }
  diag<-if(length(diag_blocks))do.call(rbind,diag_blocks)else data.frame()
  if(nrow(diag))diag<-diag[order(diag$Content,diag$CandidateRank,diag$Row),,drop=FALSE]
  logger("trace","ratio","infer",sprintf("Ratio inference summary: %d rules and %d non-reliable labels.",nrow(out),sum(statuses$Status!="reliable")),
    details=list(elapsed_ms=(proc.time()[["elapsed"]]-started)*1000))
  failure_codes<-unique(c(chr(statuses$FailureCode[nzchar(chr(statuses$FailureCode))]),
    if(any(partial_references))AUTO_REGEX_FAILURE_CODES[["partial_reference"]]else character()))
  list(table=out,status=statuses,diagnostics=diag,failure_codes=failure_codes,
    warnings=unique(warnings),timings=phase_timings)
}
