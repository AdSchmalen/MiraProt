infer_conditions <- function(df,target,logger=function(...) invisible(NULL)) {
  started<-proc.time()[["elapsed"]]
  span_token_cache<-new.env(parent=emptyenv())
  old_span_token_cache<-getOption("miraprot.condition_span_token_cache",NULL)
  options(miraprot.condition_span_token_cache=span_token_cache)
  on.exit(options(miraprot.condition_span_token_cache=old_span_token_cache),add=TRUE)
  logger("debug", "condition", "infer", sprintf("Inferring condition rules from %d rows.", nrow(df)),
         details=list(examples=head(chr(df$Column), MIRAPROT_REPRESENTATIVE_EXAMPLE_LIMIT)))
  out<-empty_condition(); diagnostic_blocks<-list(); warnings<-character()
  boundary_diagnostic_blocks<-list()
  sample_labels <- unique(chr(df$Content)[nzchar(chr(df$Content)) &
    is_sample_bearing_content(df$Content)])
  if(!nzchar(target)||!target%in%names(df)) return(list(table=out,status=data.frame(),
    diagnostics=data.frame(),failure_codes=if(length(sample_labels))
      AUTO_REGEX_FAILURE_CODES[["partial_reference"]] else character(),
    warnings=if(length(sample_labels)) "Condition target unavailable." else character()))
  labels <- unique(chr(df$Content)[nzchar(chr(df$Content))])
  simplicity <- c(whole=1L,start=2L,end=2L,phrase_position=3L,between=4L,pattern_detect=5L)
  statuses<-data.frame(); unresolved<-data.frame(); skipped_non_sample<-character()
  # Rank the persisted boundary, rather than the source fragment from which it
  # was derived.  This rank is deliberately independent of method/anchoring.
  boundary_representation_rank <- function(before="",after="",separators="") {
    value<-paste0(chr(c(before,after,separators)),collapse="")
    if(!nzchar(value))return(0L)
    structural<-grepl("\\\\s\\+",value,perl=TRUE)
    generalized<-any(vapply(c("[[:alnum:]]+","[[:alpha:]]+","\\w","\\W","\\d","\\D",".*",".+"),
      function(atom)grepl(atom,value,fixed=TRUE),logical(1)))
    without_ws<-gsub("\\\\s\\+","",value,perl=TRUE)
    punctuation<-grepl("[[:punct:]]",without_ws,perl=TRUE)
    literal_boundary<-grepl("[[:space:]]",value,perl=TRUE)||punctuation
    if(structural&&!nzchar(without_ws))1L else if(structural&&punctuation&&!generalized)2L else if(literal_boundary&&!generalized&&!structural)3L else if(generalized)4L else 5L
  }
  escaped_boundary <- function(value) {
    shown<-encodeString(chr(value),quote='"',na.encode=FALSE)
    gsub(" ","\\\\x20",shown,fixed=TRUE)
  }
  for(label in labels) {
    label_started<-proc.time()[["elapsed"]]
    group_idx <- chr(df$Content)==label
    group_rows <- which(group_idx)
    references <- chr(df[[target]][group_idx])
    available <- nzchar(references)
    unavailable_rows <- group_rows[!available]
    if (!is_sample_bearing_content(label)) {
      note <- sprintf("Content type '%s' is not sample-bearing; condition inference was skipped", label)
      statuses<-rbind(statuses,data.frame(Content=label,SelectedMethod="",Before="",After="",Separators="",Pos=1L,Status="not_applicable",ExactMatches=0L,IncorrectNonempty=0L,EmptyResults=0L,Ambiguities=0L,ConstantOutputFailure=FALSE,UnavailableReferences=length(unavailable_rows),UnresolvedReason=note,stringsAsFactors=FALSE))
      skipped_non_sample<-c(skipped_non_sample,label)
      logger("trace","condition","select",note)
      next
    }
    if(!any(available)) {
      note <- "No reference condition is available; condition inference was skipped"
      statuses<-rbind(statuses,data.frame(Content=label,SelectedMethod="",Before="",After="",Separators="",Pos=1L,Status="not_applicable",ExactMatches=0L,IncorrectNonempty=0L,EmptyResults=0L,Ambiguities=0L,ConstantOutputFailure=FALSE,UnavailableReferences=length(unavailable_rows),UnresolvedReason="",stringsAsFactors=FALSE))
      diagnostic_blocks[[length(diagnostic_blocks)+1L]]<-data.frame(Content=rep.int(unname(label),length(unavailable_rows)),CandidateRank=rep.int(unname(NA_integer_),length(unavailable_rows)),Method=rep.int(unname(""),length(unavailable_rows)),Before=rep.int(unname(""),length(unavailable_rows)),After=rep.int(unname(""),length(unavailable_rows)),Separators=rep.int(unname(""),length(unavailable_rows)),Pos=rep.int(unname(NA_integer_),length(unavailable_rows)),Row=unavailable_rows,Column=chr(df$Column[unavailable_rows]),PredictedCondition=rep.int(unname(""),length(unavailable_rows)),ExpectedCondition=rep.int(unname(""),length(unavailable_rows)),ReferenceAvailable=rep.int(unname(FALSE),length(unavailable_rows)),EmptyExtraction=rep.int(unname(NA),length(unavailable_rows)),IncorrectNonempty=rep.int(unname(NA),length(unavailable_rows)),Ambiguous=rep.int(unname(NA),length(unavailable_rows)),ExactMatch=rep.int(unname(NA),length(unavailable_rows)),ExactMatches=rep.int(unname(NA_integer_),length(unavailable_rows)),IncorrectNonemptyResults=rep.int(unname(NA_integer_),length(unavailable_rows)),EmptyResults=rep.int(unname(NA_integer_),length(unavailable_rows)),Ambiguities=rep.int(unname(NA_integer_),length(unavailable_rows)),ConstantOutputFailure=rep.int(unname(FALSE),length(unavailable_rows)),CompleteRowAccuracy=rep.int(unname(NA_real_),length(unavailable_rows)),MethodSimplicity=rep.int(unname(NA_integer_),length(unavailable_rows)),stringsAsFactors=FALSE,row.names=NULL,check.names=FALSE)
      next
    }
    idx <- group_idx & nzchar(chr(df[[target]])); xs<-chr(df$Column[idx]); ys<-chr(df[[target]][idx])
    if(length(unavailable_rows)) warnings<-c(warnings,sprintf("%s: %d row(s) have unavailable condition references and were excluded from inference.",label,length(unavailable_rows)))
    candidates <- list();candidate_predictions<-list()
    prediction_index<-new.env(hash=TRUE,parent=emptyenv())
    prediction_key<-function(prediction) paste0(ifelse(is.na(prediction),"N",
      paste0("V",nchar(prediction,type="bytes"),":",prediction)),collapse="\035")
    generated<-0L;pruned<-0L;early_reason<-"all method tiers required"
    span_log<-list(span_width=0L,alignment_type="unavailable",ambiguity=0L,
      generated=0L,deduplicated=0L,pruned=0L,scored=0L,replayed=0L,
      limit_exhausted=FALSE)
    token_cache<-new.env(parent=emptyenv());token_cache$hits<-0L
    split_cached<-function(value,separators) {
      key<-paste0(value,"\036",separators)
      if(exists(key,envir=token_cache,inherits=FALSE)) {
        token_cache$hits<-token_cache$hits+1L
        return(get(key,envir=token_cache,inherits=FALSE))
      }
      result<-strsplit(value,separators,perl=TRUE)[[1L]]
      assign(key,result,envir=token_cache);result
    }
    add <- function(method,before="",after="",separators="",pos=1L,
                    family=method,span_width=NA_integer_,alignment_type="method",
                    ambiguity=0L,dedupe_prediction=FALSE) {
      if(!method%in%CONDITION_METHODS) stop("Condition candidate uses an unsupported method.",call.=FALSE)
      generated<<-generated+1L
      candidate<-data.frame(Method=method,Before=before,After=after,Separators=separators,
        Pos=as.integer(pos),CandidateFamily=family,SpanWidth=as.integer(span_width),
        AlignmentType=alignment_type,AlignmentAmbiguity=as.integer(ambiguity),stringsAsFactors=FALSE)
      persisted<-candidate[c("Method","Before","After","Separators","Pos")]
      key<-paste(persisted,collapse="\034")
      existing<-if(length(candidates))vapply(candidates,function(z)paste(z[c("Method","Before","After","Separators","Pos")],collapse="\034"),character(1))else character()
      if(key%in%existing){
        # Retain structural support when aligned discovery independently
        # reproduces an earlier persisted rule; this is not a new rule and
        # therefore cannot displace the current selection without evidence.
        hit<-match(key,existing)
        if(identical(family,"reference_span")) {
          candidates[[hit]]$CandidateFamily<<-family
          candidates[[hit]]$SpanWidth<<-as.integer(span_width)
          candidates[[hit]]$AlignmentType<<-alignment_type
          candidates[[hit]]$AlignmentAmbiguity<<-as.integer(ambiguity)
        }
        pruned<<-pruned+1L;return(invisible(FALSE))
      }
      # Extract once at admission.  The length-prefixed key avoids confusing
      # embedded separators (or a literal "<NA>") with vector boundaries.
      prediction<-extract_condition(xs,method,before,after,separators,pos)
      prediction_string<-prediction_key(prediction)
      if(dedupe_prediction&&exists(prediction_string,envir=prediction_index,inherits=FALSE)) {
        pruned<<-pruned+1L;return(invisible(FALSE))
      }
      candidate_number<-length(candidates)+1L
      candidates[[candidate_number]] <<- candidate
      candidate_predictions[[candidate_number]] <<- prediction
      if(!exists(prediction_string,envir=prediction_index,inherits=FALSE))
        assign(prediction_string,candidate_number,envir=prediction_index)
      invisible(TRUE)
    }
    tier_exact<-function(from) {
      if(length(candidates)<from)return(FALSE)
      any(vapply(candidate_predictions[from:length(candidate_predictions)],function(p){
        all(!is.na(p)&nzchar(p)&p==ys) && !(length(unique(p))<=1L&&length(unique(ys))>1L)},logical(1)))
    }
    add("whole");tier_start<-1L
    stop_search<-tier_exact(tier_start)
    befores<-afters<-character()
    if(!stop_search) {
      befores<-head(condition_contexts(xs,ys,"before"),CONDITION_BOUNDARY_LIMIT)
      afters<-head(condition_contexts(xs,ys,"after"),CONDITION_BOUNDARY_LIMIT)
      tier_start<-length(candidates)+1L
      for(a in afters)add("start",after=a)
      for(b in befores)add("end",before=b)
      stop_search<-tier_exact(tier_start)
      if(stop_search)early_reason<-"exact nonambiguous start/end tier established"
    } else early_reason<-"exact nonambiguous whole-string tier established"
    separators<-condition_separators(xs)
    # A reference is composite only when one of the separators actually
    # observed in the source also separates it into multiple nonempty parts.
    # Do not infer this from condition vocabulary (capitalisation, known
    # treatments, and so on): the signal is entirely structural.
    composite_reference<-length(separators)>0L && any(vapply(ys,function(y)any(vapply(separators,function(s) {
      pieces<-split_cached(y,s)
      length(pieces)>1L && sum(nzchar(pieces))>1L
    },logical(1))),logical(1)))
    if(!stop_search) {
      tier_start<-length(candidates)+1L
      for(s in separators){widths<-vapply(xs,function(x)length(split_cached(x,s)),integer(1));for(p in seq_len(max(widths)))add("phrase_position",separators=s,pos=p)}
      stop_search<-tier_exact(tier_start)
      if(stop_search)early_reason<-"exact nonambiguous phrase-position tier established"
    }
    # Translate reference-aligned lexical spans into the existing persisted
    # boundary vocabulary.  This is a distinct discovery family: it neither
    # changes extraction semantics nor adds a new method to the saved table.
    # Always construct the reference-aligned family.  An earlier method tier
    # may already be exact, but that must not prevent the aligned evidence from
    # being serialized, scored, and replayed alongside it.
    {
      aligned<-condition_stable_spans(xs,ys)
      span_diagnostics<-attr(aligned,"span_diagnostics",exact=TRUE)
      aligned_pairs<-condition_stable_boundary_pairs(xs,aligned)
      boundary_records<-attr(aligned_pairs,"boundary_diagnostics",exact=TRUE)
      if(length(boundary_records)) {
        for(record in boundary_records) {
          boundary_diagnostic_blocks[[length(boundary_diagnostic_blocks)+1L]]<-data.frame(
            Content=label,Kind=record$Kind,Side=record$Side,
            AlignedFieldDepth=record$AlignedFieldDepth,TokenType=record$TokenType,
            DetectedShape=record$DetectedShape,
            Examples=paste(record$Examples,collapse="|"),
            ObservedRuns=paste(record$ObservedRuns,collapse="|"),
            PunctuationSkeletons=paste(record$PunctuationSkeletons,collapse="|"),
            stringsAsFactors=FALSE,check.names=FALSE)
          message<-if(identical(record$Kind,"unsupported_outside_field_variation"))
            sprintf("unsupported outside-field variation: side=%s depth=%d type=%s shape=%s examples=%s",
              record$Side,record$AlignedFieldDepth,record$TokenType,record$DetectedShape,
              paste(record$Examples,collapse="|")) else
            sprintf("separator incompatibility: side=%s depth=%d observed=%s punctuation skeletons=%s",
              record$Side,record$AlignedFieldDepth,paste(record$ObservedRuns,collapse="|"),
              paste(record$PunctuationSkeletons,collapse="|"))
          logger("trace","condition","boundaries",paste0(label,": ",message),details=record)
        }
      }
      if(length(span_diagnostics)) {
        span_log[names(span_diagnostics)]<-span_diagnostics
        span_log$span_width<-span_diagnostics$largest_evidence_span_width
      }
      stable<-aligned[!aligned$Ambiguous,,drop=FALSE]
      span_log$ambiguity<-if(length(span_diagnostics))span_diagnostics$ambiguous else sum(aligned$Ambiguous)
      if(nrow(stable)) {
        span_log$span_width<-max(stable$Width)
        span_log$alignment_type<-if(length(unique(stable$Method))==1L)unique(stable$Method)else"mixed"
      }
      if(!isTRUE(span_log$limit_exhausted) && nrow(stable)==length(xs)&&length(unique(stable$Row))==length(xs)) {
        # Span candidates come only from the ranges immediately adjacent to
        # the located references.  In particular, a right delimiter has been
        # proved to begin at every reference end rather than merely occurring
        # somewhere in every source string.
        alignment_type<-if(length(unique(stable$Method))==1L)unique(stable$Method)else"mixed"
        span_width<-max(stable$Width)
        ambiguity<-sum(aligned$Ambiguous)
        tier_start<-length(candidates)+1L
        if(nrow(aligned_pairs)) {
          aligned_pairs<-head(aligned_pairs,CONDITION_BOUNDARY_PAIR_LIMIT)
          for(j in seq_len(nrow(aligned_pairs)))add(aligned_pairs$Method[j],
            before=aligned_pairs$Before[j],after=aligned_pairs$After[j],
            family="reference_span",span_width=span_width,alignment_type=alignment_type,
            ambiguity=ambiguity,dedupe_prediction=TRUE)
        }
        aligned_exact<-tier_exact(tier_start)
        stop_search<-stop_search||aligned_exact
        if(aligned_exact)early_reason<-"exact nonambiguous reference-aligned span tier established"
      }
    }
    if(!stop_search) {
      if(!length(befores))befores<-head(condition_contexts(xs,ys,"before"),CONDITION_BOUNDARY_LIMIT)
      if(!length(afters))afters<-head(condition_contexts(xs,ys,"after"),CONDITION_BOUNDARY_LIMIT)
      product<-expand.grid(b=befores,a=afters,stringsAsFactors=FALSE)
      if(nrow(product)>CONDITION_BOUNDARY_PAIR_LIMIT){pruned<-pruned+nrow(product)-CONDITION_BOUNDARY_PAIR_LIMIT;product<-head(product,CONDITION_BOUNDARY_PAIR_LIMIT)}
      tier_start<-length(candidates)+1L
      for(j in seq_len(nrow(product)))add("between",before=product$b[j],after=product$a[j])
      stop_search<-tier_exact(tier_start)
      if(stop_search)early_reason<-"exact nonambiguous bounded boundary tier established"
    }
    if(!stop_search)for(s in separators){widths<-vapply(xs,function(x)length(split_cached(x,s)),integer(1));for(p in seq_len(max(widths)))add("pattern_detect",separators=s,pos=p)}
    cand <- unique(do.call(rbind,candidates)); pruned<-pruned+length(candidates)-nrow(cand);rownames(cand)<-NULL
    # Score the representation that users will actually load, not a richer
    # in-memory object which may accidentally survive coercion differently.
    candidate_path<-tempfile(fileext=".rds");saveRDS(cand,candidate_path)
    cand<-readRDS(candidate_path);unlink(candidate_path)
    logger("trace","condition","boundaries",sprintf("%s: candidates generated=%d; pruned before scoring=%d; candidates scored=%d; early stop=%s; cache hits=%d; boundary examples: %s.",
      label,generated,pruned,nrow(cand),early_reason,token_cache$hits,paste(head(unique(c(cand$Before,cand$After,cand$Separators))[nzchar(unique(c(cand$Before,cand$After,cand$Separators)))],3L),collapse=" | ")),
      details=list(candidate_families=table(cand$CandidateFamily),generated_count=generated,
        pruned_count=pruned,cache_hits=token_cache$hits,span_width=span_log$span_width,
        alignment_type=span_log$alignment_type,ambiguity=span_log$ambiguity))
    # Replay every persisted candidate through the public application path.
    # In particular, compare against the complete Options value: a prediction
    # of "mock" is incorrect for a "mock_IFNy" row even if "mock" is a valid
    # condition on another training row.
    candidate_replay <- lapply(seq_len(nrow(cand)),function(i) {
      # Construct identity before selecting/serializing the canonical fields.
      # The old ad-hoc row omitted VariantId, so base-R subsetting raised an
      # unrelated stage exception and candidate scoring could never replay it.
      rule<-canonical_rule_row("condition",Content=label,
        VariantId=stable_variant_ids(label),Method=cand$Method[i],
        Before=cand$Before[i],After=cand$After[i],Separators=cand$Separators[i],
        Pos=as.integer(cand$Pos[i]))
      path<-tempfile(fileext=".rds");on.exit(unlink(path),add=TRUE)
      saveRDS(rule,path);stored<-readRDS(path);unlink(path)
      applied<-apply_condition_table(data.frame(Column=xs,Content=rep(label,length(xs)),
        stringsAsFactors=FALSE),stored,ys)
      prediction<-chr(applied$metadata$Options)
      stable<-identical(names(stored),CONDITION_FIELDS) &&
        all(stored$Method%in%CONDITION_METHODS) &&
        length(prediction)==length(ys) &&
        nrow(applied$diagnostics)==length(ys) &&
        all(!is.na(applied$diagnostics$ExactMatch))
      list(prediction=prediction,stable=stable)
    })
    span_log$replayed<-sum(cand$CandidateFamily=="reference_span")
    logger("trace","condition","span-search",sprintf(
      "%s: span candidates generated=%d; deduplicated pairs=%d; ambiguous=%d; pruned=%d; scored=%d; replayed=%d; largest evidence-derived span width=%d; limit exhausted=%s.",
      label,span_log$generated,span_log$deduplicated,span_log$ambiguity,
      span_log$pruned,span_log$scored,span_log$replayed,span_log$span_width,
      if(isTRUE(span_log$limit_exhausted))"yes" else "no"),details=span_log)
    predictions <- lapply(candidate_replay,`[[`,"prediction")
    replay_stable <- vapply(candidate_replay,`[[`,logical(1),"stable")
    exact_count <- vapply(predictions,function(p) sum(!is.na(p)&p==ys),integer(1))
    incorrect_count <- vapply(predictions,function(p) sum(!is.na(p)&nzchar(p)&p!=ys),integer(1))
    accuracy <- exact_count/length(ys)
    empty_count <- vapply(predictions,function(p) sum(is.na(p)|!nzchar(p)),integer(1))
    constant_failure <- vapply(predictions,function(p) length(unique(p[!is.na(p)&nzchar(p)]))<=1L && length(unique(ys))>1L,logical(1))
    ambiguity_details<-lapply(seq_along(predictions),function(i) {
      z<-cand[i,]
      if(z$Method!="between")return(data.frame(BoundaryOccurrenceAmbiguity=rep(FALSE,length(xs)),
        PairAmbiguity=rep(FALSE,length(xs)),LocatedSpanAmbiguity=rep(FALSE,length(xs))))
      details<-lapply(seq_along(xs),function(row)
        condition_between_pair_diagnostics(xs[[row]],z$Before,z$After,ys[[row]]))
      result<-as.data.frame(do.call(rbind,details))
      result[]<-lapply(result,as.logical)
      result
    })
    # Preserve the established independent-boundary calculation for legacy
    # candidates. Reference-span candidates instead follow extractor ordering
    # and must identify the aligned expected range with a unique matching pair.
    ambiguous_rows<-lapply(seq_along(predictions),function(i) {
      detail<-ambiguity_details[[i]]
      if(cand$CandidateFamily[[i]]=="reference_span")
        detail$PairAmbiguity|detail$LocatedSpanAmbiguity else
        detail$BoundaryOccurrenceAmbiguity
    })
    ambiguity_count<-vapply(ambiguous_rows,sum,integer(1))
    representation_rank<-mapply(boundary_representation_rank,cand$Before,cand$After,cand$Separators)
    complexity <- nchar(cand$Before)+nchar(cand$After)+nchar(cand$Separators)+ifelse(is.na(cand$Pos),0L,cand$Pos)
    # Reference-span candidates are structurally supported by an unambiguous
    # aligned position in every row.  This deliberately contains no literal
    # reference values: only shared row alignment/context evidence contributes.
    structural_stability<-ifelse(cand$CandidateFamily=="reference_span" &
      cand$AlignmentAmbiguity==0L & !is.na(cand$SpanWidth),0L,1L)
    # Exact, nonempty, nonambiguous replay is the mandatory leading quality
    # criterion.  Method remains ahead of boundary representation so boundary
    # preference can never change anchoring or the selected extraction method.
    replay_failure<-accuracy!=1|empty_count!=0L|ambiguity_count!=0L|constant_failure|!replay_stable
    if(composite_reference) {
      ord<-order(replay_failure,-accuracy,incorrect_count,empty_count,ambiguity_count,
        structural_stability,simplicity[cand$Method],representation_rank,complexity,
        cand$Method,cand$Before,cand$After,cand$Separators,
        ifelse(is.na(cand$Pos),0L,cand$Pos))
    } else {
      # Keep the legacy ordering byte-for-byte for non-composite references.
      ord<-order(replay_failure,-accuracy,incorrect_count,empty_count,ambiguity_count,
        simplicity[cand$Method],representation_rank,complexity,cand$Method,cand$Before,
        cand$After,cand$Separators,ifelse(is.na(cand$Pos),0L,cand$Pos))
    }
    ranks<-integer(nrow(cand)); ranks[ord]<-seq_along(ord)
    label_boundary<-if(length(boundary_diagnostic_blocks))do.call(rbind,
      boundary_diagnostic_blocks)[do.call(rbind,boundary_diagnostic_blocks)$Content==label,,drop=FALSE] else data.frame()
    diagnostic_value<-function(name)if(nrow(label_boundary))
      paste(unique(label_boundary[[name]]),collapse=";") else ""
    for(i in seq_len(nrow(cand))) {
      p<-predictions[[i]]
      mismatch<-ifelse(is.na(p)|!nzchar(p),"empty extraction",ifelse(p==ys,"",sprintf("predicted '%s' instead of '%s'",p,ys)))
      n <- length(xs)
      diagnostic_blocks[[length(diagnostic_blocks)+1L]]<-data.frame(
        Content=rep.int(unname(label),n),CandidateRank=rep.int(unname(ranks[[i]]),n),
        Method=rep.int(unname(cand$Method[[i]]),n),Before=rep.int(unname(cand$Before[[i]]),n),
        After=rep.int(unname(cand$After[[i]]),n),Separators=rep.int(unname(cand$Separators[[i]]),n),
        Pos=rep.int(unname(cand$Pos[[i]]),n),CandidateFamily=rep.int(unname(cand$CandidateFamily[[i]]),n),
        SpanWidth=rep.int(unname(cand$SpanWidth[[i]]),n),AlignmentType=rep.int(unname(cand$AlignmentType[[i]]),n),
        AlignmentAmbiguity=rep.int(unname(cand$AlignmentAmbiguity[[i]]),n),
        StructuralStability=rep.int(unname(structural_stability[[i]]),n),
        ReplayStable=rep.int(unname(replay_stable[[i]]),n),
        CompositeReference=rep.int(unname(composite_reference),n),
        GeneratedCount=rep.int(unname(generated),n),PrunedCount=rep.int(unname(pruned),n),
        SpanGeneratedCount=rep.int(unname(span_log$generated),n),
        SpanDeduplicatedCount=rep.int(unname(span_log$deduplicated),n),
        SpanAmbiguousCount=rep.int(unname(span_log$ambiguity),n),
        SpanPrunedCount=rep.int(unname(span_log$pruned),n),
        SpanScoredCount=rep.int(unname(span_log$scored),n),
        SpanReplayedCount=rep.int(unname(span_log$replayed),n),
        LargestEvidenceSpanWidth=rep.int(unname(span_log$span_width),n),
        SpanLimitExhausted=rep.int(unname(span_log$limit_exhausted),n),
        BoundaryDiagnosticKind=rep.int(unname(diagnostic_value("Kind")),n),
        BoundaryDiagnosticSide=rep.int(unname(diagnostic_value("Side")),n),
        BoundaryDiagnosticAlignedFieldDepth=rep.int(unname(diagnostic_value("AlignedFieldDepth")),n),
        BoundaryDiagnosticTokenType=rep.int(unname(diagnostic_value("TokenType")),n),
        BoundaryDiagnosticDetectedShape=rep.int(unname(diagnostic_value("DetectedShape")),n),
        BoundaryDiagnosticExamples=rep.int(unname(diagnostic_value("Examples")),n),
        BoundaryDiagnosticObservedRuns=rep.int(unname(diagnostic_value("ObservedRuns")),n),
        BoundaryDiagnosticPunctuationSkeletons=rep.int(unname(diagnostic_value("PunctuationSkeletons")),n),
        Row=which(idx),Column=xs,PredictedCondition=ifelse(is.na(p),"",p),ExpectedCondition=ys,
        ReferenceAvailable=rep.int(unname(TRUE),n),EmptyExtraction=is.na(p)|!nzchar(p),
        IncorrectNonempty=!is.na(p)&nzchar(p)&p!=ys,
        BoundaryOccurrenceAmbiguity=ambiguity_details[[i]]$BoundaryOccurrenceAmbiguity,
        PairAmbiguity=ambiguity_details[[i]]$PairAmbiguity,
        LocatedSpanAmbiguity=ambiguity_details[[i]]$LocatedSpanAmbiguity,
        Ambiguous=ambiguous_rows[[i]],ExactMatch=!is.na(p)&p==ys,
        ExactMatches=rep.int(unname(exact_count[[i]]),n),
        IncorrectNonemptyResults=rep.int(unname(incorrect_count[[i]]),n),
        EmptyResults=rep.int(unname(empty_count[[i]]),n),Ambiguities=rep.int(unname(ambiguity_count[[i]]),n),
        ConstantOutputFailure=rep.int(unname(constant_failure[[i]]),n),
        CompleteRowAccuracy=rep.int(unname(accuracy[[i]]),n),
        MethodSimplicity=rep.int(unname(simplicity[cand$Method[[i]]]),n),
        BoundaryRepresentationRank=rep.int(unname(representation_rank[[i]]),n),
        TopCandidateMethod=rep.int(unname(cand$Method[[ord[[1L]]]]),n),
        TopPredictedCondition=ifelse(is.na(predictions[[ord[[1L]]]]),"",predictions[[ord[[1L]]]]),
        ExactMismatchReason=mismatch,stringsAsFactors=FALSE,row.names=NULL,check.names=FALSE)
    }
    if(length(unavailable_rows)) diagnostic_blocks[[length(diagnostic_blocks)+1L]]<-data.frame(Content=rep.int(unname(label),length(unavailable_rows)),CandidateRank=rep.int(unname(NA_integer_),length(unavailable_rows)),Method=rep.int(unname(""),length(unavailable_rows)),Before=rep.int(unname(""),length(unavailable_rows)),After=rep.int(unname(""),length(unavailable_rows)),Separators=rep.int(unname(""),length(unavailable_rows)),Pos=rep.int(unname(NA_integer_),length(unavailable_rows)),Row=unavailable_rows,Column=chr(df$Column[unavailable_rows]),PredictedCondition=rep.int(unname(""),length(unavailable_rows)),ExpectedCondition=rep.int(unname(""),length(unavailable_rows)),ReferenceAvailable=rep.int(unname(FALSE),length(unavailable_rows)),EmptyExtraction=rep.int(unname(NA),length(unavailable_rows)),IncorrectNonempty=rep.int(unname(NA),length(unavailable_rows)),Ambiguous=rep.int(unname(NA),length(unavailable_rows)),ExactMatch=rep.int(unname(NA),length(unavailable_rows)),ExactMatches=rep.int(unname(NA_integer_),length(unavailable_rows)),IncorrectNonemptyResults=rep.int(unname(NA_integer_),length(unavailable_rows)),EmptyResults=rep.int(unname(NA_integer_),length(unavailable_rows)),Ambiguities=rep.int(unname(NA_integer_),length(unavailable_rows)),ConstantOutputFailure=rep.int(unname(FALSE),length(unavailable_rows)),CompleteRowAccuracy=rep.int(unname(NA_real_),length(unavailable_rows)),MethodSimplicity=rep.int(unname(NA_integer_),length(unavailable_rows)),stringsAsFactors=FALSE,row.names=NULL,check.names=FALSE)
    eligible<-if(isTRUE(span_log$limit_exhausted))integer() else ord[accuracy[ord]==1&empty_count[ord]==0L&ambiguity_count[ord]==0L&!constant_failure[ord]&replay_stable[ord]]
    best<-if(length(eligible))eligible[1L]else ord[1L]; reliable<-length(eligible)>0L
    logger("trace","condition","score",sprintf("%s tie-break: accuracy %.3f, simplicity %d, representation rank %d, complexity %d, rank %d; Before=%s; After=%s; Separators=%s.",
      label,accuracy[best],simplicity[cand$Method[best]],representation_rank[best],complexity[best],ranks[best],
      escaped_boundary(cand$Before[best]),escaped_boundary(cand$After[best]),escaped_boundary(cand$Separators[best])))
    logger("trace","condition","select",sprintf("%s: %s Method=%s; Before=%s; After=%s; Separators=%s; Pos=%s; representation rank=%d; exact-match count=%d/%d; reason=%s; elapsed=%.1f ms.",label,
      if(reliable)"selected" else "rejected",auto_regex_diagnostic_value(cand$Method[best]),
      auto_regex_diagnostic_value(cand$Before[best]),auto_regex_diagnostic_value(cand$After[best]),
      auto_regex_diagnostic_value(cand$Separators[best]),auto_regex_diagnostic_value(cand$Pos[best]),representation_rank[best],exact_count[best],length(ys),
      if(reliable)"exact nonambiguous extraction" else "no exact nonambiguous candidate",
      (proc.time()[["elapsed"]]-label_started)*1000),details=list(selected_method=if(reliable)cand$Method[best]else"",elapsed_ms=(proc.time()[["elapsed"]]-label_started)*1000))
    if(reliable) out<-rbind(out,canonical_rule_row("condition",Content=label,
      VariantId=stable_variant_ids(label),Method=cand$Method[best],Before=cand$Before[best],
      After=cand$After[best],Separators=cand$Separators[best],Pos=cand$Pos[best]))
    reason<-if(reliable)""else if(isTRUE(span_log$limit_exhausted))
      sprintf("reference span candidate representation limit exhausted (%d); inference abstained without selecting a partial rule",span_log$limit)
      else sprintf("best supported method matched %.1f%%; %d incorrect nonempty, %d empty, %d ambiguous",100*accuracy[best],incorrect_count[best],empty_count[best],ambiguity_count[best])
    statuses<-rbind(statuses,data.frame(Content=label,SelectedMethod=if(reliable)cand$Method[best]else "",Before=if(reliable)cand$Before[best]else "",After=if(reliable)cand$After[best]else "",Separators=if(reliable)cand$Separators[best]else "",Pos=if(reliable)cand$Pos[best]else 1L,Status=if(reliable)"reliable"else"unresolved",ExactMatches=exact_count[best],IncorrectNonempty=incorrect_count[best],EmptyResults=empty_count[best],Ambiguities=ambiguity_count[best],ConstantOutputFailure=constant_failure[best],UnavailableReferences=length(unavailable_rows),UnresolvedReason=reason,stringsAsFactors=FALSE))
    if(!reliable){failure_code<-if(isTRUE(span_log$limit_exhausted))
      AUTO_REGEX_FAILURE_CODES[["resource_limit_exhausted"]] else
      AUTO_REGEX_FAILURE_CODES[["no_reliable_candidate"]]
      unresolved<-rbind(unresolved,data.frame(Content=label,FailureCode=failure_code,
        Reason=reason,stringsAsFactors=FALSE));warnings<-c(warnings,paste(label,"condition extraction unresolved:",reason))}
    if(!reliable && length(xs)==1L) logger("trace","condition","select",sprintf(
      "%s unresolved single row: source='%s'; expected='%s'; predicted='%s'; reason=%s.",label,
      auto_regex_safe_value(xs,160L),auto_regex_safe_value(ys,120L),
      auto_regex_safe_value(predictions[[best]],120L),auto_regex_safe_value(reason,200L)))
  }
  unavailable_labels <- unique(chr(statuses$Content[statuses$Status == "not_applicable" &
    is_sample_bearing_content(statuses$Content)]))
  if(length(unavailable_labels)) {
    joined <- if(length(unavailable_labels) == 1L) unavailable_labels else paste0(
      paste(head(unavailable_labels, -1L), collapse=", "), " and ", tail(unavailable_labels, 1L))
    warnings <- c(warnings, sprintf("Condition inference unavailable for %s because %s is empty.",
      joined, target))
  }
  if(length(skipped_non_sample)) logger("debug","condition","select",sprintf(
    "Skipped %d non-sample-bearing content type(s): %s.", length(skipped_non_sample),
    paste(head(skipped_non_sample, 12L), collapse=", ")))
  if(length(diagnostic_blocks)) {
    diagnostic_names<-unique(unlist(lapply(diagnostic_blocks,names),use.names=FALSE))
    diagnostic_blocks<-lapply(diagnostic_blocks,function(block) {
      for(name in setdiff(diagnostic_names,names(block))) block[[name]]<-NA
      block[,diagnostic_names,drop=FALSE]
    })
  }
  diag<-if(length(diagnostic_blocks)) do.call(rbind,diagnostic_blocks) else data.frame()
  if(nrow(diag)) diag<-diag[order(diag$Content,diag$CandidateRank,diag$Row),,drop=FALSE]
  if(nrow(statuses)) {
    statuses$FailureCode<-ifelse(statuses$Status=="reliable","",
      ifelse(statuses$Status=="not_applicable" & statuses$UnavailableReferences>0L,
        AUTO_REGEX_FAILURE_CODES[["partial_reference"]],
        ifelse(vapply(statuses$Content,function(label) any(diag$Content==label &
          diag$SpanLimitExhausted%in%TRUE),logical(1)),
          AUTO_REGEX_FAILURE_CODES[["resource_limit_exhausted"]],
          AUTO_REGEX_FAILURE_CODES[["no_reliable_candidate"]])))
    statuses$CompletionGateReplay<-statuses$Status=="reliable"
  }
  # Completion gate: replay every emitted row through the public application
  # path.  A rule is suppressed if serialization/application changes its claim.
  if(nrow(out)) {
    replay_rows<-nzchar(chr(df[[target]])) & chr(df$Content)%in%out$Content
    path<-tempfile(fileext=".rds");on.exit(unlink(path),add=TRUE)
    saveRDS(out,path);serialized_out<-readRDS(path);unlink(path)
    replay<-apply_condition_table(data.frame(Column=chr(df$Column[replay_rows]),Content=chr(df$Content[replay_rows]),stringsAsFactors=FALSE),serialized_out,chr(df[[target]][replay_rows]))
    bad<-unique(replay$diagnostics$Content[replay$diagnostics$Content%in%out$Content&!replay$diagnostics$ExactMatch])
    if(length(bad)){out<-out[!out$Content%in%bad,,drop=FALSE];affected<-statuses$Content%in%bad;statuses$Status[affected]<-"unresolved";statuses$FailureCode[affected]<-AUTO_REGEX_FAILURE_CODES[["replay_instability"]];statuses$CompletionGateReplay[affected]<-FALSE;statuses$UnresolvedReason[affected]<-"completion-gate replay failed";unresolved<-rbind(unresolved,data.frame(Content=bad,FailureCode=AUTO_REGEX_FAILURE_CODES[["replay_instability"]],Reason="completion-gate replay failed",stringsAsFactors=FALSE));warnings<-c(warnings,paste(bad,"condition extraction unresolved: completion-gate replay failed"))}
  }
  if(nrow(diag)) {
    diag$Selected<-FALSE
    for(label in chr(out$Content)) {
      candidates<-which(diag$Content==label & !is.na(diag$CandidateRank))
      if(length(candidates)) diag$Selected[candidates[diag$CandidateRank[candidates]==min(diag$CandidateRank[candidates])]]<-TRUE
    }
  }
  logger("trace","condition","infer",sprintf("Condition inference summary: %d rules and %d unresolved labels.",nrow(out),nrow(unresolved)),
    details=list(elapsed_ms=(proc.time()[["elapsed"]]-started)*1000))
  boundary_diagnostics<-if(length(boundary_diagnostic_blocks))
    do.call(rbind,boundary_diagnostic_blocks) else data.frame()
  list(table=out,status=statuses,diagnostics=diag,boundary_diagnostics=boundary_diagnostics,
    unresolved_reasons=unresolved,
    failure_codes=unique(chr(statuses$FailureCode[nzchar(chr(statuses$FailureCode))])),
    warnings=unique(warnings))
}
