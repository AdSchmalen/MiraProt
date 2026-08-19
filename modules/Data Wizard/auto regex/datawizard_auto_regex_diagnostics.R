# Auto Regex diagnostic and evidence helpers. Loaded by the legacy logic entrypoint.

AUTO_REGEX_FAILURE_CODES <- c(
  no_reliable_candidate="no_reliable_candidate",
  invalid_regex="invalid_regex",
  serialization_changed="serialization_changed",
  replay_instability="replay_instability",
  resource_limit_exhausted="resource_limit_exhausted",
  partial_reference="partial_reference",
  conflicting_labels="conflicting_labels",
  transformation_conflict="transformation_conflict"
)
auto_regex_failure_code <- function(code="") {
  code <- chr(code)[1L]
  if(is.na(code)||!nzchar(code)) return("")
  if(!code %in% unname(AUTO_REGEX_FAILURE_CODES))
    stop(sprintf("Unknown Auto Regex failure code: %s",code),call.=FALSE)
  code
}
auto_regex_stage_codes <- function(stage,label) {
  values<-character()
  has_scoped_codes<-FALSE
  for(part in c("status","unresolved_reasons")) {
    z<-stage[[part]]
    if(!is.null(z)&&nrow(z)&&"FailureCode"%in%names(z)) {
      has_scoped_codes<-has_scoped_codes||"Content"%in%names(z)
      take<-if("Content"%in%names(z)) chr(z$Content)==label else rep(TRUE,nrow(z))
      values<-c(values,chr(z$FailureCode[take]))
    }
    if(!is.null(z)&&nrow(z)&&"Code"%in%names(z)) {
      has_scoped_codes<-has_scoped_codes||"Content"%in%names(z)
      take<-if("Content"%in%names(z)) chr(z$Content)==label else rep(TRUE,nrow(z))
      values<-c(values,chr(z$Code[take]))
    }
  }
  if(!has_scoped_codes&&!is.null(stage$failure_codes))
    values<-c(values,chr(stage$failure_codes))
  unique(values[nzchar(values)&values%in%unname(AUTO_REGEX_FAILURE_CODES)])
}
auto_regex_debug_logger <- function(debug_log = NULL) {
  if (is.null(debug_log)) return(function(...) invisible(NULL))
  if (!is.function(debug_log)) stop("debug_log must be NULL or a function(message, level).", call. = FALSE)
  function(level = "info", component = NULL, step = NULL, message = "", ...) {
    numeric_level <- switch(as.character(level)[1L],
      trace = 2L, debug = 1L, info = 1L, warning = 1L, error = 1L, 1L)
    debug_log(as.character(message)[1L], numeric_level)
    invisible(NULL)
  }
}
auto_regex_safe_value <- function(value, limit = 120L) {
  value <- if (!length(value) || is.na(value[[1L]])) "<NA>" else as.character(value[[1L]])
  value <- gsub("[\r\n\t]+", " ", value)
  if (nchar(value, type="chars") > limit) paste0(substr(value, 1L, limit - 1L), "…") else value
}
auto_regex_failure_log_message <- function(diagnostic, recovery, detail = "") {
  flag <- function(value) if(isTRUE(value)) "true" else "false"
  vetoes <- chr(diagnostic$ActiveVetoes)
  if(!length(vetoes) || !nzchar(vetoes[[1L]])) vetoes <- "none"
  sprintf(paste0(
    "Content=%s; StructuralFamilyCount=%d; ContentRuleFailure=%s; ",
    "ConditionRuleFailure=%s; RatioRuleFailure=%s; ",
    "RatioReferences=complete:%d,missing:%d,partial:%d; ",
    "SafeSubsetEvidence=%s; ActiveVetoes=%s; PartitionRecovery=%s%s."),
    diagnostic$Content, diagnostic$StructuralFamilyCount,
    flag(diagnostic$ContentRuleFailure), flag(diagnostic$ConditionRuleFailure),
    flag(diagnostic$RatioRuleFailure), diagnostic$RatioCompleteFamilyCount,
    diagnostic$RatioAbsentFamilyCount, diagnostic$RatioPartialFamilyCount,
    flag(diagnostic$SafeSubsetEvidence), vetoes, recovery,
    if(nzchar(detail)) paste0(" (", detail, ")") else "")
}
auto_regex_family_signature <- function(metadata, rows=seq_len(nrow(metadata))) {
  technical_words <- c("merged", "ratio", "pvalue", "qvalue", "abundance",
    "adj", "adjusted", "value", "vs")
  signature_one <- function(row) {
    source <- chr(metadata$Column[[row]])
    references <- character()
    for(field in intersect(c("Numerator","Denominator","ContrastId"),names(metadata)))
      references <- c(references,chr(metadata[[field]][[row]]))
    references <- unique(references[nzchar(references)])
    ranges <- list()
    for(reference in references[order(nchar(references),decreasing=TRUE)]) {
      at <- gregexpr(reference,source,fixed=TRUE,useBytes=FALSE)[[1L]]
      at <- at[at>0L]
      if(length(at)!=1L) next
      candidate <- c(at[[1L]],at[[1L]]+nchar(reference,type="chars")-1L)
      if(length(ranges) && any(vapply(ranges,function(z)
          candidate[[1L]]<=z[[2L]] && candidate[[2L]]>=z[[1L]],logical(1)))) next
      ranges[[length(ranges)+1L]] <- candidate
    }
    masked <- source
    if(length(ranges)) for(range in ranges[order(vapply(ranges,`[[`,integer(1),1L),
        decreasing=TRUE)])
      substr(masked,range[[1L]],range[[2L]]) <- paste(rep("\uE000",
        range[[2L]]-range[[1L]]+1L),collapse="")
    z <- tokens(masked); z <- z[z$Source==1L,,drop=FALSE]
    if(!nrow(z)) return("")
    atoms <- vapply(seq_len(nrow(z)),function(i) {
      if(grepl("\uE000",z$Text[[i]],fixed=TRUE)) return("identifier")
      if(z$Type[[i]]!="identifier") return(z$Type[[i]])
      word <- z$Normalized[[i]]
      label_words <- tolower(unlist(strsplit(chr(metadata$Content[[row]]),
        "[^[:alnum:]]+",perl=TRUE)))
      if(word %in% c(technical_words,label_words)) paste0("marker:",word)
      else "identifier"
    },character(1))
    compound_delimiters <- c("underscore", "hyphen")
    merged <- character(); i <- 1L
    while(i <= length(atoms)) {
      merged <- c(merged, atoms[[i]])
      if(identical(atoms[[i]], "identifier")) {
        while(i + 2L <= length(atoms) &&
              atoms[[i + 1L]] %in% compound_delimiters &&
              identical(atoms[[i + 2L]], "identifier")) i <- i + 2L
      }
      i <- i + 1L
    }
    atoms <- merged
    paste(atoms[c(TRUE,atoms[-1L]!=atoms[-length(atoms)])],collapse="/")
  }
  vapply(rows,signature_one,character(1))
}
auto_regex_failure_diagnostic <- function(df, content, condition, ratio,
                                          condition_target = "Options") {
  labels <- unique(chr(df$Content)[nzchar(chr(df$Content))])
  row_stage <- data.frame(Row=seq_len(nrow(df)), Content=chr(df$Content),
    ContentApplicable=nzchar(chr(df$Content)), ContentFailed=FALSE,
    ConditionApplicable=FALSE, ConditionFailed=FALSE,
    RatioApplicable=FALSE, RatioFailed=FALSE, stringsAsFactors=FALSE,
    check.names=FALSE)
  numerator_present <- "Numerator" %in% names(df)
  denominator_present <- "Denominator" %in% names(df)
  numerator_filled <- if(numerator_present) nzchar(chr(df$Numerator)) else rep(FALSE,nrow(df))
  denominator_filled <- if(denominator_present) nzchar(chr(df$Denominator)) else rep(FALSE,nrow(df))
  row_stage$RatioReferenceState <- if(!numerator_present && !denominator_present) "absent" else
    ifelse(numerator_filled & denominator_filled,"complete",
      ifelse(!numerator_filled & !denominator_filled,"absent","partial"))
  content_rows <- if(!is.null(content$conflicts$application)) content$conflicts$application else data.frame()
  if(nrow(content_rows) && "Match" %in% names(content_rows))
    row_stage$ContentFailed <- row_stage$ContentApplicable & !content_rows$Match
  else if(nrow(content_rows) && all(c("ExpectedContent","AssignedContent") %in% names(content_rows)))
    row_stage$ContentFailed <- row_stage$ContentApplicable &
      chr(content_rows$ExpectedContent) != chr(content_rows$AssignedContent)
  condition_diag <- if(is.null(condition$diagnostics)) data.frame() else condition$diagnostics
  if(nrow(condition_diag) && "Row" %in% names(condition_diag)) {
    selected <- if("Selected"%in%names(condition_diag)) condition_diag$Selected%in%TRUE else
      if("CandidateRank"%in%names(condition_diag)) condition_diag$CandidateRank%in%1L else rep(TRUE,nrow(condition_diag))
    final_ok <- vapply(chr(condition_diag$Content),function(label) {
      z<-condition$status;is.null(z)||!nrow(z)||any(chr(z$Content)==label & z$Status=="reliable" &
        (!("CompletionGateReplay"%in%names(z))|z$CompletionGateReplay%in%TRUE))
    },logical(1))
    usable <- selected & !is.na(condition_diag$Row) & condition_diag$Row %in% row_stage$Row
    cd <- condition_diag[usable,,drop=FALSE]; at <- match(cd$Row,row_stage$Row)
    applicable <- if("ReferenceAvailable" %in% names(cd)) !is.na(cd$ReferenceAvailable) & cd$ReferenceAvailable else rep(TRUE,nrow(cd))
    failed <- applicable & ((!final_ok[usable]) | (if("ExactMatch" %in% names(cd)) is.na(cd$ExactMatch) | !cd$ExactMatch else TRUE))
    row_stage$ConditionApplicable[at] <- row_stage$ConditionApplicable[at] | applicable
    row_stage$ConditionFailed[at] <- row_stage$ConditionFailed[at] | failed
  }
  ratio_diag <- if(is.null(ratio$diagnostics)) data.frame() else ratio$diagnostics
  if(nrow(ratio_diag) && "Row" %in% names(ratio_diag)) {
    selected <- if("Selected"%in%names(ratio_diag)) ratio_diag$Selected%in%TRUE else
      if("CandidateRank"%in%names(ratio_diag)) ratio_diag$CandidateRank%in%1L else rep(TRUE,nrow(ratio_diag))
    final_ok <- vapply(chr(ratio_diag$Content),function(label) {
      z<-ratio$status;is.null(z)||!nrow(z)||any(chr(z$Content)==label & z$Status=="reliable" &
        (!("CompletionGateReplay"%in%names(z))|z$CompletionGateReplay%in%TRUE))
    },logical(1))
    usable <- selected & !is.na(ratio_diag$Row) & ratio_diag$Row %in% row_stage$Row
    rd <- ratio_diag[usable,,drop=FALSE]; at <- match(rd$Row,row_stage$Row)
    applicable <- if("Applicable" %in% names(rd)) !is.na(rd$Applicable) & rd$Applicable else rep(TRUE,nrow(rd))
    failed <- applicable & ((!final_ok[usable]) | (if("Success" %in% names(rd)) is.na(rd$Success) | !rd$Success else TRUE))
    row_stage$RatioApplicable[at] <- row_stage$RatioApplicable[at] | applicable
    row_stage$RatioFailed[at] <- row_stage$RatioFailed[at] | failed
  }
  unresolved <- if(is.null(content$unresolved_reasons)) data.frame() else content$unresolved_reasons
  status_reason <- function(stage,label) {
    z <- stage$status
    if(is.null(z) || !nrow(z) || !"Content" %in% names(z)) return("")
    fields <- intersect(c("UnresolvedReason","Reason"),names(z)); if(!length(fields)) return("")
    paste(chr(z[z$Content==label,fields[[1L]],drop=TRUE]),collapse="; ")
  }
  summaries <- lapply(labels,function(label) {
    rows <- which(row_stage$Content==label)
    ur <- if(nrow(unresolved) && "Content" %in% names(unresolved))
      unresolved[unresolved$Content==label,,drop=FALSE] else data.frame()
    content_codes <-
      auto_regex_stage_codes(
        content,
        label
      )
    condition_codes <-
      auto_regex_stage_codes(
        condition,
        label
      )
    ratio_codes <-
      auto_regex_stage_codes(
        ratio,
        label
      )
    codes <- unique(
      c(
        content_codes,
        condition_codes,
        ratio_codes
      )
    )
    transformation_conflict <-
      AUTO_REGEX_FAILURE_CODES[["transformation_conflict"]] %in%
      codes
    contradictory <-
      "conflicting_labels" %in%
      codes ||
      any(
        chr(df$Column[rows]) %in%
          chr(
            df$Column[
              chr(df$Content) != label
            ]
          )
      )
    hard_replay_failure <-
      any(
        c(
          AUTO_REGEX_FAILURE_CODES[["invalid_regex"]],
          AUTO_REGEX_FAILURE_CODES[["serialization_changed"]]
        ) %in%
          codes
      ) ||
      AUTO_REGEX_FAILURE_CODES[["replay_instability"]] %in%
      c(
        content_codes,
        condition_codes
      )
    ratio_replay_instability <-
      AUTO_REGEX_FAILURE_CODES[["replay_instability"]] %in%
      ratio_codes
    invalid_replay <-
      hard_replay_failure ||
      ratio_replay_instability
    limit_exhausted <- AUTO_REGEX_FAILURE_CODES[["resource_limit_exhausted"]] %in% codes
    row_signatures <- auto_regex_family_signature(df,rows)
    family_keys <- ifelse(nzchar(row_signatures),row_signatures,paste0("<row:",rows,">"))
    family_reference_states <- vapply(split(rows,family_keys),function(family_rows) {
      auto_regex_variant_ratio_state(df[family_rows,,drop=FALSE])
    },character(1))
    ratio_complete_families <- sum(family_reference_states=="complete")
    ratio_absent_families <- sum(family_reference_states %in% c("absent","complete_nonrepresentable"))
    ratio_nonrepresentable_families <- sum(family_reference_states=="complete_nonrepresentable")
    ratio_partial_families <- sum(family_reference_states=="partial")
    ratio_reference_state <- if(ratio_partial_families) "partial" else if(
      ratio_complete_families && ratio_absent_families) "mixed_complete_absent" else if(
      ratio_complete_families) "complete" else if(
      ratio_nonrepresentable_families) "complete_nonrepresentable" else "absent"
    ratio_requested <- ratio_complete_families > 0L
    missing_target <- (!condition_target %in% names(df) && any(is_sample_bearing_content(df$Content[rows]))) ||
      any(if(nrow(condition_diag) && all(c("Content","ReferenceAvailable")%in%names(condition_diag)))
        condition_diag$Content==label & !is.na(condition_diag$ReferenceAvailable) & !condition_diag$ReferenceAvailable else FALSE) ||
      ratio_partial_families > 0L
    signatures <- unique(row_signatures)
    signatures <- signatures[nzchar(signatures)]
    safe_subset_evidence <-
      length(signatures) > 1L &&
      all(vapply(
        signatures,
        function(signature) {
          subset_rows <- rows[
            row_signatures == signature
          ]
          evidence_rows <-
            auto_regex_family_evidence_rows(
              df,
              subset_rows
            )
          selectors <-
            auto_regex_exact_family_selectors(
              df,
              subset_rows,
              evidence_rows =
                evidence_rows
            )
          nrow(selectors) > 0L
        },
        logical(1)
      ))
    content_failed <- any(row_stage$ContentFailed[rows]) ||
      (!identical(label,"Row Index") && !label %in% chr(content$table$Content))
    unexplained_positive_rows <- rows[row_stage$ContentFailed[rows]]
    positives_exactly_explained <- !length(unexplained_positive_rows) &&
      (identical(label,"Row Index") || label %in% chr(content$table$Content))
    condition_failed <- any(row_stage$ConditionFailed[rows])
    ratio_failed <- any(row_stage$RatioFailed[rows])
    content_recovery_required <-
      length(
        unexplained_positive_rows
      ) > 0L &&
      !positives_exactly_explained &&
      content_failed
    ratio_variant_split_required <-
      label %in%
      AUTO_ASSIGNED_RATIO_OPTIONS_CONTENT &&
      ratio_complete_families > 0L &&
      ratio_nonrepresentable_families > 0L
    partition_replay_veto <-
      hard_replay_failure ||
      (
        ratio_replay_instability &&
          !ratio_variant_split_required
      )
    eligible <-
      (
        content_recovery_required ||
          ratio_variant_split_required
      ) &&
      safe_subset_evidence &&
      !any(
        c(
          contradictory,
          transformation_conflict,
          partition_replay_veto,
          missing_target,
          limit_exhausted
        )
      )
    active_vetoes <- names(Filter(isTRUE,list(
      MissingOrInapplicableReferenceTargets=missing_target,
      TransformationConflict=transformation_conflict,
      ConflictingLabelsForIdenticalSource=contradictory,
      InvalidPersistedRegexOrReplayInstability=partition_replay_veto,
      ResourceLimitExhaustion=limit_exhausted)))
    data.frame(Content=label,ContentRuleFailure=content_failed,
      ConditionRuleFailure=condition_failed,RatioRuleFailure=ratio_failed,
      RatioReferenceState=ratio_reference_state,
      RatioCompleteFamilyCount=ratio_complete_families,
      RatioAbsentFamilyCount=ratio_absent_families,
      RatioNonrepresentableFamilyCount =
        ratio_nonrepresentable_families,
      RatioVariantSplitRequired =
        ratio_variant_split_required,
      RatioPartialFamilyCount=ratio_partial_families,
      RatioInferenceRequired=ratio_requested,
      FailureCodes=paste(codes,collapse=","),
      MissingOrInapplicableReferenceTargets=missing_target,
      TransformationConflict=transformation_conflict,
      ConflictingLabelsForIdenticalSource=contradictory,
      InvalidPersistedRegexOrReplayInstability=invalid_replay,
      RecoverableRatioReplayInstability =
        ratio_replay_instability &&
        ratio_variant_split_required &&
        !hard_replay_failure,
      ResourceLimitExhaustion=limit_exhausted,
      StructuralFamilyCount=length(signatures),SafeSubsetEvidence=safe_subset_evidence,
      ActiveVetoes=paste(active_vetoes,collapse=","),
      PositiveRows=paste(rows,collapse=","),
      UnexplainedPositiveRows=paste(unexplained_positive_rows,collapse=","),
      PositiveRowsExactlyExplained=positives_exactly_explained,
      GroupedFallbackEligible=eligible,
      FailedRows=paste(rows[row_stage$ContentFailed[rows] |
        row_stage$ConditionFailed[rows] | row_stage$RatioFailed[rows]],collapse=","),
      stringsAsFactors=FALSE,check.names=FALSE)
  })
  list(by_content=if(length(summaries))do.call(rbind,summaries)else data.frame(),
    by_row=row_stage)
}
AUTO_REGEX_CONTENT_EVIDENCE_MIN_ROWS <- 2L
AUTO_REGEX_CONTENT_EVIDENCE_MIN_NAMES <- 2L
auto_regex_content_evidence <- function(
    family,
    covered_names,
    allow_singleton_exact = FALSE) {
  covered_names <- chr(
    covered_names
  )
  covered_names <- covered_names[
    !is.na(covered_names) &
      nzchar(covered_names)
  ]
  rows <- length(
    covered_names
  )
  names <- length(
    unique(
      covered_names
    )
  )
  literal_only <- identical(
    chr(family)[[1L]],
    "whole_header_literal"
  )
  singleton_exact <-
    isTRUE(
      allow_singleton_exact
    ) &&
    literal_only &&
    rows == 1L &&
    names == 1L
  independent_support <-
    !literal_only &&
    rows >=
    AUTO_REGEX_CONTENT_EVIDENCE_MIN_ROWS &&
    names >=
    AUTO_REGEX_CONTENT_EVIDENCE_MIN_NAMES
  list(
    rows = rows,
    names = names,
    literal_only = literal_only,
    singleton_exact = singleton_exact,
    passes =
      singleton_exact ||
      independent_support
  )
}
auto_regex_exact_content_selector <- function(column) {
  column <- chr(column)
  if (length(column) != 1L ||
      is.na(column[[1L]]) ||
      !nzchar(column[[1L]])) {
    return("")
  }
  runtime <- paste0(
    "^",
    regex_escape_literal(
      column[[1L]]
    ),
    "$"
  )
  tryCatch(
    regex_to_miraprot_storage(
      runtime,
      "content"
    ),
    error = function(e) ""
  )
}
auto_regex_ratio_role_pattern <- function(content) {
  content <- chr(content)
  if (length(content) != 1L ||
      !content[[1L]] %in% AUTO_ASSIGNED_RATIO_OPTIONS_CONTENT) {
    return("")
  }
  words <- unlist(
    strsplit(
      content[[1L]],
      "[^[:alnum:]]+",
      perl = TRUE
    ),
    use.names = FALSE
  )
  words <- words[
    nzchar(words)
  ]
  if (!length(words))
    return("")
  escaped <- vapply(
    words,
    function(word) {
      regex_escape_literal(word)[[1L]]
    },
    character(1)
  )
  paste0(
    "(?i:",
    paste(
      escaped,
      collapse = "[^[:alnum:]]*"
    ),
    ")"
  )
}
auto_regex_ratio_role_family_signature <- function(
    metadata,
    rows) {
  rows <- sort(
    unique(
      as.integer(rows)
    )
  )
  rows <- rows[
    !is.na(rows) &
      rows >= 1L &
      rows <= nrow(metadata)
  ]
  if (!length(rows))
    return(character())
  subset <- metadata[
    rows,
    ,
    drop = FALSE
  ]
  neutral <- chr(
    subset$Column
  )
  valid <- rep(
    TRUE,
    length(rows)
  )
  for (i in seq_along(rows)) {
    pattern <-
      auto_regex_ratio_role_pattern(
        subset$Content[[i]]
      )
    if (!nzchar(pattern)) {
      valid[[i]] <- FALSE
      next
    }
    replaced <- sub(
      pattern,
      "AUTOREGEXROLE",
      neutral[[i]],
      perl = TRUE
    )
    if (identical(
      replaced,
      neutral[[i]]
    )) {
      valid[[i]] <- FALSE
      next
    }
    neutral[[i]] <- replaced
  }
  subset$Column <- neutral
  subset$Content <-
    rep(
      "Auto Regex Ratio Role",
      nrow(subset)
    )
  for (field in intersect(
    c(
      "Numerator",
      "Denominator",
      "ContrastId"
    ),
    names(subset)
  )) {
    subset[[field]] <- ""
  }
  signatures <-
    auto_regex_family_signature(
      subset,
      seq_len(
        nrow(subset)
      )
    )
  signatures[
    !valid
  ] <- ""
  signatures
}
auto_regex_family_evidence_rows <- function(
    metadata,
    family_rows) {
  family_rows <- sort(
    unique(
      as.integer(
        family_rows
      )
    )
  )
  family_rows <- family_rows[
    !is.na(family_rows) &
      family_rows >= 1L &
      family_rows <= nrow(metadata)
  ]
  if (length(family_rows) >=
      AUTO_REGEX_CONTENT_EVIDENCE_MIN_ROWS) {
    return(family_rows)
  }
  if (length(family_rows) != 1L)
    return(family_rows)
  row <- family_rows[[1L]]
  if (!chr(metadata$Content[[row]]) %in%
      AUTO_ASSIGNED_RATIO_OPTIONS_CONTENT) {
    return(family_rows)
  }
  ratio_rows <- which(
    chr(metadata$Content) %in%
      AUTO_ASSIGNED_RATIO_OPTIONS_CONTENT
  )
  if (length(ratio_rows) <
      length(
        AUTO_ASSIGNED_RATIO_OPTIONS_CONTENT
      )) {
    return(family_rows)
  }
  signatures <-
    auto_regex_ratio_role_family_signature(
      metadata,
      ratio_rows
    )
  at <- match(
    row,
    ratio_rows
  )
  if (is.na(at) ||
      !nzchar(
        signatures[[at]]
      )) {
    return(family_rows)
  }
  support <- ratio_rows[
    signatures ==
      signatures[[at]]
  ]
  supported_roles <- unique(
    chr(
      metadata$Content[
        support
      ]
    )
  )
  if (!all(
    AUTO_ASSIGNED_RATIO_OPTIONS_CONTENT %in%
    supported_roles
  )) {
    return(family_rows)
  }
  support
}
auto_regex_example_list <- function(values, limit=3L) paste(
  head(unique(chr(values)),limit),collapse=" | ")
auto_regex_selected_candidate_diagnostics <- function(diagnostics) {
  if (!is.data.frame(diagnostics) || !nrow(diagnostics))
    return(diagnostics)
  keep <- if ("Selected" %in% names(diagnostics)) {
    diagnostics$Selected %in% TRUE
  } else if ("CandidateRank" %in% names(diagnostics)) {
    diagnostics$CandidateRank %in% 1L
  } else {
    rep(TRUE, nrow(diagnostics))
  }
  diagnostics[keep, , drop = FALSE]
}
auto_regex_reference_state <- function(subset, fields, rows=seq_len(nrow(subset))) {
  if(!length(rows)) return("absent")
  present <- vapply(fields,function(field) if(field %in% names(subset))
      nzchar(chr(subset[[field]])[rows]) else rep(FALSE,length(rows)),
    logical(length(rows)))
  if(all(present)) "complete" else if(!any(present)) "absent" else "partial"
}
auto_regex_exact_family_selectors <- function(
    metadata,
    family_rows,
    limit = CANDIDATE_FRAGMENT_SEARCH_LIMIT,
    evidence_rows = family_rows) {
  empty <- data.frame(
    Pattern = character(),
    Family = character(),
    Complexity = integer(),
    RegexLength = integer(),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  if (!is.data.frame(metadata) ||
      !"Column" %in% names(metadata))
    return(empty)
  family_rows <- sort(unique(as.integer(family_rows)))
  family_rows <- family_rows[
    !is.na(family_rows) &
      family_rows >= 1L &
      family_rows <= nrow(metadata)
  ]
  evidence_rows <- sort(
    unique(
      as.integer(
        evidence_rows
      )
    )
  )
  evidence_rows <- evidence_rows[
    !is.na(evidence_rows) &
      evidence_rows >= 1L &
      evidence_rows <= nrow(metadata)
  ]
  if (length(evidence_rows) <
      AUTO_REGEX_CONTENT_EVIDENCE_MIN_ROWS) {
    return(empty)
  }
  opposing_rows <- setdiff(
    seq_len(nrow(metadata)),
    family_rows
  )
  positives <- chr(metadata$Column[family_rows])
  negatives <- chr(metadata$Column[opposing_rows])
  raw <- candidate_fragments(
    positives,
    negatives,
    limit = limit
  )
  patterns <- if (is.data.frame(raw) && "Pattern" %in% names(raw))
    chr(raw$Pattern)
  else
    chr(raw)
  if (!length(patterns))
    return(empty)
  families <- if (is.data.frame(raw) && "Family" %in% names(raw)) {
    chr(raw$Family)
  } else {
    attr(raw, "candidate_families", exact = TRUE)
  }
  if (is.null(families) ||
      length(families) != length(patterns))
    families <- rep("legacy", length(patterns))
  expanded <- list()
  for (i in seq_along(patterns)) {
    pattern <- patterns[[i]]
    if (!nzchar(pattern))
      next
    anchored <- tryCatch(
      infer_anchors(pattern, positives, negatives),
      error = function(e) pattern
    )
    alternatives <- unique(c(
      pattern,
      anchored,
      if (!endsWith(pattern, "$"))
        paste0(pattern, "$")
      else
        pattern
    ))
    for (candidate in alternatives) {
      normalized <- tryCatch(
        regex_to_miraprot_storage(
          regex_from_miraprot_storage(
            candidate,
            "content"
          ),
          "content"
        ),
        error = function(e) ""
      )
      if (!nzchar(normalized))
        next
      runtime <- regex_from_miraprot_storage(
        normalized,
        "content"
      )
      validation <- validate_pcre(runtime)
      if (!isTRUE(validation$valid))
        next
      hit <- safe_grepl(
        normalized,
        chr(metadata$Column)
      )
      if (!all(hit[family_rows]))
        next
      if (length(opposing_rows) &&
          any(hit[opposing_rows]))
        next
      evidence <- auto_regex_content_evidence(
        families[[i]],
        chr(
          metadata$Column[
            evidence_rows
          ]
        )
      )
      if (!isTRUE(evidence$passes))
        next
      expanded[[length(expanded) + 1L]] <- data.frame(
        Pattern = normalized,
        Family = families[[i]],
        Complexity = regex_complexity(runtime),
        RegexLength = nchar(normalized, type = "chars"),
        stringsAsFactors = FALSE,
        check.names = FALSE
      )
    }
  }
  if (!length(expanded))
    return(empty)
  result <- unique(do.call(rbind, expanded))
  family_rank <- match(
    result$Family,
    c(
      "structural",
      "shape",
      "partial",
      "concrete_token",
      "legacy"
    )
  )
  family_rank[is.na(family_rank)] <- 99L
  result <- result[
    order(
      family_rank,
      result$Complexity,
      result$RegexLength,
      result$Pattern,
      method = "radix"
    ),
    ,
    drop = FALSE
  ]
  rownames(result) <- NULL
  head(result, as.integer(limit))
}
auto_regex_variant_condition_state <- function(subset, condition_target) {
  sample_rows <- is_sample_bearing_content(subset$Content)
  if(!any(sample_rows) || !nzchar(condition_target)) return("absent")
  auto_regex_reference_state(subset,condition_target,which(sample_rows))
}
auto_regex_variant_authoritative_contrast <- function(subset) {
  "ContrastId" %in% names(subset) && nrow(subset) > 0L &&
    all(nzchar(chr(subset$ContrastId)))
}
auto_regex_fixed_reference_ranges <- function(source, reference) {
  source <- chr(source)
  reference <- chr(reference)
  empty <- matrix(
    integer(),
    nrow = 0L,
    ncol = 2L,
    dimnames = list(NULL, c("Start", "End"))
  )
  if (length(source) != 1L ||
      length(reference) != 1L ||
      !nzchar(source[[1L]]) ||
      !nzchar(reference[[1L]]))
    return(empty)
  at <- gregexpr(
    reference[[1L]],
    source[[1L]],
    fixed = TRUE,
    useBytes = FALSE
  )[[1L]]
  if (length(at) == 1L && at[[1L]] < 0L)
    return(empty)
  widths <- attr(at, "match.length")
  cbind(
    Start = as.integer(at),
    End = as.integer(at + widths - 1L)
  )
}
