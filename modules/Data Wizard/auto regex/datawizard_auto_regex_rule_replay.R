# ============================================================================
# Auto Regex rule replay and export validation primitives.
# Sourced into the caller environment by datawizard_auto_regex_utils.R.
# ============================================================================

apply_content_table <- function(metadata, table) {
  table <- upgrade_rule_component(table,"content")
  stopifnot(is.data.frame(metadata), "Column" %in% names(metadata))
  table <- table[order(table$Priority, seq_len(nrow(table))),,drop=FALSE]
  out <- metadata
  expected <- if ("Content" %in% names(out)) chr(out$Content) else rep("", nrow(out))
  expected_transformation <- if ("Transformation" %in% names(out))
    normalize_transformation_values(expected,out$Transformation) else vapply(expected,function(label)
      unname(infer_content_transformation(metadata,label)),character(1))
  out$Content <- rep("", nrow(out)); out$Transformation <- rep(NA_character_, nrow(out))
  hits <- matrix(FALSE, nrow(out), nrow(table))
  winning_variant <- rep("",nrow(out))
  for (i in seq_len(nrow(table))) {
    hit <- safe_grepl(table$Include[i], out$Column)
    if (!is.na(table$Exclude[i]) && nzchar(table$Exclude[i])) hit <- hit & !safe_grepl(table$Exclude[i], out$Column)
    hits[, i] <- hit
    out$Content[hit] <- table$Content[i]
    winning_variant[hit] <- table$VariantId[i]
  }
  # Transformation is header-family state: apply it only to rows for which the
  # rule's (Content, VariantId) won, never to every row sharing the label.
  for (i in seq_len(nrow(table)))
    out$Transformation[winning_variant == table$VariantId[i] & chr(out$Content) == table$Content[i]] <- table$Transformation[i]
  attr(out,"variant_id") <- winning_variant
  counts <- rowSums(hits)
  transformation_match <- mapply(function(expected,predicted)
    (is.na(expected)&&is.na(predicted)) || (!is.na(expected)&&!is.na(predicted)&&identical(expected,predicted)),
    expected_transformation,out$Transformation,USE.NAMES=FALSE)
  mismatch_reason <- ifelse(transformation_match,"",
    ifelse(is.na(expected_transformation),"Expected NA for non-transformable content.",
      ifelse(is.na(out$Transformation),"Predicted transformation is unresolved or not assigned.",
        sprintf("Expected '%s' but predicted '%s'.",expected_transformation,out$Transformation))))
  row_diagnostics <- data.frame(Row=seq_len(nrow(out)), Column=chr(out$Column), VariantId=winning_variant, Expected=expected,
    Predicted=chr(out$Content), Match=chr(out$Content)==expected,
    ExpectedTransformation=expected_transformation,PredictedTransformation=out$Transformation,
    TransformationMatch=transformation_match,TransformationFailureReason=mismatch_reason,
    Unresolved=!nzchar(chr(out$Content)), Conflict=counts>1L, MatchingRules=counts, stringsAsFactors=FALSE)
  metrics <- if (!nrow(table)) data.frame() else do.call(rbind, lapply(seq_len(nrow(table)), function(i) {
    truth <- expected == table$Content[i]; hit <- hits[, i]
    tp<-sum(hit&truth);fp<-sum(hit&!truth);fn<-sum(!hit&truth);precision<-if(tp+fp)tp/(tp+fp)else 0;recall<-if(tp+fn)tp/(tp+fn)else 0
    data.frame(Content=table$Content[i],VariantId=table$VariantId[i],Priority=table$Priority[i],Include=table$Include[i],Exclude=table$Exclude[i],FalsePositives=fp,FalseNegatives=fn,Precision=precision,Recall=recall,F1=if(precision+recall)2*precision*recall/(precision+recall)else 0,Coverage=mean(hit),stringsAsFactors=FALSE)
  }))
  list(metadata=out, metrics=metrics, rows=row_diagnostics,
       conflicts=row_diagnostics[row_diagnostics$Conflict,,drop=FALSE])
}

# Summarize the final, ordered application result rather than adding together
# per-rule scores (which double count overlaps and omit classes without rules).
content_assignment_summary <- function(rows, table) {
  stopifnot(is.data.frame(rows), all(c("Expected","Predicted","Match","Unresolved","Conflict") %in% names(rows)))
  expected <- chr(rows$Expected); predicted <- chr(rows$Predicted)
  mismatch <- predicted != expected
  expected_labels <- unique(expected[nzchar(trimws(expected))])
  selected_labels <- unique(chr(table$Content))
  no_rule <- setdiff(expected_labels, selected_labels)
  list(
    assigned_correctly=sum(nzchar(predicted) & !mismatch),
    false_positives=sum(nzchar(predicted) & mismatch),
    false_negatives=sum(nzchar(expected) & mismatch),
    unresolved_rows=sum(!nzchar(predicted)),
    conflicts=sum(rows$Conflict),
    labels_with_no_selected_rule=length(no_rule),
    labels_without_selected_rule=no_rule
  )
}

apply_condition_table <- function(metadata, table, expected=character()) {
  table <- upgrade_rule_component(table,"condition")
  stopifnot(is.data.frame(metadata))
  out <- metadata
  # Assignment rules overwrite rows they target; they do not erase an existing
  # verified assignment on unrelated content rows.
  if(!"Options"%in%names(out))out$Options <- rep("", nrow(out))
  else out$Options <- chr(out$Options)
  # Invalid assistant condition rows are ignored, and their explicitly observed
  # non-sample metadata rows cannot contribute stale values to sample lookup.
  non_sample_rows <- !is_sample_bearing_content(out$Content)
  out$Options[non_sample_rows] <- ""
  # Ratio Options is repository content-assignment state, not an inferred
  # sample.  Restore it independently of condition-rule application.
  ratio_rows <- chr(out$Content) %in% AUTO_ASSIGNED_RATIO_OPTIONS_CONTENT
  out$Options[ratio_rows] <- "Ratio"
  table <- table[is_sample_bearing_content(table$Content),,drop=FALSE]
  failures <- list(); targeted <- rep(FALSE,nrow(out))
  for (i in seq_len(nrow(table))) {
    variants <- attr(out,"variant_id",exact=TRUE); if(is.null(variants)) { variants<-vapply(chr(out$Content),function(x) stable_variant_ids(x)[1L],character(1)); attr(out,"variant_id")<-variants }
    rows <- which(chr(out$Content) == table$Content[i] & chr(variants) == table$VariantId[i])
    if (!length(rows)) next
    targeted[rows] <- TRUE
    value <- extract_condition(chr(out$Column[rows]), table$Method[i], table$Before[i], table$After[i], table$Separators[i], table$Pos[i])
    extracted <- !is.na(value) & nzchar(value)
    out$Options[rows[extracted]] <- value[extracted]
    bad <- is.na(value) | !nzchar(value)
    if (any(bad)) failures[[length(failures)+1L]] <- data.frame(Rule=i, Content=table$Content[i], Row=rows[bad], Column=chr(out$Column[rows[bad]]), FailureReason="Condition was not extracted")
  }
  expected <- if (length(expected)==nrow(out)) chr(expected) else rep("",nrow(out))
  diagnostics <- data.frame(Row=seq_len(nrow(out)),Column=chr(out$Column),Content=chr(out$Content),VariantId=chr(attr(out,"variant_id",exact=TRUE)),PredictedCondition=chr(out$Options),ExpectedCondition=expected,ExtractionFailure=targeted&!nzchar(chr(out$Options)),ExactMatch=if(length(expected))chr(out$Options)==expected else NA,stringsAsFactors=FALSE)
  list(metadata=out, diagnostics=diagnostics, failures=if(length(failures))do.call(rbind,failures)else data.frame())
}

apply_ratio_table <- function(metadata, table, expected_numerator=character(), expected_denominator=character()) {
  table <- upgrade_rule_component(table,"ratio")
  stopifnot(is.data.frame(metadata))
  out<-metadata;out$Numerator<-out$Denominator<-rep("",nrow(out)); targeted<-rep(FALSE,nrow(out))
  known<-known_samples_after_conditions(out)
  variants<-attr(out,"variant_id",exact=TRUE);if(is.null(variants))variants<-vapply(chr(out$Content),function(x) stable_variant_ids(x)[1L],character(1))
  for(i in seq_len(nrow(table))){rows<-which(chr(out$Content)==table$Content[i]&chr(variants)==table$VariantId[i]);if(!length(rows))next;targeted[rows]<-TRUE
    got<-lapply(chr(out$Column[rows]),ratio_extract,rule=table[i,,drop=FALSE],known=known)
    for(j in seq_along(rows))if(!is.null(got[[j]])){out$Numerator[rows[j]]<-ifelse(is.na(got[[j]]$numerator),"",got[[j]]$numerator);out$Denominator[rows[j]]<-ifelse(is.na(got[[j]]$denominator),"",got[[j]]$denominator)}
  }
  en<-if(length(expected_numerator)==nrow(out))chr(expected_numerator)else rep("",nrow(out));ed<-if(length(expected_denominator)==nrow(out))chr(expected_denominator)else rep("",nrow(out))
  failure<-targeted&(!nzchar(chr(out$Numerator))|!nzchar(chr(out$Denominator)))
  diagnostics<-data.frame(Row=seq_len(nrow(out)),Column=chr(out$Column),Content=chr(out$Content),VariantId=chr(variants),PredictedNumerator=chr(out$Numerator),ExpectedNumerator=en,PredictedDenominator=chr(out$Denominator),ExpectedDenominator=ed,Success=nzchar(chr(out$Numerator))&nzchar(chr(out$Denominator))&out$Numerator==en&out$Denominator==ed,ExtractionFailure=failure,FailureReason=ifelse(failure,"Ratio components were not fully extracted",ifelse(out$Numerator!=en,"Numerator mismatch",ifelse(out$Denominator!=ed,"Denominator mismatch",""))),stringsAsFactors=FALSE)
  list(metadata=out,diagnostics=diagnostics,failures=diagnostics[diagnostics$ExtractionFailure,,drop=FALSE])
}

test_rules <- function(df,rules) {
  pred<-rep("",nrow(df)); conflicts<-integer(nrow(df)); for(i in seq_len(nrow(rules$table))){hit<-safe_grepl(rules$table$Include[i],df$Column);if(!is.na(rules$table$Exclude[i])&&nzchar(rules$table$Exclude[i]))hit<-hit&!safe_grepl(rules$table$Exclude[i],df$Column);pred[hit]<-rules$table$Content[i];conflicts[hit]<-conflicts[hit]+1L}
  data.frame(Row=seq_len(nrow(df)),Column=chr(df$Column),Expected=if("Content"%in%names(df))chr(df$Content)else"",Predicted=pred,Match=if("Content"%in%names(df))pred==chr(df$Content)else NA,Conflict=conflicts>1)
}
coerce_contract <- function(rules) {
  # Coerce storage modes without globally replacing NA.  In particular, ratio
  # method-specific NAs are part of the persisted contract, not missing input.
  rules$table<-upgrade_rule_component(rules$table,"content"); rules$condition<-upgrade_rule_component(rules$condition,"condition");rules$ratio<-upgrade_rule_component(rules$ratio,"ratio")
  rules$table <- coerce_rule_component_classes(rules$table, "table")
  rules$table$Transformation <- normalize_transformation_values(
    rules$table$Content,rules$table$Transformation)
  rules$condition <- coerce_rule_component_classes(rules$condition, "condition")
  rules$ratio <- coerce_rule_component_classes(rules$ratio, "ratio")
  # UI-created condition rows use empty character values and retain Pos=1 even
  # where Pos is ignored; normalize imported spreadsheet blanks accordingly.
  for(n in c("Before","After","Separators")) rules$condition[[n]][is.na(rules$condition[[n]])] <- ""
  rules$condition$Pos[is.na(rules$condition$Pos) & !rules$condition$Method %in% c("phrase_position","pattern_detect")] <- 1L
  rules
}
data_wizard_normalize_rules <- function(rules) {
  # Both Data Wizard loaders consume these three named data frames directly.
  if (!is.list(rules) || !all(c("table", "condition", "ratio") %in% names(rules)))
    stop("Data Wizard requires table, condition, and ratio components.")
  out <- rules[c("table", "condition", "ratio")]
  if (!all(vapply(out, is.data.frame, logical(1)))) stop("Every rule component must be a data frame.")
  out$table <- upgrade_rule_component(out$table,"content")
  out$condition <- upgrade_rule_component(out$condition,"condition")
  out$ratio <- upgrade_rule_component(out$ratio,"ratio")
  coerce_contract(out)
}

build_export_template <- function(rules, exported_at=Sys.time(), logger=function(...) invisible(NULL),
                                  rules_are_normalized=FALSE) {
  started<-proc.time()[["elapsed"]]
  core <- if (isTRUE(rules_are_normalized)) rules[c("table","condition","ratio")] else data_wizard_normalize_rules(rules)
  result<-c(core, list(debug_info=list(
    exported_at=as.POSIXct(exported_at),
    export_options=list(save_ui=FALSE, include_filtering=FALSE,
      include_imputation=FALSE, include_batch_effects=FALSE,
      include_pivot=FALSE, include_merge=FALSE, include_edit_ops=FALSE,
      include_ratios=FALSE),
    components_exported="assignment_rules",
    module_versions=list(auto_assign="modular_v1", filtering="enhanced_v2",
      imputation="enhanced_v1", edit_operations="enhanced_v1",
      batch_effects="enhanced_v1", pivot="enhanced_v1", merge="enhanced_v1"),
    last_import_info=NULL, debug_level=0, processing_history=list()
  )))
  logger("info","export","construct",sprintf("RDS object constructed with %d content, %d condition, and %d ratio rules.",
    nrow(core$table),nrow(core$condition),nrow(core$ratio)),details=list(elapsed_ms=(proc.time()[["elapsed"]]-started)*1000))
  result
}

regex_complexity <- function(x) {
  x <- chr(x)
  # Bound constructs which cause branching/backtracking, rather than merely
  # counting harmless literal characters.
  lengths(regmatches(x, gregexpr("[|*+?{}()]|\\(\\?", x, perl=TRUE)))
}

validate_export <- function(rules, metadata=NULL, logger=function(...) invisible(NULL)) {
  # Transformation belongs to a header family, identified by (Content,
  # VariantId), rather than to the display label Content.  Sibling variants of
  # one Content may therefore intentionally carry different transformations;
  # replay must use the variant which won content selection.
  started<-proc.time()[["elapsed"]]
  errors <- character(); add <- function(x) errors <<- c(errors, x)
  if (!is.list(rules) || !all(c("table","condition","ratio") %in% names(rules)))
    return("Rule collection must contain table, condition, and ratio components.")
  if (!identical(names(rules)[seq_len(3L)], c("table","condition","ratio")))
    add("table, condition, and ratio must be the first components in that order.")
  core <- rules[c("table","condition","ratio")]
  prototypes <- canonical_rule_schemas()
  schemas <- lapply(prototypes, names)
  classes <- canonical_rule_classes()
  for (component in names(schemas)) {
    x <- core[[component]]
    if (!is.data.frame(x)) { add(sprintf("%s must be a data frame.", component)); next }
    if (!identical(names(x), schemas[[component]])) { add(sprintf(
      "%s fields or field order are invalid (expected %s; actual %s).",
      component, paste(schemas[[component]], collapse=", "),
      paste(names(x), collapse=", "))); next }
    actual <- vapply(x, function(z) class(z)[1L], character(1))
    invalid <- names(actual)[actual != classes[[component]]]
    for (field in invalid) add(sprintf(
      "%s column classes are invalid: field=%s; expected %s=%s; actual %s=%s.",
      component, field, field, classes[[component]][[field]], field, actual[[field]]))
  }
  if (length(errors)) return(unique(errors))
  core$table$Transformation <- normalize_transformation_values(
    core$table$Content,core$table$Transformation)
  missing_text <- function(x) is.na(x) | !nzchar(trimws(x))
  missing_pattern <- function(x) is.na(x) | !nzchar(x)
  if (any(missing_text(core$table$Content)) || any(missing_pattern(core$table$Include))) add("Content rules require nonblank Content and Include values.")
  if (any(missing_text(core$condition$Content)) || any(missing_text(core$condition$Method))) add("Condition rules require nonblank Content and Method values.")
  for (message in condition_content_validation_messages(core$condition)) add(message)
  if (any(missing_text(core$ratio$Content)) || any(missing_text(core$ratio$Method)) || any(is.na(core$ratio$Invert))) add("Ratio rules require Content, Method, and Invert values.")
  if (any(!core$condition$Method %in% CONDITION_METHODS)) add("Unsupported condition method.")
  if (any(!core$ratio$Method %in% RATIO_METHODS)) add("Unsupported ratio method.")
  content_keys <- paste(chr(core$table$Content),chr(core$table$VariantId),sep="\r")
  for (component in c("condition","ratio")) {
    foreign_keys <- paste(chr(core[[component]]$Content),chr(core[[component]]$VariantId),sep="\r")
    missing_fk <- which(!foreign_keys %in% content_keys)
    if (length(missing_fk)) for (i in missing_fk) add(sprintf(
      "%s rule '%s' has foreign-key VariantId '%s' with no matching content rule.",
      tools::toTitleCase(component),core[[component]]$Content[i],core[[component]]$VariantId[i]))
  }
  if (any(is.na(core$table$Priority)|core$table$Priority<0L) || anyDuplicated(core$table$Priority))
    add("Content rule Priority values must be unique non-negative integers.")
  row_index <- core$table$Content == "Row Index"
  if (nrow(core$table) && (sum(row_index, na.rm=TRUE) != 1L || !identical(core$table$Include[row_index], "Row Index") ||
      !identical(core$table$Exclude[row_index], "") || !is.na(core$table$Transformation[row_index])))
    add("Content rules require exactly one canonical Row Index special row.")
  transformable <- core$table$Content %in% TRANSFORMATION_CONTENT_TYPES
  invalid_transform <- transformable & (missing_text(core$table$Transformation) |
    !core$table$Transformation %in% SUPPORTED_TRANSFORMATIONS)
  if (any(invalid_transform)) for (i in which(invalid_transform)) add(sprintf(
    "Content rule '%s' variant '%s' has an invalid or ambiguous Transformation; choose exactly one of %s before export.",
    core$table$Content[i],core$table$VariantId[i],paste(SUPPORTED_TRANSFORMATIONS,collapse=", ")))
  invalid_nontransform <- !transformable & !is.na(core$table$Transformation)
  if (any(invalid_nontransform)) for(i in which(invalid_nontransform)) add(sprintf(
    "Content rule '%s' variant '%s' does not support transformations; set Transformation to NA.",
    core$table$Content[i],core$table$VariantId[i]))
  if ("debug_info" %in% names(rules)) {
    info <- rules$debug_info
    required_info <- c("exported_at","export_options","components_exported","module_versions",
      "last_import_info","debug_level","processing_history")
    if (!is.list(info) || !all(required_info %in% names(info))) add("debug_info is missing required diagnostic fields.")
    else {
      flags <- c("save_ui","include_filtering","include_imputation","include_batch_effects",
        "include_pivot","include_merge","include_edit_ops","include_ratios")
      if (!inherits(info$exported_at,"POSIXct")) add("debug_info$exported_at must be POSIXct/POSIXt.")
      if (!is.list(info$export_options) || !all(flags %in% names(info$export_options)) ||
          any(!vapply(info$export_options[flags], function(x)is.logical(x)&&length(x)==1L&&!is.na(x), logical(1))))
        add("debug_info$export_options must contain the eight scalar logical flags.")
      if (!is.character(info$components_exported) || !"assignment_rules" %in% info$components_exported) add("debug_info$components_exported must include assignment_rules.")
      if (!is.list(info$module_versions) || !is.list(info$processing_history) ||
          !is.numeric(info$debug_level) || length(info$debug_level)!=1L) add("debug_info diagnostic field classes are invalid.")
    }
  }
  cnd <- core$condition
  if (any(cnd$Method=="between" & (missing_pattern(cnd$Before)|missing_pattern(cnd$After)))) add("Condition method 'between' requires Before and After.")
  if (any(cnd$Method=="start" & missing_pattern(cnd$After))) add("Condition method 'start' requires After.")
  if (any(cnd$Method=="end" & missing_pattern(cnd$Before))) add("Condition method 'end' requires Before.")
  positional <- cnd$Method %in% c("phrase_position","pattern_detect")
  if (any(positional & (missing_pattern(cnd$Separators)|is.na(cnd$Pos)|cnd$Pos<1L))) add("Positional condition methods require Separators and a positive Pos.")
  rat <- core$ratio
  if (any(rat$Method=="Position in String" & (missing_pattern(rat$Separators)|is.na(rat$NumPos)|rat$NumPos<1L|is.na(rat$DenPos)|rat$DenPos<1L))) add("Position in String ratio rules require Separators and positive numerator/denominator positions.")
  if (any(rat$Method=="Pattern Recognition" & missing_pattern(rat$Separators))) add("Pattern Recognition ratio rules require Separators.")
  re_rows <- rat$Method=="Regular Expressions"
  if (any(re_rows & missing_pattern(rat$NumAfter) & missing_pattern(rat$DenBefore))) add("Regular Expressions ratio rules require NumAfter or DenBefore.")
  # Enforce the exact method-dependent empty representation emitted by the UI.
  if (any(rat$Method=="Regular Expressions" & (!is.na(rat$Separators)|!is.na(rat$NumPos)|!is.na(rat$DenPos)|rat$Invert))) add("Regular Expressions ratio rows require NA separators/positions and Invert FALSE.")
  non_regex <- rat$Method %in% c("Pattern Recognition","Position in String")
  if (any(non_regex & (!is.na(rat$NumBefore)|!is.na(rat$NumAfter)|!is.na(rat$DenBefore)|!is.na(rat$DenAfter)))) add("Non-regex ratio rows require NA regex boundaries.")
  if (any(rat$Method=="Pattern Recognition" & (!is.na(rat$NumPos)|!is.na(rat$DenPos)))) add("Pattern Recognition ratio rows require NA positions.")
  patterns <- c(core$table$Include,core$table$Exclude,cnd$Before,cnd$After,cnd$Separators,rat$Separators,rat$NumBefore,rat$NumAfter,rat$DenBefore,rat$DenAfter)
  patterns <- chr(patterns); used <- nzchar(patterns)
  if (any(nchar(patterns[used], type="chars") > MAX_REGEX_LENGTH)) add(sprintf("Regex values may not exceed %d characters.", MAX_REGEX_LENGTH))
  if (any(regex_complexity(patterns[used]) > MAX_REGEX_COMPLEXITY)) add(sprintf("Regex values may not exceed %d complexity constructs.", MAX_REGEX_COMPLEXITY))
  # Report every bad cell independently.  Messages contain only bounded scalar
  # fields, never a pasted rule or metadata row.
  checks <- list()
  queue <- function(component, table, fields, engine="PCRE", rows=seq_len(nrow(table))) {
    if (!length(rows)) return()
    for (i in rows) for (field in fields) checks[[length(checks)+1L]] <<- list(
      component=component,row=i,content=table$Content[i],method=if("Method"%in%names(table))table$Method[i]else"content",
      field=field,value=table[[field]][i],engine=engine)
  }
  queue("Content",core$table,c("Include","Exclude"))
  queue("Condition",cnd,c("Before","After"))
  queue("Condition",cnd,"Separators","PCRE")
  queue("Ratio",rat,c("Separators","NumBefore","NumAfter","DenBefore","DenAfter"))
  invalid_cells <- 0L
  for (check in checks) {
    value <- check$value
    if (is.na(value) || !nzchar(value)) next
    result <- validate_pcre(regex_from_miraprot_storage(value,
      if(check$field=="Separators")"separator" else if(check$component=="Content")"content" else "condition_boundary"))
    if (!result$valid) {
      invalid_cells <- invalid_cells + 1L
      message <- sprintf("%s rule row %d: Content=%s; Method=%s; field=%s; value=%s is invalid for PCRE.",
        check$component,check$row,auto_regex_diagnostic_value(check$content),
        auto_regex_diagnostic_value(check$method),check$field,auto_regex_diagnostic_value(value))
      add(message); logger("warning","regex","validate",message)
    }
  }
  pcre_results <- lapply(patterns[used], validate_pcre)
  stringr_patterns <- chr(c(cnd$Separators[positional], rat$Separators[non_regex]))
  stringr_patterns <- stringr_patterns[nzchar(stringr_patterns)]
  stringr_results <- lapply(stringr_patterns, validate_stringr_pattern)
  separator_checks <- c(lapply(which(positional),function(i)list(component="Condition",row=i,content=cnd$Content[i],method=cnd$Method[i],field="Separators",value=cnd$Separators[i])),
    lapply(which(non_regex),function(i)list(component="Ratio",row=i,content=rat$Content[i],method=rat$Method[i],field="Separators",value=rat$Separators[i])))
  for(check in separator_checks) if(!is.na(check$value)&&nzchar(check$value)&&!validate_stringr_pattern(check$value)$valid) {
    invalid_cells<-invalid_cells+1L
    message<-sprintf("%s rule row %d: Content=%s; Method=%s; field=%s; value=%s is invalid for stringr/ICU.",check$component,check$row,
      auto_regex_diagnostic_value(check$content),auto_regex_diagnostic_value(check$method),check$field,auto_regex_diagnostic_value(check$value))
    add(message);logger("warning","regex","validate",message)
  }
  if(invalid_cells) add(sprintf("Rule validation failed: %d invalid field value(s); see diagnostics and the Session log for row details.",invalid_cells))
  logger("debug","regex","validate",sprintf("Regex validation checked %d PCRE and %d stringr patterns: %d invalid.",
    length(pcre_results),length(stringr_results),sum(!vapply(c(pcre_results,stringr_results),`[[`,logical(1),"valid"))))
  if (!is.null(metadata) && !length(errors)) {
    application_error <- tryCatch({
      a <- apply_content_table(metadata, core$table)
      b <- apply_condition_table(a$metadata, core$condition)
      apply_ratio_table(b$metadata, core$ratio); NULL
    }, error=function(e) conditionMessage(e))
    if (!is.null(application_error)) add(paste("Rules cannot be applied to current metadata:", application_error))
  }
  result<-unique(errors)
  logger(if(length(result))"warning" else "info","export","validate",sprintf("Export validation completed with %d error(s).",length(result)),
    details=list(elapsed_ms=(proc.time()[["elapsed"]]-started)*1000))
  result
}

prepare_export_inputs <- function(rules, metrics, metadata, unreliable_action, unresolved_action) {
  stopifnot(unreliable_action %in% c("exclude","include"),
    unresolved_action %in% c("exclude","include"))
  if (is.null(rules) || !is.list(rules)) stop("No inferred rules exist.")
  if (is.null(metadata) || !is.data.frame(metadata)) stop("Mapped metadata do not exist.")
  core <- rules[c("table","condition","ratio")]
  status_sets <- if (is.list(metrics) && !is.null(metrics$rule_status)) metrics$rule_status else metrics
  unreliable <- lapply(c("table","condition","ratio"), function(component) {
    diagnostic <- if (is.list(status_sets)) status_sets[[component]] else NULL
    if (is.null(diagnostic) && component == "table" && is.list(status_sets)) diagnostic <- status_sets$content
    if (!is.data.frame(diagnostic) || !"Content" %in% names(diagnostic)) return(character())
    bad <- if ("Reliable" %in% names(diagnostic)) !diagnostic$Reliable else
      if ("Status" %in% names(diagnostic)) tolower(chr(diagnostic$Status)) %in% c("unresolved","provisional","conflicting") else rep(FALSE,nrow(diagnostic))
    unique(chr(diagnostic$Content[!is.na(bad) & bad]))
  }); names(unreliable) <- c("table","condition","ratio")
  effective <- core
  excluded_rules <- integer(3L); names(excluded_rules) <- names(effective)
  invalid_condition_rows <- !is_sample_bearing_content(effective$condition$Content)
  excluded_rules[["condition"]] <- sum(invalid_condition_rows)
  effective$condition <- effective$condition[!invalid_condition_rows,,drop=FALSE]
  if (identical(unreliable_action,"exclude")) for (component in names(effective)) {
    remove <- effective[[component]]$Content %in% unreliable[[component]]
    if (component == "table") remove <- remove & effective[[component]]$Content != "Row Index"
    excluded_rules[[component]] <- excluded_rules[[component]] + sum(remove)
    effective[[component]] <- effective[[component]][!remove,,drop=FALSE]
  }
  included_rules <- vapply(effective,nrow,integer(1))
  included_unreliable <- vapply(names(effective),function(component)
    sum(effective[[component]]$Content %in% unreliable[[component]]),integer(1))

  # Prefer retained retest row diagnostics when supplied.  Otherwise create the
  # same structured diagnostic with the selected content rules; never create a
  # rule for an unresolved row.
  rows <- if (is.list(metrics) && is.data.frame(metrics$rows)) metrics$rows else NULL
  if (is.null(rows) || !"Unresolved" %in% names(rows) || nrow(rows) != nrow(metadata))
    rows <- apply_content_table(metadata,effective$table)$rows
  unresolved <- !is.na(rows$Unresolved) & rows$Unresolved
  keep <- if (identical(unresolved_action,"exclude")) !unresolved else rep(TRUE,nrow(metadata))
  effective_metadata <- metadata[keep,,drop=FALSE]
  replay_rows <- rows[keep,,drop=FALSE]
  counts <- list(rules=list(included=included_rules,excluded_unreliable=excluded_rules,
      included_unreliable=included_unreliable),
    metadata=list(included=sum(keep),excluded_unresolved=sum(!keep),
      included_unresolved=sum(unresolved & keep),unresolved=sum(unresolved)))
  warnings <- character()
  invalid_types <- invalid_condition_content(core$condition)
  if (length(invalid_types)) warnings <- c(warnings,sprintf(
    "Excluded non-sample condition rule Content '%s'; change its Content type to include it.",invalid_types))
  if (sum(excluded_rules)) warnings <- c(warnings,sprintf("Excluded %d unreliable rule row(s).",sum(excluded_rules)))
  if (sum(included_unreliable)) warnings <- c(warnings,sprintf("Included %d diagnostically unreliable rule row(s).",sum(included_unreliable)))
  if (sum(!keep)) warnings <- c(warnings,sprintf("Excluded %d unresolved metadata row(s) from replay diagnostics.",sum(!keep)))
  if (sum(unresolved & keep)) warnings <- c(warnings,sprintf("Included %d unresolved metadata row(s) in replay diagnostics.",sum(unresolved & keep)))
  payload <- list(rules=effective,metadata=effective_metadata,replay_rows=replay_rows,
    actions=list(unreliable_action=unreliable_action,unresolved_action=unresolved_action),counts=counts)
  list(table=effective$table,condition=effective$condition,ratio=effective$ratio,
    metadata=effective_metadata,replay_rows=replay_rows,counts=counts,warnings=warnings,
    signature_payload=payload)
}
