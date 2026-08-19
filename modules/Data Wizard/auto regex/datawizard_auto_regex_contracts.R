# ============================================================================
# Sub-script: Auto Regex metadata and rule contracts
# Purpose: Define canonical metadata, rule schemas, identities, coercion, and
# validation contracts used by the Auto Regex compatibility layer.
# Source-time dependencies: base and stats.
# Call-time dependencies: datawizard_metadata_content_choices, chr,
# content_transformation_details, and coerce_contract supplied by the complete
# Auto Regex utility layer.
# ============================================================================

AUTO_REGEX_METADATA_SCHEMA <- c("Column", "Content", "Options", "Numerator",
  "Denominator", "Transformation", "Sample")

# Turn the canonical Content vocabulary into recognizable example headers for
# the no-data workbook. Ratio headers deliberately contain a numerator and
# denominator so they are also useful inputs for Auto RegEx ratio inference.
auto_regex_metadata_example_columns <- function(content_choices) {
  content_choices <- unique(as.character(content_choices))
  content_choices <- content_choices[
    !is.na(content_choices) & nzchar(content_choices) &
      content_choices != "Row Index"
  ]
  ratio_examples <- c(
    "Abundance Ratio" = "Abundance Ratio Sample_1 / Sample_2",
    "Abundance Ratio p-Value" =
      "Abundance Ratio p-Value Sample_1 / Sample_2",
    "Abundance Ratio Adj. p-Value" =
      "Abundance Ratio Adj. p-Value Sample_1 / Sample_2"
  )
  examples <- ifelse(
    content_choices %in% names(ratio_examples),
    unname(ratio_examples[content_choices]),
    paste(content_choices, "Example")
  )
  # Row Index is intentionally last in the default template.
  c(examples, "Row Index")
}

auto_regex_build_metadata_template <- function(active_data, schema,
                                               example_columns = NULL) {
  fields <- if (is.data.frame(schema)) names(schema) else as.character(schema)
  fields <- fields[!is.na(fields) & nzchar(fields)]
  if (!length(fields) || !identical(fields[[1L]], "Column") ||
      !"Content" %in% fields) stop(
    "Canonical metadata schema must begin with Column and include Content.",
    call. = FALSE)
  usable <- is.data.frame(active_data) && ncol(active_data) > 0L
  columns <- if (usable) names(active_data) else {
    examples <- example_columns
    if (is.null(examples)) examples <- auto_regex_metadata_example_columns(
      datawizard_metadata_content_choices(include_blank = FALSE))
    examples <- as.character(examples)
    examples <- examples[!is.na(examples) & nzchar(examples)]
    unique(c(examples[examples != "Row Index"], "Row Index"))
  }
  values <- stats::setNames(rep(list(rep("", length(columns))), length(fields)), fields)
  values$Column <- columns
  values$Content[which(columns == "Row Index")] <- "Row Index"
  as.data.frame(values, stringsAsFactors = FALSE, check.names = FALSE)
}

auto_regex_template_name_issues <- function(columns) {
  columns <- as.character(columns)
  issues <- character()
  blank <- which(is.na(columns) | !nzchar(trimws(columns)))
  duplicate <- which(duplicated(columns) | duplicated(columns, fromLast = TRUE))
  if (length(blank)) issues <- c(issues, sprintf(
    "Blank dataset column name(s) at position(s): %s.", paste(blank, collapse = ", ")))
  if (length(duplicate)) issues <- c(issues, sprintf(
    "Duplicate dataset column name(s): %s.", paste(unique(columns[duplicate]), collapse = ", ")))
  issues
}

# ---- compatibility constants and constructors ------------------------------
# Legacy contract prefixes retained for migration audits:
# CONTENT_FIELDS <- c("Content", "VariantId", "Priority"
# CONDITION_FIELDS <- c("Content", "VariantId"
# RATIO_FIELDS <- c("Content", "VariantId"
CONTENT_FIELDS <- c("RuleId", "Content", "VariantId", "Priority", "Include", "Exclude", "Transformation")
CONDITION_FIELDS <- c("RuleId", "Content", "VariantId", "Method", "Before", "After", "Separators", "Pos")
RATIO_FIELDS <- c("RuleId", "Content", "VariantId", "Method", "Separators", "Invert", "NumBefore", "NumAfter",
                  "DenBefore", "DenAfter", "NumPos", "DenPos")
CONDITION_METHODS <- c("between", "start", "end", "whole", "phrase_position", "pattern_detect")
RATIO_METHODS <- c("Regular Expressions", "Pattern Recognition", "Position in String")
# Keep the exact transformation-capable Data Wizard content allowlist separate
# from sample-bearing content so neither behavior is inferred from naming.
TRANSFORMATION_CONTENT_TYPES <- c(
  "Abundance Ratio",
  "Abundance Ratio p-Value",
  "Abundance Ratio Adj. p-Value",
  "Raw Abundance",
  "Normalized Abundance",
  "Batch Corrected Abundance",
  "Batch Corrected Normalized Abundance",
  "Batch Corrected Raw Abundance",
  "Imputed Raw Abundance",
  "Imputed Normalized Abundance",
  "Imputed Batch Corrected Abundance",
  "Imputed Batch Corrected Normalized Abundance",
  "Imputed Batch Corrected Raw Abundance"
)
SUPPORTED_TRANSFORMATIONS <- c("None", "log2", "log10", "-log10")

# Automatic sample-name inference is deliberately opt-in.
SAMPLE_BEARING_CONTENT_TYPES <- c(
  "Found in Sample",
  "Found in File",
  "Raw Abundance",
  "Normalized Abundance",
  "Imputed Raw Abundance",
  "Imputed Normalized Abundance",
  "Imputed Batch Corrected Abundance",
  "Batch Corrected Normalized Abundance",
  "Batch Corrected Raw Abundance",
  "Imputed Batch Corrected Normalized Abundance",
  "Imputed Batch Corrected Raw Abundance"
)

is_sample_bearing_content <- function(content) {
  trimws(as.character(content)) %in% SAMPLE_BEARING_CONTENT_TYPES
}
invalid_condition_content <- function(table) {
  if (!is.data.frame(table) || !"Content" %in% names(table)) return(character())
  unique(chr(table$Content[!is_sample_bearing_content(table$Content)]))
}
condition_content_validation_messages <- function(table) {
  invalid <- invalid_condition_content(table)
  if (!length(invalid)) return(character())
  sprintf("Condition rule Content '%s' is not sample-bearing; change its Content type or remove the rule.", invalid)
}
# Last-resort vocabulary for columns explicitly labelled Identifier.  Keep this
# deliberately small and centralized: it is not a general semantic dictionary.
# Each entry is a complete lexical unit (with controlled pluralisation), rather
# than a substring.  The two Protein* compounds retain the canonical
# Proteome-Discoverer headers without allowing a bare term to match inside an
# unrelated alphanumeric word.
IDENTIFIER_FALLBACK_VOCABULARY <- c(
  "proteinaccessions?", "proteinnames?", "accessions?", "genes?", "names?",
  "entrez", "ensembl", "ids?"
)
TECHNICAL_DESCRIPTION_HEADERS <- c("PG.ProteinDescriptions")

empty_content <- function() data.frame(RuleId=character(), Content=character(), VariantId=character(), Priority=integer(), Include=character(), Exclude=character(), Transformation=character(), stringsAsFactors=FALSE, check.names=FALSE)
empty_condition <- function() data.frame(RuleId=character(), Content=character(), VariantId=character(), Method=character(), Before=character(), After=character(), Separators=character(), Pos=integer(), check.names=FALSE)
empty_ratio <- function() data.frame(RuleId=character(), Content=character(), VariantId=character(), Method=character(), Separators=character(), Invert=logical(), NumBefore=character(), NumAfter=character(), DenBefore=character(), DenAfter=character(), NumPos=integer(), DenPos=integer(), stringsAsFactors=FALSE, check.names=FALSE)

# These zero-row constructors are the type authority as well as the allocation
# helpers.  Keeping schema inspection here prevents export validation from
# acquiring a second, positional list of column classes that can silently drift.
canonical_rule_schemas <- function() {
  schemas <- list(table=empty_content(), condition=empty_condition(), ratio=empty_ratio())
  fields <- list(table=CONTENT_FIELDS, condition=CONDITION_FIELDS, ratio=RATIO_FIELDS)
  if (!identical(lapply(schemas, names), fields))
    stop("Canonical rule fields and schema prototypes are out of sync.", call.=FALSE)
  schemas
}

canonical_rule_classes <- function() {
  lapply(canonical_rule_schemas(), function(schema)
    vapply(schema, function(column) class(column)[1L], character(1)))
}

coerce_rule_component_classes <- function(table, component) {
  expected <- canonical_rule_classes()[[component]]
  for (field in names(expected)) table[[field]] <- switch(expected[[field]],
    character=as.character(table[[field]]),
    integer=as.integer(table[[field]]),
    logical=as.logical(table[[field]]),
    stop(sprintf("Unsupported canonical class '%s' for %s$%s.",
      expected[[field]], component, field), call.=FALSE))
  table
}

# Rule construction and contract errors have one public shape.  In particular,
# callers must not have to interpret an accidental "undefined columns selected"
# error to discover that a rule did not satisfy the canonical schema.
rule_validation_failure <- function(message, component, code, details=list()) {
  condition <- structure(list(message=message, call=NULL, component=component,
    code=code, details=details), class=c("miraprot_rule_validation_error","error","condition"))
  stop(condition)
}

canonical_rule_row <- function(kind=c("content","condition","ratio"), ...) {
  kind <- match.arg(kind); values <- list(...)
  content <- chr(values$Content)
  if (length(content)!=1L || !nzchar(trimws(content)))
    rule_validation_failure("Rule identity requires one nonblank Content value.",kind,
      "missing_content_identity")
  values$VariantId <- stable_variant_ids(content,values$VariantId)[[1L]]
  if (!nzchar(trimws(values$VariantId)))
    rule_validation_failure("Rule identity requires a nonblank VariantId.",kind,
      "missing_variant_identity")
  required_payload <- switch(kind,
    content=c("Priority","Include","Exclude","Transformation"),
    condition=c("Method","Before","After","Separators","Pos"),
    ratio=c("Method","Separators","Invert","NumBefore","NumAfter","DenBefore","DenAfter","NumPos","DenPos"))
  missing_payload <- required_payload[!required_payload %in% names(values)]
  invalid_lengths <- required_payload[required_payload %in% names(values) &
    vapply(values[required_payload[required_payload %in% names(values)]], length, integer(1)) != 1L]
  if (length(c(missing_payload, invalid_lengths))) rule_validation_failure(
    sprintf("Rule is missing canonical fields: %s", paste(unique(c(missing_payload, invalid_lengths)), collapse=", ")),
    kind, "incomplete_schema")
  values$RuleId <- stable_rule_ids(kind, values$VariantId, values$RuleId)[[1L]]
  if (!nzchar(trimws(values$RuleId)))
    rule_validation_failure("Rule identity requires a nonblank RuleId.",kind,
      "missing_rule_identity")
  if (kind=="content") {
    transformation <- as.character(values$Transformation)
    if (!content %in% TRANSFORMATION_CONTENT_TYPES && !is.na(transformation) && nzchar(transformation))
      rule_validation_failure("Transformation is owned only by transformation-capable content rules.", kind, "invalid_transformation_owner")
    if (!is.na(transformation) && nzchar(transformation) && !transformation %in% SUPPORTED_TRANSFORMATIONS)
      rule_validation_failure("Unsupported content transformation.", kind, "invalid_transformation")
    priority <- suppressWarnings(as.integer(values$Priority))
    if (length(priority)!=1L || is.na(priority) || priority<0L)
      rule_validation_failure("Content rule Priority must be a non-negative integer.",kind,
        "invalid_priority",list(value=values$Priority))
    row <- data.frame(RuleId=values$RuleId,Content=content,VariantId=values$VariantId,Priority=priority,
      Include=chr(values$Include),Exclude=chr(values$Exclude),
      Transformation=transformation,
      stringsAsFactors=FALSE,check.names=FALSE)
  } else if (kind=="condition") {
    row <- data.frame(RuleId=values$RuleId,Content=content,VariantId=values$VariantId,Method=chr(values$Method),
      Before=chr(values$Before),After=chr(values$After),Separators=chr(values$Separators),
      Pos=as.integer(values$Pos),stringsAsFactors=FALSE,check.names=FALSE)
  } else {
    row <- data.frame(RuleId=values$RuleId,Content=content,VariantId=values$VariantId,Method=chr(values$Method),
      Separators=as.character(values$Separators),Invert=as.logical(values$Invert),
      NumBefore=as.character(values$NumBefore),NumAfter=as.character(values$NumAfter),
      DenBefore=as.character(values$DenBefore),DenAfter=as.character(values$DenAfter),
      NumPos=as.integer(values$NumPos),DenPos=as.integer(values$DenPos),
      stringsAsFactors=FALSE,check.names=FALSE)
  }
  fields <- switch(kind,content=CONTENT_FIELDS,condition=CONDITION_FIELDS,ratio=RATIO_FIELDS)
  row[,fields,drop=FALSE]
}

# VariantId is logical identity, not a regex fingerprint or a displayed row
# number.  Legacy rules are upgraded once at the contract boundary.  The
# content/occurrence seed remains stable when Include or Exclude is refined.
stable_variant_ids <- function(content, supplied=NULL) {
  content <- chr(content); supplied <- if (is.null(supplied)) rep("", length(content)) else chr(supplied)
  seen <- new.env(parent=emptyenv()); out <- supplied
  for (i in seq_along(content)) if (!nzchar(out[i])) {
    key <- content[i]; ordinal <- if (exists(key, seen, inherits=FALSE)) get(key, seen)+1L else 1L
    assign(key, ordinal, seen); slug <- tolower(gsub("(^-+|-+$)", "", gsub("[^[:alnum:]]+", "-", key)))
    if (!nzchar(slug)) slug <- "content"
    out[i] <- sprintf("%s-v%d", slug, ordinal)
  }
  out
}

stable_rule_ids <- function(kind, variant_id, supplied=NULL) {
  variant_id <- chr(variant_id)
  supplied <- if (is.null(supplied)) rep("", length(variant_id)) else chr(supplied)
  seen <- new.env(parent=emptyenv()); out <- supplied
  for (i in seq_along(variant_id)) if (!nzchar(trimws(out[i]))) {
    seed <- paste(kind, variant_id[i], sep="-")
    ordinal <- if (exists(seed, seen, inherits=FALSE)) get(seed, seen)+1L else 1L
    assign(seed, ordinal, seen)
    slug <- tolower(gsub("(^-+|-+$)", "", gsub("[^[:alnum:]]+", "-", seed)))
    out[i] <- sprintf("%s-r%d", slug, ordinal)
  }
  out
}

upgrade_rule_component <- function(table, kind=c("content","condition","ratio")) {
  kind <- match.arg(kind)
  if (!is.data.frame(table)) rule_validation_failure("Rule component must be a data frame.",kind,"invalid_component_type")
  if (!"Content" %in% names(table)) rule_validation_failure("Rule component is missing Content.",kind,"missing_content_identity")
  table$VariantId <- stable_variant_ids(table$Content, if ("VariantId" %in% names(table)) table$VariantId else NULL)
  table$RuleId <- stable_rule_ids(kind, table$VariantId, if ("RuleId" %in% names(table)) table$RuleId else NULL)
  if (kind == "content" && !"Priority" %in% names(table)) table$Priority <- seq_len(nrow(table))
  fields <- switch(kind, content=CONTENT_FIELDS, condition=CONDITION_FIELDS, ratio=RATIO_FIELDS)
  missing <- setdiff(fields,names(table)); if(length(missing)) rule_validation_failure(
    sprintf("Rule component is missing: %s",paste(missing,collapse=", ")),kind,"schema_mismatch",list(missing=missing))
  out <- table[,fields,drop=FALSE]
  if (any(!nzchar(trimws(chr(out$Content)))) || any(!nzchar(trimws(chr(out$VariantId)))) || any(!nzchar(trimws(chr(out$RuleId)))))
    rule_validation_failure("Rule component contains a missing RuleId/Content/VariantId identity.",kind,"missing_identity")
  if (anyDuplicated(chr(out$RuleId))) rule_validation_failure("RuleId values must be unique within a component.",kind,"duplicate_rule_id")
  if (kind=="content") {
    transformations <- as.character(out$Transformation)
    non_transformable <- !chr(out$Content) %in% TRANSFORMATION_CONTENT_TYPES
    invalid_owner <- non_transformable & !is.na(transformations) & nzchar(transformations)
    if (any(invalid_owner))
      rule_validation_failure("Transformation is owned only by transformation-capable content rules.", kind,
        "invalid_transformation_owner", list(rows=which(invalid_owner)))
    invalid_transformation <- !is.na(transformations) & nzchar(transformations) &
      !transformations %in% SUPPORTED_TRANSFORMATIONS
    if (any(invalid_transformation))
      rule_validation_failure("Unsupported content transformation.", kind, "invalid_transformation",
        list(rows=which(invalid_transformation)))
    priority <- suppressWarnings(as.integer(out$Priority))
    if (any(is.na(priority)|priority<0L) || anyDuplicated(priority))
      rule_validation_failure("Content rule Priority values must be unique non-negative integers.",kind,"invalid_priority")
    out$Priority <- priority
  }
  out
}

# Prerequisite injection is deliberately strict: accepting a table with only a
# subset of the application schema would make ratio diagnostics depend on which
# columns happened to survive.  Callers may pass NULL to request inference.
canonical_prerequisite_rules <- function(table, kind=c("content", "condition")) {
  kind <- match.arg(kind)
  fields <- if (kind == "content") CONTENT_FIELDS else CONDITION_FIELDS
  table <- upgrade_rule_component(table,kind)
  if (anyDuplicated(paste(chr(table$Content),chr(table$VariantId),sep="\r")))
    stop(sprintf("Supplied %s_rules contain duplicate Content/VariantId values.", kind), call. = FALSE)
  contract <- list(table=empty_content(), condition=empty_condition(), ratio=empty_ratio())
  contract[[if (kind == "content") "table" else "condition"]] <- table
  coerce_contract(contract)[[if (kind == "content") "table" else "condition"]]
}

# ---- metadata validation ----------------------------------------------------
validate_metadata <- function(df, original_names = names(df), condition_field = "") {
  out <- data.frame(Severity=character(), Check=character(), Message=character())
  add <- function(s,c,m) out <<- rbind(out, data.frame(Severity=s,Check=c,Message=m))
  if (!is.data.frame(df) || !nrow(df)) add("Error","worksheet","Worksheet is empty.")
  if (anyDuplicated(original_names)) add("Error","column names","Duplicated worksheet column names must be resolved.")
  if (any(grepl("^\\.\\.\\.[0-9]+$", names(df)))) add("Warning","repaired names","Excel reader repaired one or more column names.")
  if (!"Column" %in% names(df)) add("Error","Column","Map a source field to the required Column field.") else {
    if (!is.character(df$Column)) add("Warning","Column type","Column was not character and will be converted without changing displayed values.")
    z <- trimws(chr(df$Column)); if (any(!nzchar(z))) add("Error","Column values","Column contains missing or whitespace-only values.")
    if (anyDuplicated(z[nzchar(z)])) add("Warning","duplicates","Column contains duplicated source values.")
  }
  empty_rows <- if (nrow(df)) apply(df,1,function(r) all(is.na(r) | !nzchar(trimws(chr(r))))) else logical()
  if (any(empty_rows)) add("Warning","empty rows",sprintf("%d entirely empty row(s) will be excluded.",sum(empty_rows)))
  if (any(duplicated(df))) add("Warning","duplicate rows","Exact duplicate metadata rows are present.")
  unsupported <- names(df)[vapply(df,function(x) is.list(x) && !is.data.frame(x),logical(1))]
  if (length(unsupported)) add("Error","types",paste("Unsupported list columns:",paste(unsupported,collapse=", ")))
  if (!"Content" %in% names(df)) add("Warning","Content","Content target unavailable; content inference will be skipped.")
  if (!"Transformation" %in% names(df)) {
    add("Warning","Transformation","Transformation metadata is absent; inference will use MiraProt-compatible defaults ('None' for transformation-capable content and NA otherwise).")
  } else if ("Content" %in% names(df)) {
    supplied <- trimws(
      as.character(
        df$Transformation
      )
    )

    unknown <-
      !is.na(supplied) &
      nzchar(supplied) &
      !supplied %in%
      SUPPORTED_TRANSFORMATIONS

    if (any(unknown)) {

      add(
        "Warning",
        "Transformation",
        sprintf(
          paste0(
            "Unknown Transformation value(s) %s at row ID(s) %s will be ",
            "ignored by Auto RegEx inference. If no supported value remains ",
            "for a transformation-capable Content type, inference uses 'None'."
          ),
          paste(
            sprintf(
              "'%s'",
              unique(
                supplied[unknown]
              )
            ),
            collapse = ", "
          ),
          paste(
            which(unknown),
            collapse = ", "
          )
        )
      )
    }
    labels <- unique(as.character(df$Content[!is.na(df$Content)]))
    for (label in labels[nzchar(labels)]) {
      inferred <- content_transformation_details(df,label)
      if (inferred$source == "conflict") add("Error","Transformation",inferred$message)
    }
  }
  # Missing inference evidence is intentionally not structural damage.  The
  # frozen-source readiness pass and individual inference stages report absent
  # targets; keeping that concern out of this function prevents a valid
  # metadata frame from being reclassified as structurally invalid.
  if (!nrow(out)) add("Info","validation","No structural problems detected.")
  out
}
