
# ---- application, diagnostics, compatibility validation --------------------

# Replay the selected downstream rules after WP3 without invoking either
# downstream candidate search.  The first-pass application defines the rows
# which were reachable; refinements may add targets, but may not remove or
# redirect one of those rows or change its verified extraction.
auto_regex_gate_content_refinements <- function(metadata, original_table, refined,
                                                 condition_table, ratio_table,
                                                 condition_target="Options") {
  stopifnot(is.data.frame(refined$lineage), identical(names(original_table),CONTENT_FIELDS))
  expected_condition <- if(nzchar(condition_target) && condition_target %in% names(metadata))
    chr(metadata[[condition_target]]) else rep("",nrow(metadata))
  expected_numerator <- if("Numerator" %in% names(metadata)) chr(metadata$Numerator) else rep("",nrow(metadata))
  expected_denominator <- if("Denominator" %in% names(metadata)) chr(metadata$Denominator) else rep("",nrow(metadata))
  replay <- function(table) {
    content <- apply_content_table(metadata,table)
    condition_input <- content$metadata
    # Do not let reference Options mask a failed extraction during replay.
    # Auto-Assign writes these targeted cells, so begin them empty here too.
    if("Options" %in% names(condition_input))
      condition_input$Options[chr(condition_input$Content) %in% chr(condition_table$Content)] <- ""
    condition <- apply_condition_table(condition_input,condition_table,expected_condition)
    ratio <- apply_ratio_table(condition$metadata,ratio_table,expected_numerator,expected_denominator)
    list(content=content,condition=condition,ratio=ratio)
  }
  baseline <- replay(original_table)
  condition_rows <- which(nzchar(expected_condition) &
    chr(baseline$content$metadata$Content) %in% chr(condition_table$Content))
  baseline_variants <- attr(
    baseline$content$metadata,
    "variant_id",
    exact = TRUE
  )

  if (is.null(baseline_variants))
    baseline_variants <- rep("", nrow(metadata))

  candidate_ratio_rows <- which(
    nzchar(expected_numerator) &
      nzchar(expected_denominator) &
      chr(baseline$content$metadata$Content) %in% chr(ratio_table$Content)
  )

  ratio_rows <- auto_regex_ratio_obligation_rows(
    metadata = metadata,
    rows = candidate_ratio_rows,
    variant_ids = baseline_variants
  )
  violations <- function(value) unique(c(
    condition_rows[chr(value$content$metadata$Content[condition_rows]) !=
      chr(baseline$content$metadata$Content[condition_rows]) |
      chr(value$condition$metadata$Options[condition_rows]) !=
      chr(baseline$condition$metadata$Options[condition_rows])],
    ratio_rows[chr(value$content$metadata$Content[ratio_rows]) !=
      chr(baseline$content$metadata$Content[ratio_rows]) |
      chr(value$ratio$metadata$Numerator[ratio_rows]) != chr(baseline$ratio$metadata$Numerator[ratio_rows]) |
      chr(value$ratio$metadata$Denominator[ratio_rows]) != chr(baseline$ratio$metadata$Denominator[ratio_rows])]
  ))
  final <- refined$table; current <- replay(final); rolled_back <- character()
  # Re-evaluate after every rollback: removing one ordered rule can expose a
  # different later/earlier winner.  Thus the returned application/conflict
  # summary is computed once from the stable final combination below.
  repeat {
    bad <- violations(current)
    if(!length(bad)) break
    accepted <- which(refined$lineage$Accepted & !refined$lineage$Content %in% rolled_back)
    if(!length(accepted)) break
    trials <- lapply(accepted,function(i) {
      label <- refined$lineage$Content[[i]]; z <- final
      row <- match(label,z$Content)
      z$Include[[row]] <- refined$lineage$OriginalInclude[[i]]
      z$Exclude[[row]] <- refined$lineage$OriginalExclude[[i]]
      value <- replay(z)
      list(i=i,label=label,table=z,value=value,bad=violations(value))
    })
    improvement <- lengths(lapply(trials,`[[`,"bad")) < length(bad)
    if(!any(improvement)) {
      # Interacting refinements can require a joint rollback.  Restore every
      # refined rule participating as either the old or new winner on bad rows.
      labels <- unique(c(chr(baseline$content$metadata$Content[bad]),
        chr(current$content$metadata$Content[bad])))
      chosen <- accepted[refined$lineage$Content[accepted] %in% labels]
      if(!length(chosen)) chosen <- accepted
      for(i in chosen) {
        row <- match(refined$lineage$Content[[i]],final$Content)
        final$Include[[row]] <- refined$lineage$OriginalInclude[[i]]
        final$Exclude[[row]] <- refined$lineage$OriginalExclude[[i]]
      }
      rolled_back <- unique(c(rolled_back,refined$lineage$Content[chosen]))
      current <- replay(final)
    } else {
      trial <- trials[[which(improvement)[[1L]]]]
      final <- trial$table; current <- trial$value
      rolled_back <- c(rolled_back,trial$label)
    }
  }
  remaining <- violations(current)
  lineage <- refined$lineage
  lineage$DownstreamReplay <- ifelse(lineage$Accepted,"accepted","not_refined")
  lineage$DownstreamReplay[lineage$Content %in% rolled_back] <- "rejected"
  lineage$DownstreamReplayReason <- ""
  lineage$DownstreamReplayReason[lineage$Content %in% rolled_back] <-
    "first-pass rule restored because a previously reachable condition or ratio row became unreachable or changed extraction"
  lineage$Accepted[lineage$Content %in% rolled_back] <- FALSE
  for(i in which(lineage$Content %in% rolled_back)) {
    lineage$FinalInclude[[i]] <- lineage$OriginalInclude[[i]]
    lineage$FinalExclude[[i]] <- lineage$OriginalExclude[[i]]
    lineage$RejectionReason[[i]] <- paste(c(lineage$RejectionReason[[i]],
      lineage$DownstreamReplayReason[[i]])[nzchar(c(lineage$RejectionReason[[i]],
        lineage$DownstreamReplayReason[[i]]))],collapse="; ")
  }
  summary <- data.frame(Check=c("assigned_content","extracted_conditions","numerator","denominator",
      "condition_applicable_rows","ratio_applicable_rows"),
    Baseline=c(sum(nzchar(chr(baseline$content$metadata$Content))),
      sum(nzchar(chr(baseline$condition$metadata$Options[condition_rows]))),
      sum(nzchar(chr(baseline$ratio$metadata$Numerator[ratio_rows]))),
      sum(nzchar(chr(baseline$ratio$metadata$Denominator[ratio_rows]))),length(condition_rows),length(ratio_rows)),
    Final=c(sum(nzchar(chr(current$content$metadata$Content))),
      sum(nzchar(chr(current$condition$metadata$Options[condition_rows]))),
      sum(nzchar(chr(current$ratio$metadata$Numerator[ratio_rows]))),
      sum(nzchar(chr(current$ratio$metadata$Denominator[ratio_rows]))),length(condition_rows),length(ratio_rows)),
    Preserved=c(identical(chr(current$content$metadata$Content[unique(c(condition_rows,ratio_rows))]),
        chr(baseline$content$metadata$Content[unique(c(condition_rows,ratio_rows))])),
      identical(chr(current$condition$metadata$Options[condition_rows]),chr(baseline$condition$metadata$Options[condition_rows])),
      identical(chr(current$ratio$metadata$Numerator[ratio_rows]),chr(baseline$ratio$metadata$Numerator[ratio_rows])),
      identical(chr(current$ratio$metadata$Denominator[ratio_rows]),chr(baseline$ratio$metadata$Denominator[ratio_rows])),
      TRUE,TRUE),stringsAsFactors=FALSE)
  list(table=final,lineage=lineage,application=current$content,condition=current$condition,
    ratio=current$ratio,summary=summary,remaining_violations=remaining)
}


# Validate and materialize the optional, already-frozen provenance input before
# any generic candidate search.  This function is intentionally pure: callers
# decide whether the frozen input came from active Data Wizard state or from a
# self-contained workbook.
auto_regex_prepare_provenance <- function(metadata, provenance = NULL) {
  empty <- data.frame(Row = integer(), Outcome = character(), Origin = character(),
    SourceColumns = character(), Configuration = character(), Reason = character(),
    stringsAsFactors = FALSE, check.names = FALSE)
  if (is.null(provenance)) return(list(metadata = metadata, diagnostics = empty,
    authoritative_rows = integer()))
  if (!is.list(provenance) || length(provenance$source) != 1L ||
      !provenance$source %in% c("active_datawizard", "workbook") ||
      !is.data.frame(provenance$data))
    stop("provenance must be a frozen active_datawizard or workbook descriptor.", call. = FALSE)

  configurations <- provenance$configurations %||% list()
  # Workbook descriptors are never permitted to carry session configurations
  # or a live contrast collection, even if a caller constructs one manually.
  if (identical(provenance$source, "workbook")) {
    configurations <- list()
    provenance$contrast_mapping_collection <- NULL
  }
  diagnostics <- auto_regex_resolve_provenance(metadata, provenance$data,
    configurations = configurations,
    workbook = identical(provenance$source, "workbook"))
  resolved <- diagnostics$Outcome == "resolved"
  out <- metadata
  ensure <- function(field) if (!field %in% names(out)) out[[field]] <<- ""
  for (field in c("Content", "VariantId", "Numerator", "Denominator")) ensure(field)

  # Prefer the separately versioned active contrast map.  It is authoritative
  # only after every generated column has resolved persisted/live lineage.
  collection <- provenance$contrast_mapping_collection
  mappings <- if (identical(provenance$source, "active_datawizard") &&
      auto_regex_contrast_collection_matches_source(collection,
        provenance$source_revision %||% NULL)) collection$mappings else list()
  contrast_number <- 0L
  for (mapping in mappings) {
    if (!is.list(mapping) || !is.data.frame(mapping$Columns) ||
        !all(c("Column", "Content", "VariantId", "ContrastId") %in% names(mapping$Columns))) next
    rows <- match(as.character(mapping$Columns$Column), as.character(out$Column))
    if (anyNA(rows) || length(rows) != 3L || !all(resolved[rows]) ||
        !all(diagnostics$Origin[rows] == "ratio")) next
    contrast_number <- contrast_number + 1L
    numerator <- trimws(as.character(mapping$NumeratorRefs %||% character()))
    denominator <- trimws(as.character(mapping$DenominatorRefs %||% character()))
    numerator <- numerator[nzchar(numerator)]; denominator <- denominator[nzchar(denominator)]
    if (!length(numerator)) numerator <- sprintf("datawizard_contrast_%d_numerator", contrast_number)
    if (!length(denominator)) denominator <- sprintf("datawizard_contrast_%d_denominator", contrast_number)
    out$Content[rows] <- as.character(mapping$Columns$Content)
    out$VariantId[rows] <- as.character(mapping$Columns$VariantId)
    out$Numerator[rows] <- paste(numerator, collapse = "+")
    out$Denominator[rows] <- paste(denominator, collapse = "+")
  }

  # A persisted workbook normally has no separate contrast collection.  Join a
  # complete ratio triplet by its exact Data-Wizard suffix and give all members
  # one deterministic contrast identity.  Incomplete families stay diagnostic
  # and are not frozen.
  ratio_rows <- which(resolved & diagnostics$Origin == "ratio")
  suffixes <- c("_Abundance Ratio", "_Abundance Ratio p-Value",
    "_Abundance Ratio Adj. p-Value")
  bases <- unique(sub(paste0("(", paste(regex_escape_literal(suffixes), collapse = "|"), ")$"),
    "", as.character(out$Column[ratio_rows]), perl = TRUE))
  bases <- sort(bases[nzchar(bases)])
  for (base in bases) {
    columns <- paste0(base, suffixes); rows <- match(columns, as.character(out$Column))
    if (anyNA(rows) || !all(rows %in% ratio_rows) || any(nzchar(chr(out$VariantId[rows])))) next
    contrast_number <- contrast_number + 1L
    id <- sprintf("datawizard_contrast_%d", contrast_number)
    out$Content[rows] <- sub("^_", "", suffixes)
    out$VariantId[rows] <- paste0(id, ":", c("ratio", "p_value", "adjusted_p_value"))
    out$Numerator[rows] <- paste0(id, "_numerator")
    out$Denominator[rows] <- paste0(id, "_denominator")
  }
  list(metadata = out, diagnostics = diagnostics,
    authoritative_rows = which(resolved))
}

auto_regex_reconcile_downstream_rules <- function(rules) {

  required_components <- c(
    "table",
    "condition",
    "ratio"
  )

  if (!is.list(rules) ||
      !all(
        required_components %in%
        names(rules)
      )) {

    stop(
      paste(
        "Rule reconciliation requires",
        "table, condition, and ratio components."
      ),
      call. = FALSE
    )
  }

  if (!all(
    vapply(
      rules[
        required_components
      ],
      is.data.frame,
      logical(1)
    )
  )) {

    stop(
      "Rule reconciliation requires data-frame rule components.",
      call. = FALSE
    )
  }

  required_identity <- c(
    "RuleId",
    "Content",
    "VariantId"
  )

  for (component in
       required_components) {

    if (!all(
      required_identity %in%
      names(
        rules[[component]]
      )
    )) {

      stop(
        sprintf(
          "%s rules are missing RuleId/Content/VariantId identity.",
          tools::toTitleCase(
            component
          )
        ),
        call. = FALSE
      )
    }
  }

  out <- rules

  content_keys <- paste(
    chr(
      out$table$Content
    ),
    chr(
      out$table$VariantId
    ),
    sep = "\r"
  )

  removed <- list()

  for (component in
       c(
         "condition",
         "ratio"
       )) {

    child <- out[[component]]

    if (!nrow(child)) {
      next
    }

    child_keys <- paste(
      chr(
        child$Content
      ),
      chr(
        child$VariantId
      ),
      sep = "\r"
    )

    orphan <- !child_keys %in%
      content_keys

    if (any(orphan)) {

      removed[[
        length(removed) + 1L
      ]] <- data.frame(
        Component = component,
        RuleId = chr(
          child$RuleId[orphan]
        ),
        Content = chr(
          child$Content[orphan]
        ),
        VariantId = chr(
          child$VariantId[orphan]
        ),
        Reason =
          "parent_content_rule_not_published",
        stringsAsFactors = FALSE,
        check.names = FALSE
      )
    }

    out[[component]] <- child[
      !orphan,
      ,
      drop = FALSE
    ]
  }

  removed_table <-
    if (length(removed)) {

      do.call(
        rbind,
        removed
      )

    } else {

      data.frame(
        Component = character(),
        RuleId = character(),
        Content = character(),
        VariantId = character(),
        Reason = character(),
        stringsAsFactors = FALSE,
        check.names = FALSE
      )
    }

  rownames(
    removed_table
  ) <- NULL

  list(
    rules = out,
    removed = removed_table
  )
}

#' Infer canonical Data Wizard assignment rules from mapped metadata.
#'
#' The input must contain Column and Content. Options is the default condition
#' target; Numerator and Denominator opt rows into ratio inference. No input is
#' mutated and no package, file, reactive, or application state is touched.
