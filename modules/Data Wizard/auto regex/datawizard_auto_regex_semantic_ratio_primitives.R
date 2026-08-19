# ============================================================================
# Auto Regex semantic and ratio primitives.
# Sourced into the caller environment by datawizard_auto_regex_utils.R.
# ============================================================================

# Sub-script: Auto Regex migrated pure rule primitives
# Purpose: Provide the single migrated implementation of schemas, tokenization,
# regex construction/scoring, rule replay/coercion, validation, and export shape.
# Owns: deterministic value-to-value rule semantics shared by the logic layer.
# Does not own: inference orchestration, Shiny/UI/reactivity, filesystem/network
# I/O, runtime installation, downloads, Session UI, logger state, application
# lifecycle, or authoritative Auto-Assign mutation.
# Interface: private to modEnv. Unprefixed names/constants intentionally retain
# the approved standalone-oracle vocabulary documented in the migration baseline
# for parity testing; they are compatibility primitives, not new global helpers.
# Dependencies: base/stats; optional stringr validation only when installed.
# Known limit: exact parity requires the host bootstrap dependencies; bounded
# search limits trade exhaustive discovery for predictable runtime, and ICU and
# PCRE validation can differ for engine-specific expressions.
# ============================================================================

MIRAPROT_REPRESENTATIVE_EXAMPLE_LIMIT <- 3L
AUTO_REGEX_DIAGNOSTIC_ROW_LIMIT <- 500L

auto_regex_bound_diagnostics <- function(value,
    limit=AUTO_REGEX_DIAGNOSTIC_ROW_LIMIT) {
  if (!is.data.frame(value)) return(data.frame())
  utils::head(value,max(0L,as.integer(limit)[[1L]]))
}

GENERALIZED_INCLUDE_LIMIT <- 12L
GENERALIZED_EXCLUDE_LIMIT <- 8L
GENERALIZED_COMBINATION_LIMIT <- 64L
RATIO_CONTEXT_SEARCH_WIDTH <- 16L
RATIO_CONTEXT_LIMIT <- 12L
RATIO_CONTEXT_PAIR_LIMIT <- 48L
WORKBOOK_PREVIEW_ROW_LIMIT <- 100L

identifier_fallback_pattern <- function() {
  runtime <- paste0("(?i:(?<![[:alnum:]])(?:",
    paste(IDENTIFIER_FALLBACK_VOCABULARY, collapse="|"),
    ")(?![[:alnum:]]))")
  validation <- validate_pcre(runtime)
  if (!isTRUE(validation$valid))
    stop(sprintf("Identifier fallback is not valid PCRE: %s", validation$message), call. = FALSE)
  stored <- regex_to_miraprot_storage(runtime, "content")
  replay <- regex_from_miraprot_storage(stored, "content")
  if (!identical(runtime, replay) || !isTRUE(validate_pcre(replay)$valid))
    stop("Identifier fallback did not survive MiraProt storage validation.", call. = FALSE)
  stored
}

# Compatibility entrypoint: the fallback is one fixed content rule, never a
# candidate search, combination, or source-dependent exclusion.
identifier_fallback_candidates <- function(positive = character(), negative = character()) {
  list(list(include=identifier_fallback_pattern(), exclude=""))
}

# This is intentionally not a semantic vocabulary.  It is a closed protocol
# map for vendor headers whose spelling is sufficiently canonical to support a
# singleton rule.  Additions require their own release-gate fixtures.
technical_description_fallback_pattern <- function() {
  runtime <- paste0("^(?:", paste(regex_escape_literal(
    TECHNICAL_DESCRIPTION_HEADERS), collapse="|"), ")$")
  validation <- validate_pcre(runtime)
  if(!isTRUE(validation$valid))
    stop(sprintf("Technical Description fallback is not valid PCRE: %s",
      validation$message), call.=FALSE)
  stored <- regex_to_miraprot_storage(runtime,"content")
  replay <- regex_from_miraprot_storage(stored,"content")
  if(!identical(runtime,replay) || !isTRUE(validate_pcre(replay)$valid))
    stop("Technical Description fallback did not survive MiraProt storage validation.",
      call.=FALSE)
  stored
}

# Bounded, lossless-enough display for diagnostics.  In particular, do not let
# a blank value and literal whitespace collapse to the same text in a log.
auto_regex_diagnostic_value <- function(value, limit = 120L) {
  if (!length(value) || is.na(value[[1L]])) return("<NA>")
  value <- as.character(value[[1L]])
  if (!nzchar(value)) return("<empty>")
  clipped <- nchar(value, type="chars") > limit
  shown <- if (clipped) substr(value, 1L, limit) else value
  quoted <- encodeString(shown, quote='"')
  escaped <- encodeString(shown, quote='"', justify="none")
  escaped <- gsub(" ", "\\\\x20", escaped, fixed=TRUE)
  suffix <- if (clipped) " (truncated)" else ""
  if (grepl("[[:space:]]", shown))
    sprintf("%s; escaped=%s%s", quoted, escaped, suffix)
  else paste0(quoted, suffix)
}


ratio_between <- function(x, before=NA_character_, after=NA_character_) {
  tryCatch({
    if (is.na(x) || !nzchar(x)) return(NA_character_)
    start <- 1L; finish <- nchar(x)
    if (!is.na(before) && nzchar(before)) {
      loc <- stringr::str_locate(x, before)[1L,]
      if (is.na(loc[1L])) return(NA_character_)
      start <- loc[2L] + 1L
    }
    if (!is.na(after) && nzchar(after)) {
      loc <- stringr::str_locate(substr(x,start,nchar(x)), after)[1L,]
      if (is.na(loc[1L])) return(NA_character_)
      finish <- start + loc[1L] - 2L
    }
    if (finish < start) return(NA_character_)
    value <- trimws(substr(x,start,finish)); if (nzchar(value)) value else NA_character_
  }, error=function(e) NA_character_)
}

# These helpers intentionally reproduce datawizard_auto_assign_utils.R rather
# than implementing more conventional split/regex behaviour.
ratio_normalize_boundaries <- function(nb,na,db,da) {
  clean <- function(x) if (length(x) && !is.na(x) && nzchar(x)) as.character(x) else NA_character_
  nb<-clean(nb); na<-clean(na); db<-clean(db); da<-clean(da)
  if (is.na(na) && is.na(db)) return(list(skip=TRUE))
  if (is.na(na)) na<-db
  if (is.na(db)) db<-na
  list(skip=FALSE,nb=nb,na=na,db=db,da=da)
}
ratio_tokenize_exact <- function(x,separators=NA_character_) {
  cache<-getOption("miraprot.ratio_token_cache",NULL)
  sep_key<-if(length(separators)&&!is.na(separators)&&nzchar(separators))as.character(separators)else"<default>"
  key<-paste0(enc2utf8(x),"\034",sep_key)
  if(is.environment(cache)&&exists(key,envir=cache,inherits=FALSE)){
    cache$hits<-cache$hits+1L
    return(get(key,envir=cache,inherits=FALSE))
  }
  if (is.na(x)||!nzchar(x)) {
    result<-character();if(is.environment(cache))assign(key,result,envir=cache)
    return(result)
  }
  sep<-if(length(separators)&&!is.na(separators)&&nzchar(separators)) separators else "\\s+|\\(|\\)|\\[|\\]|\\{|\\}|/|_|-"
  locs<-tryCatch(stringr::str_locate_all(x,paste0("(",sep,")"))[[1]],error=function(e)matrix(numeric(),ncol=2))
  if(!nrow(locs)){z<-trimws(x);result<-if(nzchar(z))z else character();if(is.environment(cache))assign(key,result,envir=cache);return(result)}
  starts<-c(1L,locs[,"end"]+1L); ends<-c(locs[,"start"]-1L,nchar(x)); keep<-starts<=ends
  starts<-starts[keep];ends<-ends[keep]; atomic<-character();ranges<-list()
  for(i in seq_along(starts)){z<-trimws(substr(x,starts[i],ends[i]));if(nzchar(z)&&(!length(atomic)||tail(atomic,1)!=z)){atomic<-c(atomic,z);ranges[[length(ranges)+1L]]<-c(starts[i],ends[i])}}
  grams<-character();n<-length(atomic);if(n>=2L)for(len in 2L:n)for(i in seq_len(n-len+1L))grams<-c(grams,trimws(substr(x,ranges[[i]][1],ranges[[i+len-1L]][2])))
  result<-unique(c(atomic,grams[nzchar(grams)]))
  if(is.environment(cache))assign(key,result,envir=cache)
  result
}
ratio_extract <- function(x,rule,known=character()) {
  scalar<-function(name,default=NA)if(name%in%names(rule))rule[[name]][1L]else default
  method<-as.character(scalar("Method","")); invert<-isTRUE(scalar("Invert",FALSE)); num<-den<-NA_character_
  if(method=="Regular Expressions") {
    z<-ratio_normalize_boundaries(scalar("NumBefore"),scalar("NumAfter"),scalar("DenBefore"),scalar("DenAfter"))
    if(isTRUE(z$skip)) return(NULL)
    num<-ratio_between(x,z$nb,z$na);den<-ratio_between(x,z$db,z$da)
  } else if(method=="Position in String") {
    toks<-ratio_tokenize_exact(x,scalar("Separators"))
    at<-function(p)if(!is.na(p)&&p>=1L&&p<=length(toks))toks[[p]]else NA_character_
    num<-at(as.integer(scalar("NumPos")));den<-at(as.integer(scalar("DenPos")))
  } else if(method=="Pattern Recognition") {
    return(datawizard_resolve_known_sample_ratio(x,known,invert=invert))
  }
  if(invert&&!is.na(num)&&!is.na(den)){tmp<-num;num<-den;den<-tmp}
  if(is.na(num)&&is.na(den))NULL else list(numerator=num,denominator=den)
}

# Derive auditable source-character ranges for values claimed by already
# selected condition and ratio rules.  This is deliberately separate from rule
# inference: it may reject a span, but can never alter a selected rule or its
# score.  Start/End are R character offsets (not UTF-8 byte offsets).
auto_regex_semantic_spans <- function(metadata, condition_rules,
                                      condition_diagnostics,
                                      ratio_rules, ratio_diagnostics) {
  fields <- c("Row", "Content", "Semantic", "Reference", "Method", "Start", "End",
    "LeftDelimiter", "RightDelimiter", "ReplacementAtom", "SafeToGeneralize",
    "ExtractionConfirmed", "Diagnostic")
  empty <- data.frame(Row=integer(), Content=character(), Semantic=character(),
    Reference=character(), Method=character(), Start=integer(), End=integer(),
    LeftDelimiter=character(), RightDelimiter=character(), ReplacementAtom=character(),
    SafeToGeneralize=logical(), ExtractionConfirmed=logical(), Diagnostic=character(),
    stringsAsFactors=FALSE, check.names=FALSE)
  if (!is.data.frame(metadata) || !"Column" %in% names(metadata)) return(empty)
  metadata <- auto_regex_normalize_metadata(metadata)
  condition_rules <- if (is.data.frame(condition_rules)) condition_rules else empty_condition()
  ratio_rules <- if (is.data.frame(ratio_rules)) ratio_rules else empty_ratio()
  known <- known_samples_after_conditions(metadata)
  result <- list()

  fixed_range <- function(source, value) {
    if (!nzchar(value) || identical(value, "<NA>")) return(list(ok=FALSE, reason="reference is empty"))
    hits <- gregexpr(value, source, fixed=TRUE, useBytes=FALSE)[[1L]]
    hits <- hits[hits > 0L]
    if (length(hits) != 1L) return(list(ok=FALSE,
      reason=if (length(hits)) "reference occurs more than once" else "reference is absent from source"))
    list(ok=TRUE, start=hits[[1L]], end=hits[[1L]] + nchar(value, type="chars") - 1L)
  }
  exact_token_ranges <- function(source, separators=NA_character_) {
    # Keep this in lockstep with ratio_tokenize_exact(): the returned value
    # order (atomic tokens followed by n-grams) is part of Position semantics.
    sep <- if (length(separators) && !is.na(separators) && nzchar(separators))
      separators else "\\s+|\\(|\\)|\\[|\\]|\\{|\\}|/|_|-"
    locs <- tryCatch(stringr::str_locate_all(source,paste0("(",sep,")"))[[1L]],
      error=function(e) matrix(numeric(),ncol=2L))
    if (!nrow(locs)) {
      value <- trimws(source); if (!nzchar(value)) return(data.frame())
      start <- regexpr(value, source, fixed=TRUE, useBytes=FALSE)[[1L]]
      return(data.frame(Value=value,Start=start,End=start+nchar(value,type="chars")-1L))
    }
    starts <- c(1L,locs[,"end"]+1L); ends <- c(locs[,"start"]-1L,nchar(source,type="chars"))
    keep <- starts<=ends; starts<-starts[keep]; ends<-ends[keep]
    atoms <- list()
    for (i in seq_along(starts)) {
      raw <- substr(source,starts[i],ends[i]); value <- trimws(raw)
      if (!nzchar(value) || (length(atoms) && tail(atoms,1L)[[1L]]$Value==value)) next
      lead <- nchar(raw,type="chars")-nchar(sub("^[[:space:]]+","",raw),type="chars")
      trail <- nchar(raw,type="chars")-nchar(sub("[[:space:]]+$","",raw),type="chars")
      atoms[[length(atoms)+1L]] <- list(Value=value,Start=starts[i]+lead,End=ends[i]-trail)
    }
    grams <- list(); n <- length(atoms)
    if (n>=2L) for (len in 2L:n) for (i in seq_len(n-len+1L)) {
      raw <- substr(source,atoms[[i]]$Start,atoms[[i+len-1L]]$End); value <- trimws(raw)
      if (nzchar(value)) grams[[length(grams)+1L]] <- list(Value=value,
        Start=atoms[[i]]$Start,End=atoms[[i+len-1L]]$End)
    }
    all <- c(atoms,grams); if (!length(all)) return(data.frame())
    answer <- do.call(rbind,lapply(all,as.data.frame,stringsAsFactors=FALSE))
    answer[!duplicated(answer$Value),,drop=FALSE]
  }
  regex_boundary_range <- function(source, before, after) {
    start <- 1L; finish <- nchar(source,type="chars")
    if (!is.na(before) && nzchar(before)) {
      left <- tryCatch(stringr::str_locate(source,before)[1L,],error=function(e)c(NA,NA))
      if (is.na(left[[1L]])) return(list(ok=FALSE,reason="normalized left boundary is absent"))
      start <- left[[2L]]+1L
    }
    if (!is.na(after) && nzchar(after)) {
      right <- tryCatch(stringr::str_locate(substr(source,start,nchar(source,type="chars")),after)[1L,],
        error=function(e)c(NA,NA))
      if (is.na(right[[1L]])) return(list(ok=FALSE,reason="normalized right boundary is absent"))
      finish <- start+right[[1L]]-2L
    }
    raw <- substr(source,start,finish)
    lead <- nchar(raw,type="chars")-nchar(sub("^[[:space:]]+","",raw),type="chars")
    trail <- nchar(raw,type="chars")-nchar(sub("[[:space:]]+$","",raw),type="chars")
    start <- start+lead; finish <- finish-trail
    if (finish<start) list(ok=FALSE,reason="normalized boundaries select an empty range")
    else list(ok=TRUE,start=start,end=finish)
  }
  delimiters_and_atom <- function(source, start, end) {

    lex <- tokens(source)

    inside <- which(
      lex$Start <= end &
        lex$End >= start
    )

    if (!length(inside)) {
      return(list(
        left = "",
        right = "",
        atom = "",
        reason = "semantic span does not overlap a source token"
      ))
    }

    first <- min(inside)
    last <- max(inside)

    left <- if (
      first > 1L &&
      lex$End[[first - 1L]] == start - 1L
    ) {
      lex$Text[[first - 1L]]
    } else {
      ""
    }

    right <- if (
      last < nrow(lex) &&
      lex$Start[[last + 1L]] == end + 1L
    ) {
      lex$Text[[last + 1L]]
    } else {
      ""
    }

    # Never generalize a substring taken from inside a larger token.
    #
    # Examples:
    #
    #   "Ratio ERU C"  -> ERU is a complete token: allowed
    #   "RatioERUC"    -> ERU cuts through RatioERUC: rejected
    token_aligned <-
      lex$Start[[first]] == start &&
      lex$End[[last]] == end

    if (!token_aligned) {
      return(list(
        left = left,
        right = right,
        atom = "",
        reason = "semantic range cuts through a source token"
      ))
    }

    value <- substr(
      source,
      start,
      end
    )

    if (!nzchar(value) ||
        !grepl(
          "[[:alnum:]]",
          value,
          perl = TRUE
        )) {
      return(list(
        left = left,
        right = right,
        atom = "",
        reason = "semantic span contains no alphanumeric value to generalize"
      ))
    }

    atom <- ""
    reason <- "no safe semantic replacement atom"

    # Preserve the existing broad delimiter-aware representations.
    if (identical(left, "(") &&
        identical(right, ")")) {

      atom <- "[^()]+"
      reason <- "semantic extent is fixed by parentheses"

    } else if (identical(left, "[") &&
               identical(right, "]")) {

      atom <- "[^\\[\\]]+"
      reason <- "semantic extent is fixed by brackets"

    } else if (identical(left, "{") &&
               identical(right, "}")) {

      atom <- "[^{}]+"
      reason <- "semantic extent is fixed by braces"

    } else {

      # For token-aligned semantic values, preserve observed punctuation and
      # whitespace structure while replacing biological alphanumeric runs by
      # flexible character classes.
      #
      # ERU       -> [[:alpha:]]+
      # C         -> [[:alpha:]]+
      # F2        -> [[:alpha:]]+[[:digit:]]+
      # F2_ERU    -> [[:alpha:]]+[[:digit:]]+_[[:alpha:]]+
      chars <- strsplit(
        value,
        "",
        fixed = TRUE
      )[[1L]]

      shape <- vapply(
        chars,
        function(ch) {

          if (grepl(
            "^[[:alpha:]]$",
            ch,
            perl = TRUE
          )) {
            return("[[:alpha:]]+")
          }

          if (grepl(
            "^[[:digit:]]$",
            ch,
            perl = TRUE
          )) {
            return("[[:digit:]]+")
          }

          if (grepl(
            "^[[:space:]]$",
            ch,
            perl = TRUE
          )) {
            return("\\s+")
          }

          regex_escape_literal(ch)
        },
        character(1)
      )

      keep <- c(
        TRUE,
        shape[-1L] !=
          shape[-length(shape)]
      )

      atom <- paste0(
        shape[keep],
        collapse = ""
      )

      validation <- validate_pcre(
        atom
      )

      if (!isTRUE(validation$valid)) {
        atom <- ""
      }

      if (nzchar(atom)) {
        reason <- paste(
          "token-aligned semantic extent",
          "was generalized by observed character shape"
        )
      }
    }

    list(
      left = left,
      right = right,
      atom = atom,
      reason = reason
    )
  }
  add <- function(row, semantic, reference, method, confirmed, located=NULL, evidence="") {
    source <- metadata$Column[[row]]; loc <- if(is.null(located)) fixed_range(source,reference) else located
    bounded <- if(isTRUE(loc$ok)) delimiters_and_atom(source,loc$start,loc$end) else
      list(left="",right="",atom="",reason=loc$reason)
    safe <- isTRUE(loc$ok) && isTRUE(confirmed) && nzchar(bounded$atom)
    diagnostic <- if (safe) paste("accepted:", evidence, bounded$reason) else paste("rejected:",
      if(!isTRUE(loc$ok)) loc$reason else if(!isTRUE(confirmed)) evidence else bounded$reason)
    result[[length(result)+1L]] <<- data.frame(Row=row,Content=metadata$Content[[row]],
      Semantic=semantic,Reference=reference,Method=method,
      Start=if(isTRUE(loc$ok))loc$start else NA_integer_,End=if(isTRUE(loc$ok))loc$end else NA_integer_,
      LeftDelimiter=bounded$left,RightDelimiter=bounded$right,ReplacementAtom=bounded$atom,
      SafeToGeneralize=safe,ExtractionConfirmed=isTRUE(confirmed),Diagnostic=diagnostic,
      stringsAsFactors=FALSE,check.names=FALSE)
  }

  if (nrow(condition_rules) && "Options" %in% names(metadata)) for (i in seq_len(nrow(condition_rules))) {
    rule <- condition_rules[i,,drop=FALSE]; rows <- which(metadata$Content==rule$Content)
    for (row in rows) {
      reference <- metadata$Options[[row]]
      if (metadata$Content[[row]] %in% AUTO_ASSIGNED_RATIO_OPTIONS_CONTENT && reference=="Ratio")
        add(row,"condition",reference,rule$Method,FALSE,evidence="automatically assigned Ratio is not semantic")
      else {
        extracted <- extract_condition(metadata$Column[[row]],rule$Method,rule$Before,rule$After,rule$Separators,rule$Pos)[[1L]]
        add(row,"condition",reference,rule$Method,!is.na(extracted)&&identical(extracted,reference),
          evidence=if(!is.na(extracted)&&identical(extracted,reference)) "selected condition rule extracts the located value;" else "selected condition rule does not extract the reference")
      }
    }
  }
  if (nrow(ratio_rules)) for (i in seq_len(nrow(ratio_rules))) {
    rule <- ratio_rules[i,,drop=FALSE]; rows <- which(metadata$Content==rule$Content)
    for (row in rows) {
      extracted <- ratio_extract(metadata$Column[[row]],rule,known); source <- metadata$Column[[row]]
      for (semantic in c("numerator","denominator")) {
        reference <- if(semantic=="numerator") metadata$Numerator[[row]] else metadata$Denominator[[row]]
        predicted <- if(is.null(extracted)) NA_character_ else extracted[[semantic]]
        loc <- fixed_range(source,reference)
        evidence <- "selected ratio rule does not extract the reference"
        if (isTRUE(loc$ok) && rule$Method=="Position in String") {
          ranges <- exact_token_ranges(source,rule$Separators)
          position <- if(semantic=="numerator") rule$NumPos else rule$DenPos
          if(isTRUE(rule$Invert)) position <- if(semantic=="numerator") rule$DenPos else rule$NumPos
          mapped <- if(!is.na(position)&&position>=1L&&position<=nrow(ranges)) ranges[position,,drop=FALSE] else NULL
          confirmed <- !is.null(mapped) && mapped$Value==reference && mapped$Start==loc$start && mapped$End==loc$end
          evidence <- if(confirmed) "exact ratio tokenizer position maps to the unique source range;" else "ratio tokenizer position does not map to the unique source range"
        } else if (isTRUE(loc$ok) && rule$Method=="Regular Expressions") {
          normalized <- ratio_normalize_boundaries(rule$NumBefore,rule$NumAfter,
            rule$DenBefore,rule$DenAfter)
          if (isTRUE(normalized$skip)) mapped <- list(ok=FALSE,reason="normalized boundaries are incomplete")
          else {
            raw_semantic <- if(isTRUE(rule$Invert))
              if(semantic=="numerator") "denominator" else "numerator" else semantic
            mapped <- if(raw_semantic=="numerator") regex_boundary_range(source,normalized$nb,normalized$na)
              else regex_boundary_range(source,normalized$db,normalized$da)
          }
          confirmed <- isTRUE(mapped$ok) && mapped$start==loc$start && mapped$end==loc$end &&
            !is.na(predicted) && identical(predicted,reference)
          evidence <- if(confirmed) "normalized regex boundaries map extraction to the unique source range;" else "normalized regex boundaries do not map extraction to the unique source range"
        } else {
          confirmed <- isTRUE(loc$ok) && !is.na(predicted) && identical(predicted,reference)
          if(confirmed) evidence <- "Pattern Recognition maps its selected value uniquely to source;"
        }
        add(row,semantic,reference,rule$Method,confirmed,loc,evidence)
      }
    }
  }
  if (!length(result)) empty else do.call(rbind,result)[,fields,drop=FALSE]
}

# A source template is deliberately constructed before any regex escaping takes
# place.  Literal and semantic nodes therefore retain character offsets into the
# original source, making it impossible for a later stage to edit an escaped
# regex string and accidentally consume punctuation belonging to a neighbour.
auto_regex_source_template <- function(source, spans = data.frame()) {
  source <- chr(source)[[1L]]
  required <- c("Start", "End", "Semantic", "Reference", "ReplacementAtom")
  if (!is.data.frame(spans) || !nrow(spans)) spans <- data.frame()
  if (nrow(spans) && !all(required %in% names(spans)))
    stop(sprintf("Semantic spans require fields: %s.", paste(required, collapse=", ")), call.=FALSE)
  if (nrow(spans) && "SafeToGeneralize" %in% names(spans))
    spans <- spans[!is.na(spans$SafeToGeneralize) & spans$SafeToGeneralize,,drop=FALSE]
  size <- nchar(source, type="chars")
  if (nrow(spans)) {
    spans$Start <- as.integer(spans$Start); spans$End <- as.integer(spans$End)
    invalid <- is.na(spans$Start) | is.na(spans$End) | spans$Start < 1L |
      spans$End < spans$Start | spans$End > size
    if (any(invalid)) stop("Semantic span offsets fall outside the original source.", call.=FALSE)
    spans <- spans[order(spans$Start, spans$End, method="radix"),,drop=FALSE]
    if (nrow(spans) > 1L && any(spans$Start[-1L] <= spans$End[-nrow(spans)]))
      stop("Semantic spans overlap; a source character may have only one template owner.", call.=FALSE)
  }
  nodes <- list(); add <- function(type, start, end, text, semantic="", atom="") {
    nodes[[length(nodes)+1L]] <<- data.frame(Type=type,Start=as.integer(start),End=as.integer(end),
      Text=text,Semantic=semantic,ReplacementAtom=atom,stringsAsFactors=FALSE,check.names=FALSE)
  }
  cursor <- 1L
  if (nrow(spans)) for (i in seq_len(nrow(spans))) {
    if (cursor < spans$Start[[i]]) add("literal",cursor,spans$Start[[i]]-1L,
      substr(source,cursor,spans$Start[[i]]-1L))
    observed <- substr(source,spans$Start[[i]],spans$End[[i]])
    if (!identical(observed,chr(spans$Reference[[i]])[[1L]]))
      stop("Semantic span reference does not equal its original source range.", call.=FALSE)
    add("semantic",spans$Start[[i]],spans$End[[i]],observed,
      chr(spans$Semantic[[i]])[[1L]],chr(spans$ReplacementAtom[[i]])[[1L]])
    cursor <- spans$End[[i]]+1L
  }
  if (cursor <= size) add("literal",cursor,size,substr(source,cursor,size))
  if (!length(nodes) && !nzchar(source)) add("literal",1L,0L,"")
  structure(list(source=source,nodes=do.call(rbind,nodes)),class="auto_regex_source_template")
}

.render_source_template_nodes <- function(nodes) {
  if (!nrow(nodes)) return("")
  atoms <- vapply(seq_len(nrow(nodes)), function(i) {
    if (nodes$Type[[i]] == "literal")
      return(regex_to_miraprot_storage(regex_atom_for_token(nodes$Text[[i]]),"content"))
    atom <- nodes$ReplacementAtom[[i]]
    # Semantic nodes may use only the atom approved by WP1.  In particular,
    # unrestricted dots are never synthesized or accepted here.
    if (!nzchar(atom) || grepl("(^|[^\\\\])\\.",atom,perl=TRUE) ||
        !validate_pcre(atom)$valid) stop("Semantic span has an unsafe replacement atom.",call.=FALSE)
    regex_to_miraprot_storage(atom,"content")
  },character(1))
  paste0(atoms,collapse="")
}

render_auto_regex_source_template <- function(template) {
  if (!inherits(template,"auto_regex_source_template")) stop("Expected a source template.",call.=FALSE)
  .render_source_template_nodes(template$nodes)
}

# Produce the small, auditable family consumed by content scoring.  `spans`
# uses WP1 Row offsets; callers may instead supply Source (1-based in `sources`).
# Anchoring is a subsequent independent transformation and its decisions are
# retained alongside the unanchored candidate provenance.
auto_regex_generalized_content_candidates <- function(sources, spans, negatives=character(),
    original_rule="", include_limit=GENERALIZED_INCLUDE_LIMIT,
    exclusion_limit=GENERALIZED_EXCLUDE_LIMIT,
    combination_limit=GENERALIZED_COMBINATION_LIMIT) {
  sources <- chr(sources); negatives <- chr(negatives)
  if (!is.data.frame(spans)) stop("spans must be a data frame.",call.=FALSE)
  index_field <- if ("Source" %in% names(spans)) "Source" else if ("Row" %in% names(spans)) "Row" else ""
  if (!nzchar(index_field) && nrow(spans)) stop("spans require a Row or Source field.",call.=FALSE)
  templates <- lapply(seq_along(sources),function(i) auto_regex_source_template(sources[[i]],
    if(nrow(spans))spans[spans[[index_field]]==i,,drop=FALSE] else spans))
  node_keys <- lapply(templates,function(z)paste(z$nodes$Type,z$nodes$Text,z$nodes$Semantic,
    z$nodes$ReplacementAtom,sep="\034"))
  common_edge <- function(end=FALSE) {
    widths<-lengths(node_keys); if(!length(widths)||!min(widths))return(0L)
    answer<-0L
    for(j in seq_len(min(widths))) {
      values<-mapply(function(keys,w)keys[[if(end)w-j+1L else j]],node_keys,widths,USE.NAMES=FALSE)
      if(length(unique(values))!=1L)break
      answer<-j
    }
    answer
  }
  records <- list(); add <- function(pattern,family,node_rows) {
    if(!nzchar(pattern)||!validate_pcre(regex_from_miraprot_storage(pattern,"content"))$valid)return()
    semantic <- unique(chr(node_rows$Semantic[node_rows$Type=="semantic"])); semantic<-semantic[nzchar(semantic)]
    removed <- unique(chr(node_rows$Text[node_rows$Type=="semantic"])); removed<-removed[nzchar(removed)]
    records[[length(records)+1L]] <<- data.frame(Pattern=pattern,Family=family,
      OriginalRule=chr(original_rule)[[1L]],SemanticRoles=paste(semantic,collapse=","),
      RemovedLiteralValues=paste(removed,collapse="\034"),stringsAsFactors=FALSE,check.names=FALSE)
  }
  for(z in templates) if(any(z$nodes$Type=="semantic"))
    add(render_auto_regex_source_template(z),"full_generalized_template",z$nodes)
  pre<-common_edge(FALSE); suf<-common_edge(TRUE)
  if(pre) { n<-templates[[1L]]$nodes[seq_len(pre),,drop=FALSE]; add(.render_source_template_nodes(n),"stable_prefix",n) }
  if(suf) { n<-templates[[1L]]$nodes[rev(seq_len(suf)),,drop=FALSE]; add(.render_source_template_nodes(n),"stable_suffix",n) }
  for(z in templates) for(i in which(z$nodes$Type=="semantic")) {
    take<-seq.int(max(1L,i-1L),min(nrow(z$nodes),i+1L)); n<-z$nodes[take,,drop=FALSE]
    add(.render_source_template_nodes(n),"partial_structural_template",n)
  }
  if(!length(records)) records_df<-data.frame() else {
    records_df<-do.call(rbind,records)
    # Equivalent templates share one search slot, but their provenance is the
    # union of all semantic values removed to obtain that pattern.
    records_df<-do.call(rbind,lapply(split(seq_len(nrow(records_df)),records_df$Pattern),function(rows) {
      answer<-records_df[rows[[1L]],,drop=FALSE]
      collect<-function(field,separator) {
        values<-unlist(strsplit(chr(records_df[[field]][rows]),separator,fixed=TRUE),use.names=FALSE)
        paste(unique(values[nzchar(values)]),collapse=separator)
      }
      answer$SemanticRoles<-collect("SemanticRoles",",")
      answer$RemovedLiteralValues<-collect("RemovedLiteralValues","\034")
      answer
    }))
    records_df<-records_df[order(match(records_df$Family,c("full_generalized_template",
      "stable_prefix","stable_suffix","partial_structural_template")),records_df$Pattern,
      method="radix"),,drop=FALSE]
  }
  anchored <- if(nrow(records_df)) lapply(seq_len(nrow(records_df)),function(i)
    infer_anchors(list(pattern=records_df$Pattern[[i]]),sources,negatives)) else list()
  includes <- if(length(anchored)) head(do.call(rbind,lapply(seq_along(anchored),function(i) {
    cbind(records_df[i,,drop=FALSE],AnchoredPattern=anchored[[i]]$pattern,
      AnchorDecisions=paste(ifelse(anchored[[i]]$anchor_history$Accepted,"accepted","rejected"),
        anchored[[i]]$anchor_history$Transformation,sep=":",collapse=","),stringsAsFactors=FALSE)
  })),as.integer(include_limit)) else data.frame()
  exclusion_pool <- if(nrow(records_df)) records_df[vapply(records_df$Pattern,function(p)
    any(safe_grepl(p,negatives))&&!any(safe_grepl(p,sources)),logical(1)),,drop=FALSE] else data.frame()
  exclusions <- head(exclusion_pool,as.integer(exclusion_limit))
  combinations <- if(nrow(includes)) expand.grid(Include=seq_len(nrow(includes)),
    Exclude=0L: nrow(exclusions),KEEP.OUT.ATTRS=FALSE,stringsAsFactors=FALSE) else data.frame()
  combinations <- head(combinations,as.integer(combination_limit))
  list(templates=templates,includes=includes,exclusions=exclusions,combinations=combinations,
    limits=c(includes=as.integer(include_limit),exclusions=as.integer(exclusion_limit),
      combinations=as.integer(combination_limit)))
}

# Replace a confirmed semantic value with a deterministic, delimiter-safe value.
# These rows are prospective checks only: they never participate in scoring or
# reliability calculations.
auto_regex_prospective_semantic_sources <- function(sources, spans) {
  sources <- chr(sources)
  if (!is.data.frame(spans) || !nrow(spans)) return(character())
  spans <- spans[!is.na(spans$SafeToGeneralize) & spans$SafeToGeneralize,,drop=FALSE]
  if (!nrow(spans)) return(character())
  index_field <- if ("Source" %in% names(spans)) "Source" else "Row"
  shape_compatible_replacement <- function(reference) {

    reference <- chr(reference)[[1L]]

    if (is.na(reference) ||
        !nzchar(reference)) {
      return(reference)
    }

    chars <- strsplit(
      reference,
      "",
      fixed = TRUE
    )[[1L]]

    mapped <- vapply(
      chars,
      function(ch) {

        if (grepl(
          "^[[:alpha:]]$",
          ch,
          perl = TRUE
        )) {

          # Always change the observed letter while remaining alphabetic.
          if (ch %in% c("X", "x")) {
            return("Y")
          }

          return("X")
        }

        if (grepl(
          "^[[:digit:]]$",
          ch,
          perl = TRUE
        )) {

          # Always change the observed digit while remaining numeric.
          if (identical(ch, "7")) {
            return("8")
          }

          return("7")
        }

        # Delimiters, punctuation and whitespace belong to the observed
        # structural shape and remain unchanged.
        ch
      },
      character(1)
    )

    paste0(
      mapped,
      collapse = ""
    )
  }
  result <- character()
  for (source_index in unique(as.integer(spans[[index_field]]))) {
    if (is.na(source_index) || source_index < 1L || source_index > length(sources)) next
    value <- sources[[source_index]]
    selected <- spans[as.integer(spans[[index_field]]) == source_index,,drop=FALSE]
    selected <- selected[order(selected$Start,decreasing=TRUE,method="radix"),,drop=FALSE]
    for (i in seq_len(nrow(selected))) {
      replacement <-
        shape_compatible_replacement(
          selected$Reference[[i]]
        )
      value <- paste0(substr(value,1L,selected$Start[[i]]-1L),replacement,
        substr(value,selected$End[[i]]+1L,nchar(value,type="chars")))
    }
    result <- c(result,value)
  }
  unique(result)
}

# Pure second-pass selector.  Candidate zero is always the accepted first-pass
# rule, and all evidence scores use the untouched Column vector.  A replacement
# is committed only after replaying the complete candidate table, so conflicts
# introduced by interactions between labels cannot be hidden by local scoring.
refine_content_with_semantic_spans <- function(metadata, content_table, spans) {
  stopifnot(is.data.frame(metadata), "Column" %in% names(metadata),
    identical(names(content_table),CONTENT_FIELDS), is.data.frame(spans))
  metric_text <- function(m) paste(sprintf("%s=%s",c("TP","FP","FN","Precision","Recall","F1"),
    format(unlist(m[1L,c("TP","FP","FN","Precision","Recall","F1")]),trim=TRUE,scientific=FALSE)),collapse=";")
  lineage <- data.frame(Content=character(),OriginalInclude=character(),OriginalExclude=character(),
    FinalInclude=character(),FinalExclude=character(),Accepted=logical(),RemovedValues=character(),
    OriginalMetrics=character(),FinalMetrics=character(),ProspectiveChecks=character(),
    RejectionReason=character(),stringsAsFactors=FALSE,check.names=FALSE)
  final <- content_table; x <- chr(metadata$Column); expected <- chr(metadata$Content)
  baseline_application <- apply_content_table(metadata,final)
  baseline_conflicts <- nrow(baseline_application$conflicts)
  for (rule_index in seq_len(nrow(final))) {
    label <- final$Content[[rule_index]]; truth <- expected==label
    original_include <- final$Include[[rule_index]]; original_exclude <- final$Exclude[[rule_index]]
    original_metric <- score_pattern(original_include,x,truth,original_exclude,
      constraint_count=1L+nzchar(original_exclude))
    label_spans <- spans[spans$Content==label & !is.na(spans$SafeToGeneralize) &
      spans$SafeToGeneralize,,drop=FALSE]
    reason <- "no SafeToGeneralize semantic span"; chosen <- NULL; prospective <- character()
    if (nrow(label_spans)) {
      positive_rows <- which(truth); local <- label_spans
      local$Source <- match(as.integer(local$Row),positive_rows)
      local <- local[!is.na(local$Source),,drop=FALSE]
      generated <- auto_regex_generalized_content_candidates(x[truth],local,x[!truth],original_include)
      candidate_rows <- data.frame(Include=original_include,Exclude=original_exclude,
        RemovedLiteralValues="",Family="original",stringsAsFactors=FALSE)
      if (nrow(generated$includes)) {
        excludes <- unique(c(original_exclude,"",
          if(nrow(generated$exclusions))generated$exclusions$Pattern else character()))
        additions <- do.call(rbind,lapply(seq_len(nrow(generated$includes)),function(i)
          data.frame(Include=generated$includes$AnchoredPattern[[i]],Exclude=excludes,
            RemovedLiteralValues=generated$includes$RemovedLiteralValues[[i]],
            Family=generated$includes$Family[[i]],stringsAsFactors=FALSE)))
        candidate_rows <- unique(rbind(candidate_rows,additions))
      }
      prospective <- auto_regex_prospective_semantic_sources(x[truth],local)
      evaluated <- list(); rejected <- character()
      for (candidate_index in seq_len(nrow(candidate_rows))) {
        z <- candidate_rows[candidate_index,,drop=FALSE]
        validations <- c(validate_pcre(regex_from_miraprot_storage(z$Include,"content"))$valid,
          validate_pcre(regex_from_miraprot_storage(z$Exclude,"content"))$valid)
        m <- score_pattern(z$Include,x,truth,z$Exclude,constraint_count=1L+nzchar(z$Exclude))
        normalized_include <- regex_to_miraprot_storage(regex_from_miraprot_storage(z$Include,"content"),"content")
        normalized_exclude <- regex_to_miraprot_storage(regex_from_miraprot_storage(z$Exclude,"content"),"content")
        replay <- score_pattern(normalized_include,x,truth,normalized_exclude,
          constraint_count=1L+nzchar(normalized_exclude))
        candidate_table <- final; candidate_table$Include[[rule_index]] <- normalized_include
        candidate_table$Exclude[[rule_index]] <- normalized_exclude
        application <- apply_content_table(metadata,candidate_table)
        prospective_ok <- length(prospective)>0L && all(safe_grepl(normalized_include,prospective) &
          (!nzchar(normalized_exclude) | !safe_grepl(normalized_exclude,prospective)))
        gates <- c(pcre=all(validations),positives=m$TP>=original_metric$TP && m$FN<=original_metric$FN,
          false_positives=m$FP<=original_metric$FP,conflicts=nrow(application$conflicts)<=baseline_conflicts,
          replay=identical(as.integer(m[1L,c("TP","FP","FN")]),as.integer(replay[1L,c("TP","FP","FN")])),
          prospective=candidate_index==1L || prospective_ok)
        if (!all(gates)) { rejected <- c(rejected,paste(names(gates)[!gates],collapse=",")); next }
        removed <- strsplit(z$RemovedLiteralValues,"\034",fixed=TRUE)[[1L]]
        removed <- removed[nzchar(removed)]
        safe_classes <- lengths(regmatches(z$Include,gregexpr("\\[\\^[^]]+\\]\\+",z$Include,perl=TRUE)))
        evaluated[[length(evaluated)+1L]] <- cbind(z,m,RemovedCount=length(unique(removed)),
          SafeClasses=safe_classes,Constraints=1L+nzchar(z$Exclude),RepresentationLength=nchar(z$Include)+nchar(z$Exclude),
          CandidateIndex=candidate_index,stringsAsFactors=FALSE)
      }
      if (length(evaluated)) {
        pool <- do.call(rbind,evaluated)
        pool <- pool[order(-pool$RemovedCount,-pool$SafeClasses,pool$Constraints,pool$Complexity,
          pool$RepresentationLength,pool$Include,pool$Exclude,method="radix"),,drop=FALSE]
        chosen <- pool[1L,,drop=FALSE]
        reason <- if(chosen$CandidateIndex==1L) paste0("first-pass retained; ",
          if(length(rejected))paste(unique(rejected),collapse=" | ") else "no superior candidate") else ""
      } else reason <- paste0("all refinements rejected: ",paste(unique(rejected),collapse=" | "))
    }
    if (is.null(chosen)) chosen <- cbind(data.frame(Include=original_include,Exclude=original_exclude,
      RemovedLiteralValues="",stringsAsFactors=FALSE),original_metric,CandidateIndex=1L)
    accepted <- !identical(chr(chosen$Include)[[1L]],original_include) ||
      !identical(chr(chosen$Exclude)[[1L]],original_exclude)
    if (accepted) { final$Include[[rule_index]] <- chr(chosen$Include)[[1L]]; final$Exclude[[rule_index]] <- chr(chosen$Exclude)[[1L]] }
    final_metric <- score_pattern(final$Include[[rule_index]],x,truth,final$Exclude[[rule_index]],
      constraint_count=1L+nzchar(final$Exclude[[rule_index]]))
    lineage <- rbind(lineage,data.frame(Content=label,OriginalInclude=original_include,
      OriginalExclude=original_exclude,FinalInclude=final$Include[[rule_index]],FinalExclude=final$Exclude[[rule_index]],
      Accepted=accepted,RemovedValues=gsub("\034",", ",chr(chosen$RemovedLiteralValues)[[1L]],fixed=TRUE),
      OriginalMetrics=metric_text(original_metric),FinalMetrics=metric_text(final_metric),
      ProspectiveChecks=if(length(prospective))paste(prospective,collapse=" | ") else "not applicable",
      RejectionReason=reason,stringsAsFactors=FALSE,check.names=FALSE))
    baseline_conflicts <- nrow(apply_content_table(metadata,final)$conflicts)
  }
  list(table=final,lineage=lineage,application=apply_content_table(metadata,final))
}

known_samples_after_conditions <- function(metadata) {
  # Data Wizard builds the recognition dictionary from the assignment column
  # after condition rules have run.  Keep this in one place: inference and the
  # exported-rule executor must never independently reinterpret Options.
  if (!is.data.frame(metadata) || !all(c("Content","Options") %in% names(metadata))) return(character())
  values <- trimws(chr(metadata$Options[is_sample_bearing_content(metadata$Content)]))
  unique(values[!is.na(values) & nzchar(values)])
}

ratio_diagnostics <- function(label,rows,xs,ns,ds,rule,known,content_targeted=TRUE) {
  got<-lapply(xs,ratio_extract,rule=rule,known=known)
  pn<-vapply(got,function(z)if(is.null(z))NA_character_ else z$numerator,character(1));pd<-vapply(got,function(z)if(is.null(z))NA_character_ else z$denominator,character(1))
  applicable<-nzchar(ns)&nzchar(ds);ok<-applicable&!is.na(pn)&!is.na(pd)&pn==ns&pd==ds
  reason<-ifelse(ok,"",ifelse(vapply(got,is.null,logical(1)),"No components extracted",ifelse(is.na(pn)|!nzchar(pn),"Numerator was not extracted",ifelse(is.na(pd)|!nzchar(pd),"Denominator was not extracted",ifelse(pn!=ns,"Numerator mismatch","Denominator mismatch")))))
  reason[!applicable]<-"Target pair is incomplete"
  data.frame(Content=label,Row=rows,Column=xs,Method=rule$Method,
    LocalNumerator=ifelse(is.na(pn),"",pn),LocalDenominator=ifelse(is.na(pd),"",pd),
    PredictedNumerator=ifelse(is.na(pn),"",pn),ExpectedNumerator=ns,
    PredictedDenominator=ifelse(is.na(pd),"",pd),ExpectedDenominator=ds,
    ExpectedComponents=paste(ns,ds,sep=" / "),KnownSampleCount=length(known),
    ApplicableContentTargeted=rep(content_targeted,length(rows)),
    ReplayNumerator="",ReplayDenominator="",ReplaySuccess=FALSE,ReplayFailureReason="Not replayed",
    Applicable=applicable,Success=ok,FailureReason=reason,stringsAsFactors=FALSE)
}
