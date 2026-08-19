# ============================================================================
# Auto Regex Content candidate construction, scoring, and bounded search.
# Sourced after regex primitives into the same caller environment.
# ============================================================================

CONTENT_MIN_F1 <- 0.8
CONTENT_MIN_RECALL <- 0.8
CANDIDATE_FRAGMENT_SEARCH_LIMIT <- 250L
CANDIDATE_FAMILY_LIMIT <- 60L
REFINEMENT_FRONTIER_LIMIT <- 40L
REFINEMENT_CANDIDATE_LIMIT <- 400L
REFINEMENT_DEPTH_LIMIT <- 6L

# ---- content candidates, scoring, redundancy, anchors ----------------------
# Lossless structural lexer.  In contrast to the old word splitter, this is a
# stable representation of the input: whitespace and punctuation are data,
# delimiters remain separate boundary spans, and Text is never normalized.
TOKEN_PUNCTUATION <- c(
  "."="period", ","="comma", ":"="colon", ";"="semicolon",
  "_"="underscore", "-"="hyphen", "/"="slash", "\\"="backslash",
  "="="equals", "+"="plus", "*"="asterisk", "&"="ampersand",
  "|"="pipe", "!"="exclamation", "?"="question", "%"="percent",
  "#"="hash", "@"="at", "$"="dollar", "^"="caret", "~"="tilde",
  "'"="apostrophe", "\""="quote", "`"="backtick", "<"="less_than",
  ">"="greater_than"
)
TOKEN_DELIMITERS <- c(
  "("="paren_open", ")"="paren_close", "["="bracket_open",
  "]"="bracket_close", "{"="brace_open", "}"="brace_close"
)

.format_special_character <- function(value) {
  names <- c("."="period", "/"="slash", "_"="underscore", "["="left bracket",
    ","="comma", ":"="colon", ";"="semicolon", "-"="hyphen", "\\"="backslash",
    "("="left parenthesis", ")"="right parenthesis", "]"="right bracket",
    "{"="left brace", "}"="right brace")
  value <- chr(value)[[1L]]
  name <- unname(names[value])
  if (length(name) && !is.na(name)) sprintf("%s (%s)", name, value)
  else sprintf("character %s", encodeString(value, quote="\""))
}

.token_base_shape <- function(text) {
  if (grepl("^[[:alpha:]]+$", text, perl=TRUE)) return("letters")
  if (grepl("^[[:digit:]]+$", text, perl=TRUE)) return("digits")
  if (grepl("^[[:alpha:]]+[[:digit:]]+$", text, perl=TRUE)) return("prefix_numeric_suffix")
  if (grepl("^[[:digit:]]+[[:alpha:]]+$", text, perl=TRUE)) return("numeric_prefix_letters")
  "mixed_identifier"
}

tokens <- function(x) {
  source <- chr(x)
  columns <- list(Source=integer(), Span=integer(), Start=integer(), End=integer(),
                  Text=character(), Type=character(), Shape=character(),
                  BaseShape=character(), Normalized=character(),
                  Parenthesized=logical(), ReplicateLike=logical(),
                  ConditionLike=logical())
  empty <- as.data.frame(columns, stringsAsFactors=FALSE, check.names=FALSE)
  lex_one <- function(value, source_index) {
    chars <- strsplit(value, "", fixed=TRUE)[[1L]]
    if (!length(chars)) return(empty)
    rows <- vector("list", length(chars)); row_count <- 0L; i <- 1L
    while (i <= length(chars)) {
      char <- chars[[i]]
      is_space <- grepl("^[[:space:]]$", char, perl=TRUE)
      is_word <- grepl("^[[:alnum:]]$", char, perl=TRUE)
      if (is_space || is_word) {
        j <- i
        while (j < length(chars) && grepl(if (is_space) "^[[:space:]]$" else "^[[:alnum:]]$",
                                          chars[[j+1L]], perl=TRUE)) j <- j+1L
        text <- paste0(chars[i:j], collapse="")
        type <- if (is_space) "whitespace" else "identifier"
        base_shape <- if (is_space) "whitespace" else .token_base_shape(text)
      } else {
        j <- i; text <- char
        if (char %in% names(TOKEN_DELIMITERS)) {
          type <- unname(TOKEN_DELIMITERS[[char]]); base_shape <- "delimiter"
        } else if (char %in% names(TOKEN_PUNCTUATION)) {
          type <- unname(TOKEN_PUNCTUATION[[char]]); base_shape <- "punctuation"
        } else {
          type <- "unknown"; base_shape <- "unknown"
        }
      }
      row_count <- row_count+1L
      rows[[row_count]] <- data.frame(Source=source_index, Span=row_count,
        Start=i, End=j, Text=text, Type=type, Shape=base_shape,
        BaseShape=base_shape, Normalized=tolower(text), Parenthesized=FALSE,
        ReplicateLike=FALSE, ConditionLike=FALSE, stringsAsFactors=FALSE,
        check.names=FALSE)
      i <- j+1L
    }
    do.call(rbind, rows[seq_len(row_count)])
  }
  result <- do.call(rbind, lapply(seq_along(source), function(i) lex_one(source[[i]], i)))
  if (is.null(result) || !nrow(result)) return(empty)
  rownames(result) <- NULL

  # These contextual shapes annotate one span without consuming its delimiter
  # neighbours.  Thus every character continues to have exactly one owner.
  by_source <- split(seq_len(nrow(result)), result$Source)
  for (indices in by_source) {
    if (length(indices) >= 3L) for (position in 2:(length(indices)-1L)) {
      row <- indices[[position]]
      if (result$Type[row] == "identifier" &&
          result$Type[indices[[position-1L]]] == "paren_open" &&
          result$Type[indices[[position+1L]]] == "paren_close") {
        result$Parenthesized[row] <- TRUE
        result$Shape[row] <- "parenthesized_value"
      }
    }
    identifier_rows <- indices[result$Type[indices] == "identifier"]
    replicate_rows <- identifier_rows[grepl("^(?:rep(?:licate)?|r)[[:digit:]]+$",
      result$Normalized[identifier_rows], perl=TRUE)]
    result$ReplicateLike[replicate_rows] <- TRUE
    result$Shape[replicate_rows] <- "replicate_like"
  }

  # Learn condition candidates from variation in structurally aligned training
  # strings.  There is deliberately no vocabulary of biological conditions:
  # only a varying value at an otherwise matching span position is evidence.
  if (length(by_source) > 1L && length(unique(lengths(by_source))) == 1L) {
    width <- lengths(by_source)[[1L]]
    for (position in seq_len(width)) {
      aligned <- vapply(by_source, function(indices) indices[[position]], integer(1))
      same_type <- length(unique(result$Type[aligned])) == 1L
      variable <- length(unique(result$Normalized[aligned])) > 1L
      eligible <- all(result$Type[aligned] == "identifier")
      if (same_type && variable && eligible) {
        result$ConditionLike[aligned] <- TRUE
        plain <- aligned[!result$Parenthesized[aligned] & !result$ReplicateLike[aligned]]
        result$Shape[plain] <- "condition_like"
      }
    }
  }
  result
}
common_values <- function(xs) Reduce(intersect, lapply(xs, unique))

.candidate_token_rows <- function(values) {
  lapply(seq_along(values), function(i) {
    z <- tokens(values[[i]]); z[z$Source == 1L,,drop=FALSE]
  })
}

.shape_atom <- function(shape) switch(shape,
  letters="[[:alpha:]]+", digits="[[:digit:]]+",
  prefix_numeric_suffix="[[:alpha:]]+[[:digit:]]+",
  numeric_prefix_letters="[[:digit:]]+[[:alpha:]]+",
  replicate_like="[[:alpha:]]+[[:digit:]]+",
  mixed_identifier="[[:alnum:]]+", condition_like="[[:alnum:]]+", NULL)

# A literal is discriminative when its observed class support is high, its
# opposing support is materially lower, and erasing the term buys no recall.
# The final comparison is contextual, so a broad word atom is not allowed to
# replace a class-defining term merely because it has the same token type.
.protect_literal <- function(literal, generalized, pos, neg) {
  literal <- regex_to_miraprot_storage(regex_atom_for_token(literal), "content")
  lp <- safe_grepl(literal,pos); gp <- safe_grepl(generalized,pos)
  ln <- safe_grepl(literal,neg); gn <- safe_grepl(generalized,neg)
  mean(lp) >= .75 && (mean(lp) - if(length(ln)) mean(ln) else 0) >= .25 &&
    sum(gp) <= sum(lp) && sum(gn) > sum(ln)
}

.candidate_family_builders <- function(values, opposing=character()) {
  values <- unique(chr(values)); values <- values[nzchar(values)]
  if (!length(values)) return(list(structural=character(), shape=character(),
    partial=character(), concrete_token=character(), whole_header_literal=character()))
  seqs <- .candidate_token_rows(values)
  types <- lapply(seqs, `[[`, "Type")
  widths <- lengths(types); aligned <- length(unique(widths)) == 1L &&
    all(vapply(types[-1L], identical, logical(1), types[[1L]]))
  structural <- shape <- partial <- character()
  literal_atom <- function(x) regex_to_miraprot_storage(regex_atom_for_token(x), "content")
  atom_for_column <- function(column, shapes=FALSE) {
    text <- vapply(seqs, function(z) z$Text[[column]], character(1))
    type <- vapply(seqs, function(z) z$Type[[column]], character(1))
    base <- vapply(seqs, function(z) z$BaseShape[[column]], character(1))
    if (length(unique(text)) == 1L && !(shapes && all(type == "identifier")))
      return(literal_atom(text[[1L]]))
    if (all(type == "whitespace")) return(regex_atom_for_token(" ", evidence=text))
    # Stable punctuation is structural, never a wildcard.
    if (all(type != "identifier")) return(NULL)
    same_shape <- length(unique(base)) == 1L &&
      (length(unique(tolower(text))) > 1L || (shapes && length(text) == 1L))
    atom <- if (same_shape) .shape_atom(base[[1L]]) else NULL
    if (is.null(atom)) return(NULL)
    protected <- any(vapply(unique(text), .protect_literal, logical(1),
                            generalized=atom, pos=values, neg=opposing))
    if (protected && !shapes) return(paste0("(?:",paste(literal_atom(unique(text)),collapse="|"),")"))
    atom
  }
  if (aligned) {
    atoms <- lapply(seq_len(widths[[1L]]), atom_for_column)
    if (all(lengths(atoms) == 1L)) structural <- paste0(unlist(atoms),collapse="")
    shape_atoms <- lapply(seq_len(widths[[1L]]), atom_for_column, shapes=TRUE)
    if (all(lengths(shape_atoms) == 1L)) shape <- paste0(unlist(shape_atoms),collapse="")
    invariant <- which(vapply(seq_len(widths[[1L]]), function(j)
      length(unique(vapply(seqs,function(z)z$Text[[j]],character(1)))) == 1L, logical(1)))
    informative <- invariant[vapply(invariant,function(j) seqs[[1L]]$Type[[j]] == "identifier" &&
      nchar(seqs[[1L]]$Text[[j]]) >= 2L,logical(1))]
    partial <- literal_atom(vapply(informative,function(j)seqs[[1L]]$Text[[j]],character(1)))
  } else {
    # Relative-position alignment: preserve the longest common typed/literal
    # prefix and suffix and treat only the intervening span as optional.
    minw <- min(widths)
    compatible <- function(j, from_end=FALSE) {
      rows <- mapply(function(z,w) if(from_end) w-j+1L else j, seqs, widths)
      ts <- mapply(function(z,k) z$Type[[k]],seqs,rows,USE.NAMES=FALSE)
      tx <- mapply(function(z,k) z$Text[[k]],seqs,rows,USE.NAMES=FALSE)
      bs <- mapply(function(z,k) z$BaseShape[[k]],seqs,rows,USE.NAMES=FALSE)
      length(unique(ts)) == 1L && (length(unique(tx)) == 1L ||
        (from_end && ts[[1L]] == "identifier" && length(unique(bs)) == 1L) ||
        ts[[1L]] == "whitespace")
    }
    pre <- 0L; while(pre < minw && compatible(pre+1L)) pre <- pre+1L
    suf <- 0L; while(suf < minw-pre && compatible(suf+1L,TRUE)) suf <- suf+1L
    edge_atom <- function(j, end=FALSE) {

      positions <- mapply(
        function(z, w) {
          if (end) {
            w - j + 1L
          } else {
            j
          }
        },
        seqs,
        widths,
        USE.NAMES = FALSE
      )

      text <- mapply(
        function(z, k) z$Text[[k]],
        seqs,
        positions,
        USE.NAMES = FALSE
      )

      type <- mapply(
        function(z, k) z$Type[[k]],
        seqs,
        positions,
        USE.NAMES = FALSE
      )

      # Exact text is always the most specific safe representation.
      if (length(unique(text)) == 1L) {
        return(
          literal_atom(
            text[[1L]]
          )
        )
      }

      # Relative alignment explicitly permits different whitespace runs.
      # Represent those runs structurally instead of sending "whitespace"
      # through .shape_atom(), which only owns identifier morphology.
      if (all(type == "whitespace")) {
        return(
          regex_atom_for_token(
            " ",
            evidence = text
          )
        )
      }

      # Varying edge identifiers may be generalized only when they share one
      # supported identifier shape.
      if (all(type == "identifier")) {

        bases <- mapply(
          function(z, k) z$BaseShape[[k]],
          seqs,
          positions,
          USE.NAMES = FALSE
        )

        if (length(unique(bases)) == 1L) {

          atom <- .shape_atom(
            bases[[1L]]
          )

          if (length(atom) == 1L &&
              !is.na(atom) &&
              nzchar(atom)) {
            return(atom)
          }
        }
      }

      # edge_atom() is consumed by vapply(..., character(1)).
      # Unsupported alignment must therefore abstain with one scalar value,
      # never NULL.
      ""
    }
    pa <- if(pre) vapply(seq_len(pre),edge_atom,character(1)) else character()
    sa <- if(suf) vapply(rev(seq_len(suf)),edge_atom,character(1),end=TRUE) else character()
    middle <- mapply(function(z,w) {
      first <- pre+1L; last <- w-suf
      if(first > last) return("")
      atoms <- vapply(first:last,function(k) {
        if(z$Type[[k]] == "identifier") .shape_atom(z$BaseShape[[k]])
        else if(z$Type[[k]] == "whitespace") regex_atom_for_token(" ",evidence=z$Text[[k]])
        else literal_atom(z$Text[[k]])
      },character(1))
      paste0(atoms,collapse="")
    },seqs,widths,USE.NAMES=FALSE)
    middle <- unique(middle)
    has_empty <- "" %in% middle; middle <- middle[nzchar(middle)]
    middle_atom <- if(!length(middle)) "" else paste0("(?:",paste(middle,collapse="|"),")",
      if(has_empty) "?" else "")
    if(length(pa)+length(sa) && all(nzchar(c(pa,sa))))
      structural <- paste0(c(pa,middle_atom,sa),collapse="")
  }
  concrete_token <- unlist(lapply(seqs,function(z) {
    ids <- z$Text[z$Type == "identifier" & nchar(z$Text) >= 2L]
    c(literal_atom(ids), if(length(ids)>1L) paste(literal_atom(ids[-length(ids)]),
      literal_atom(ids[-1L]),sep="[^[:alnum:]]+") else character())
  }),use.names=FALSE)
  list(structural=structural,shape=shape,partial=partial,
    concrete_token=concrete_token,whole_header_literal=literal_atom(values))
}

candidate_fragments <- function(pos, neg=character(), limit=CANDIDATE_FRAGMENT_SEARCH_LIMIT) {
  started <- proc.time()[["elapsed"]]
  # Families are intentionally ordered from most generalized to most concrete.
  # Negative families are included so the same bounded list can supply safe
  # exclusions.  Semantic keys prevent equivalent runtime patterns generated
  # by two builders from occupying the search budget twice.
  pf <- .candidate_family_builders(pos,neg)
  nf <- .candidate_family_builders(neg,pos)
  # Content words are evidence in their own right.  In addition to complete
  # tokens, offer bounded fragments which really occur in every positive row.
  # Five characters is deliberately the lower bound for fragments: shorter
  # pieces (and especially arbitrary single characters) are rarely
  # interpretable.  Complete short tokens such as "Raw" remain candidates.
  positive_values <- unique(chr(pos)); positive_values <- positive_values[nzchar(positive_values)]
  positive_tokens <- if(length(positive_values)) .candidate_token_rows(positive_values) else list()
  common_tokens <- if(length(positive_tokens)) Reduce(intersect,lapply(positive_tokens,function(z)
    unique(z$Text[z$Type=="identifier" & nchar(z$Text)>=2L]))) else character()
  fragment_source <- if(length(common_tokens)) common_tokens else if(length(positive_values)) positive_values[[1L]] else ""
  literal_fragments <- character()
  for(value in fragment_source) {
    width <- nchar(value,type="chars")
    if(width>=5L) for(size in 5L:width) for(start in seq_len(width-size+1L)) {
      fragment <- substr(value,start,start+size-1L)
      if(all(grepl(fragment,positive_values,fixed=TRUE)))
        literal_fragments <- c(literal_fragments,regex_to_miraprot_storage(regex_escape_literal(fragment),"content"))
    }
  }
  pf$concrete_token <- c(pf$concrete_token,
    regex_to_miraprot_storage(regex_escape_literal(common_tokens),"content"),literal_fragments)
  family_names <- c("structural","shape","partial","concrete_token","whole_header_literal")
  records <- do.call(rbind,lapply(family_names,function(family) {
    patterns <- c(pf[[family]],nf[[family]])
    patterns <- unique(patterns[nzchar(patterns)])
    if(!length(patterns)) return(NULL)
    data.frame(Pattern=patterns,Family=family,Constraint=family,stringsAsFactors=FALSE)
  }))
  if(is.null(records) || !nrow(records)) return(character())
  generated_count <- nrow(records)
  records$Runtime <- regex_from_miraprot_storage(records$Pattern,"content")
  records$Key <- paste(records$Runtime,records$Constraint,sep="\034")
  records <- records[!duplicated(records$Key),,drop=FALSE]
  records <- do.call(rbind,lapply(family_names,function(f) head(records[records$Family==f,,drop=FALSE],
    as.integer(CANDIDATE_FAMILY_LIMIT))))
  selected<-head(records,as.integer(limit))
  result<-selected$Pattern
  attr(result,"candidate_families")<-selected$Family
  attr(result,"candidate_stats")<-list(generated=generated_count,
    deduplicated=nrow(records),selected=nrow(selected),
    elapsed_ms=(proc.time()[["elapsed"]]-started)*1000)
  result
}

# Anchors describe position; they are not content and therefore never consume
# redundancy.  These helpers keep that distinction explicit in selection and
# diagnostics.
content_unanchor <- function(pattern) {
  pattern <- chr(pattern)[[1L]]
  pattern <- sub("^\\^","",pattern)
  sub("\\$$","",pattern)
}

content_anchor_flags <- function(pattern) c(left=startsWith(chr(pattern)[[1L]],"^"),
  right=endsWith(chr(pattern)[[1L]],"$"))

content_apply_anchor_flags <- function(pattern,flags) paste0(if(isTRUE(flags[["left"]]))"^"else"",
  content_unanchor(pattern),if(isTRUE(flags[["right"]]))"$"else"")

# Return one-character, source-backed extensions.  Each returned character is
# adjacent to an actual match in every positive example; intersections ensure
# that divergent boundaries can never manufacture a ladder step.
content_context_extensions <- function(pattern, positives) {
  pattern <- content_unanchor(pattern); positives <- chr(positives)
  per_value <- lapply(positives,function(value) {
    locations <- gregexpr(regex_from_miraprot_storage(pattern,"content"),value,perl=TRUE)[[1L]]
    if(identical(locations,-1L)) return(data.frame(Side=character(),Character=character()))
    widths <- attr(locations,"match.length")
    rows <- lapply(seq_along(locations),function(i) {
      start<-locations[[i]]; end<-start+widths[[i]]-1L
      rbind(if(start>1L)data.frame(Side="left",Character=substr(value,start-1L,start-1L))else NULL,
        if(end<nchar(value,type="chars"))data.frame(Side="right",Character=substr(value,end+1L,end+1L))else NULL)
    })
    unique(do.call(rbind,rows))
  })
  if(!length(per_value)||any(!lengths(per_value)))return(data.frame(Side=character(),Character=character()))
  keys <- Reduce(intersect,lapply(per_value,function(z)paste(z$Side,z$Character,sep="\034")))
  if(!length(keys))return(data.frame(Side=character(),Character=character()))
  pieces<-strsplit(keys,"\034",fixed=TRUE)
  data.frame(Side=vapply(pieces,`[[`,character(1),1L),Character=vapply(pieces,`[[`,character(1),2L),
    stringsAsFactors=FALSE)
}
.regex_constraint_count <- function(pattern, exclude="") {
  # Position is a transformation of a constraint, not another constraint.
  z <- gsub("(?:^\\^|\\$$)","",paste(chr(c(pattern,exclude)),collapse=""),perl=TRUE)
  if(!nzchar(z)) return(0L)
  as.integer(1L+nzchar(exclude)+lengths(regmatches(z,gregexpr("\\(\\?=|\\(\\?!|\\||\\^|\\$|\\{|\\+|\\*|\\?",z,perl=TRUE))))
}

# Score the effective include/exclude rule against every labelled row.  The
# legacy long names are retained because the diagnostics UI consumes them.
score_pattern <- function(pattern, x, truth, exclude="", constraint_count=NULL) {
  x<-chr(x); truth<-as.logical(truth); truth[is.na(truth)]<-FALSE
  if(length(x)!=length(truth)) stop("x and truth must have identical lengths.",call.=FALSE)
  include_validation<-validate_pcre(regex_from_miraprot_storage(pattern,"content"))
  exclude_validation<-validate_pcre(regex_from_miraprot_storage(exclude,"content"))
  if(!include_validation$valid || !exclude_validation$valid)
    stop(paste("Invalid pattern:",paste(c(include_validation$message,exclude_validation$message)[nzchar(c(include_validation$message,exclude_validation$message))],collapse="; ")),call.=FALSE)
  hit<-safe_grepl(pattern,x); if(nzchar(exclude)) hit<-hit&!safe_grepl(exclude,x)
  tp<-sum(hit&truth); fp<-sum(hit&!truth); fn<-sum(!hit&truth); tn<-sum(!hit&!truth)
  precision<-if(tp+fp)tp/(tp+fp)else 0; recall<-if(tp+fn)tp/(tp+fn)else 0
  specificity<-if(tn+fp)tn/(tn+fp)else 0
  f1<-if(precision+recall)2*precision*recall/(precision+recall)else 0
  balanced<-(recall+specificity)/2
  # Deterministic leave-one-out recall.  Regex rules are not fitted here, so it
  # measures observation stability without introducing a random fold split.
  cv_recall<-if(any(truth)) mean(hit[truth]) else 0
  constraints<-if(is.null(constraint_count)).regex_constraint_count(pattern,exclude)else as.integer(constraint_count)
  complexity<-regex_complexity(regex_from_miraprot_storage(pattern,"content"))+
    if(nzchar(exclude))regex_complexity(regex_from_miraprot_storage(exclude,"content"))else 0L
  data.frame(TP=tp,TN=tn,FP=fp,FN=fn,Precision=precision,Recall=recall,F1=f1,
    Specificity=specificity,BalancedAccuracy=balanced,Coverage=if(length(hit))mean(hit)else 0,
    RegexLength=nchar(pattern)+nchar(exclude),Complexity=complexity,ConstraintCount=constraints,
    CrossValidationRecall=cv_recall,FalsePositives=fp,FalseNegatives=fn,stringsAsFactors=FALSE)
}

.refinement_tier <- function(m, conflicting=FALSE) {
  if(conflicting) return("conflicting")
  if(m$F1>=CONTENT_MIN_F1 && m$Recall>=CONTENT_MIN_RECALL && m$FP==0L) "reliable"
  else if(m$F1>=CONTENT_MIN_F1 && m$Recall>=CONTENT_MIN_RECALL) "provisional"
  else "unresolved"
}

.refinement_order <- function(candidates) {
  tier<-match(candidates$Tier,c("reliable","provisional","unresolved","conflicting"))
  order(tier,candidates$FN,candidates$FP,-candidates$CrossValidationRecall,
    -candidates$Specificity,candidates$ConstraintCount,candidates$Complexity,
    candidates$RegexLength,-candidates$Generalization,candidates$Include,
    candidates$Exclude,candidates$CandidateID,method="radix")
}

.refinement_mutations <- function(candidate,x,truth) {
  hit<-safe_grepl(candidate$Include,x); if(nzchar(candidate$Exclude))hit<-hit&!safe_grepl(candidate$Exclude,x)
  fp<-x[hit&!truth]; fn<-x[!hit&truth]; pos<-x[truth]; neg<-x[!truth]
  out<-list(); add<-function(action,include=candidate$Include,exclude=candidate$Exclude,reason) {
    out[[length(out)+1L]]<<-list(action=action,include=include,exclude=exclude,reason=reason)
  }
  if(length(fp)) {
    stable<-candidate_fragments(pos,neg,60L)
    context<-stable[vapply(stable,function(p)all(safe_grepl(p,pos))&&any(!safe_grepl(p,fp)),logical(1))]
    if(length(context)) add("add_stable_structural_context",paste0("(?=.*(?:",context[[1L]],"))",candidate$Include),reason="stable context occurs in every positive and excludes an observed false positive")
    literals<-unique(tokens(pos)$Text); literals<-literals[nchar(literals)>=2L]
    informative<-literals[vapply(literals,function(z)all(safe_grepl(regex_escape_literal(z),pos))&&!all(safe_grepl(regex_escape_literal(z),fp)),logical(1))]
    if(length(informative)) add("add_protected_literal",paste0("(?=.*(?:",regex_escape_literal(sort(informative)[[1L]]),"))",candidate$Include),reason="literal is invariant in positives and informative against false positives")
    harmless<-candidate_fragments(fp,pos,60L); harmless<-harmless[vapply(harmless,function(p)!any(safe_grepl(p,pos))&&any(safe_grepl(p,fp)),logical(1))]
    if(length(harmless)) add("add_harmless_exclusion",exclude=harmless[[1L]],reason="exclusion matches a false positive and no positive")
    anchored<-infer_anchors(candidate$Include,pos,fp)
    if(!identical(anchored,candidate$Include))add("apply_justified_anchor",anchored,reason="anchor preserves all positive matches and removes opposing matches")
    classes<-c("[[:alnum:]]+"="[[:alpha:]]+",".*"="[^[:space:]]+",".+"="[^[:space:]]+")
    for(broad in names(classes))if(grepl(broad,candidate$Include,fixed=TRUE)){add("specialize_broadest_class",sub(broad,classes[[broad]],candidate$Include,fixed=TRUE),reason="observed positive span supports the narrower class");break}
    if(grepl("|",candidate$Include,fixed=TRUE)) {
      parts<-strsplit(candidate$Include,"|",fixed=TRUE)[[1L]]; supported<-parts[vapply(parts,function(p)any(safe_grepl(p,pos)),logical(1))]
      if(length(supported)&&length(supported)<length(parts))add("narrow_unsupported_alternatives",paste(supported,collapse="|"),reason="removed alternatives match no positive observation")
    }
  }
  if(length(fn)) {
    literal_tokens<-unique(tokens(candidate$Include)$Text); literal_tokens<-literal_tokens[grepl("^[[:alnum:]]{2,}$",literal_tokens)]
    unsupported<-literal_tokens[!vapply(literal_tokens,function(z)all(grepl(z,pos,fixed=TRUE)),logical(1))]
    if(length(unsupported))add("remove_unsupported_literal",gsub(regex_escape_literal(unsupported[[1L]]),"",candidate$Include,fixed=TRUE),reason="literal is absent from at least one positive")
    generalized<-gsub("[[:digit:]]+","[[:digit:]]+",candidate$Include,perl=TRUE)
    if(!identical(generalized,candidate$Include))add("generalize_numeric_or_identifier_span",generalized,reason="positive false negatives demonstrate a variable numeric span")
    relaxed<-sub("\\\\s\\+","\\\\s*",candidate$Include)
    if(!identical(relaxed,candidate$Include)&&any(grepl("[[:space:]]",pos)))add("relax_supported_whitespace",relaxed,reason="observed positives support optional whitespace")
    separators<-unique(tokens(fn)$Text); separators<-sort(separators[grepl("^[[:punct:]]$",separators)])
    if(length(separators))add("add_observed_separator_alternative",paste0("(?:",candidate$Include,"|",regex_escape_literal(separators[[1L]]),")"),reason="separator alternative is directly observed in a missed positive")
    unanchored<-sub("^\\^","",sub("\\$$","",candidate$Include))
    if(!identical(unanchored,candidate$Include))add("remove_unsupported_anchor",unanchored,reason="anchor excludes an observed positive")
    run<-sub("\\{[0-9]+\\}","+",candidate$Include,perl=TRUE)
    if(!identical(run,candidate$Include))add("generalize_supported_run_length",run,reason="positive variation does not support a fixed run length")
  }
  out
}

# Bounded deterministic best-first refinement.  Every disposition is retained
# in `audit`, including invalid/over-limit proposals and an explicit abstention.
refine_pattern_search <- function(pattern,x,truth,exclude="",max_depth=REFINEMENT_DEPTH_LIMIT,
  max_candidates=REFINEMENT_CANDIDATE_LIMIT,frontier_limit=REFINEMENT_FRONTIER_LIMIT,
  max_length=MAX_REGEX_LENGTH,max_complexity=MAX_REGEX_COMPLEXITY,quality_f1=CONTENT_MIN_F1) {
  started<-proc.time()[["elapsed"]]; scoring_ms<-0; serialization_ms<-0
  x<-chr(x); truth<-as.logical(truth); conflict<-any(x[truth]%in%x[!truth])
  frontier<-data.frame(Include=pattern,Exclude=exclude,Depth=0L,ParentID=NA_integer_,Action="initial",Reason="initial candidate",Generalization=0L,stringsAsFactors=FALSE)
  accepted<-data.frame(); audit<-data.frame(CandidateID=integer(),ParentID=integer(),Depth=integer(),Action=character(),Disposition=character(),Reason=character(),Include=character(),Exclude=character(),stringsAsFactors=FALSE)
  seen<-character(); next_id<-1L; stop_reason<-"frontier_exhausted"
  score_cache<-new.env(hash=TRUE,parent=emptyenv())
  cached_score<-function(include,exclude) {
    key<-paste(include,exclude,sep="\034")
    if(exists(key,score_cache,inherits=FALSE))return(get(key,score_cache,inherits=FALSE))
    at<-proc.time()[["elapsed"]];value<-score_pattern(include,x,truth,exclude)
    scoring_ms<<-scoring_ms+(proc.time()[["elapsed"]]-at)*1000
    assign(key,value,score_cache);value
  }
  while(nrow(frontier)&&next_id<=max_candidates) {
    row<-frontier[1L,,drop=FALSE]; frontier<-frontier[-1L,,drop=FALSE]; id<-next_id; next_id<-next_id+1L
    key<-paste(row$Include,row$Exclude,sep="\034")
    disposition<-"accepted"; reason<-row$Reason
    serial_at<-proc.time()[["elapsed"]]
    runtime_include<-regex_from_miraprot_storage(row$Include,"content")
    runtime_exclude<-regex_from_miraprot_storage(row$Exclude,"content")
    serialization_ms<-serialization_ms+(proc.time()[["elapsed"]]-serial_at)*1000
    validations<-c(validate_pcre(runtime_include)$valid,validate_pcre(runtime_exclude)$valid)
    length_now<-nchar(row$Include)+nchar(row$Exclude)
    complexity_now<-regex_complexity(regex_from_miraprot_storage(row$Include,"content"))+if(nzchar(row$Exclude))regex_complexity(regex_from_miraprot_storage(row$Exclude,"content"))else 0L
    if(key%in%seen){disposition<-"rejected";reason<-"duplicate stored pattern"}
    else if(!all(validations)){disposition<-"rejected";reason<-"invalid PCRE pattern"}
    else if(length_now>max_length){disposition<-"rejected";reason<-"length limit exceeded"}
    else if(complexity_now>max_complexity){disposition<-"rejected";reason<-"complexity limit exceeded"}
    audit<-rbind(audit,data.frame(CandidateID=id,ParentID=as.integer(row$ParentID),Depth=row$Depth,Action=row$Action,Disposition=disposition,Reason=reason,Include=row$Include,Exclude=row$Exclude,stringsAsFactors=FALSE))
    if(disposition=="rejected")next
    seen<-c(seen,key); m<-cached_score(row$Include,row$Exclude)
    tier<-.refinement_tier(m,conflict)
    accepted<-rbind(accepted,cbind(data.frame(CandidateID=id,ParentID=as.integer(row$ParentID),Depth=row$Depth,Action=row$Action,Reason=reason,Include=row$Include,Exclude=row$Exclude,Tier=tier,Generalization=row$Generalization,stringsAsFactors=FALSE),m))
    if(tier=="reliable"&&m$F1>=quality_f1){stop_reason<-"quality_limit_reached";break}
    if(row$Depth>=max_depth){stop_reason<-"depth_limit_reached";next}
    children<-.refinement_mutations(row,x,truth)
    if(length(children))for(child in children)frontier<-rbind(frontier,data.frame(Include=child$include,Exclude=child$exclude,Depth=row$Depth+1L,ParentID=id,Action=child$action,Reason=child$reason,Generalization=row$Generalization+as.integer(grepl("remove|generalize|relax",child$action)),stringsAsFactors=FALSE))
    if(nrow(frontier)>frontier_limit){
      dropped<-frontier[-seq_len(frontier_limit),,drop=FALSE]
      audit<-rbind(audit,data.frame(CandidateID=NA_integer_,ParentID=dropped$ParentID,
        Depth=dropped$Depth,Action=dropped$Action,Disposition="rejected",
        Reason="frontier limit exceeded",Include=dropped$Include,Exclude=dropped$Exclude,
        stringsAsFactors=FALSE))
      frontier<-frontier[seq_len(frontier_limit),,drop=FALSE];stop_reason<-"frontier_limit_applied"
    }
  }
  if(next_id>max_candidates&&nrow(frontier))stop_reason<-"candidate_limit_reached"
  timing<-function()c(total_ms=(proc.time()[["elapsed"]]-started)*1000,
    scoring_ms=scoring_ms,serialization_ms=serialization_ms)
  if(!nrow(accepted))return(list(status=if(conflict)"conflicting"else"unresolved",best=NULL,candidates=accepted,audit=audit,stop_reason=stop_reason,abstention=list(code="no_valid_candidate",reason="all candidates were rejected before scoring"),timings=timing(),pruning=table(audit$Reason)))
  accepted<-accepted[.refinement_order(accepted),,drop=FALSE];best<-accepted[1L,,drop=FALSE]
  abstention<-if(best$Tier%in%c("unresolved","conflicting"))list(code=paste0(best$Tier,"_evidence"),reason=if(conflict)"identical source text has conflicting labels"else"quality thresholds were not met")else NULL
  list(status=best$Tier,best=best,candidates=accepted,audit=audit,stop_reason=stop_reason,abstention=abstention,
    timings=timing(),pruning=table(audit$Reason))
}
infer_anchors <- function(fragment, measured, opposing=character()) {
  # Candidate records are accepted directly; character input remains supported
  # for callers outside the assistant.  Start and end anchoring are deliberately
  # independent transformations of the discriminating pattern.
  object <- is.list(fragment) && !is.data.frame(fragment)
  pattern <- if(object) { z<-fragment$pattern;if(is.null(z))z<-fragment$Pattern;if(is.null(z))z<-fragment$Include;chr(z)[1L] } else chr(fragment)[1L]
  positives <- chr(measured); negatives <- chr(opposing)
  quality <- function(p) {
    hitp<-safe_grepl(p,positives); hitn<-safe_grepl(p,negatives)
    tp<-sum(hitp);fp<-sum(hitn);fn<-length(hitp)-tp;tn<-length(hitn)-fp
    recall<-if(length(hitp))tp/length(hitp)else 0; specificity<-if(length(hitn))tn/length(hitn)else 1
    precision<-if(tp+fp)tp/(tp+fp)else 0
    c(recall=recall,balanced=(recall+specificity)/2,f1=if(precision+recall)2*precision*recall/(precision+recall)else 0,fp=fp)
  }
  base_coverage <- safe_grepl(pattern,positives); current<-pattern; history<-data.frame()
  for(side in c("start","end")) {
    proposal<-if(side=="start")paste0("^",current)else paste0(current,"$")
    before<-quality(current);after<-quality(proposal)
    accepted<-identical(safe_grepl(proposal,positives),base_coverage) &&
      after[["recall"]]>=before[["recall"]] && after[["balanced"]]>=before[["balanced"]] &&
      after[["f1"]]>=before[["f1"]]
    history<-rbind(history,data.frame(Transformation=paste0("anchor_",side),Before=current,
      After=proposal,Accepted=accepted,DeltaFalsePositives=after[["fp"]]-before[["fp"]],
      Reason=if(accepted)"positive coverage preserved and classification quality preserved or improved" else "positive coverage or classification quality would decrease",stringsAsFactors=FALSE))
    if(accepted)current<-proposal
  }
  if(!object)return(current)
  fragment$pattern<-current;fragment$anchor_history<-history;fragment
}
