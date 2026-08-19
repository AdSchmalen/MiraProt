# ============================================================================
# Auto Regex condition extraction, boundary, span, and context primitives.
# Sourced after Content search primitives into the same caller environment.
# ============================================================================

CONDITION_CONTEXT_SEARCH_WIDTH <- 12L
CONDITION_BOUNDARY_LIMIT <- 16L
CONDITION_BOUNDARY_PAIR_LIMIT <- 64L
CONDITION_BOUNDARY_DIAGNOSTIC_EXAMPLE_LIMIT <- 8L
CONDITION_BOUNDARY_DIAGNOSTIC_EXAMPLE_WIDTH <- 80L
# Alphabetic boundary fields may use the policy shape only within 1..30 chars.
CONDITION_BOUNDARY_ALPHA_POLICY_MAX <- 30L
# Bounds the number of source-span representations inspected for one Content.
# It deliberately does not bound the width of a reference supported by the
# evidence: a single reference of any token width still contributes a span.
CONDITION_SPAN_REPRESENTATION_LIMIT <- 10000L

# ---- condition extraction ---------------------------------------------------
# Keep the Auto RegEx API while delegating canonical rule execution to the
# module-neutral primitive shared with Auto-Assign.
extract_condition <- function(x, method, before="", after="", separators="", pos=1L) {
  datawizard_extract_condition_vector(x, method, before, after, separators, pos)
}

# Describe the ordered boundary pairs considered by the public `between`
# extractor.  Each left occurrence is paired with the first right occurrence
# beginning after it; the extractor then selects the first resulting pair.
condition_between_pair_diagnostics <- function(value, before, after, expected) {
  locate <- function(pattern) stringr::str_locate_all(value,
    paste0("(?-i:",pattern,")"))[[1L]]
  left<-locate(before);right<-locate(after)
  pairs<-if(!nrow(left)||!nrow(right))matrix(integer(),nrow=0L,ncol=2L) else
    do.call(rbind,lapply(seq_len(nrow(left)),function(i) {
      j<-which(right[,"start"]>left[i,"end"])[1L]
      if(length(j))c(left[i,"end"]+1L,right[j,"start"]-1L)else NULL
    }))
  if(is.null(pairs))pairs<-matrix(integer(),nrow=0L,ncol=2L)
  expected_locations<-stringr::str_locate_all(value,stringr::fixed(expected))[[1L]]
  expected_unique<-nrow(expected_locations)==1L
  matches<-if(expected_unique&&nrow(pairs))
    pairs[,1L]==expected_locations[1L,"start"] &
      pairs[,2L]==expected_locations[1L,"end"] else logical(nrow(pairs))
  list(
    BoundaryOccurrenceAmbiguity=nrow(left)>1L||nrow(right)>1L,
    PairAmbiguity=sum(matches)!=1L,
    LocatedSpanAmbiguity=!expected_unique||!length(matches)||!isTRUE(matches[[1L]])
  )
}

condition_separators <- function(xs=character()) {
  # These are persisted regex atoms, not display characters.  In particular a
  # slash is stored as `\/`, a backslash as `\\`, and runs of observed white
  # space as `\s+`, exactly as the flow UI writes them.
  observed <- tokens(chr(xs)); text <- if(nrow(observed)) observed$Text[observed$Type!="identifier"] else character()
  atom <- function(z) if(grepl("^[[:space:]]+$",z,perl=TRUE)) "\\s+" else if(z=="/") "\\/" else regex_escape_literal(z)
  atoms <- unique(vapply(text,atom,character(1)))
  atoms <- atoms[nzchar(atoms)]
  if(!length(atoms)) return(character())
  # Preserve complete observed separator runs as well as their atoms.  The
  # Data Wizard persists one regex alternation, so e.g. `) / (` must remain a
  # usable alternative rather than being reduced to the punctuation singletons.
  runs<-unlist(regmatches(chr(xs),gregexpr("[[:space:]_/():-]+",chr(xs),perl=TRUE)),use.names=FALSE)
  run_pattern<-function(z) {
    pieces<-regmatches(z,gregexpr("[[:space:]]+|[^[:space:]]",z,perl=TRUE))[[1L]]
    paste0(vapply(pieces,atom,character(1)),collapse="")
  }
  sequences<-unique(vapply(runs[nzchar(runs)],run_pattern,character(1)))
  pairs<-if(length(atoms)>=2L)apply(combn(atoms,2L),2,paste,collapse="|")else character()
  alternatives<-unique(c(atoms,sequences))
  unique(c(alternatives,pairs,if(length(alternatives)>1L)paste(alternatives,collapse="|")else character()))
}

# Lex a condition source without throwing away delimiters.  Unlike strsplit(),
# this representation makes every candidate auditable in original R character
# offsets (including multi-character whitespace runs).
condition_token_ranges <- function(x) {
  z <- tokens(chr(x))
  if (!nrow(z)) return(data.frame(Source=integer(), Token=integer(),
    Start=integer(), End=integer(), Text=character(), Kind=character(),
    stringsAsFactors=FALSE, check.names=FALSE))
  data.frame(Source=z$Source, Token=ave(z$Span,z$Source,FUN=seq_along),
    Start=z$Start, End=z$End, Text=z$Text,
    Kind=ifelse(z$Type=="identifier","token","separator"),
    stringsAsFactors=FALSE, check.names=FALSE)
}

# Build regex candidates from aligned fields outside a protected condition.
# A field is indivisible in the primary tiers: candidates are either a complete
# observed token, a numeric token, or a mixed identifier's numeric tail when
# every row has the same alphabetic prefix.  Wider candidates are contiguous
# concatenations and are emitted only after all single-field candidates.
condition_outside_field_candidates <- function(fields,
    representation_limit=CONDITION_SPAN_REPRESENTATION_LIMIT) {
  if(is.data.frame(fields)) fields<-as.matrix(fields)
  if(is.null(dim(fields))) fields<-matrix(chr(fields),ncol=1L)
  fields<-apply(fields,c(1L,2L),chr)
  if(is.null(dim(fields))) fields<-matrix(fields,nrow=1L)
  limit<-max(0L,as.integer(representation_limit)[[1L]])
  empty<-data.frame(FirstField=integer(),LastField=integer(),Width=integer(),
    Pattern=character(),Variant=character(),stringsAsFactors=FALSE,check.names=FALSE)
  if(!nrow(fields)||!ncol(fields)||!limit)return(empty)

  # Variant ranks are part of the enumeration contract. Alphabetic policy and
  # evidence shapes precede their alternation; numeric shapes retain their
  # position ahead of the legacy numeric fallback.
  variant_rank<-c(literal=1L,policy_bounded_alpha_shape=1L,
    fixed_width_shape=2L,bounded_width_shape=2L,observed_alternation=3L,
    unbounded_numeric_fallback=5L)
  width_shape<-function(class,widths,prefix="") {
    bounds<-range(widths)
    quantifier<-if(bounds[[1L]]==bounds[[2L]])
      paste0("{",bounds[[1L]],"}") else
      paste0("{",bounds[[1L]],",",bounds[[2L]],"}")
    paste0(prefix,"[[:",class,":]]",quantifier)
  }
  complete_identifier_values<-function(values) {
    z<-tokens(values)
    if(!nrow(z))return(FALSE)
    counts<-tabulate(z$Source,nbins=length(values))
    all(counts==1L) && all(z$Type=="identifier") &&
      identical(unname(z$Text),unname(values))
  }
  field_variants<-lapply(seq_len(ncol(fields)),function(j) {
    values<-unname(fields[,j]); complete<-unique(values)
    out<-character();kind<-character();rank<-integer()
    if(length(complete)==1L&&nzchar(complete)) {
      out<-regex_escape_literal(complete);kind<-"complete";rank<-variant_rank[["literal"]]
    }
    complete_identifiers<-complete_identifier_values(values)
    widths<-nchar(values,type="chars")
    alphabetic_identifiers<-complete_identifiers && length(complete)>=2L &&
      all(grepl("^[[:alpha:]]+$",values,perl=TRUE)) &&
      all(widths>=1L & widths<=CONDITION_BOUNDARY_ALPHA_POLICY_MAX)
    if(complete_identifiers && length(complete)>=2L &&
        all(grepl("^[[:alpha:]]+$",values,perl=TRUE))) {
      alternation<-paste0("(?:",paste0(vapply(complete,regex_escape_literal,
        character(1)),collapse="|"),")")
      shape_kind<-if(length(unique(widths))==1L) "fixed_width_shape" else "bounded_width_shape"
      evidence_shape<-width_shape("alpha",widths)
      policy_shape<-paste0("[[:alpha:]]{1,",CONDITION_BOUNDARY_ALPHA_POLICY_MAX,"}")
      if(alphabetic_identifiers) {
        out<-c(out,policy_shape);kind<-c(kind,"policy_bounded_alpha_shape")
        rank<-c(rank,variant_rank[["policy_bounded_alpha_shape"]])
      }
      out<-c(out,evidence_shape,alternation)
      kind<-c(kind,shape_kind,"observed_alternation")
      rank<-c(rank,variant_rank[[shape_kind]],variant_rank[["observed_alternation"]])
    }
    if(complete_identifiers && all(grepl("^[[:digit:]]+$",values,perl=TRUE))) {
      shape_kind<-if(length(unique(widths))==1L) "fixed_width_shape" else "bounded_width_shape"
      out<-c(out,width_shape("digit",widths),"[[:digit:]]+")
      kind<-c(kind,shape_kind,"unbounded_numeric_fallback")
      rank<-c(rank,variant_rank[[shape_kind]],variant_rank[["unbounded_numeric_fallback"]])
    }
    parts<-if(complete_identifiers)regmatches(values,
      regexec("^([[:alpha:]]+)([[:digit:]]+)$",values,perl=TRUE)) else list()
    if(length(parts)&&all(lengths(parts)==3L)) {
      prefixes<-vapply(parts,`[[`,character(1),2L)
      if(length(unique(prefixes))==1L) {
        tails<-vapply(parts,`[[`,character(1),3L);tail_widths<-nchar(tails,type="chars")
        prefix<-regex_escape_literal(prefixes[[1L]])
        shape_kind<-if(length(unique(tail_widths))==1L) "fixed_width_shape" else "bounded_width_shape"
        out<-c(out,width_shape("digit",tail_widths,prefix),paste0(prefix,"[[:digit:]]+"))
        kind<-c(kind,paste0("numeric_tail_",shape_kind),"numeric_tail_unbounded_numeric_fallback")
        rank<-c(rank,variant_rank[[shape_kind]],variant_rank[["unbounded_numeric_fallback"]])
      }
    }
    keep<-!duplicated(out)
    variants<-data.frame(Pattern=out[keep],Variant=kind[keep],Rank=rank[keep],stringsAsFactors=FALSE)
    variants<-variants[order(variants$Rank,seq_len(nrow(variants))),c("Pattern","Variant"),drop=FALSE]
    rownames(variants)<-NULL
    variants
  })
  answer<-list();add_span<-function(first,last) {
    choices<-field_variants[first:last]
    if(any(!lengths(choices)))return(FALSE)
    products<-expand.grid(lapply(choices,function(z)seq_len(nrow(z))),KEEP.OUT.ATTRS=FALSE)
    for(i in seq_len(nrow(products))) {
      if(length(answer)>=limit)return(TRUE)
      picked<-mapply(function(z,k)z[k,,drop=FALSE],choices,products[i,],SIMPLIFY=FALSE)
      answer[[length(answer)+1L]]<<-data.frame(FirstField=first,LastField=last,
        Width=last-first+1L,Pattern=paste0(vapply(picked,`[[`,character(1),"Pattern"),collapse=""),
        Variant=paste(vapply(picked,`[[`,character(1),"Variant"),collapse="+"),stringsAsFactors=FALSE)
    }
    FALSE
  }
  # Width is the observed aligned field width; never search character widths.
  for(width in seq_len(ncol(fields)))for(first in seq_len(ncol(fields)-width+1L))
    if(add_span(first,first+width-1L))break
  if(!length(answer))return(empty)
  result<-unique(do.call(rbind,answer));rownames(result)<-NULL
  head(result,limit)
}

# Locate all (not merely the first) contiguous lexical spans which reconstruct
# a complete reference exactly.  Candidate width is bounded solely by the
# tokenized references supplied as evidence, so long condition names remain
# discoverable without an arbitrary word ceiling.
condition_reference_spans <- function(xs, ys,
    representation_limit=CONDITION_SPAN_REPRESENTATION_LIMIT) {
  xs <- chr(xs); ys <- chr(ys)
  if (length(xs)!=length(ys)) stop("Condition sources and references must have equal lengths.",call.=FALSE)
  representation_limit<-max(0L,as.integer(representation_limit)[[1L]])
  empty<-function() data.frame(Row=integer(),FirstToken=integer(),
    LastToken=integer(),Width=integer(),Start=integer(),End=integer(),
    Reference=character(),Occurrences=integer(),Ambiguous=logical(),
    stringsAsFactors=FALSE,check.names=FALSE)
  diagnostics<-list(generated=0L,deduplicated=0L,ambiguous=0L,pruned=0L,
    scored=0L,largest_evidence_span_width=0L,limit=representation_limit,
    limit_exhausted=FALSE)
  # Identical evidence pairs have identical spans. Search each only once, then
  # replay its offsets onto every original row. This also makes the limit
  # independent of repeated workbook rows.
  pair_key<-paste0(nchar(xs,type="bytes"),":",xs,"\036",nchar(ys,type="bytes"),":",ys)
  first_pair<-!duplicated(pair_key); pair_rows<-which(first_pair)
  diagnostics$deduplicated<-length(xs)-length(pair_rows)
  token_cache<-getOption("miraprot.condition_span_token_cache",NULL)
  if(!is.environment(token_cache)) token_cache<-new.env(parent=emptyenv())
  tokenize<-function(value) {
    key<-paste0(nchar(value,type="bytes"),":",value)
    if(!exists(key,envir=token_cache,inherits=FALSE))
      assign(key,condition_token_ranges(value),envir=token_cache)
    get(key,envir=token_cache,inherits=FALSE)
  }
  out <- list()
  for(i in pair_rows) {
    source_tokens <- tokenize(xs[[i]])
    source_tokens <- source_tokens[source_tokens$Source==1L,,drop=FALSE]
    reference_tokens<-tokenize(ys[[i]])
    reference_tokens<-reference_tokens[reference_tokens$Source==1L,,drop=FALSE]
    width<-nrow(reference_tokens)
    diagnostics$largest_evidence_span_width<-max(diagnostics$largest_evidence_span_width,width)
    if(!nzchar(ys[[i]]) || !nrow(source_tokens) || !width) next
    # Every exact reconstruction has the reference token count. All other
    # widths are impossible and are counted without materializing them.
    total_windows<-nrow(source_tokens)*(nrow(source_tokens)+1L)/2L
    possible_windows<-if(width<=nrow(source_tokens))nrow(source_tokens)-width+1L else 0L
    diagnostics$pruned<-diagnostics$pruned+as.integer(total_windows-possible_windows)
    if(!possible_windows) next
    for(first in seq_len(possible_windows)) {
      if(diagnostics$generated>=representation_limit) {
        diagnostics$limit_exhausted<-TRUE; break
      }
      last <- first+width-1L; start <- source_tokens$Start[[first]]; end <- source_tokens$End[[last]]
      diagnostics$generated<-diagnostics$generated+1L
      # Character length is a cheap necessary condition and prevents building
      # candidate strings which cannot equal the reference.
      if(end-start+1L!=nchar(ys[[i]],type="chars")) {
        diagnostics$pruned<-diagnostics$pruned+1L; next
      }
      diagnostics$scored<-diagnostics$scored+1L
      if(identical(substr(xs[[i]],start,end),ys[[i]])) {
        rows<-which(pair_key==pair_key[[i]])
        for(row in rows) out[[length(out)+1L]] <- data.frame(Row=row,
          FirstToken=first,LastToken=last,Width=width,Start=start,End=end,
          Reference=ys[[i]],stringsAsFactors=FALSE)
      }
    }
    if(diagnostics$limit_exhausted) break
  }
  if(!length(out)) { answer<-empty(); attr(answer,"span_diagnostics")<-diagnostics; return(answer) }
  answer <- do.call(rbind,out)
  answer$Occurrences <- ave(answer$Row,answer$Row,FUN=length)
  answer$Ambiguous <- answer$Occurrences>1L
  diagnostics$ambiguous<-sum(answer$Ambiguous)
  row.names(answer)<-NULL; answer
  attr(answer,"span_diagnostics")<-diagnostics
  answer
}

# Summarize alignment without collapsing repeated occurrences.  Callers can
# consequently reject ambiguity while still translating unambiguous evidence
# to the established whole/start/end/between condition vocabulary.
condition_span_boundary_candidates <- function(xs, ys, spans, side) {
  stable <- spans[!spans$Ambiguous,,drop=FALSE]
  if(!nrow(stable) || nrow(stable)!=length(xs) ||
      length(unique(stable$Row))!=length(xs)) return(character())
  pieces <- lapply(seq_len(nrow(stable)), function(i) {
    row <- stable$Row[[i]]
    raw <- if(side=="before") substr(xs[[row]],1L,stable$Start[[i]]-1L) else
      substr(xs[[row]],stable$End[[i]]+1L,nchar(xs[[row]],type="chars"))
    z <- tokens(raw); z <- z[z$Source==1L,,drop=FALSE]
    if(!nrow(z)) return(character())
    # Only adjacency is relevant.  Bounding the lexical window and the number
    # of literal/generalized numeric alternatives prevents identifier-heavy
    # suffixes from creating an exponential representation set.
    if(nrow(z)>16L) z<-if(side=="before")tail(z,16L)else head(z,16L)
    variable_digits<-0L
    atoms <- lapply(seq_len(nrow(z)),function(j) {
      literal<-regex_escape_literal(z$Text[[j]])
      if(z$Type[[j]]=="whitespace") return(c("\\s+",literal))
      if(z$Type[[j]]=="identifier" && z$BaseShape[[j]]=="digits") {
        variable_digits<<-variable_digits+1L
        if(variable_digits<=8L)return(c(literal,"[[:digit:]]+"))
        return("[[:digit:]]+")
      }
      literal
    })
    variants<-list(character())
    for(options in atoms) variants<-unlist(lapply(variants,function(prefix)
      lapply(options,function(option)c(prefix,option))),recursive=FALSE)
    unique(unlist(lapply(variants,function(atom) {
      indices <- if(side=="before") lapply(seq_len(length(atom)),function(n)
        seq.int(length(atom)-n+1L,length(atom))) else lapply(seq_len(length(atom)),seq_len)
      vapply(indices,function(ix)paste0(atom[ix],collapse=""),character(1))
    }),use.names=FALSE))
  })
  candidates <- Reduce(intersect,pieces)
  if(!length(candidates)) return(character())
  # A delimiter learned from outside a reference must never also identify text
  # inside a mapped value.  Such a delimiter could silently truncate a longer
  # condition during replay.
  candidates <- candidates[!vapply(candidates,function(pattern)
    any(vapply(ys,function(y)nrow(stringr::str_locate_all(y,paste0("(?-i:",pattern,")"))[[1L]])>0L,
      logical(1))),logical(1))]
  candidates[vapply(candidates,function(pattern)all(vapply(seq_len(nrow(stable)),function(i) {
    row<-stable$Row[[i]]
    loc<-stringr::str_locate_all(xs[[row]],paste0("(?-i:",pattern,")"))[[1L]]
    if(!nrow(loc))return(FALSE)
    if(side=="before") {
      any(loc[,"end"]==stable$Start[[i]]-1L)
    } else {
      exact<-stable$End[[i]]+1L
      # The terminator starts at the exact reference end, and it is the first
      # possible terminator after the reference begins.
      any(loc[,"start"]==exact) && !any(loc[,"start"]>=stable$Start[[i]] & loc[,"start"]<exact)
    }
  },logical(1))),logical(1))]
}

condition_stable_spans <- function(xs, ys) {
  xs<-chr(xs); spans<-condition_reference_spans(xs,ys)
  diagnostics<-attr(spans,"span_diagnostics",exact=TRUE)
  if(!nrow(spans)) {
    attr(spans,"boundary_candidates")<-list(before=character(),after=character())
    attr(spans,"span_diagnostics")<-diagnostics
    return(spans)
  }
  spans$Before<-mapply(function(row,start)substr(xs[[row]],1L,start-1L),spans$Row,spans$Start,USE.NAMES=FALSE)
  spans$After<-mapply(function(row,end)substr(xs[[row]],end+1L,nchar(xs[[row]],type="chars")),spans$Row,spans$End,USE.NAMES=FALSE)
  spans$Method<-ifelse(!nzchar(spans$Before)&!nzchar(spans$After),"whole",
    ifelse(!nzchar(spans$Before),"start",ifelse(!nzchar(spans$After),"end","between")))
  pairs<-condition_stable_boundary_pairs(xs,spans)
  attr(spans,"boundary_pairs")<-pairs
  attr(spans,"boundary_candidates")<-list(
    before=unique(pairs$Before[nzchar(pairs$Before)]),
    after=unique(pairs$After[nzchar(pairs$After)]))
  attr(spans,"span_diagnostics")<-diagnostics
  spans
}

# Render boundary evidence without allowing invisible whitespace to collapse in
# diagnostics.  Quotes are part of the rendering; backslash, tab, newline,
# carriage return, and non-breaking space use unambiguous escaped forms.
condition_boundary_evidence_render <- function(x) {
  vapply(chr(x),function(value) {
    chars<-strsplit(value,"",fixed=TRUE)[[1L]]
    shown<-vapply(chars,function(char)switch(char,
      "\\"="\\\\", "\t"="\\t", "\n"="\\n", "\r"="\\r",
      "\u00a0"="\\u00A0", char),character(1))
    paste0('"',paste0(shown,collapse=""),'"')
  },character(1),USE.NAMES=FALSE)
}

# Convert the exact outside-span evidence recorded above into the smallest
# aligned boundary pair which identifies those offsets in every source row.
# Tokens are compared by distance from the reference: punctuation and equal
# identifiers (including fixed numeric slots) remain literals, while only an
# aligned field whose observed values differ is generalized.
condition_stable_boundary_pairs <- function(xs, spans,
    representation_limit=CONDITION_SPAN_REPRESENTATION_LIMIT) {
  xs<-chr(xs)
  representation_limit<-max(0L,as.integer(representation_limit)[[1L]])
  empty<-data.frame(Method=character(),Before=character(),After=character(),
    BeforeTokens=integer(),AfterTokens=integer(),stringsAsFactors=FALSE)
  diagnostic_records<-list()
  escaped_values<-function(values) {
    values<-sort(unique(chr(values)),method="radix")
    values<-head(values,CONDITION_BOUNDARY_DIAGNOSTIC_EXAMPLE_LIMIT)
    values<-vapply(values,function(value)substr(value,1L,
      CONDITION_BOUNDARY_DIAGNOSTIC_EXAMPLE_WIDTH),character(1))
    sub('^"(.*)"$','\\1',condition_boundary_evidence_render(values))
  }
  record_diagnostic<-function(kind,side,depth,token_type,shape,
      examples=character(),observed_runs=character(),punctuation_skeletons=character()) {
    diagnostic_records[[length(diagnostic_records)+1L]]<<-list(
      Kind=kind,Side=side,AlignedFieldDepth=as.integer(depth),TokenType=token_type,
      DetectedShape=shape,Examples=escaped_values(examples),
      ObservedRuns=escaped_values(observed_runs),
      PunctuationSkeletons=escaped_values(punctuation_skeletons))
  }
  finish<-function(value) {
    if(length(diagnostic_records)) {
      keys<-vapply(diagnostic_records,function(z)paste(z$Kind,z$Side,
        sprintf("%09d",z$AlignedFieldDepth),z$TokenType,z$DetectedShape,
        paste(z$Examples,collapse="\035"),paste(z$ObservedRuns,collapse="\035"),sep="\034"),character(1))
      ordered<-order(keys,method="radix")
      diagnostic_records<<-diagnostic_records[ordered[!duplicated(keys[ordered])]]
      diagnostic_records<<-diagnostic_records[order(vapply(diagnostic_records,function(z)
        paste(z$Side,sprintf("%09d",z$AlignedFieldDepth),z$Kind,sep="\034"),character(1)),method="radix")]
    }
    attr(value,"boundary_diagnostics")<-diagnostic_records
    value
  }
  stable<-spans[!spans$Ambiguous,,drop=FALSE]
  if(!nrow(stable) || nrow(stable)!=length(xs) ||
      length(unique(stable$Row))!=length(xs)) return(finish(empty))
  stable<-stable[order(stable$Row),,drop=FALSE]
  # An outside-span step is one identifier or one complete run of separators.
  # In particular, never manufacture a boundary by stopping midway through a
  # long identifier merely because the legacy context search stops at 12
  # characters.
  token_rows<-function(values) lapply(values,function(value) {
    z<-condition_token_ranges(value);z<-z[z$Source==1L,,drop=FALSE]
    if(!nrow(z))return(data.frame(Text=character(),Type=character()))
    group<-cumsum(z$Kind=="token" | c(TRUE,head(z$Kind,-1L)=="token"))
    groups<-split(seq_len(nrow(z)),group)
    data.frame(Text=vapply(groups,function(ix)paste0(z$Text[ix],collapse=""),character(1)),
      Type=vapply(groups,function(ix)if(z$Kind[ix[[1L]]]=="token")"identifier" else
        if(grepl("^[[:space:]]+$",paste0(z$Text[ix],collapse=""),perl=TRUE))"whitespace" else
          "separator",character(1)),stringsAsFactors=FALSE)
  })
  left<-token_rows(stable$Before);right<-token_rows(stable$After)
  # A separator run is generalized only by aligning the gaps around its
  # literal punctuation.  This deliberately does not align punctuation by
  # character class or edit distance: a missing, additional, or reordered
  # punctuation character makes the whole aligned atom unsupported.
  separator_atom<-function(parts,side,depth) {
    observed<-vapply(parts,function(z)z$Text[[1L]],character(1))
    decompose<-function(value) {
      chars<-strsplit(value,"",fixed=TRUE)[[1L]]
      # Classify with the same ICU semantics used when the persisted `\s`
      # boundary is replayed through stringr.  Base-R PCRE [[:space:]] does
      # not classify every character matched by ICU `\s` (notably U+00A0).
      whitespace<-if(length(chars))stringr::str_detect(chars,"^\\s$") else logical()
      punctuation<-chars[!whitespace]
      slots<-logical(length(punctuation)+1L);slot<-1L
      for(i in seq_along(chars)) {
        if(whitespace[[i]])slots[[slot]]<-TRUE else slot<-slot+1L
      }
      list(punctuation=punctuation,whitespace=slots)
    }
    pieces<-lapply(observed,decompose)
    skeletons<-lapply(pieces,`[[`,"punctuation")
    if(!all(vapply(skeletons,identical,logical(1),skeletons[[1L]]))) {
      record_diagnostic("separator_incompatibility",side,depth,"separator",
        "punctuation_skeleton",observed_runs=observed,
        punctuation_skeletons=vapply(skeletons,paste0,character(1),collapse=""))
      return(NA_character_)
    }
    skeleton<-skeletons[[1L]]
    slot_count<-length(skeleton)+1L
    atoms<-character(slot_count+length(skeleton))
    for(slot in seq_len(slot_count)) {
      present<-vapply(pieces,function(piece)piece$whitespace[[slot]],logical(1))
      # Optionality is evidence-driven: emit `\s*` only when this aligned slot
      # is present in some observed runs and absent in others.
      atoms[[2L*slot-1L]]<-if(all(present))"\\s+" else
        if(any(present))"\\s*" else ""
      if(slot<=length(skeleton))
        atoms[[2L*slot]]<-regex_escape_literal(skeleton[[slot]])
    }
    paste0(atoms,collapse="")
  }
  atom<-function(parts,side,depth) {
    text<-vapply(parts,function(z)z$Text[[1L]],character(1))
    type<-vapply(parts,function(z)z$Type[[1L]],character(1))
    if(all(type=="whitespace"))return("\\s+")
    if(all(type=="separator"))return(separator_atom(parts,side,depth))
    if(length(unique(text))==1L)return(regex_escape_literal(text[[1L]]))
    if(!all(type=="identifier"))return(NA_character_)
    supported<-condition_outside_field_candidates(matrix(text,ncol=1L),
      representation_limit=representation_limit)
    if(!nrow(supported)) {
      shape<-if(all(grepl("^[[:alpha:]]+$",text,perl=TRUE)))"letters" else
        if(all(grepl("^[[:digit:]]+$",text,perl=TRUE)))"digits" else
        if(all(grepl("^[[:alnum:]]+$",text,perl=TRUE)))"alphanumeric" else "mixed"
      record_diagnostic("unsupported_outside_field_variation",side,depth,
        "identifier",shape,examples=text)
      NA_character_
    } else supported$Pattern
  }
  boundary<-function(side,depth) {
    rows<-if(side=="before")left else right
    selected<-lapply(rows,function(z) {
      if(nrow(z)<depth)return(NULL)
      ix<-if(side=="before")seq.int(nrow(z)-depth+1L,nrow(z))else seq_len(depth)
      lapply(ix,function(j)z[j,,drop=FALSE])
    })
    if(any(vapply(selected,is.null,logical(1))))return(NA_character_)
    values<-lapply(seq_len(depth),function(j)atom(lapply(selected,`[[`,j),side,
      if(side=="before")depth-j+1L else j))
    if(any(vapply(values,function(value)!length(value)||anyNA(value),logical(1))))
      return(character())
    # Preserve the preference order established by
    # condition_outside_field_candidates(), but do not discard its later,
    # evidence-supported representations.  Adjacent fields form a bounded
    # Cartesian product which the caller validates against the exact spans.
    # Enumerate the same first-field-fastest order as expand.grid(), without
    # materializing a product which can be much larger than the inspection
    # budget.
    count<-1L
    for(value in values)
      count<-min(representation_limit,count*length(value))
    vapply(seq_len(count),function(i) {
      offset<-i-1L;stride<-1L;indices<-integer(length(values))
      for(j in seq_along(values)) {
        indices[[j]]<-(offset%/%stride)%%length(values[[j]])+1L
        stride<-stride*length(values[[j]])
      }
      paste0(mapply(`[`,values,indices,SIMPLIFY=TRUE),collapse="")
    },character(1))
  }
  # Check that every boundary has an occurrence at the evidence-aligned
  # location.  Do not require the pattern itself to occur only once: a policy
  # boundary such as `[[:alpha:]]{1,30}_` can legitimately match both the
  # carrier and an underscore-delimited component of the condition.  The
  # ordered public extraction replay below is the authority for deciding
  # whether that broader pattern still selects the evidenced span.
  aligned<-function(method,before,after) all(vapply(seq_len(nrow(stable)),function(i) {
    source<-xs[[stable$Row[[i]]]]
    locate<-function(pattern)if(!nzchar(pattern))matrix(integer(),nrow=0L,ncol=2L,
      dimnames=list(NULL,c("start","end")))else
      stringr::str_locate_all(source,paste0("(?-i:",pattern,")"))[[1L]]
    b<-locate(before);a<-locate(after)
    if(method=="start") return(stable$Start[[i]]==1L &&
      any(a[,"start"]-1L==stable$End[[i]]))
    if(method=="end") return(stable$End[[i]]==nchar(source,type="chars") &&
      any(b[,"end"]==stable$Start[[i]]-1L))
    any(b[,"end"]==stable$Start[[i]]-1L) &&
      any(a[,"start"]==stable$End[[i]]+1L)
  },logical(1)))
  method<-if(all(!nzchar(stable$Before)))"start" else
    if(all(!nzchar(stable$After)))"end" else
    if(all(nzchar(stable$Before)&nzchar(stable$After)))"between" else return(finish(empty))
  before_depths<-if(method=="start")0L else seq_len(min(vapply(left,nrow,integer(1))))
  after_depths<-if(method=="end")0L else seq_len(min(vapply(right,nrow,integer(1))))
  depths<-expand.grid(BeforeTokens=before_depths,AfterTokens=after_depths)
  depths<-depths[order(depths$BeforeTokens+depths$AfterTokens,
    depths$BeforeTokens,depths$AfterTokens),,drop=FALSE]
  references<-mapply(function(row,start,end)substr(xs[[row]],start,end),
    stable$Row,stable$Start,stable$End,USE.NAMES=FALSE)
  generated<-0L
  for(i in seq_len(nrow(depths))) {
    if(generated>=representation_limit)break
    bd<-depths$BeforeTokens[[i]];ad<-depths$AfterTokens[[i]]
    before<-if(bd)boundary("before",bd)else ""
    after<-if(ad)boundary("after",ad)else ""
    if(!length(before)||!length(after))next
    pair_count<-min(representation_limit-generated,length(before)*length(after))
    for(j in seq_len(pair_count)) {
      generated<-generated+1L
      # Retain expand.grid()'s deterministic order while bounding enumeration
      # before a complete Before/After product is constructed.
      before_candidate<-before[[(j-1L)%%length(before)+1L]]
      after_candidate<-after[[(j-1L)%/%length(before)%%length(after)+1L]]
      if(!aligned(method,before_candidate,after_candidate))next
      replay<-extract_condition(xs,method,before_candidate,after_candidate)
      if(!identical(chr(replay),chr(references)))next
      candidate<-data.frame(Method=method,Before=before_candidate,After=after_candidate,
        BeforeTokens=bd,AfterTokens=ad,stringsAsFactors=FALSE)
      path<-tempfile(fileext=".rds");saveRDS(candidate,path)
      stored<-readRDS(path);unlink(path)
      stored_replay<-extract_condition(xs,stored$Method[[1L]],stored$Before[[1L]],stored$After[[1L]])
      if(identical(chr(stored_replay),chr(references))) {
        selected_evidence<-function(side,depth) {
          if(!depth)return(character())
          rows<-if(side=="before")left else right
          unlist(lapply(rows,function(z) {
            ix<-if(side=="before")seq.int(nrow(z)-depth+1L,nrow(z))else seq_len(depth)
            condition_boundary_evidence_render(z$Text[ix])
          }),use.names=FALSE)
        }
        attr(stored,"boundary_evidence")<-list(
          Before=selected_evidence("before",bd),After=selected_evidence("after",ad))
        return(finish(stored))
      }
    }
  }
  finish(empty)
}

# Content assignment invokes set_ratio_or_identifier_options() immediately
# after setting Content.  These rows therefore receive Options="Ratio" before
# condition rules run and must not be treated as failed condition extraction.
AUTO_ASSIGNED_RATIO_OPTIONS_CONTENT <- c("Abundance Ratio", "Abundance Ratio p-Value",
  "Abundance Ratio Adj. p-Value")

condition_contexts <- function(xs, ys, side, max_width=CONDITION_CONTEXT_SEARCH_WIDTH) {
  values <- character()
  located <- condition_stable_spans(xs,ys)
  for (i in seq_along(xs)) {
    row_spans <- located[located$Row==i,,drop=FALSE]
    # Ambiguous occurrences are intentionally retained by the locator and
    # explicitly excluded here; no boundary is inferred from an arbitrary hit.
    if(nrow(row_spans)!=1L || row_spans$Ambiguous[[1L]]) next
    loc <- row_spans$Start[[1L]]
    raw <- if (side=="before") substr(xs[i],1L,loc-1L) else substr(xs[i],loc+nchar(ys[i]),nchar(xs[i]))
    if (!nzchar(raw)) next
    widths <- seq_len(min(nchar(raw), max_width))
    fragments <- if (side=="before") substring(raw,nchar(raw)-widths+1L) else substring(raw,1L,widths)
    values <- c(values, regex_to_miraprot_storage(regex_atom_for_token(fragments), "condition_boundary"))
    # Preserve the punctuation nearest the label and generalise only a complete
    # adjacent identifier.  Partial identifiers are never replaced.
    z<-tokens(raw); if(nrow(z)) {
      rows<-if(side=="before")rev(seq_len(nrow(z)))else seq_len(nrow(z))
      kept<-character()
      for(j in rows) {
        a<-if(z$Type[j]=="identifier") "[[:alnum:]]+" else if(z$Type[j]=="whitespace") "\\s+" else regex_escape_literal(z$Text[j])
        kept<-if(side=="before")c(a,kept)else c(kept,a)
        candidate<-paste0(kept,collapse="")
        if(nchar(candidate)<=max_width+12L)values<-c(values,candidate)
      }
    }
  }
  values<-unique(values[nzchar(values)])
  # A boundary which is absent from even one applicable row cannot possibly
  # produce a complete rule.  Prefer short, literal contexts before structural
  # generalisations so subsequent bounded products remain deterministic.
  present_everywhere<-vapply(values,function(value)all(vapply(xs,function(x)
    any(!is.na(stringr::str_locate_all(x,paste0("(?-i:",value,")"))[[1L]][,"start"])),logical(1))),logical(1))
  values<-values[present_everywhere]
  values[order(nchar(values),grepl("[[:",values,fixed=TRUE),values,method="radix")]
}
