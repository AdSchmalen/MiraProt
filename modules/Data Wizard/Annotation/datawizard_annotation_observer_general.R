# ==============================================================================
# File: modules/Data Wizard/Annotation/datawizard_annotation_observer_general.R
#
# Purpose:
#   Contains general-purpose observers for the Annotation submodule: source
#   column dropdown population, species-change keytype loading (intra-species /
#   AnnotationDB path), the status render output, and the UI config import
#   observer.
#
# Architectural Role:
#   One of four concern-based observer files that together replace the former
#   monolithic datawizard_annotation_observer.R.  This file covers all
#   observers that are NOT specific to BioMart cross-species mode, cache
#   management, or ID-mapping execution.
#
#   Called indirectly: register_annotation_observers() (the thin entrypoint in
#   datawizard_annotation_observer.R) delegates to
#   register_annotation_observers_general() defined here.
#
# Responsibilities:
#   - Immediate feedback observers for species input changes.
#   - Observer a:  Source column dropdown (metadata-driven).
#   - Observer b:  Species change (debounced, AnnotationDB keytype load).
#   - Observer e:  Status render output (annotation_status).
#   - Observer f:  UI config import.
#
# Integration Points / Dependencies:
#   - Shiny session objects (input, output, session, ns) from moduleServer.
#   - Reactive state from create_annotation_state() via destructured handles.
#   - Debounced species reactives created in the entrypoint and passed in.
#   - Pure-logic helpers from datawizard_annotation_utils.R (organism_to_orgdb,
#     load_keytypes_from_cache, load_keytypes_with_download, etc.).
#   - GO cache helpers available in modEnv (organism_to_orgdb,
#     load_organism_cache, etc.).
#
# Maintenance Guidance:
#   - Keep this file focused on general / non-BioMart observers.
#   - If a new observer does not fit BioMart, cache, or mapping concerns,
#     add it here.
#   - Target: stay below 1000 lines.
# ==============================================================================


# Strict keytype cache TTL: keytypes older than this are not loaded directly.
# Organism cache TTL: a recent OrgDb cache can still use static keytype defaults
# to avoid startup/species-change downloads after a strict keytype cache miss.
KEYTYPE_CACHE_MAX_AGE_DAYS <- 10
ORGANISM_CACHE_MAX_AGE_DAYS <- 30

#' Copy text to the clipboard via JavaScript with scroll preservation.
#'
#' Uses Navigator.clipboard API where available, with a textarea-based
#' fallback for older browsers. Scroll position is saved and restored
#' to prevent the page from jumping.
#'
#' Matches the implementation used in the Volcano module
#' (modules/Volcano/volcano_export.R).
#'
#' @param text       Character string to copy.
#' @param debug_log  Logging function.
copy_to_clipboard <- function(text, debug_log = function(msg, level = 1) cat(msg, "\n")) {

  # Escape text for safe JavaScript string embedding
  escaped <- gsub("\\\\", "\\\\\\\\", text)
  escaped <- gsub('"', '\\\\"', escaped)
  escaped <- gsub("\n", "\\\\n", escaped)
  escaped <- gsub("\r", "\\\\r", escaped)

  shinyjs::runjs(paste0('
    (function() {
      var currentScrollTop = window.pageYOffset || document.documentElement.scrollTop;
      var currentScrollLeft = window.pageXOffset || document.documentElement.scrollLeft;

      function fallbackCopy(text) {
        var textArea = document.createElement("textarea");
        textArea.value = text;
        textArea.style.position = "fixed";
        textArea.style.left = "-999999px";
        textArea.style.top = "-999999px";
        textArea.style.width = "2em";
        textArea.style.height = "2em";
        textArea.style.padding = "0";
        textArea.style.border = "none";
        textArea.style.outline = "none";
        textArea.style.boxShadow = "none";
        textArea.style.background = "transparent";
        textArea.style.opacity = "0";
        textArea.style.pointerEvents = "none";
        document.body.appendChild(textArea);
        textArea.focus();
        textArea.select();
        try {
          document.execCommand("copy");
        } catch (err) {
          console.error("Fallback clipboard copy failed:", err);
        }
        document.body.removeChild(textArea);
        window.scrollTo(currentScrollLeft, currentScrollTop);
      }

      if (navigator.clipboard && navigator.clipboard.writeText) {
        navigator.clipboard.writeText("', escaped, '").then(function() {
          console.log("Clipboard copy successful");
        }, function(err) {
          console.error("navigator.clipboard failed, using fallback:", err);
          fallbackCopy("', escaped, '");
        });
      } else {
        fallbackCopy("', escaped, '");
      }
    })();
  '))

  debug_log("Clipboard copy executed via JavaScript", 2)
}


#' Register general-purpose annotation observers.
#'
#' @param input,output,session,ns  Shiny module objects.
#' @param state              Named list from create_annotation_state().
#' @param get_data           Function returning the current data frame.
#' @param data_def           Reactive returning the metadata data frame.
#' @param apply_ui_config    Function to apply UI configuration.
#' @param debug_log          Logging function.
#' @param DEBUG_LEVEL        Numeric debug verbosity.
#' @param species_src_debounced  Debounced reactive for source species.
#' @param species_tgt_debounced  Debounced reactive for target species.
register_annotation_observers_general <- function(input, output, session, ns,
                                                   state, get_data, data_def,
                                                   apply_ui_config,
                                                   debug_log, DEBUG_LEVEL,
                                                   species_src_debounced,
                                                   species_tgt_debounced) {

  # -- Destructure state handles -----------------------------------------------
  ui_config_update_active  <- state$ui_config_update_active
  current_ui_config        <- state$current_ui_config
  get_ui_config            <- state$get_ui_config
  last_mapping_result      <- state$last_mapping_result
  cached_org_db            <- state$cached_org_db
  cached_orgdb_name        <- state$cached_orgdb_name
  cached_keytypes          <- state$cached_keytypes
  keytype_loading          <- state$keytype_loading
  keytype_last_organism    <- state$keytype_last_organism
  keytype_applied_orgdb    <- state$keytype_applied_orgdb
  keytype_choices_applied  <- state$keytype_choices_applied
  annotation_keytype_initialized <- state$annotation_keytype_initialized
  source_update_token      <- state$source_update_token
  keytype_status_message   <- state$keytype_status_message
  stale_cache_accepted     <- state$stale_cache_accepted
  pending_stale_cache_organism <- state$pending_stale_cache_organism

  last_keytype_select_snapshots <- reactiveVal(list())

  same_choices_and_selection <- function(input_id, choices, selected) {
    snapshots <- last_keytype_select_snapshots()
    snapshot <- snapshots[[input_id]]
    if (is.null(snapshot)) {
      return(FALSE)
    }

    current_selected <- isolate(input[[input_id]])
    identical(snapshot$choices, choices) &&
      identical(current_selected %||% character(0), selected %||% character(0))
  }

  record_keytype_choices_applied <- function(orgdb_name, from_choices, to_choices = NULL) {
    keytype_applied_orgdb(orgdb_name)

    applied <- keytype_choices_applied()
    applied[[orgdb_name]] <- list(
      from = from_choices,
      to = to_choices %||% from_choices
    )
    keytype_choices_applied(applied)
  }


  minimal_keytype_fallback <- function() {
    c("SYMBOL", "ENTREZID", "ENSEMBL", "UNIPROT")
  }

  safe_default_keytypes_for_organism <- function(orgdb_name) {
    defaults <- tryCatch(
      get_default_keytypes_for_organism(orgdb_name),
      error = function(e) {
        debug_log(sprintf(
          "KeyType: organism-specific default resolution failed for %s: %s",
          orgdb_name %||% "<NULL>", e$message
        ), 1)
        NULL
      }
    )

    defaults <- sort(unique(as.character(defaults %||% character(0))))
    if (length(defaults) > 0) {
      return(defaults)
    }

    # True last resort: helper was unavailable/errored/empty, so retain the
    # minimal keytypes that are broadly supported by OrgDb databases.
    minimal_keytype_fallback()
  }

  update_keytype_select_input <- function(input_id, choices, selected) {
    if (same_choices_and_selection(input_id, choices, selected)) {
      debug_log(sprintf(
        "KeyType: Skipping %s update; choices and selected value are unchanged",
        input_id
      ), 2)
      return(invisible(FALSE))
    }

    updateSelectInput(session, input_id, choices = choices, selected = selected)

    snapshots <- last_keytype_select_snapshots()
    snapshots[[input_id]] <- list(choices = choices, selected = selected)
    last_keytype_select_snapshots(snapshots)

    invisible(TRUE)
  }

  apply_keytype_choices_to_dropdowns <- function(key_types, orgdb_name) {
    key_types <- sort(unique(as.character(key_types)))
    cached_keytypes(key_types)

    is_cross <- identical(input$annotation_strategy, "biomart")
    from_choices <- if (is_cross) get_biomart_compatible_keytypes(key_types) else key_types
    if (length(from_choices) == 0) {
      fallback_choices <- safe_default_keytypes_for_organism(orgdb_name)
      from_choices <- if (is_cross) get_biomart_compatible_keytypes(fallback_choices) else fallback_choices
      if (length(from_choices) == 0) from_choices <- minimal_keytype_fallback()
    }

    current_from <- input$from_keytype_annotation
    selected_from <- if (!is.null(current_from) && current_from %in% from_choices) {
      current_from
    } else if ("SYMBOL" %in% from_choices) {
      "SYMBOL"
    } else {
      from_choices[1]
    }
    update_keytype_select_input(
      "from_keytype_annotation",
      choices = from_choices,
      selected = selected_from
    )

    if (!is_cross) {
      current_to <- input$to_keytype_annotation
      selected_to <- if (!is.null(current_to) && current_to %in% key_types) {
        current_to
      } else if ("ENSEMBL" %in% key_types) {
        "ENSEMBL"
      } else {
        key_types[1]
      }
      update_keytype_select_input(
        "to_keytype_annotation",
        choices = key_types,
        selected = selected_to
      )
      record_keytype_choices_applied(orgdb_name, from_choices, key_types)
    } else {
      record_keytype_choices_applied(orgdb_name, from_choices, from_choices)
    }

    invisible(key_types)
  }

  # --------------------------------------------------------------------------
  # Immediate feedback observers
  #
  # These fire on every raw input change (no debounce) and set a status
  # message so the user sees instant confirmation that their click was
  # registered.  The actual work happens later in the debounced workers.
  # --------------------------------------------------------------------------

  observeEvent(input$species_annotation, {
    sp <- input$species_annotation
    if (!is.null(sp) && nzchar(sp)) {
      keytype_status_message(paste("Scheduling keytype update for", sp, "..."))
      debug_log(sprintf("[immediate] Species input changed to '%s' -- scheduling", sp), 2)
    }
  }, ignoreNULL = TRUE, ignoreInit = TRUE)

  observeEvent(input$target_species_annotation, {
    sp <- input$target_species_annotation
    if (!is.null(sp) && nzchar(sp) && identical(isolate(input$annotation_strategy), "biomart")) {
      keytype_status_message(paste("Scheduling target keytype update for", sp, "..."))
      debug_log(sprintf("[immediate] Target species input changed to '%s' -- scheduling", sp), 2)
    }
  }, ignoreNULL = TRUE, ignoreInit = TRUE)

  # --------------------------------------------------------------------------
  # a. Source column dropdown observer
  #    Populates the source column selector with character columns and
  #    metadata-labeled identifier columns whenever metadata changes.
  # --------------------------------------------------------------------------

  observe({
    meta <- data_def()
    req(meta)

    if (ui_config_update_active()) return()

    tryCatch({
      data <- tryCatch(get_data(), error = function(e) NULL)
      if (is.null(data)) return()

      # Include character columns
      char_cols <- names(data)[vapply(data, is.character, logical(1))]

      # Include factor columns
      factor_cols <- names(data)[vapply(data, is.factor, logical(1))]

      # Include columns whose metadata Content is exactly "Identifier"
      id_cols <- character(0)
      if ("Content" %in% names(meta) && "Column" %in% names(meta)) {
        id_cols <- meta$Column[!is.na(meta$Content) & meta$Content == "Identifier"]
      }

      if(length(id_cols)>0){
        all_candidates <- intersect(id_cols, names(data))
      } else {
        all_candidates <- unique(c(char_cols, factor_cols, id_cols))
        all_candidates <- intersect(all_candidates, names(data))
      }

      if (length(all_candidates) == 0) {
        all_candidates <- names(data)
      }

      current_selected <- isolate(input$source_column_annotation)
      selected <- if (!is.null(current_selected) && current_selected %in% all_candidates) {
        current_selected
      } else {
        all_candidates[1]
      }

      updateSelectInput(session, "source_column_annotation",
                        choices = all_candidates, selected = selected)

      debug_log(paste("Source column dropdown updated:",
                      length(all_candidates), "candidates"), 2)

    }, error = function(e) {
      debug_log(paste("Error updating source column dropdown:", e$message), 1)
    })
  })

  # --------------------------------------------------------------------------
  # b. Species change observer (debounced, latest-only)
  #    Mirrors GO module's observeEvent(input$OrgDb_GO) exactly.
  #    Full fallback chain: cache -> valid organism cache defaults ->
  #    download with progress -> minimal fallback.
  #    Updates both source and target key type dropdowns.
  #
  #    Uses debounced species input to coalesce rapid switches: when the
  #    user clicks through multiple species quickly, only the final
  #    selection triggers expensive cache/network operations.  Stale
  #    request tokens provide a second safety net.
  # --------------------------------------------------------------------------

  process_source_keytypes <- function(organism_display, force_ignore_ttl = FALSE, trigger = "species_change") {
    req(organism_display, nzchar(organism_display))

    # Everything below is isolated to avoid extra reactive dependencies
    isolate({
    # Guard: in BioMart mode the source keytype dropdown is
    # driven by observer c1d which loads BioMart keytypes for the selected
    # source species.  Do not overwrite those with AnnotationDB keytypes here.
    # Also skip in merge mode where mapping keytypes are not relevant.
    if (identical(input$annotation_strategy, "biomart") ||
        identical(input$annotation_strategy, "merge")) {
      debug_log("KeyType (observer b): non-annothub mode active - skipping AnnotationDB keytype load", 2)
      return()
    }

    organism_display <- normalize_organism_name(organism_display)
    orgdb_name <- organism_to_orgdb(organism_display)

    # -- Request versioning: increment token, capture for stale detection --
    my_token <- source_update_token() + 1L
    source_update_token(my_token)
    t0 <- proc.time()[["elapsed"]]
    debug_log(sprintf("KeyType [src-token=%d]: Processing organism (%s): %s -> %s",
                      my_token, trigger, organism_display, orgdb_name), 1)

    force_replace_refresh <- identical(trigger, "stale_cache_replace")
    user_ignore_ttl <- isTRUE(force_ignore_ttl)

    if (isTRUE(user_ignore_ttl)) {
      debug_log(sprintf(
        "KeyType [src-token=%d]: use_old decision loading keytypes.rds for %s with ignore_ttl=TRUE before defaults",
        my_token, orgdb_name
      ), 1)
      stale_key_types <- load_keytypes_from_cache(
        orgdb_name,
        max_cache_age_days = KEYTYPE_CACHE_MAX_AGE_DAYS,
        ignore_ttl = TRUE,
        debug_log = debug_log
      )

      if (!is.null(stale_key_types) && length(stale_key_types) > 0) {
        annotation_keytype_initialized(TRUE)
        keytype_last_organism(orgdb_name)
        keytype_status_message(paste("Applying cached keytypes for", organism_display, "..."))

        if (is.null(session$userData$orgdb_keytypes_session_cache) ||
            !is.list(session$userData$orgdb_keytypes_session_cache)) {
          session$userData$orgdb_keytypes_session_cache <- list()
        }
        stale_key_types <- apply_keytype_choices_to_dropdowns(stale_key_types, orgdb_name)
        shared_cache <- session$userData$orgdb_keytypes_session_cache %||% list()
        shared_cache[[orgdb_name]] <- stale_key_types
        session$userData$orgdb_keytypes_session_cache <- shared_cache

        debug_log("Loaded stale keytypes from cache by user choice", 1)
        debug_log(sprintf(
          "KeyType [src-token=%d]: Loaded %d stale keytypes for %s",
          my_token, length(stale_key_types), orgdb_name
        ), 2)
        showNotification(paste("Loaded cached key types for", organism_display),
                         type = "message", duration = 2)
        keytype_status_message(NULL)
        return()
      }

      debug_log("User chose stale cache, but keytypes.rds missing; using organism defaults", 1)
      debug_log(sprintf(
        "KeyType [src-token=%d]: keytypes.rds was missing or invalid for %s; applying organism defaults",
        my_token, orgdb_name
      ), 2)
      annotation_keytype_initialized(TRUE)
      keytype_last_organism(orgdb_name)
      default_key_types <- safe_default_keytypes_for_organism(orgdb_name)
      apply_keytype_choices_to_dropdowns(default_key_types, orgdb_name)
      showNotification(paste("Using default key types for", organism_display),
                       type = "warning", duration = 3)
      keytype_status_message(NULL)
      return()
    }

    # Skip only duplicate in-flight work or a same-organism event whose choices
    # have already been applied.  keytype_last_organism alone is not proof that
    # updateSelectInput() ran (notably on the lazy Homo sapiens startup path), so
    # same-organism refreshes can still expand the four startup defaults.
    same_organism <- identical(orgdb_name, keytype_last_organism())
    choices_already_applied <- identical(orgdb_name, keytype_applied_orgdb())
    if (!isTRUE(force_replace_refresh) && same_organism &&
        (isTRUE(keytype_loading()) || choices_already_applied)) {
      debug_log(sprintf(
        "KeyType [src-token=%d]: Same organism with %s, skipping",
        my_token,
        if (isTRUE(keytype_loading())) "in-flight work" else "applied choices"
      ), 2)
      keytype_status_message(NULL)
      return()
    }

    # On the initial default-species event, the UI already ships with the full
    # safe Homo sapiens defaults for both keytype dropdowns. Accept them as-is so
    # startup does not immediately touch cache state, show progress, or download
    # data. Later user species changes still follow the full cache/download path.
    startup_default_keytypes <- safe_default_keytypes_for_organism("org.Hs.eg.db")
    startup_has_usable_defaults <-
      !is.null(input$from_keytype_annotation) &&
      nzchar(input$from_keytype_annotation) &&
      input$from_keytype_annotation %in% startup_default_keytypes &&
      !is.null(input$to_keytype_annotation) &&
      nzchar(input$to_keytype_annotation) &&
      input$to_keytype_annotation %in% startup_default_keytypes

    if (!isTRUE(annotation_keytype_initialized()) &&
        identical(organism_display, "Homo sapiens") &&
        isTRUE(startup_has_usable_defaults)) {
      annotation_keytype_initialized(TRUE)
      keytype_last_organism(orgdb_name)
      keytype_status_message(NULL)
      if (is.null(session$userData$orgdb_keytypes_session_cache) ||
          !is.list(session$userData$orgdb_keytypes_session_cache)) {
        session$userData$orgdb_keytypes_session_cache <- list()
      }
      startup_result <- resolve_orgdb_keytypes(
        orgdb_name,
        mode = "startup",
        session_cache = session$userData$orgdb_keytypes_session_cache,
        max_keytype_cache_age_days = KEYTYPE_CACHE_MAX_AGE_DAYS,
        max_organism_cache_age_days = ORGANISM_CACHE_MAX_AGE_DAYS,
        debug_log = debug_log
      )
      # Startup-lazy resolution must never shrink the default Homo sapiens UI
      # choices below the documented safe defaults. Cache refresh/user-triggered
      # loads can still expand or replace them when needed.
      startup_keytypes <- unique(c(startup_default_keytypes, startup_result$keytypes %||% character(0)))
      shared_cache <- session$userData$orgdb_keytypes_session_cache %||% list()
      shared_cache[[orgdb_name]] <- startup_keytypes
      session$userData$orgdb_keytypes_session_cache <- shared_cache
      cached_keytypes(startup_keytypes)
      debug_log(sprintf("KeyType [src-token=%d]: %s; retained %d startup defaults",
                        my_token, startup_result$message, length(startup_keytypes)), 1)
      return()
    }

    annotation_keytype_initialized(TRUE)

    # -- Stale cache modal gate --
    # If an AnnotationHub cache exists but is older than the organism cache TTL and
    # the user has not yet made a decision for this organism in this session, show a
    # modal asking whether to replace or keep the stale cache.
    decisions <- stale_cache_accepted()
    cache_age <- get_organism_cache_age_days(orgdb_name, debug_log = debug_log)

    if (!isTRUE(force_ignore_ttl) && !is.null(cache_age) && cache_age > ORGANISM_CACHE_MAX_AGE_DAYS) {
      decision <- decisions[[orgdb_name]]
      if (is.null(decision)) {
        # No decision yet -- show modal and pause the keytype load
        debug_log(sprintf(
          "KeyType [src-token=%d]: Stale cache detected for %s (%.1f days) -- showing modal",
          my_token, orgdb_name, cache_age), 1)

        pending_stale_cache_organism(list(
          orgdb_name       = orgdb_name,
          organism_display = organism_display,
          cache_age        = cache_age
        ))

        keytype_status_message(
          sprintf("Cache for %s is %.0f days old -- waiting for your decision...",
                  organism_display, cache_age))

        showModal(modalDialog(
          title = "Annotation Cache Outdated",
          tags$p(
            "The local AnnotationHub cache for ",
            tags$b(organism_display),
            sprintf(" is %.0f days old (last updated %s ago).",
                    cache_age,
                    if (cache_age >= 1) {
                      paste0(floor(cache_age), " day", if (floor(cache_age) != 1) "s" else "")
                    } else {
                      paste0(round(cache_age * 24, 1), " hours")
                    })
          ),
          tags$p("Would you like to download a fresh cache or continue using the existing one?"),
          footer = tagList(
            actionButton(ns("stale_cache_replace"), "Replace Cache",
                         class = "btn-primary"),
            actionButton(ns("stale_cache_use_old"), "Use Existing Cache",
                         class = "btn-default")
          ),
          size = "m",
          easyClose = FALSE
        ))
        return()

      } else if (identical(decision, "use_old")) {
        user_ignore_ttl <- TRUE
        debug_log(sprintf(
          "KeyType [src-token=%d]: User chose use_old for %s (%.1f days); stale keytype TTL will be ignored",
          my_token, orgdb_name, cache_age), 1)
      } else if (identical(decision, "replace")) {
        debug_log(sprintf(
          "KeyType [src-token=%d]: User chose replace for %s (%.1f days); stale keytype cache will be bypassed",
          my_token, orgdb_name, cache_age), 1)
      }
    }

    keytype_loading(TRUE)
    keytype_last_organism(orgdb_name)
    keytype_status_message(paste("Loading keytypes for", organism_display, "..."))

    tryCatch({

      # 1. Try loading key types from GO cache (disk)
      withProgress(message = paste("Updating key types for", organism_display), value = 0.1, {

        t_progress <- proc.time()[["elapsed"]]
        debug_log(sprintf("KeyType [src-token=%d]: time-to-progress-start=%.3fs",
                          my_token, t_progress - t0), 2)

        incProgress(0.1, detail = "Checking cache...")
        keytype_status_message(paste("Checking cache for", organism_display, "..."))
        if (is.null(session$userData$orgdb_keytypes_session_cache) ||
            !is.list(session$userData$orgdb_keytypes_session_cache)) {
          session$userData$orgdb_keytypes_session_cache <- list()
        }

        resolver_mode <- if (isTRUE(force_replace_refresh)) "force_refresh" else "user_change"
        if (isTRUE(force_replace_refresh)) {
          debug_log(sprintf(
            "KeyType [src-token=%d]: Replace path using force_refresh resolver for %s",
            my_token, orgdb_name
          ), 1)
        }
        result <- resolve_orgdb_keytypes(
          orgdb_name,
          mode = resolver_mode,
          session_cache = session$userData$orgdb_keytypes_session_cache,
          max_keytype_cache_age_days = KEYTYPE_CACHE_MAX_AGE_DAYS,
          max_organism_cache_age_days = ORGANISM_CACHE_MAX_AGE_DAYS,
          ignore_ttl = user_ignore_ttl,
          debug_log = debug_log
        )
        key_types <- result$keytypes
        if (identical(result$source, "static_default") ||
            identical(result$source, "minimal") ||
            is.null(key_types) || length(key_types) == 0) {
          key_types <- safe_default_keytypes_for_organism(orgdb_name)
        }

        # -- Stale check after cache/download/default resolution --
        if (!identical(source_update_token(), my_token)) {
          debug_log(sprintf("KeyType [src-token=%d]: STALE after resolver (current=%d) -- discarding",
                            my_token, source_update_token()), 1)
          keytype_status_message(sprintf("Discarded stale request for %s (newer selection active)", organism_display))
          return()
        }

        if (!is.null(key_types) && length(key_types) > 0) {
          shared_cache <- session$userData$orgdb_keytypes_session_cache %||% list()
          shared_cache[[orgdb_name]] <- key_types
          session$userData$orgdb_keytypes_session_cache <- shared_cache

          incProgress(0.3, detail = sprintf("Resolved from %s...", result$source))
          keytype_status_message(paste("Applying keytypes for", organism_display, "..."))
          debug_log(sprintf(
            "KeyType [src-token=%d]: Resolver returned %d types from %s (elapsed=%.3fs)",
            my_token, length(key_types), result$source, proc.time()[["elapsed"]] - t0
          ), 1)
        }

          # -- Stale check before applying fetch results --
          if (!identical(source_update_token(), my_token) ||
              !identical(input$species_annotation, organism_display)) {
            debug_log(sprintf("KeyType [src-token=%d]: STALE before UI apply (fetch path) -- discarding",
                              my_token), 1)
            keytype_status_message(sprintf("Discarded stale request for %s", organism_display))
            return()
          }

          if (!is.null(key_types) && length(key_types) > 0) {

            key_types <- sort(unique(key_types))
            cached_keytypes(key_types)
            keytype_status_message(paste("Applying keytypes for", organism_display, "..."))

            incProgress(0.3, detail = "Updating keytype dropdown...")
            debug_log(sprintf("KeyType [src-token=%d]: Updating UI with %d types for %s",
                              my_token, length(key_types), orgdb_name), 1)

            is_cross <- identical(input$annotation_strategy, "biomart")
            from_choices <- if (is_cross) get_biomart_compatible_keytypes(key_types) else key_types
            if (length(from_choices) == 0) {
              fallback_choices <- safe_default_keytypes_for_organism(orgdb_name)
              from_choices <- if (is_cross) get_biomart_compatible_keytypes(fallback_choices) else fallback_choices
              if (length(from_choices) == 0) from_choices <- minimal_keytype_fallback()
            }

            current_from <- input$from_keytype_annotation
            selected_from <- if (!is.null(current_from) && current_from %in% from_choices) {
              current_from
            } else if ("SYMBOL" %in% from_choices) {
              "SYMBOL"
            } else {
              from_choices[1]
            }
            update_keytype_select_input(
              "from_keytype_annotation",
              choices = from_choices,
              selected = selected_from
            )

            if (!is_cross) {
              current_to <- input$to_keytype_annotation
              selected_to <- if (!is.null(current_to) && current_to %in% key_types) {
                current_to
              } else if ("ENSEMBL" %in% key_types) {
                "ENSEMBL"
              } else {
                key_types[1]
              }
              update_keytype_select_input(
                "to_keytype_annotation",
                choices = key_types,
                selected = selected_to
              )
              record_keytype_choices_applied(orgdb_name, from_choices, key_types)
            } else {
              record_keytype_choices_applied(orgdb_name, from_choices, from_choices)
            }

            incProgress(0.1, detail = "Finalizing UI state")

            showNotification(paste("Loaded key types for", organism_display),
                              type = "message", duration = 2)

            debug_log(sprintf("KeyType [src-token=%d]: Applied fetch results for %s (total=%.3fs)",
                              my_token, organism_display, proc.time()[["elapsed"]] - t0), 1)

          } else {

            # 4. Final default fallback. safe_default_keytypes_for_organism()
            # retains the minimal four-keytype vector only as a true last resort
            # when organism-specific defaults cannot be resolved.
            debug_log(sprintf("KeyType [src-token=%d]: All methods failed, using default fallback",
                              my_token), 1)

            minimal_types <- safe_default_keytypes_for_organism(orgdb_name)
            cached_keytypes(minimal_types)

            is_cross <- identical(input$annotation_strategy, "biomart")
            update_keytype_select_input(
              "from_keytype_annotation",
              choices = minimal_types,
              selected = "SYMBOL"
            )
            if (!is_cross) {
              update_keytype_select_input(
                "to_keytype_annotation",
                choices = minimal_types,
                selected = "ENSEMBL"
              )
              record_keytype_choices_applied(orgdb_name, minimal_types, minimal_types)
            } else {
              record_keytype_choices_applied(orgdb_name, minimal_types, minimal_types)
            }

            showNotification(paste("Using minimal key types for", organism_display),
                              type = "warning", duration = 3)
          }

      }) # end withProgress

      # Invalidate session OrgDb cache if species changed
      if (!identical(cached_orgdb_name(), orgdb_name)) {
        cached_org_db(NULL)
        cached_orgdb_name(orgdb_name)
        debug_log("Session OrgDb cache invalidated (species changed)", 2)
      }

    }, finally = {
      keytype_loading(FALSE)
      keytype_status_message(NULL)
      debug_log(sprintf("KeyType [src-token=%d]: Processing completed for %s (total=%.3fs)",
                        my_token, orgdb_name, proc.time()[["elapsed"]] - t0), 2)
    })
    }) # end isolate
  }

  observe({
    # Reactive dependency: only the debounced species value
    organism_display <- species_src_debounced()
    process_source_keytypes(organism_display, trigger = "species_change")
  })


  # --------------------------------------------------------------------------
  # b2. Stale cache modal -- "Use Existing Cache" button
  #     Records the user's decision and resumes the paused keytype load
  #     directly with ignore_ttl = TRUE.
  # --------------------------------------------------------------------------

  observeEvent(input$stale_cache_use_old, {
    removeModal()

    pending <- isolate(pending_stale_cache_organism())
    if (is.null(pending)) return()

    orgdb_name       <- pending$orgdb_name
    organism_display <- pending$organism_display

    debug_log(sprintf("Stale cache modal: user chose 'Use Existing Cache' for %s",
                      orgdb_name), 1)

    # Record decision for this organism in the session
    decisions <- isolate(stale_cache_accepted())
    decisions[[orgdb_name]] <- "use_old"
    stale_cache_accepted(decisions)

    pending_stale_cache_organism(NULL)

    debug_log(sprintf(
      "Stale cache modal: use_old path resuming shared keytype worker with ignore_ttl=TRUE for %s",
      orgdb_name
    ), 1)
    process_source_keytypes(
      organism_display,
      force_ignore_ttl = TRUE,
      trigger = "stale_cache_use_old"
    )
  })


  # --------------------------------------------------------------------------
  # b3. Stale cache modal -- "Replace Cache" button
  #     Records the decision and resumes the paused keytype load directly
  #     (ignore_ttl stays FALSE so TTL-expired caches are skipped and a
  #     download occurs).
  # --------------------------------------------------------------------------

  observeEvent(input$stale_cache_replace, {
    removeModal()

    pending <- isolate(pending_stale_cache_organism())
    if (is.null(pending)) return()

    orgdb_name       <- pending$orgdb_name
    organism_display <- pending$organism_display

    debug_log(sprintf("Stale cache modal: user chose 'Replace Cache' for %s",
                      orgdb_name), 1)

    # Record decision so the modal won't re-appear for this organism
    decisions <- isolate(stale_cache_accepted())
    decisions[[orgdb_name]] <- "replace"
    stale_cache_accepted(decisions)

    pending_stale_cache_organism(NULL)

    debug_log(sprintf(
      "Stale cache modal: replace path resuming shared keytype worker with ignore_ttl=FALSE for %s",
      orgdb_name
    ), 1)

    process_source_keytypes(
      organism_display,
      force_ignore_ttl = FALSE,
      trigger = "stale_cache_replace"
    )
  })

  # --------------------------------------------------------------------------
  # e. Status render output
  #    Displays summary of last mapping operation.
  # --------------------------------------------------------------------------

  output$annotation_status <- renderUI({
    # Show immediate keytype loading status when active
    kt_msg <- keytype_status_message()
    result <- last_mapping_result()

    if (!is.null(kt_msg) && nzchar(kt_msg)) {
      return(div(
        class = "well well-sm",
        style = "margin-top: 5px; padding: 10px; background-color: #eef6ff;",
        tags$b("Keytype Loading Status"),
        tags$p(style = "margin-top: 5px; font-size: 13px;", kt_msg)
      ))
    }

    if (is.null(result)) return(NULL)

    pct <- round(result$n_mapped / result$n_total * 100, 1)

    # Build backend detail rows
    step_rows <- NULL
    if (isTRUE(result$cross_species)) {
      if (isTRUE(result$direct_biomart)) {
        step_rows <- tagList(
          tags$tr(tags$td("Backend:"), tags$td("BioMart direct (single step)")),
          if (!is.null(result$step1_mapped))
            tags$tr(tags$td("Orthologs found:"),
                    tags$td(sprintf("%d / %d (%.1f%%)",
                                    result$step1_mapped, result$n_total,
                                    round(100 * result$step1_mapped / result$n_total, 1))))
        )
      } else if (isTRUE(result$two_step) && !is.null(result$step1_mapped)) {
        step1_pct <- round(100 * result$step1_mapped / result$n_total, 1)
        step_rows <- tagList(
          tags$tr(tags$td("Backend:"), tags$td("BioMart + OrgDb (two-step fallback)")),
          tags$tr(tags$td("Step 1 (orthologs):"),
                  tags$td(sprintf("%d / %d (%.1f%%)",
                                  result$step1_mapped, result$n_total, step1_pct))),
          tags$tr(tags$td("Step 2 (ID conversion):"),
                  tags$td(sprintf("%d / %d converted to %s",
                                  result$step2_mapped, result$step2_input,
                                  result$to_keytype)))
        )
      } else if (!is.null(result$step1_mapped)) {
        step_rows <- tagList(
          tags$tr(tags$td("Backend:"), tags$td("BioMart (single step)")),
          tags$tr(tags$td("Orthologs found:"),
                  tags$td(sprintf("%d / %d (%.1f%%)",
                                  result$step1_mapped, result$n_total,
                                  round(100 * result$step1_mapped / result$n_total, 1))))
        )
      }
    }

    # Build mode label
    mode_label <- if (isTRUE(result$merge_mode)) {
      "Identifier Merging"
    } else if (isTRUE(result$cross_species)) {
      "BioMart Intra-/Inter-species (BioMart)"
    } else {
      "Annotation Hub Intraspecies (OrgDb)"
    }

    # Cache date row (not applicable for merge mode)
    cache_date_label <- if (isTRUE(result$merge_mode)) {
      NULL
    } else if (!is.null(result$cache_date) && nzchar(result$cache_date)) {
      result$cache_date
    } else {
      "cache date unavailable"
    }

    # Mapping database date row (BioMart mode only)
    mapping_cache_date_label <- if (isTRUE(result$cross_species) &&
                                    !is.null(result$mapping_cache_date) &&
                                    nzchar(result$mapping_cache_date)) {
      result$mapping_cache_date
    } else {
      NULL
    }

    # Build table rows conditionally
    detail_rows <- tagList(
      tags$tr(tags$td("Source column:"), tags$td(result$source_col)),
      tags$tr(tags$td("New column:"), tags$td(tags$code(result$new_col))),
      tags$tr(tags$td("Mapping:"),
              tags$td(paste(result$from_keytype, "->", result$to_keytype))),
      tags$tr(tags$td("Mode:"), tags$td(mode_label)),
      tags$tr(tags$td("Strategy:"), tags$td(result$strategy)),
      tags$tr(tags$td("Merged/Mapped:"),
              tags$td(sprintf("%d / %d (%s%%)", result$n_mapped,
                              result$n_total, pct))),
      tags$tr(tags$td("Empty/Unmapped:"), tags$td(result$n_unmapped)),
      if (!is.null(cache_date_label))
        tags$tr(tags$td(
          if (isTRUE(result$cross_species)) "Keytype cache date:" else "Cache date:"
        ), tags$td(cache_date_label)),
      if (!is.null(mapping_cache_date_label))
        tags$tr(tags$td("Database cache date:"), tags$td(mapping_cache_date_label)),
      step_rows
    )

    # Methods text block (merge mode only)
    methods_block <- NULL
    if (isTRUE(result$merge_mode) &&
        !is.null(result$methods_text) && nzchar(result$methods_text)) {
      methods_block <- div(
        style = "margin-top: 10px; padding: 10px; background-color: #f0f7f0; border: 1px solid #c3e6c3; border-radius: 4px;",
        tags$b("Methods Section Text"),
        tags$p(
          style = "font-size: 12px; color: #666; margin-top: 2px; margin-bottom: 6px;",
          "Copy the text below into the methods section of your manuscript."
        ),
        tags$textarea(
          readonly = "readonly",
          style = paste0(
            "width: 100%; min-height: 80px; font-size: 13px; padding: 8px; ",
            "border: 1px solid #ccc; border-radius: 3px; resize: vertical; ",
            "background-color: #fff; font-family: serif;"
          ),
          result$methods_text
        ),
        actionButton(
          ns("copy_merge_methods"),
          label = tagList(icon("clipboard"), " Copy to clipboard"),
          class = "btn-default btn-sm",
          style = "margin-top: 6px;"
        )
      )
    }

    div(
      class = "well well-sm",
      style = "margin-top: 5px; padding: 10px; background-color: #f9f9f9;",
      tags$b("Last Mapping Result"),
      tags$table(
        style = "width: 100%; font-size: 13px; margin-top: 5px;",
        detail_rows
      ),
      methods_block
    )
  })

  # --------------------------------------------------------------------------
  # Copy methods text to clipboard (merge mode).
  # Uses the same copy_to_clipboard() pattern as the Volcano module's
  # Protein Selection & Labeling section.
  # --------------------------------------------------------------------------

  observeEvent(input$copy_merge_methods, {
    tryCatch({
      result <- last_mapping_result()
      if (!is.null(result) && !is.null(result$methods_text) &&
          nzchar(result$methods_text)) {
        copy_to_clipboard(result$methods_text, debug_log)
        showNotification("Methods text copied to clipboard",
                         type = "message", duration = 2)
      } else {
        showNotification("No methods text available to copy",
                         type = "warning", duration = 3)
      }
    }, error = function(e) {
      debug_log(paste("Error in copy_merge_methods:", e$message), 1)
      showNotification("Error copying to clipboard", type = "error")
    })
  })

  # --------------------------------------------------------------------------
  # f. UI config import observer
  #    Detects changes to the external UI_config and applies the annotation
  #    sub-configuration if present.
  # --------------------------------------------------------------------------

  observeEvent(get_ui_config(), {
    tryCatch({
      config <- get_ui_config()

      if (is.null(config)) {
        debug_log("UI config observer: no config detected - skipping", 2)
        return()
      }

      cfg <- NULL
      if (!is.null(config$annotation_configurations)) {
        cfg <- config$annotation_configurations
      }

      if (!is.null(cfg) && is.list(cfg)) {
        isolate({
          apply_ui_config(cfg)
        })
        debug_log("Annotation UI configuration applied from import", 1)
      }

      current_ui_config(config)

    }, error = function(e) {
      debug_log(paste("Error in UI config observer:", e$message), 1)
    })
  }, ignoreNULL = TRUE, ignoreInit = TRUE)
}
