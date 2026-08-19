auto_regex_content_redundancy_input_id <- function(rule_id) {
  rule_id <- chr(rule_id)
  if (length(rule_id) != 1L || is.na(rule_id[[1L]]) || !nzchar(rule_id[[1L]]))
    return("content_redundancy_rule_missing")
  bytes <- as.integer(charToRaw(enc2utf8(rule_id[[1L]])))
  paste0("content_redundancy_rule_", paste(sprintf("%02x", bytes), collapse = ""))
}

auto_regex_normalize_redundancy_overrides <- function(values) {
  if (is.null(values) || !length(values)) return(integer())
  value_names <- names(values)
  values <- suppressWarnings(as.integer(values)); names(values) <- value_names
  keep <- !is.na(values) & nzchar(names(values)); values <- values[keep]
  if (!length(values)) return(integer())
  values[order(names(values), method = "radix")]
}

auto_regex_collect_content_redundancy_overrides <- function(rules, input, state, input_id) {
    table <- if (is.list(rules)) {
      rules$table
    } else {
      NULL
    }

    if (!is.data.frame(table) ||
        !nrow(table) ||
        !"RuleId" %in%
        names(table)) {
      return(integer())
    }

    existing <-
      isolate(
        state$redundancy_overrides()
      )

    result <- integer()

    for (i in seq_len(
      nrow(table)
    )) {

      rule_id <- chr(
        table$RuleId[[i]]
      )

      if (!nzchar(rule_id))
        next

      input_value <- isolate(
        input[[input_id(rule_id)]]
      )

      # If the control has not yet been rendered, retain the last explicit
      # override for that canonical RuleId.
      if (is.null(input_value)) {

        if (length(existing) &&
            !is.null(names(existing)) &&
            rule_id %in%
            names(existing)) {

          result[[rule_id]] <-
            auto_regex_redundancy_value(
              existing[[rule_id]],
              fallback = 0L
            )
        }

        next
      }

      value <- as.character(
        input_value
      )[[1L]]

      if (identical(
        value,
        "global"
      ) ||
      !nzchar(value)) {
        next
      }

      parsed <- suppressWarnings(
        as.integer(value)
      )

      if (is.na(parsed))
        next

      result[[rule_id]] <-
        auto_regex_redundancy_value(
          parsed,
          fallback = 0L
        )
    }

    result
}

auto_regex_register_redundancy_handlers <- function(context) {
  list2env(unclass(context), envir = environment())
  global_redundancy_request <-
    shiny::debounce(
      shiny::reactive({

        auto_regex_redundancy_value(
          input$redundancy,
          fallback =
            shiny::isolate(
              state$global_redundancy()
            )
        )
      }),
      millis = 600
    )

  shiny::observeEvent(
    global_redundancy_request(),
    {

      if (!identical(
        session$userData$auto_regex_handler_tokens[[handler_key]],
        handler_token
      )) {
        return()
      }

      requested <-
        global_redundancy_request()

      previous <-
        shiny::isolate(
          state$global_redundancy()
        )

      if (identical(
        requested,
        previous
      )) {
        return()
      }

      logger(
        sprintf(
          "Global Regex redundancy changed: %d -> %d.",
          previous,
          requested
        ),
        2L
      )

      overrides <-
        shiny::isolate(
          state$redundancy_overrides()
        )

      # Before inference there is no cache. Still retain the requested global
      # value so the subsequent full inference uses it.
      if (is.null(
        shiny::isolate(
          state$redundancy_base()
        )
      )) {

        state$global_redundancy(
          requested
        )

        return()
      }

      rebuild_redundancy_candidate(
        global_redundancy =
          requested,
        overrides =
          overrides,
        reason =
          sprintf(
            "global redundancy %d -> %d",
            previous,
            requested
          )
      )
    },
    ignoreInit = TRUE,
    ignoreNULL = TRUE
  )

  redundancy_override_request <-
    shiny::debounce(
      shiny::reactive({

      rules <-
        state$candidate_rules()

      table <-
        if (is.list(rules)) {
          rules$table
        } else {
          NULL
        }

      if (!is.data.frame(table) ||
          !nrow(table)) {
        return(NULL)
      }

      values <-
        vapply(
          seq_len(nrow(table)),
          function(i) {

            rule_id <- chr(
              table$RuleId[[i]]
            )

            value <-
              input[[content_redundancy_input_id(rule_id)]]

            if (is.null(value)) {
              return(NA_character_)
            }

            as.character(value)[[1L]]
          },
          character(1)
        )

      names(values) <-
        chr(
          table$RuleId
        )

      values
    }),
  millis = 600
  )


  shiny::observeEvent(
    redundancy_override_request(),
    {

      if (!identical(
        session$userData$auto_regex_handler_tokens[[handler_key]],
        handler_token
      )) {
        return()
      }

      if (isTRUE(
        state$processing()
      )) {
        return()
      }

      requested <-
        redundancy_override_request()

      if (is.null(requested)) {
        return()
      }

      existing <-
        normalize_redundancy_overrides(
          shiny::isolate(
            state$redundancy_overrides()
          )
        )

      next_overrides <-
        integer()

      for (rule_id in names(requested)) {

        value <-
          requested[[rule_id]]

        # Controls temporarily become NULL when their renderUI is recreated.
        # Preserve the existing setting in that case.
        if (is.na(value)) {

          if (rule_id %in%
              names(existing)) {

            next_overrides[[rule_id]] <-
              existing[[rule_id]]
          }

          next
        }

        if (!nzchar(value) ||
            identical(
              value,
              "global"
            )) {
          next
        }

        parsed <-
          suppressWarnings(
            as.integer(value)
          )

        if (is.na(parsed)) {
          next
        }

        next_overrides[[rule_id]] <-
          auto_regex_redundancy_value(
            parsed,
            fallback = 0L
          )
      }

      next_overrides <-
        normalize_redundancy_overrides(
          next_overrides
        )

      if (identical(
        existing,
        next_overrides
      )) {
        return()
      }

      changed_rule_ids <-
        union(
          names(existing),
          names(next_overrides)
        )

      for (rule_id in changed_rule_ids) {

        old_value <-
          if (rule_id %in%
              names(existing)) {
            as.character(
              existing[[rule_id]]
            )
          } else {
            "global"
          }

        new_value <-
          if (rule_id %in%
              names(next_overrides)) {
            as.character(
              next_overrides[[rule_id]]
            )
          } else {
            "global"
          }

        if (!identical(
          old_value,
          new_value
        )) {

          logger(
            sprintf(
              paste0(
                "Per-rule Regex redundancy changed | ",
                "RuleId=%s | %s -> %s."
              ),
              rule_id,
              old_value,
              new_value
            ),
            2L
          )
        }
      }

      rebuild_redundancy_candidate(
        global_redundancy =
          shiny::isolate(
            state$global_redundancy()
          ),
        overrides =
          next_overrides,
        reason =
          "per-rule redundancy override changed"
      )
    },
    ignoreInit = TRUE
  )

  rebuild_redundancy_candidate <- function(
    global_redundancy,
    overrides,
    reason = "redundancy control changed") {

    if (isTRUE(
      state$processing()
    )) {
      return(invisible(FALSE))
    }

    candidate <-
      shiny::isolate(
        state$candidate_rules()
      )

    cache <-
      shiny::isolate(
        state$redundancy_base()
      )

    if (!is.list(candidate) ||
        !is.list(cache) ||
        !is.list(cache$base) ||
        !is.list(cache$base$rules) ||
        !is.list(cache$base$analysis_cache) ||
        !is.data.frame(cache$metadata)) {

      logger(
        paste(
          "Regex redundancy change recorded, but no reusable",
          "redundancy ladder cache is available. Infer Rules once",
          "to establish the cache."
        ),
        2L
      )

      return(invisible(FALSE))
    }

    if (isTRUE(
      shiny::isolate(
        state$stale()
      )
    )) {

      logger(
        "Regex redundancy rebuild skipped because the Auto RegEx candidate is stale.",
        2L
      )

      return(invisible(FALSE))
    }

    current_source <-
      tryCatch(
        source_snapshot(),
        error = function(e) NULL
      )

    if (is.null(current_source) ||
        !identical(
          cache$source_signature,
          current_source$signature
        )) {

      logger(
        paste(
          "Regex redundancy rebuild rejected because",
          "the effective source changed."
        ),
        1L
      )

      auto_regex_invalidate_effective_source(
        state
      )

      return(invisible(FALSE))
    }

    global_redundancy <-
      auto_regex_redundancy_value(
        global_redundancy,
        fallback = 0L
      )

    overrides <-
      normalize_redundancy_overrides(
        overrides
      )

    started <-
      proc.time()[["elapsed"]]

    rebuilt <-
      tryCatch(
        auto_regex_apply_cached_content_redundancy(
          metadata = cache$metadata,
          table = cache$base$rules$table,
          cache = cache$base$analysis_cache,
          redundancy = global_redundancy,
          redundancy_overrides = overrides
        ),
        error = function(e) {

          logger(
            sprintf(
              "Regex redundancy rebuild failed: %s",
              conditionMessage(e)
            ),
            1L
          )

          NULL
        }
      )

    if (is.null(rebuilt)) {
      return(invisible(FALSE))
    }

    refreshed_rules <-
      cache$base$rules

    refreshed_rules$table <-
      rebuilt$table

    validation_errors <-
      tryCatch(
        validate_export(
          refreshed_rules,
          cache$metadata
        ),
        error = function(e) {
          conditionMessage(e)
        }
      )

    if (length(validation_errors)) {

      logger(
        sprintf(
          paste0(
            "Regex redundancy rebuild rejected during payload validation: ",
            "%s"
          ),
          validation_errors[[1L]]
        ),
        1L
      )

      return(invisible(FALSE))
    }

    old_table <-
      candidate$table

    new_table <-
      refreshed_rules$table

    old_position <-
      match(
        new_table$RuleId,
        old_table$RuleId
      )

    old_include <-
      rep(
        "",
        nrow(new_table)
      )

    valid_old <-
      !is.na(old_position)

    old_include[valid_old] <-
      chr(
        old_table$Include[
          old_position[valid_old]
        ]
      )

    new_include <-
      chr(
        new_table$Include
      )

    changed <-
      which(
        old_include !=
          new_include
      )

    elapsed_ms <-
      (
        proc.time()[["elapsed"]] -
          started
      ) * 1000

    state$global_redundancy(
      global_redundancy
    )

    state$redundancy_overrides(
      overrides
    )

    refreshed <-
      state$refresh_candidate(
        candidate_rules =
          refreshed_rules,
        payload =
          refreshed_rules,
        redundancy_lineage =
          rebuilt$lineage,
        rebuild_ms =
          elapsed_ms
      )

    if (!isTRUE(refreshed)) {

      logger(
        "Regex redundancy rebuild was valid but could not replace the current candidate.",
        1L
      )

      return(invisible(FALSE))
    }

    logger(
      sprintf(
        paste0(
          "Regex redundancy candidate loaded from cache | reason: %s | ",
          "global=%d | overrides=%d | changed rules=%d | %.1f ms."
        ),
        reason,
        global_redundancy,
        length(overrides),
        length(changed),
        elapsed_ms
      ),
      1L
    )

    if (is.data.frame(
      rebuilt$lineage
    ) &&
    nrow(
      rebuilt$lineage
    )) {

      for (i in seq_len(
        nrow(rebuilt$lineage)
      )) {

        row <-
          rebuilt$lineage[
            i,
            ,
            drop = FALSE
          ]

        override_text <-
          if (is.na(
            row$OverrideRedundancy[[1L]]
          )) {
            "global"
          } else {
            as.character(
              row$OverrideRedundancy[[1L]]
            )
          }

        logger(
          sprintf(
            paste0(
              "Regex redundancy resolved | RuleId=%s | Content=%s | ",
              "global=%d | override=%s | requested=%d | effective=%d | ",
              "regex='%s'."
            ),
            row$RuleId[[1L]],
            row$Content[[1L]],
            row$GlobalRedundancy[[1L]],
            override_text,
            row$RequestedRedundancy[[1L]],
            row$EffectiveRedundancy[[1L]],
            auto_regex_safe_value(
              row$FinalInclude[[1L]],
              120L
            )
          ),
          2L
        )
      }
    }

    if (length(changed)) {

      for (i in changed) {

        logger(
          sprintf(
            paste0(
              "Inferred Content Rule updated | RuleId=%s | ",
              "'%s' -> '%s'."
            ),
            new_table$RuleId[[i]],
            auto_regex_safe_value(
              old_include[[i]],
              100L
            ),
            auto_regex_safe_value(
              new_include[[i]],
              100L
            )
          ),
          2L
        )
      }
    }

    invisible(TRUE)
  }

  invisible(NULL)
}
