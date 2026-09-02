# utils.R
# session_management.R is auto-sourced by Shiny (no explicit source() needed)

## Functions
closeUnusedConnections <- function(exclude_patterns = NULL) {

  # Überprüfe offene Verbindungen gezielt
  open_conns <- showConnections(all = TRUE)
  if (nrow(open_conns) == 0) {
    return(invisible(NULL))
  }

  # Konvertiere die Matrix in einen DataFrame, falls nötig
  if (!is.data.frame(open_conns)) {
    open_conns <- as.data.frame(open_conns, stringsAsFactors = FALSE)
  }

  # Baue das Muster für auszuschließende Verbindungen
  exclude_regex <- paste0(exclude_patterns, collapse = "|")

  # Wähle nur Verbindungen vom Typ "file" oder "url" aus und schließe bestimmte Muster aus
  unused_conns <- open_conns[open_conns$opened == "opened" &
                               open_conns$description != "" &
                               !grepl(exclude_regex, open_conns$description), ]

  # Prüfen, ob unbenutzte Verbindungen gefunden wurden
  if (nrow(unused_conns) > 0) {
    for (conn_id in as.integer(rownames(unused_conns))) {

      # Versuche die Verbindung sicher zu holen
      conn <- tryCatch(getConnection(conn_id), error = function(e) {
        debug_log(sprintf("Error accessing connection ID: %d - %s", conn_id, e$message), level = 2)
        NULL
      })

      # Schließe die Verbindung nur, wenn sie gültig ist und noch offen ist
      if (!is.null(conn) && inherits(conn, "connection") && isOpen(conn)) {
        tryCatch({
          close(conn)
          debug_log(sprintf("Closed unused connection ID: %d", conn_id), level = 2)
        }, error = function(e) {
          debug_log(sprintf("Error closing connection ID: %d - %s", conn_id, e$message), level = 2)
        })
      }
    }
  } else {
    debug_log("No unused connections to close.", level = 2)
  }
}

#' Convert pixels to inches
#' @param px pixel value
#' @param device_pixel_ratio device pixel ratio
#' @param scale scaling factor
#' @param dpi dots per inch
#' @return inches value
px2inch <- function(px, device_pixel_ratio = 1, scale = 1, dpi = 96) {
  return((px / device_pixel_ratio) * scale / dpi)
}

# Operator function
`%||%` <- function(x, y) {
  if (!is.null(x)) x else y
}


truncate_text <- function(x, max_chars = 40, ellipsis = " [...]") {
  # Funktion zur Bearbeitung eines einzelnen Character-Vektors
  truncate_vector <- function(vec) {
    # vapply stellt sicher, dass für jedes Element genau ein String zurückgegeben wird
    vapply(vec, function(elem) {
      if (is.na(elem)) {
        NA_character_
      } else if (nchar(elem) > max_chars) {
        paste0(substr(elem, 1, max_chars), ellipsis)
      } else {
        elem
      }
    }, FUN.VALUE = character(1))
  }

  if (is.data.frame(x)) {
    # Für Dataframes: Wende die Funktion spaltenweise an, sofern es sich um Character-Spalten handelt
    x[] <- lapply(x, function(col) {
      if (is.character(col)) {
        truncate_vector(col)
      } else {
        col
      }
    })
    return(x)
  } else if (is.character(x)) {
    # Für Character-Vektoren
    return(truncate_vector(x))
  } else {
    # Für alle anderen Objekte: Unverändert zurückgeben
    return(x)
  }
}

retransform_data_global <- function(df, index, transformation_df) {
  candidate_df <- df
  failed_columns <- integer(0)

  for (i in seq_along(index)) {
    ci <- index[i]; tr <- transformation_df[i]
    orig <- df[, ci]
    new <- switch(tr,
                  "log2"    = 2^orig,
                  "-log10"  = 10^(-orig),
                  "log10"   = 10^orig,
                  orig)

    # Missing and non-finite input values are not retransformation failures.
    # In particular, retain ordinary NA rather than allowing it to be confused
    # with a NaN newly produced from a finite input value.
    input_na <- is.na(orig) & !is.nan(orig)
    new[input_na] <- NA
    newly_non_finite <- is.finite(orig) & (is.infinite(new) | is.nan(new))

    if (any(newly_non_finite)) {
      showNotification(paste("Retransformation produces infinite values in column:", colnames(df)[ci]), type="error", duration=5)
      failed_columns <- c(failed_columns, ci)
    }
    candidate_df[, ci] <- new
  }

  if (length(failed_columns) > 0) df else candidate_df
}

#Imputation functions

# Function to perform left-censored imputation

performGenericImputation <- function(data,
                                     data_def,
                                     imputation_columns,
                                     impute_fun,
                                     prefix = "Imputed ",
                                     transform_policy = c("raw_no_backtransform"),
                                     random_seed = NULL) {
  if (!is.null(random_seed)) set.seed(random_seed)
  if (is.data.frame(data_def) && "Custom" %in% names(data_def)) {
    data_def <- data_def[, names(data_def) != "Custom", drop = FALSE]
  }
  transform_policy <- match.arg(transform_policy)

  allowed_transformations <- c("None", "log2", "log10", "-log10")

  assert_transform_consistency <- function(values, transformation_label, col_name) {
    if (!is.numeric(values)) return(invisible(TRUE))
    if (!transformation_label %in% allowed_transformations) {
      stop("Column '", col_name, "' has unsupported Transformation value: '", transformation_label,
           "'. Allowed: ", paste(allowed_transformations, collapse = ", "), ".")
    }
    if (identical(transformation_label, "None")) {
      if (any(values < 0, na.rm = TRUE)) {
        showNotification(
          paste0("Imputed output for column '", col_name, "' contains negative raw values. ",
                 "Check transformation and imputation settings."),
          type = "error", duration = 8
        )
      }
    }
    invisible(TRUE)
  }

  # will collect each block of imputed columns
  imputed_blocks <- list()
  total_missing_values_imputed <- 0  # Track actual missing values replaced

  # for each Content string that needs imputation:
  for (orig_content in imputation_columns) {
    # find the indices in data_def with that Content
    base_rows   <- which(data_def$Content == orig_content)
    new_content <- paste0(prefix, orig_content)

    # for each distinct Options under that Content…
    for (opt in unique(data_def$Options[base_rows])) {
      # which exact rows in data_def have this Content+Option
      opt_rows <- base_rows[data_def$Options[base_rows] == opt]

      # pull the explicit source column names from metadata (never by position)
      cols_to_impute <- as.character(data_def$Column[opt_rows])
      if (length(cols_to_impute) == 0 || any(is.na(cols_to_impute)) || any(cols_to_impute == "")) {
        stop("Could not resolve source columns for content '", orig_content, "' and option '", opt, "'.")
      }
      if (!all(cols_to_impute %in% names(data))) {
        missing_cols <- setdiff(cols_to_impute, names(data))
        stop("Metadata references columns not present in data: ", paste(missing_cols, collapse = ", "), ".")
      }
      block_to_impute <- data[, cols_to_impute, drop = FALSE]
      transformations <- as.character(data_def$Transformation[opt_rows])
      transformations[is.na(transformations) | transformations == ""] <- "None"
      if (any(!transformations %in% allowed_transformations)) {
        bad <- unique(transformations[!transformations %in% allowed_transformations])
        stop("Unsupported Transformation values found: ", paste(bad, collapse = ", "),
             ". Allowed: ", paste(allowed_transformations, collapse = ", "), ".")
      }

      # Explicit transformation policy:
      # Path A (raw_no_backtransform): retransform to raw domain, impute in raw, keep raw output.
      # This keeps imputation scale explicit and makes left-censored domain validation deterministic.
      block_for_imputation <- block_to_impute
      transformed_idx <- which(transformations %in% c("log2", "log10", "-log10"))
      if (length(transformed_idx) > 0) {
        block_for_imputation <- retransform_data_global(
          block_for_imputation,
          transformed_idx,
          transformations[transformed_idx]
        )
      }

      # IMPORTANT: Count missing values BEFORE imputation
      missing_count_before <- sum(is.na(block_for_imputation))

      if (identical(impute_fun, performLeftCensoredImputation)) {
        invalid_cols <- vapply(names(block_for_imputation), function(cn) {
          vec <- block_for_imputation[[cn]]
          is.numeric(vec) && any(vec < 0, na.rm = TRUE)
        }, logical(1))
        if (any(invalid_cols)) {
          bad_cols <- names(block_for_imputation)[invalid_cols]
          stop(
            "Left-censored imputation requires non-negative raw-domain values. ",
            "Fix values or transformation tags for: ", paste(bad_cols, collapse = ", "), "."
          )
        }
      }

      # run your imputation function
      imputed_block <- impute_fun(block_for_imputation)

      # IMPORTANT: Count missing values AFTER imputation and calculate actual imputed count
      missing_count_after <- sum(is.na(imputed_block))
      actual_imputed_in_this_block <- missing_count_before - missing_count_after
      total_missing_values_imputed <- total_missing_values_imputed + actual_imputed_in_this_block

      # Generate unique output column names: prefix + original col name.
      # If the desired name already exists in `data` or in an earlier imputed
      # block from this call, append "_dup1", "_dup2", … until unique.
      desired_names <- paste0(prefix, cols_to_impute)
      # Compute the set of taken names once, before the per-column loop.
      already_taken <- c(names(data),
                         unlist(lapply(imputed_blocks, names), use.names = FALSE))
      final_names <- character(length(desired_names))
      for (k in seq_along(desired_names)) {
        nm <- desired_names[k]
        # Check against both the pre-existing names and any final names already
        # assigned within this call (avoid duplicates across the current block).
        assigned_so_far <- final_names[seq_len(k - 1L)]
        if (!(nm %in% already_taken) && !(nm %in% assigned_so_far)) {
          final_names[k] <- nm
        } else {
          dup_n <- 1L
          repeat {
            candidate <- paste0(nm, "_dup", dup_n)
            if (!(candidate %in% already_taken) && !(candidate %in% assigned_so_far)) break
            dup_n <- dup_n + 1L
          }
          final_names[k] <- candidate
        }
      }

      # Apply the final (unique) names to the imputed block.
      colnames(imputed_block) <- final_names

      # stash for later cbind:
      imputed_blocks[[ length(imputed_blocks) + 1 ]] <- imputed_block

      # Build Sample metadata: for renamed columns append the same "_dup{n}"
      # suffix so Sample entries stay unique across repeated imputations.
      orig_samples  <- data_def$Sample[opt_rows]
      final_samples <- vapply(seq_along(final_names), function(k) {
        samp <- orig_samples[k]
        if (final_names[k] != desired_names[k] && !is.na(samp) && nchar(samp) > 0L) {
          # final_names[k] has the form desired_names[k] + "_dup{n}", so
          # substring() extracts exactly the "_dup{n}" portion.
          suffix <- substring(final_names[k], nchar(desired_names[k]) + 1L)
          paste0(samp, suffix)
        } else {
          samp
        }
      }, character(1L))

      # now build the new definition‐rows, copying exactly Options & Sample
      new_def <- data.frame(
        Column         = final_names,
        Content        = new_content,
        Options        = data_def$Options[opt_rows],
        Sample         = final_samples,
        Transformation = rep("None", length(opt_rows)),
        stringsAsFactors = FALSE,
        check.names    = FALSE
      )

      for (j in seq_along(final_names)) {
        assert_transform_consistency(imputed_block[[j]], new_def$Transformation[j], final_names[j])
      }

      # Remove any stale metadata rows that share a name with the new output
      # columns, then append the fresh definitions.
      data_def <- data_def[!(data_def$Column %in% new_def$Column), , drop = FALSE]

      # append onto your master data_def
      data_def <- bind_rows(data_def, new_def)
    }
  }

  # bind all imputed blocks onto the original data
  all_imputed <- if (length(imputed_blocks) > 0) {
    do.call(cbind, imputed_blocks)
  } else {
    data.frame()
  }
  final_data <- cbind(data, all_imputed)

  # return both updated data + updated definition + actual count of imputed values
  list(
    data     = final_data,
    data_def = data_def,
    total_imputed = total_missing_values_imputed  # NEW: actual count of missing values that were replaced
  )
}

##########################
# 1. Links-zensierte Imputation
#    (Beibehaltung der ursprünglichen Logik, aber auf eine Data.frame-Spalte angewendet)

performLeftCensoredImputation <- function(data) {
  # Speichere die Originaldaten (vor der Imputation)
  original_data <- data
  
  # 1) Log-Likelihood für links-zen­sierte Lognormal-Verteilung
  left_censored_log_normal_log_likelihood <- function(mu, sigma, x, lloq) {
    sum(dlnorm(na.omit(x), mu, sigma, log = TRUE)) +
      sum(is.na(x)) * plnorm(lloq, mu, sigma, log = TRUE)
  }
  
  # 2) Safe-Wrapper fürs Optim, der Inf/NaN auf hohe Strafwerte mapped
  safe_neg_loglik <- function(theta, x, lloq) {
    mu    <- theta[1]
    sigma <- theta[2]
    if (sigma <= 0) return(1e10)
    obs      <- na.omit(x)
    ll_obs   <- sum(dlnorm(obs, mu, sigma, log = TRUE))
    cdf_lloq <- plnorm(lloq, mu, sigma)
    if (cdf_lloq <= 0) return(1e10)
    ll_cens  <- sum(is.na(x)) * log(cdf_lloq)
    nll      <- - (ll_obs + ll_cens)
    if (!is.finite(nll)) return(1e10)
    nll
  }
  
  # Hilfsfunktion für Startwerte
  mean_sd <- function(x, ...) c(mean(x, ...), sd(x, ...))
  
  # 3) Imputation je Spalte
  impute_column <- function(col, col_name = "Spalte") {
    orig_col <- col
    
    # a) Überspringen, wenn komplett NA oder zu wenige Werte
    if (all(is.na(col)))       return(col)
    if (sum(!is.na(col)) < 2)  return(col)
    
    # b) Harte Fehler bei negativen Werten → gesamter Prozess stoppt
    if (any(col < 0, na.rm = TRUE)) {
      showNotification(
        ui       = paste0("Column ‘", col_name,
                          "’: Log-normal fit requires strictly positive values;\n",
                          "negative values detected – aborting imputation."),
        type     = "error",
        duration = NULL
      )
      stop("Negative values in column ‘", col_name, "’, aborting imputation.")
    }
    
    # c) Null-Werte als links-zen­siert markieren
    if (any(col == 0, na.rm = TRUE)) {
      showNotification(
        ui       = paste0("Column ‘", col_name,
                          "’: Zero values will be treated as left-censored (< LLOQ>)."),
        type     = "warning",
        duration = 5
      )
      col[col == 0] <- NA
    }
    
    # Only strictly positive observed values can define the log-normal fit.
    # Re-check after converting zeros to censored values: the earlier
    # observation-count check happened before zeros were converted to NA.
    observed <- na.omit(col)
    
    if (length(observed) < 2L) {
      warning(
        sprintf(
          "Column '%s': fewer than two positive observed values remain after treating zeros as censored; imputation skipped.",
          col_name
        ),
        call. = FALSE
      )
      return(orig_col)
    }
    
    theta0   <- mean_sd(log(observed))
    lloq     <- min(observed)
    
    # A log-normal distribution cannot be estimated robustly without
    # variation among the observed log-values.
    if (any(!is.finite(theta0)) || theta0[2] <= 0) {
      warning(
        sprintf(
          "Column '%s': insufficient variation in positive observed values for left-censored log-normal imputation; imputation skipped.",
          col_name
        ),
        call. = FALSE
      )
      return(orig_col)
    }
    
    # d) Robust fitten mit Grenzen und Safe-Zielfunktion
    fit <- tryCatch({
      optim(
        par    = theta0,
        fn     = safe_neg_loglik,
        x      = col,
        lloq   = lloq,
        method = "L-BFGS-B",
        lower  = c(-Inf, 1e-6)
      )
    }, error = function(e) {
      theta1 <- theta0 + c(rnorm(1, 0, 0.1), runif(1, 0.01, 0.1))
      optim(
        par    = theta1,
        fn     = safe_neg_loglik,
        x      = col,
        lloq   = lloq,
        method = "L-BFGS-B",
        lower  = c(-Inf, 1e-6)
      )
    })
    
    
    # Do not draw from a failed or invalid fit.
    if (is.null(fit$par) ||
        length(fit$par) < 2L ||
        !isTRUE(fit$convergence == 0L) ||
        any(!is.finite(fit$par)) ||
        fit$par[2] <= 0) {
      warning(
        sprintf(
          "Column '%s': left-censored log-normal fit did not converge; imputation skipped.",
          col_name
        ),
        call. = FALSE
      )
      return(orig_col)
    }
    
    # e) Imputation der fehlenden Werte (inkl. ehemals Null-Werte)
    n_miss <- sum(is.na(col))
    
    # Draw from the fitted log-normal distribution conditional on X <= LLOQ.
    # This matches the censoring assumption used by safe_neg_loglik().
    cdf_lloq <- plnorm(lloq, fit$par[1], fit$par[2])
    
    if (!is.finite(cdf_lloq) || cdf_lloq <= 0) {
      warning(
        sprintf(
          "Column '%s': fitted probability below the LLOQ is invalid; imputation skipped.",
          col_name
        ),
        call. = FALSE
      )
      return(orig_col)
    }
    
    p <- runif(n_miss, 0, cdf_lloq)
    p <- pmax(p, .Machine$double.xmin)
    
    y <- qlnorm(p, fit$par[1], fit$par[2])
    
    # Numerical safeguard only. By construction these draws should already
    # lie at or below the censoring threshold.
    y <- pmin(y, lloq)
    
    col[is.na(col)] <- y
    
    col
  }

  # 4) Liste von imputierten Spalten mit korrekten Namen erzeugen
  imputed_list <- setNames(
    lapply(seq_along(data), function(i) {
      impute_column(data[[i]], col_name = colnames(data)[i])
    }),
    nm = colnames(data)
  )

  # 5) In Data-Frame umwandeln (oder im Fehlerfall Original zurückgeben)
  imputed_data <- tryCatch({
    as.data.frame(imputed_list, stringsAsFactors = FALSE, check.names = FALSE)
  }, error = function(e) {
    original_data
  })

  return(imputed_data)
}

# Robust parallel backend cleanup
robust_parallel_cleanup <- function(force = FALSE, debug = FALSE) {
  log_cleanup <- function(message) {
    if (isTRUE(debug)) {
      timestamp <- format(Sys.time(), "%H:%M:%S")
      cat("[ PARALLEL CLEANUP", timestamp, "]", message, "\n")
    }
  }

  registered <- tryCatch(foreach::getDoParRegistered(), error = function(e) FALSE)
  backend_name <- tryCatch(foreach::getDoParName(), error = function(e) "doSEQ")

  if (registered && (!identical(backend_name, "doSEQ") || isTRUE(force))) {
    tryCatch({
      foreach::registerDoSEQ()
      log_cleanup("foreach backend reset to doSEQ")
    }, error = function(e) {
      log_cleanup(paste("Failed to reset foreach backend:", e$message))
    })
  }

  if ("doParallel" %in% loadedNamespaces()) {
    tryCatch({
      doParallel::stopImplicitCluster()
      log_cleanup("Stopped implicit doParallel cluster")
    }, error = function(e) {
      log_cleanup(paste("stopImplicitCluster warning:", e$message))
    })
  }

  invisible(TRUE)
}

# Enhanced cluster cleanup using session management
clean_open_clusters <- function() {
  robust_parallel_cleanup(debug = FALSE)
}

# Wrapper-Funktion für links-zensierte Imputation in Gruppen
performGroupedLeftCensoredImputation <- function(loadedData, data_def, imputation_columns) {
  performGenericImputation(loadedData, data_def, imputation_columns, performLeftCensoredImputation, prefix = "Imputed ")
}

##########################
# 2. Random Forest Imputation

# Random Forest Imputation with robust session management
impute_random_forest <- function(data, random_seed = NULL) {

  if (!is.null(random_seed)) set.seed(random_seed)
  worker_seed <- sample.int(.Machine$integer.max, 1L)

  # Work only with numeric columns
  numeric_data <- as.data.frame(data[sapply(data, is.numeric)], check.names = FALSE)
  num_variables <- ncol(numeric_data)

  if (num_variables == 0) {
    debug_log("No numeric columns found. Returning original data.", level = 2)
    return(data)
  }

  run_missforest <- function(parallel_mode = c("variables", "no"), cores_hint = 1L) {
    parallel_mode <- match.arg(parallel_mode)
    mode_label <- if (parallel_mode == "variables") {
      paste0("parallel cores=", cores_hint)
    } else {
      "sequential"
    }

    debug_log(paste0("Running missForest (", mode_label, ")"), level = 2)
    missForest::missForest(
      xmis = numeric_data,
      verbose = TRUE,
      parallelize = parallel_mode
    )
  }

  run_parallel_missforest <- function(num_cores) {
    cl <- NULL
    old_backend_registered <- tryCatch(foreach::getDoParRegistered(), error = function(e) FALSE)
    old_backend_name <- tryCatch(foreach::getDoParName(), error = function(e) "doSEQ")

    on.exit({
      if (!is.null(cl)) {
        try(parallel::stopCluster(cl), silent = TRUE)
      }
      if ("doParallel" %in% loadedNamespaces()) {
        try(doParallel::stopImplicitCluster(), silent = TRUE)
      }

      # We only restore doSEQ for backends we created ourselves.
      if (!old_backend_registered || identical(old_backend_name, "doSEQ")) {
        try(foreach::registerDoSEQ(), silent = TRUE)
      }
    }, add = TRUE)

    cl <- parallel::makeCluster(num_cores)
    parallel::clusterSetRNGStream(cl, iseed = worker_seed)
    doParallel::registerDoParallel(cl)

    run_missforest("variables", cores_hint = num_cores)
  }

  # Get the number of cores safely, but not exceeding `num_variables`
  num_cores <- tryCatch({
    max(1, min(parallel::detectCores() - 1, num_variables, 6))
  }, error = function(e) {
    1
  })

  debug_log("Starting Random Forest imputation", level = 2)

  backend_name <- tryCatch(foreach::getDoParName(), error = function(e) "doSEQ")
  has_external_backend <- tryCatch({
    foreach::getDoParRegistered() && !identical(backend_name, "doSEQ")
  }, error = function(e) FALSE)

  result <- NULL

  if (has_external_backend) {
    debug_log(paste0("External foreach backend detected (", backend_name, "), using sequential fallback to avoid backend conflicts."), level = 1)
  } else if (num_cores > 1 && num_variables >= 2) {
    result <- tryCatch({
      run_parallel_missforest(num_cores)
    }, error = function(e) {
      debug_log(paste("Parallel missForest execution failed:", e$message), level = 1)
      NULL
    })
  }

  if (is.null(result)) {
    debug_log("Retrying missForest in sequential mode", level = 1)
    result <- tryCatch({
      run_missforest("no", cores_hint = 1L)
    }, error = function(e) {
      debug_log(paste("Sequential missForest execution failed:", e$message), level = 1)
      NULL
    })
  }

  if (is.null(result)) {
    debug_log("MissForest failed, returning original data", level = 1)
    return(data)
  }

  character_data <- data[sapply(data, is.character)]
  imputed_matrix <- result$ximp
  if (is.null(imputed_matrix)) {
    debug_log("missForest result had no ximp element, returning original data", level = 1)
    return(data)
  }

  imputed_data <- cbind(character_data, imputed_matrix)

  debug_log("Random Forest imputation completed successfully", level = 2)
  return(imputed_data)
}

performRandomForestImputation <- function(data, data_def, imputation_columns) {
  debug_log("Starting Random Forest imputation workflow", level = 2)

  # Clean up before starting
  robust_parallel_cleanup(debug = FALSE)

  # Perform imputation
  result <- performGenericImputation(data, data_def, imputation_columns, impute_random_forest, prefix = "Imputed ")

  debug_log("Random Forest imputation workflow completed", level = 2)
  return(result)
}

##########################
# 3. MICE CART Imputation

impute_mice_cart <- function(data) {
  # Arbeite zunächst nur mit numerischen Spalten
  numeric_data <- data[sapply(data, is.numeric)]

  if (ncol(numeric_data) > 0) {
    original_names <- names(numeric_data)
    colnames(numeric_data) <- make.names(original_names)

    mice_result <- mice::mice(numeric_data, method = "cart", m = 1, maxit = 5, printFlag = FALSE)
    imputed_data_numeric <- mice::complete(mice_result)

    # Stelle die Original-Spaltennamen wieder her
    colnames(imputed_data_numeric) <- original_names

    # Kombiniere mit Zeichen-Spalten, falls vorhanden
    character_data <- data[sapply(data, is.character)]
    imputed_data <- cbind(character_data, imputed_data_numeric)
  } else {
    imputed_data <- data
  }

  return(imputed_data)
}

performMICECartImputation <- function(data, data_def, imputation_columns) {
  performGenericImputation(data, data_def, imputation_columns, impute_mice_cart, prefix = "Imputed ")
}

# Cheap Data Wizard metadata-lifecycle helpers shared by downstream modules.
# The assignment-pending flag is TRUE while Data Wizard is actively loading or
# applying rule-driven metadata. Downstream choice observers must defer during
# the whole assignment window, even if previously committed metadata was ready,
# so they do not populate or warn from an intermediate metadata table.
datawizard_metadata_assignment_pending <- function(rv = NULL) {
  tryCatch({
    !is.null(rv) && (
      isTRUE(rv$datawizard_metadata_assignment_pending) ||
        identical(rv$datawizard_metadata_lifecycle_state, "metadata_assigning")
    )
  }, error = function(e) FALSE)
}

datawizard_metadata_meaningful_ready <- function(rv = NULL) {
  tryCatch(!is.null(rv) && isTRUE(rv$datawizard_metadata_meaningful_ready), error = function(e) FALSE)
}

datawizard_metadata_committed_ready <- function(rv = NULL) {
  if (datawizard_metadata_assignment_pending(rv)) {
    return(FALSE)
  }

  lifecycle_state <- tryCatch(rv$datawizard_metadata_lifecycle_state, error = function(e) NULL)
  datawizard_metadata_meaningful_ready(rv) && identical(lifecycle_state, "metadata_ready")
}

set_session_restore_phase <- function(rv = NULL, phase) {
  if (is.null(rv)) {
    return(invisible(NULL))
  }

  tryCatch({
    rv$session_restore_phase <- phase
    # Legacy mirror: keep this in sync while older restore code still reads it.
    rv$restore_phase <- phase
  }, error = function(e) NULL)

  invisible(phase)
}

# Classify conditions which indicate that an imperative API accidentally read a
# reactive outside a consumer.  Callers must not turn this programming error
# into the same FALSE/NULL used for an optional or not-yet-populated value.
datawizard_condition_class <- function(condition) {
  message <- tryCatch(conditionMessage(condition), error = function(e) "")
  classes <- tryCatch(class(condition), error = function(e) character(0))
  if ("reactiveContextError" %in% classes ||
      grepl("without an active reactive context|outside of reactive consumer",
            message, ignore.case = TRUE)) {
    return("reactive_context_violation")
  }
  "operational_error"
}

datawizard_restore_phase_active <- function(rv = NULL, phases = NULL) {
  snapshot <- shiny::isolate(list(
    session_restore_phase = if (!is.null(rv)) rv$session_restore_phase else NULL,
    restore_phase = if (!is.null(rv)) rv$restore_phase else NULL
  ))
  phase <- snapshot$session_restore_phase %||% snapshot$restore_phase %||% NULL
  if (is.null(phase)) {
    return(FALSE)
  }
  if (!is.null(phases)) {
    return(phase %in% phases)
  }
  !identical(phase, "complete")
}

datawizard_metadata_defer_downstream_choices <- function(rv = NULL) {
  datawizard_metadata_assignment_pending(rv) || datawizard_restore_phase_active(rv)
}

# Treat downstream abundance-choice warnings as actionable only after Data
# Wizard has committed meaningful metadata. Inferred Content values in data_def
# are deliberately not enough here: during rule application they may appear
# before rv$data_def/core metadata synchronization has completed, which should
# defer choices rather than emit the user-facing abundance warning.
datawizard_metadata_ready_for_abundance_warning <- function(rv = NULL, data_def = NULL) {
  datawizard_metadata_committed_ready(rv)
}

##########################

# ========================================
# ERGÄNZUNG FÜR utils.R - Alle statistischen Funktionen für Ratios
# Diese Funktionen sollten am Ende der bestehenden utils.R hinzugefügt werden
# ========================================




# ========================================
# Direct Data Access Functions for Excel Export
# ========================================

#' Access data directly from rv or module with multiple fallback methods
#' @param rv reactive values object
#' @param module_out module output object
#' @param data_type type of data to extract ("original", "processed", "metadata")
#' @param debug_log debug logging function
#' @return data frame or NULL
access_data_for_excel <- function(rv, module_out, data_type, debug_log) {
  tryCatch({
    debug_log(paste("Attempting to access", data_type, "data"), level = 2)

    # Method 1: Direct access from rv
    if (!is.null(rv)) {
      debug_log("Trying rv direct access", level = 2)

      if (data_type == "original" && !is.null(rv$data_og)) {
        debug_log("Found original data in rv$data_og", level = 2)
        return(rv$data_og)
      }

      if (data_type == "processed" && !is.null(rv$data_mod)) {
        debug_log("Found processed data in rv$data_mod", level = 2)
        return(rv$data_mod)
      }

      if (data_type == "metadata" && !is.null(rv$data_def)) {
        debug_log("Found metadata in rv$data_def", level = 2)
        return(rv$data_def)
      }
    }

    # Method 2: Access from module functions
    if (!is.null(module_out) && is.list(module_out)) {
      debug_log("Trying module function access", level = 2)

      if (data_type == "processed") {
        # Try different function names for processed data
        func_names <- c("final_processed_data", "get_file_data", "processed_data", "data_mod")
        for (func_name in func_names) {
          if (func_name %in% names(module_out) && is.function(module_out[[func_name]])) {
            tryCatch({
              result <- module_out[[func_name]]()
              if (is.reactive(result)) result <- result()
              if (!is.null(result) && is.data.frame(result) && nrow(result) > 0) {
                debug_log(paste("Found processed data using", func_name), level = 2)
                return(result)
              }
            }, error = function(e) {
              debug_log(paste("Error calling", func_name, ":", e$message), level = 2)
            })
          }
        }
      }

      if (data_type == "metadata") {
        # Try different function names for metadata
        func_names <- c("final_processed_metadata", "handson_metadata", "metadata_def", "data_def")
        for (func_name in func_names) {
          if (func_name %in% names(module_out) && is.function(module_out[[func_name]])) {
            tryCatch({
              result <- module_out[[func_name]]()
              if (is.reactive(result)) result <- result()
              if (!is.null(result) && is.data.frame(result) && nrow(result) > 0) {
                debug_log(paste("Found metadata using", func_name), level = 2)
                return(result)
              }
            }, error = function(e) {
              debug_log(paste("Error calling", func_name, ":", e$message), level = 2)
            })
          }
        }
      }

      if (data_type == "original") {
        # Try to access original data
        func_names <- c("primary_data_raw", "loader_out$primary", "original_data")
        for (func_name in func_names) {
          if (func_name %in% names(module_out) && is.function(module_out[[func_name]])) {
            tryCatch({
              result <- module_out[[func_name]]()
              if (is.reactive(result)) result <- result()
              if (!is.null(result) && is.data.frame(result) && nrow(result) > 0) {
                debug_log(paste("Found original data using", func_name), level = 2)
                return(result)
              }
            }, error = function(e) {
              debug_log(paste("Error calling", func_name, ":", e$message), level = 2)
            })
          }
        }
      }
    }

    # Method 3: Access from modEnv if available
    if (exists("modEnv", envir = globalenv())) {
      debug_log("Trying modEnv access", level = 2)
      modEnv <- get("modEnv", envir = globalenv())

      if (exists("datawizard_out", envir = modEnv) && !is.null(modEnv$datawizard_out)) {
        dw_out <- modEnv$datawizard_out

        if (data_type == "processed") {
          tryCatch({
            if (is.function(dw_out$final_processed_data)) {
              result <- dw_out$final_processed_data()
              if (!is.null(result) && is.data.frame(result) && nrow(result) > 0) {
                debug_log("Found processed data in modEnv", level = 2)
                return(result)
              }
            }
          }, error = function(e) {
            debug_log(paste("Error accessing modEnv processed data:", e$message), level = 2)
          })
        }

        if (data_type == "metadata") {
          tryCatch({
            if (is.function(dw_out$final_processed_metadata)) {
              result <- dw_out$final_processed_metadata()
              if (!is.null(result) && is.data.frame(result) && nrow(result) > 0) {
                debug_log("Found metadata in modEnv", level = 2)
                return(result)
              }
            }
          }, error = function(e) {
            debug_log(paste("Error accessing modEnv metadata:", e$message), level = 2)
          })
        }
      }
    }

    debug_log(paste("No", data_type, "data found"), level = 2)
    return(NULL)

  }, error = function(e) {
    debug_log(paste("Error in access_data_for_excel for", data_type, ":", e$message), level = 1)
    return(NULL)
  })
}

# ========================================
# Safe Parallel Operation Wrapper
# ========================================

safe_parallel_operation <- function(operation_func, cleanup_after = TRUE,
                                    preserve_gsea_cluster = TRUE, rv = NULL, ...) {
  if (!is.function(operation_func)) stop("operation_func must be a function")

  skip_cleanup <- preserve_gsea_cluster && !is.null(rv) && !is.null(rv$gsea_cluster)

  backend_state <- list(
    registered = tryCatch(foreach::getDoParRegistered(), error = function(e) FALSE),
    name = tryCatch(foreach::getDoParName(), error = function(e) "doSEQ")
  )

  on.exit({
    if (!cleanup_after || skip_cleanup) {
      return(invisible(NULL))
    }

    current_registered <- tryCatch(foreach::getDoParRegistered(), error = function(e) FALSE)
    current_name <- tryCatch(foreach::getDoParName(), error = function(e) "doSEQ")

    if (!backend_state$registered || identical(backend_state$name, "doSEQ")) {
      if (current_registered && !identical(current_name, "doSEQ")) {
        try(foreach::registerDoSEQ(), silent = TRUE)
        if ("doParallel" %in% loadedNamespaces()) {
          try(doParallel::stopImplicitCluster(), silent = TRUE)
        }
      }
      return(invisible(NULL))
    }

    if (!identical(current_name, backend_state$name)) {
      debug_log(paste0("Parallel backend changed during operation (", backend_state$name, " -> ", current_name, "), preserving current backend."), level = 1)
    }

    invisible(NULL)
  }, add = TRUE)

  tryCatch({
    operation_func(...)
  }, error = function(e) {
    debug_log(paste("Error in parallel operation:", e$message), level = 1)
    return(NULL)
  })
}
# Compact Data Wizard import barrier helpers. These deliberately read only
# scalar state, so choice/readiness observers do not subscribe to rv$data_mod.
datawizard_import_phase <- function(rv) {
  phase <- tryCatch(rv$datawizard_import_phase, error = function(e) NULL)
  if (is.null(phase) || !nzchar(as.character(phase)[1])) "ready" else as.character(phase)[1]
}

datawizard_import_barrier_active <- function(rv) {
  phase_blocked <- datawizard_import_phase(rv) %in% c("reading", "publishing_raw", "creating_metadata")
  started <- tryCatch(rv$datawizard_import_generation_started %||% 0L, error = function(e) 0L)
  committed <- tryCatch(rv$datawizard_import_generation_committed %||% 0L, error = function(e) 0L)
  phase_blocked || !identical(as.integer(started), as.integer(committed))
}

datawizard_import_ready_signature <- function(rv) {
  list(
    ready = !datawizard_import_barrier_active(rv),
    release = tryCatch(rv$datawizard_import_ready_revision %||% 0L, error = function(e) 0L),
    generation = tryCatch(rv$datawizard_import_generation_committed %||% 0L, error = function(e) 0L),
    data = tryCatch(rv$datawizard_data_revision_id %||% 0L, error = function(e) 0L),
    metadata = tryCatch(rv$datawizard_metadata_revision_id %||% 0L, error = function(e) 0L)
  )
}
