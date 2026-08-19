# session_management.R - COMPLETE FIXED VERSION
# Ultra-fast startup cleanup for thread accumulation issues

# ========================================
# SYSTEM-LEVEL ORPHANED PROCESS CLEANUP
# ========================================

# ========================================
# SESSION LOCK FILE MANAGEMENT
# ========================================

get_session_lock_file <- function() {
  # In portable mode, use the launcher-managed log directory
  # instead of the user's home directory
  log_dir <- Sys.getenv("MIRAPROT_LOG_DIR", "")
  if (nzchar(log_dir) && dir.exists(log_dir)) {
    return(file.path(log_dir, ".miraprot_session_lock"))
  }
  file.path(normalizePath("~", mustWork = FALSE), ".miraprot_session_lock")
}

should_run_orphan_cleanup_from_lock <- function(stale_after_seconds = 300) {
  lock_file <- get_session_lock_file()
  run_orphan_cleanup <- FALSE

  if (file.exists(lock_file)) {
    lock_age_seconds <- suppressWarnings(as.numeric(difftime(Sys.time(), file.info(lock_file)$mtime, units = "secs")))

    if (is.finite(lock_age_seconds) && lock_age_seconds > stale_after_seconds) {
      run_orphan_cleanup <- TRUE
    }

    try(unlink(lock_file, force = TRUE), silent = TRUE)
  }

  return(run_orphan_cleanup)
}

write_session_lock_file <- function() {
  lock_file <- get_session_lock_file()
  try(writeLines(as.character(Sys.getpid()), lock_file), silent = TRUE)
  invisible(lock_file)
}

remove_session_lock_file <- function() {
  lock_file <- get_session_lock_file()
  if (file.exists(lock_file)) {
    try(unlink(lock_file, force = TRUE), silent = TRUE)
  }
  invisible(NULL)
}

#' Count current R processes (for accumulation detection)
count_r_processes <- function() {
  count <- 1  # At least the current process

  # Completely suppress all warnings and output
  suppressWarnings({
    tryCatch({
      if (.Platform$OS.type == "unix") {
        # Count R processes on Unix
        cmd <- "ps aux | grep -E '[Rr]script|[Rr] --slave|[Rr] CMD' | grep -v grep | wc -l"
        count_str <- system(cmd, intern = TRUE, ignore.stderr = TRUE)
        count <- as.numeric(trimws(count_str[1]))
      } else {
        # ENHANCED: Windows process counting with better error handling

        # Method 1: Try tasklist with CSV output (most reliable)
        tryCatch({
          cmd <- 'tasklist /FI "IMAGENAME eq R.exe" /FO CSV 2>nul'
          result_r <- system(cmd, intern = TRUE, ignore.stderr = TRUE)

          cmd2 <- 'tasklist /FI "IMAGENAME eq Rscript.exe" /FO CSV 2>nul'
          result_rscript <- system(cmd2, intern = TRUE, ignore.stderr = TRUE)

          # Count actual processes (excluding headers)
          count_r <- max(0, length(result_r) - 1)  # -1 for header
          count_rscript <- max(0, length(result_rscript) - 1)  # -1 for header

          count <- count_r + count_rscript
        }, error = function(e) {
          # Method 2: Fallback to PowerShell (if tasklist fails)
          tryCatch({
            cmd_ps <- 'powershell -Command "Get-Process | Where-Object {$_.ProcessName -match \\"^(R|Rscript)$\\"} | Measure-Object | Select-Object -ExpandProperty Count"'
            count_str <- system(cmd_ps, intern = TRUE, ignore.stderr = TRUE)
            count <<- as.numeric(trimws(count_str[1]))
          }, error = function(e2) {
            # Method 3: Ultimate fallback - assume minimal processes
            count <<- 1
          })
        })
      }

      # Ensure reasonable bounds
      if (is.na(count) || count < 1) count <- 1
      if (count > 100) count <- 100  # Cap at reasonable maximum

    }, error = function(e) {
      count <- 1  # Safe fallback
    })
  })

  return(count)
}

#' Parse process age from ps etime output into minutes
parse_process_age <- function(age_str) {
  if (is.na(age_str) || nchar(age_str) == 0) return(NA)

  # Handle different etime formats:
  # MM:SS -> minutes:seconds
  # HH:MM:SS -> hours:minutes:seconds
  # DD-HH:MM:SS -> days-hours:minutes:seconds

  if (grepl("-", age_str)) {
    # Days format: DD-HH:MM:SS
    parts <- strsplit(age_str, "-")[[1]]
    if (length(parts) == 2) {
      days <- as.numeric(parts[1])
      time_part <- parts[2]
    } else {
      return(NA)
    }
  } else {
    days <- 0
    time_part <- age_str
  }

  # Parse time part HH:MM:SS or MM:SS
  time_components <- strsplit(time_part, ":")[[1]]
  time_components <- as.numeric(time_components)

  if (length(time_components) == 2) {
    # MM:SS format
    minutes <- time_components[1]
    seconds <- time_components[2]
    hours <- 0
  } else if (length(time_components) == 3) {
    # HH:MM:SS format
    hours <- time_components[1]
    minutes <- time_components[2]
    seconds <- time_components[3]
  } else {
    return(NA)
  }

  # Convert everything to minutes
  total_minutes <- (days * 24 * 60) + (hours * 60) + minutes + (seconds / 60)
  return(total_minutes)
}

#' Clean up orphaned R processes on Unix-like systems
cleanup_unix_r_processes <- function(verbose = FALSE, max_age_minutes = 30) {
  orphaned_count <- 0

  try({
    # Single ps call: fetch PID and elapsed time for all R worker processes at once
    cmd <- "ps -eo pid=,etime=,args= | grep -E 'R --slave|Rscript.*worker|R CMD.*parallel' | grep -v grep"
    lines <- system(cmd, intern = TRUE, ignore.stderr = TRUE)

    if (length(lines) > 0 && any(nchar(lines) > 0)) {
      current_pid <- Sys.getpid()
      pids_to_kill <- integer(0)

      for (line in lines) {
        if (nchar(line) == 0) next

        # Format: "  PID  ELAPSED  ARGS..."
        parts <- strsplit(trimws(line), "\\s+", perl = TRUE)[[1]]
        if (length(parts) < 2) next

        pid <- suppressWarnings(as.numeric(parts[1]))
        if (is.na(pid) || pid == current_pid) next

        age_minutes <- parse_process_age(parts[2])
        if (!is.na(age_minutes) && age_minutes > max_age_minutes) {
          if (verbose) {
            cat(sprintf("[ CLEANUP ] Killing orphaned R process PID %d (age: %.1f min)\n",
                        pid, age_minutes))
          }
          pids_to_kill <- c(pids_to_kill, pid)
        }
      }

      # Kill all orphaned processes in one batch
      if (length(pids_to_kill) > 0) {
        kill_cmd <- paste0("kill -TERM ", paste(pids_to_kill, collapse = " "), " 2>/dev/null")
        system(kill_cmd, ignore.stderr = TRUE)
        orphaned_count <- length(pids_to_kill)
      }
    }
  }, silent = TRUE)

  return(orphaned_count)
}

#' Clean up orphaned R processes on Windows
cleanup_windows_r_processes <- function(verbose = FALSE, max_age_minutes = 30) {
  orphaned_count <- 0

  try({
    # Single batch WMIC query for all R/Rscript processes
    cmd <- "wmic process where \"Name='R.exe' or Name='Rscript.exe'\" get ProcessId,CommandLine /format:csv"
    processes <- system(cmd, intern = TRUE, ignore.stderr = TRUE)

    if (length(processes) > 0) {
      current_pid <- Sys.getpid()
      process_lines <- trimws(processes)
      process_lines <- process_lines[nchar(process_lines) > 0]
      process_lines <- process_lines[!grepl("^Node,", process_lines)]

      if (length(process_lines) > 0) {
        for (process_line in process_lines) {
          fields <- strsplit(process_line, ",", fixed = TRUE)[[1]]
          if (length(fields) < 3) next

          pid <- suppressWarnings(as.numeric(fields[length(fields)]))
          if (is.na(pid) || pid == current_pid) next

          command_line <- paste(fields[2:(length(fields) - 1)], collapse = ",")

          # Check if this looks like a parallel worker process
          if (grepl("--slave|worker|parallel", command_line, ignore.case = TRUE)) {
            if (verbose) {
              cat(sprintf("[ CLEANUP ] Killing orphaned R process PID %d\n", pid))
            }

            # Kill the process
            kill_cmd <- sprintf('taskkill /PID %d /F >nul 2>&1', pid)
            system(kill_cmd, ignore.stderr = TRUE)
            orphaned_count <- orphaned_count + 1
          }
        }
      }
    }
  }, silent = TRUE)

  return(orphaned_count)
}


#' Clean up orphaned R processes from previous crashed sessions
cleanup_orphaned_r_processes <- function(verbose = FALSE, max_age_minutes = 120) {
  orphaned_count <- 0

  if (.Platform$OS.type == "unix") {
    # Unix/Linux/macOS: Use ps and pkill
    orphaned_count <- cleanup_unix_r_processes(verbose, max_age_minutes)
  } else {
    # Windows: Use tasklist and taskkill
    orphaned_count <- cleanup_windows_r_processes(verbose, max_age_minutes)
  }

  return(orphaned_count)
}

# ========================================
# LIGHTNING-FAST THREAD CLEANUP
# ========================================

#' Ultra-fast thread cleanup - runs in <100ms
fast_thread_cleanup <- function(verbose = FALSE, include_orphaned = FALSE) {
  if (verbose) cat("[ CLEANUP ] Starting fast thread cleanup\n")
  t0 <- proc.time()[3]

  # 1. Reset parallel backends (fastest operations first)
  if (tryCatch(foreach::getDoParRegistered(), error = function(e) FALSE)) {
    try(foreach::registerDoSEQ(), silent = TRUE)
  }
  if ("doParallel" %in% loadedNamespaces()) {
    try(doParallel::stopImplicitCluster(), silent = TRUE)
  }
  t1 <- proc.time()[3]

  # 2. Clean up orphaned R processes from previous crashed sessions
  if (include_orphaned) {
    try({
      orphaned_count <- cleanup_orphaned_r_processes(verbose)
      if (verbose && orphaned_count > 0) {
        cat(sprintf("[ CLEANUP ] Removed %d orphaned R processes\n", orphaned_count))
      }
    }, silent = TRUE)
  }
  t2 <- proc.time()[3]

  # 3. Clear any remaining parallel package state
  try({
    # Reset options that might hold parallel references
    options(mc.cores = 1)
    if (exists("RNGkind")) RNGkind("default")
  }, silent = TRUE)

  elapsed <- proc.time()[3] - t0
  if (verbose) {
    cat(sprintf("[ CLEANUP ] Completed in %.0fms (backends=%.0fms, orphans=%.0fms)\n",
                elapsed * 1000,
                (t1 - t0) * 1000,
                (t2 - t1) * 1000))
  }

  invisible(elapsed < 0.2)  # Return TRUE if under 200ms
}

#' Initialize clean session state
initialize_session_management <- function(clean_orphaned = TRUE) {
  # Run fast cleanup once (includes orphaned processes when requested)
  fast_thread_cleanup(verbose = TRUE, include_orphaned = clean_orphaned)

  # Set safe defaults
  options(
    mc.cores = 1,           # Prevent accidental multicore
    warn = 1,               # Show warnings immediately
    error = NULL            # Reset any custom error handlers
  )

  invisible(TRUE)
}

#' Emergency cleanup for severe thread issues (128+ threads)
emergency_thread_reset <- function() {
  cat("[EMERGENCY] Resetting all parallel connections and orphaned processes\n")

  # Nuclear option - reset everything
  try({
    # 1. Clean up orphaned R processes from previous sessions (aggressive)
    orphaned_count <- cleanup_orphaned_r_processes(verbose = TRUE, max_age_minutes = 120)
    if (orphaned_count > 0) {
      cat(sprintf("[EMERGENCY] Removed %d orphaned processes\n", orphaned_count))
    }

    # 2. Reset all parallel packages
    if (tryCatch(foreach::getDoParRegistered(), error = function(e) FALSE)) {
      foreach::registerDoSEQ()
    }
    if ("doParallel" %in% loadedNamespaces()) {
      doParallel::stopImplicitCluster()
    }

    # 3. Force close all connections
    all_connections <- showConnections(all = TRUE)
    if (nrow(all_connections) > 0) {
      for (i in 1:nrow(all_connections)) {
        try(close(as.numeric(rownames(all_connections)[i])), silent = TRUE)
      }
    }

    # 4. Force garbage collection
    gc(verbose = FALSE)

    # 5. Clear any global parallel environments
    env_names <- c(".app_parallel_env", ".parallel_env")
    for (env_name in env_names) {
      if (exists(env_name, envir = .GlobalEnv)) {
        env <- get(env_name, envir = .GlobalEnv)
        if (is.environment(env) && !is.null(env$cluster)) {
          try(parallel::stopCluster(env$cluster), silent = TRUE)
          env$cluster <- NULL
        }
      }
    }

  }, silent = TRUE)

  cat("[EMERGENCY] Reset completed - system should be clean\n")
  invisible(TRUE)
}

#' Check for thread issues and auto-cleanup (FIXED - with parameter)
check_thread_health <- function(include_orphaned_check = FALSE) {
  # Quick check for excessive connections (fast operation)
  conn_count <- tryCatch({
    length(showConnections(all = FALSE))
  }, error = function(e) 0)

  # Check for excessive R processes if requested
  process_count <- 1
  if (include_orphaned_check) {
    process_count <- count_r_processes()
  }

  # Trigger cleanup if too many connections or processes
  cleanup_triggered <- FALSE

  if (conn_count > 10) {
    cat(sprintf("[WARNING] High connection count: %d, running cleanup\n", conn_count))
    fast_thread_cleanup(verbose = FALSE, include_orphaned = FALSE)
    cleanup_triggered <- TRUE
  }

  if (include_orphaned_check && process_count > 15) {
    cat(sprintf("[WARNING] High R process count: %d, cleaning orphaned processes\n", process_count))
    orphaned_count <- cleanup_orphaned_r_processes(verbose = FALSE, max_age_minutes = 120)
    if (orphaned_count > 0) {
      cat(sprintf("[WARNING] Removed %d orphaned processes\n", orphaned_count))
    }
    cleanup_triggered <- TRUE
  }

  return(!cleanup_triggered)
}

# ========================================
# APP STARTUP SAFETY FUNCTIONS
# ========================================

#' Wait for cleanup to complete and verify app readiness
wait_for_cleanup_completion <- function(timeout_seconds = 10) {
  startup_lock_file <- file.path(tempdir(), "shiny_cleanup_lock")
  start_time <- Sys.time()

  # Wait for lock file to be removed (indicates cleanup is done)
  while (file.exists(startup_lock_file)) {
    if (as.numeric(Sys.time() - start_time) > timeout_seconds) {
      warning("Cleanup timeout - proceeding anyway")
      try(file.remove(startup_lock_file), silent = TRUE)
      break
    }
    Sys.sleep(0.1)
  }

  # Verify app files exist
  app_ready <- file.exists("app.R") || file.exists("server.R")

  if (!app_ready && nzchar(Sys.getenv("MIRAPROT_IN_PORTABLE", ""))) {
    # In portable mode the Go launcher guarantees the working directory.
    # If app.R is not found, something is seriously wrong — do not search.
    cat("[STARTUP] Portable mode: app.R not found in launcher-provided directory:", getwd(), "\n")
    return(FALSE)
  }

  if (!app_ready) {
    cat("[STARTUP] App files not found in current directory:", getwd(), "\n")
    cat("[STARTUP] Looking for app files...\n")

    # Try to find the correct directory
    possible_dirs <- c(".", "..", "../modular-shiny-app", "./modular-shiny-app", "app")
    for (dir in possible_dirs) {
      if (file.exists(file.path(dir, "app.R")) || file.exists(file.path(dir, "server.R"))) {
        cat(sprintf("[STARTUP] Found app files in: %s\n", normalizePath(dir)))
        try(setwd(dir), silent = TRUE)
        app_ready <- TRUE
        break
      }
    }
  }

  return(app_ready)
}

#' Safe app startup wrapper
safe_shiny_startup <- function(app_dir = ".", ...) {
  cat("[STARTUP] Ensuring safe Shiny startup...\n")

  # In portable mode, trust the launcher-provided working directory
  if (nzchar(Sys.getenv("MIRAPROT_IN_PORTABLE", "")) && app_dir == ".") {
    cat("[STARTUP] Portable mode: using launcher-provided working directory\n")
  } else if (app_dir != ".") {
    if (dir.exists(app_dir)) {
      old_wd <- getwd()
      setwd(app_dir)
      on.exit(setwd(old_wd), add = TRUE)
    }
  }

  # Wait for cleanup to complete
  app_ready <- wait_for_cleanup_completion()

  if (!app_ready) {
    stop("App files (app.R or server.R) not found. Please check your working directory.")
  }

  cat("[STARTUP] App is ready to launch\n")
  return(TRUE)
}
