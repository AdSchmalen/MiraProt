# update.R
# MiraProt — dependency update script
#
# This script updates:
#   - Required Bioconductor packages
#   - Required CRAN packages (used at runtime, as loaded in app.R)
#   - GitHub packages (e.g., shinyTree)
#   - Optional/developer/documentation packages
#
# The script is safe to run multiple times: already installed packages are skipped.


## 1) Basic setup -----------------------------------------------------------

get_missing_pkgs <- function(pkgs) {
  pkgs[!vapply(pkgs, requireNamespace, quietly = TRUE, FUN.VALUE = logical(1))]
}

get_missing_pkgs <- function(pkgs) {
  pkgs[!vapply(pkgs, requireNamespace, quietly = TRUE, FUN.VALUE = logical(1))]
}

# BiocManager is needed to install Bioconductor packages
if (!requireNamespace("BiocManager", quietly = TRUE)) {
  options(repos = c(CRAN = "https://cran.r-project.org"))
  install.packages("BiocManager")
}

# pak is needed to install packages from GitHub
if (!requireNamespace("pak", quietly = TRUE)) {
  install.packages("pak")
}

# pkgbuild is used to detect whether local build/compile tools are available.
# This prevents hard failures in fresh environments (e.g., missing Rtools on Windows)
# when pak needs to build source packages.
if (!requireNamespace("pkgbuild", quietly = TRUE)) {
  options(repos = c(CRAN = "https://cran.r-project.org"))
  install.packages("pkgbuild")
}



ensure_user_library_writable <- function() {
  if (file.access(.Library, 2) == 0) return(invisible(.libPaths()[1]))
  user_lib <- Sys.getenv("R_LIBS_USER")
  if (!nzchar(user_lib)) {
    minor_major <- sub("^([0-9]+).*$", "\\1", R.version$minor)
    user_lib <- file.path(path.expand("~"), "R", paste0(R.version$platform, "-library"), paste0(R.version$major, ".", minor_major))
  }
  dir.create(user_lib, recursive = TRUE, showWarnings = FALSE)
  .libPaths(unique(c(normalizePath(user_lib, mustWork = FALSE), .libPaths())))
  message("System library is not writable; using user library: ", .libPaths()[1])
  invisible(.libPaths()[1])
}

annotationhub_binary_available <- function(repos = BiocManager::repositories()) {
  ap <- tryCatch(utils::available.packages(repos = repos, type = "binary"), error = function(e) NULL)
  !is.null(ap) && "AnnotationHub" %in% rownames(ap)
}

install_annotationhub_reliably <- function(lib = NULL, update = TRUE) {
  if (requireNamespace("AnnotationHub", quietly = TRUE)) return(invisible(TRUE))

  ensure_user_library_writable()

  options(repos = BiocManager::repositories())
  repos <- getOption("repos")
  bioc_args <- list(ask = FALSE, update = FALSE)
  if (!is.null(lib)) bioc_args$lib <- lib

  old_pkg_type <- getOption("pkgType")
  on.exit(options(pkgType = old_pkg_type), add = TRUE)

  if (annotationhub_binary_available(repos)) {
    message("Installing AnnotationHub (BiocManager binary-first)...")
    options(pkgType = "binary")
    try(do.call(BiocManager::install, c(list(pkgs = "AnnotationHub"), bioc_args)), silent = TRUE)
  } else {
    message(
      "AnnotationHub binary not available for this platform; trying source install next. "
    )
  }

  if (!requireNamespace("AnnotationHub", quietly = TRUE)) {
    options(pkgType = "source")
    options(install.packages.check.source = "no")
    try(do.call(BiocManager::install, c(list(pkgs = "AnnotationHub"), bioc_args)), silent = TRUE)
  }

  if (requireNamespace("AnnotationHub", quietly = TRUE)) return(invisible(TRUE))

  if (requireNamespace("pak", quietly = TRUE)) {
    message("AnnotationHub still missing; retrying with pak (bioc::AnnotationHub)...")
    pak_args <- list("bioc::AnnotationHub", upgrade = FALSE)
    if (!is.null(lib)) pak_args$lib <- lib
    try(do.call(pak::pak, pak_args), silent = TRUE)
  }

  if (requireNamespace("AnnotationHub", quietly = TRUE)) return(invisible(TRUE))

  if (!requireNamespace("AnnotationHub", quietly = TRUE)) {
    message("AnnotationHub still missing; retrying from Bioconductor source with dependencies...")
    src_args <- c(list(pkgs = "AnnotationHub", dependencies = TRUE), bioc_args)
    try(do.call(BiocManager::install, src_args), silent = TRUE)
  }

  has_tools <- isTRUE(pkgbuild::has_build_tools(debug = FALSE))
  if (!has_tools && .Platform$OS.type == "windows") {
    message("Build tools not detected. Attempting automatic Rtools installation...")
    try(pkgbuild::install_build_tools(quiet = TRUE), silent = TRUE)
    has_tools <- isTRUE(pkgbuild::has_build_tools(debug = FALSE))
  }

  if (!has_tools) {
    stop(
      paste0(
        "AnnotationHub could not be installed. Binary packages for one or more dependencies are unavailable ",
        "for this R/platform, and local build tools were not detected.\n",
        "Install build tools (e.g., Rtools on Windows) and rerun this script."
      ),
      call. = FALSE
    )
  }

  stop("AnnotationHub installation failed even with build tools. Check network/repo configuration and retry.", call. = FALSE)
}

## 2) Bioconductor packages -------------------------------------------------

bioc_packages <- c(
  "AnnotationHub",
  "clusterProfiler",
  "enrichplot",
  "STRINGdb",
  "DEqMS",
  "BiocFileCache",
  "sva",            # ComBat batch correction
  "GO.db",          # GO term hierarchy (e.g. for dropdowns)
  "AnnotationDbi",  # generic annotation infrastructure
  "biomaRt"         # annotation via Ensembl
  # "ensembldb"     # optional: additional annotation via Ensembl databases
)

# Update Bioconductor packages
if (length(bioc_packages)) {
  options(repos = BiocManager::repositories())
  BiocManager::install(bioc_packages, ask = FALSE, update = FALSE)
}

bioc_missing_after <- get_missing_pkgs(bioc_packages)
if (length(bioc_missing_after)) {
  message("Retrying missing Bioconductor packages: ", paste(bioc_missing_after, collapse = ", "))
  options(repos = BiocManager::repositories())
  BiocManager::install(bioc_missing_after, ask = FALSE, update = FALSE, force = TRUE)
}

install_annotationhub_reliably(update = TRUE)
bioc_missing_after_retry <- get_missing_pkgs(bioc_packages)
if (length(bioc_missing_after_retry) && !isTRUE(pkgbuild::has_build_tools(debug = FALSE))) {
  stop(
    paste0(
      "Bioconductor packages still missing after retry: ",
      paste(bioc_missing_after_retry, collapse = ", "),
      "\nCause is typically source-only availability for your platform/R version ",
      "without local build tools.\nInstall R build tools (e.g., Rtools on Windows) and rerun update.R."
    ),
    call. = FALSE
  )
}


## 3) Required CRAN packages (runtime) --------------------------------------

# NOTE:
# - This list mirrors the CRAN packages loaded in app.R via `required_packages`.

cran_packages <- c(
  "shiny",        # Shiny web applications
  "htmltools",    # HTML generation (core shiny dependency)
  "shinyjs",      # JavaScript integration in Shiny
  "magrittr",     # piping operator %>%
  "shinydashboard", # dashboard layout
  "rhandsontable",  # interactive, editable tables
  "plotly",         # interactive plots
  "DT",             # interactive data tables
  "readxl",         # read Excel files
  "writexl",        # write Excel files
  "reshape2",       # data reshaping
  "ggplot2",        # base plotting system
  "ggrepel",        # non-overlapping text labels
  "viridis",        # colorblind-friendly color scales
  "stringr",        # string handling
  "gridExtra",      # arrange multiple plots
  "doParallel",     # parallel backend for foreach
  "foreach",        # loop abstraction for parallel processing
  "cowplot",        # plot composition (e.g. publication-style plots)
  "igraph",         # static network plots
  "visNetwork",     # interactive network visualizations
  "dplyr",          # data manipulation
  "RColorBrewer",   # additional color palettes
  "ComplexUpset",   # complex UpSet plots
  "VennDiagram",    # Venn diagrams
  "circlize",       # circular plots and color mapping
  "missForest",     # random forest-based imputation
  "mice",           # multiple imputation by chained equations
  "pracma",         # numerical methods and utilities
  "colourpicker",   # color picker input for Shiny
  "scales",         # axis breaks and label helpers
  "shinyalert",     # alert dialogs in Shiny
  "svglite",        # SVG graphics device
  "tidyverse",      # collection of packages for data analysis
  "purrr",          # functional programming and list-column tools
  "openxlsx",       # advanced Excel file creation
  "ggupset",        # UpSet-style plotting for sets
  "ggridges",       # ridge plots (e.g. for distributions/GSEA)
  "umap",           # UMAP dimensionality reduction
  # "rlang",          # tidy evaluation / programming utilities
  "sortable",       # drag-and-drop UI components
  "png",            # read/write PNG images
  "rsvg",           # render SVG into other formats
  "xml2",           # parse and manipulate XML (e.g. SVG)
  "bslib",          # Bootstrap theming for Shiny
  "later",          # scheduling/debouncing for Shiny
  "patchwork",      # plot composition (used in GSEA plots)
  "promises",       # Dependency
  "Rcpp",           # Dependency
  "rlang",          # Dependency
  "qs"              # Stack-safe serialization used by session save/restore
)

# Update only missing required CRAN packages
if (length(cran_packages)) {
  options(repos = c(CRAN = "https://cran.r-project.org"))
  install.packages(cran_packages, dependencies = TRUE)
}

cran_missing_after <- get_missing_pkgs(cran_packages)
if (length(cran_missing_after)) {
  message("Retrying missing CRAN packages: ", paste(cran_missing_after, collapse = ", "))
  options(repos = c(CRAN = "https://cran.r-project.org"))
  install.packages(cran_missing_after, dependencies = TRUE)
}


## 4) Optional / Developer / Documentation packages ------------------------

# These packages are mentioned in the technical documentation (Documentation/MiraProt_doc_tech.R)
# and are useful for:
#   - vignette and documentation rendering (rmarkdown, knitr, etc.)
#   - data cleaning helpers (janitor, etc.)
#   - development and tooling (fs, glue, withr, progress, cli, checkmate, remotes, etc.)
# They are NOT strictly required to run the Shiny app, but recommended for
# development, diagnostics, and documentation workflows.

optional_cran_packages <- c(
  "devtools",  # source-development tooling; not required by the application
  "tidyr",     # data tidying (already covered by tidyverse, but listed explicitly in docs)
  "tibble",    # modern data frames (also pulled in by tidyverse)
  "jsonlite",  # JSON handling (e.g. metadata export)
  "yaml",      # YAML configs
  "knitr",     # report generation / markdown knitting
  "rmarkdown", # R Markdown documents
  "janitor",   # data cleaning helpers
  "fs",        # file system helpers
  "glue",      # string interpolation
  "withr",     # scoped side effects (e.g. temp options)
  "progress",  # progress bars
  "pillar",    # tibble column display
  "cli",       # command-line user interface helpers
  "checkmate", # argument checking and assertions
  "httr",      # HTTP requests
  "curl",      # low-level HTTP client
  "dataRetrieval", # Curl dependency
  "remotes",   # install packages from remote repositories
  "gtable"     # layout tool used under the hood by ggplot2
)

if (length(optional_cran_packages)) {
  options(repos = c(CRAN = "https://cran.r-project.org"))
  install.packages(optional_cran_packages, dependencies = TRUE)
}

opt_missing_after <- get_missing_pkgs(optional_cran_packages)
if (length(opt_missing_after)) {
  message("Retrying missing optional packages: ", paste(opt_missing_after, collapse = ", "))
  options(repos = c(CRAN = "https://cran.r-project.org"))
  install.packages(opt_missing_after, dependencies = TRUE)
}


## 5) GitHub packages -------------------------------------------------------

# Packages that are installed from GitHub (via pak)
github_repos <- c(
  "shinyTree/shinyTree"  # shinyTree: tree widgets for Shiny
)

for (repo in github_repos) {
  has_build_tools <- isTRUE(pkgbuild::has_build_tools(debug = FALSE))

  if (has_build_tools) {
    # Preferred path: install from GitHub to get the exact upstream version.
    pak::pak(repo, upgrade = FALSE)
  } else {
    # Fallback path for fresh systems without compilers/build tools.
    # Try installing a binary from CRAN by package name instead of GitHub source.
    pkg_name <- sub("^.*/", "", repo)
    message(
      sprintf(
        "Build tools not detected. Falling back to CRAN install for '%s'.",
        pkg_name
      )
    )
    options(repos = c(CRAN = "https://cran.r-project.org"))
    install.packages(pkg_name, dependencies = TRUE)
  }
}

github_pkg_names <- sub("^.*/", "", github_repos)
required_all <- unique(c(bioc_packages, cran_packages, github_pkg_names))
missing_final <- get_missing_pkgs(required_all)
if (length(missing_final)) {
  missing_bioc <- intersect(missing_final, bioc_packages)
  missing_non_bioc <- setdiff(missing_final, bioc_packages)
  install_hints <- c()
  if (length(missing_non_bioc)) {
    install_hints <- c(
      install_hints,
      paste0(
        "For CRAN/GitHub packages use: install.packages(c(",
        paste(sprintf("\"%s\"", missing_non_bioc), collapse = ", "),
        "))"
      )
    )
  }
  if (length(missing_bioc)) {
    install_hints <- c(
      install_hints,
      paste0(
        "For Bioconductor packages use: BiocManager::install(c(",
        paste(sprintf("\"%s\"", missing_bioc), collapse = ", "),
        "), ask = FALSE, update = TRUE)"
      )
    )
  }
  stop(
    paste0(
      "Dependency update incomplete. Missing packages: ",
      paste(missing_final, collapse = ", "),
      "\n",
      paste(install_hints, collapse = "\n")
    ),
    call. = FALSE
  )
}

cat("MiraProt dependency updates finished successfully.\n")
