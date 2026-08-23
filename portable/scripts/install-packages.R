# install-packages.R
# Install all MiraProt R dependencies into a specified library directory.
#
# Usage:  Rscript install-packages.R <target-lib-path>
#
# Called by the portable build scripts (bundle-r.sh, bundle-r-windows.ps1).
# Package lists mirror those in the project root install.R.

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 1) {
  stop("Usage: Rscript install-packages.R <target-lib-path>")
}

lib_path <- normalizePath(args[1], mustWork = FALSE)
if (!dir.exists(lib_path)) {
  dir.create(lib_path, recursive = TRUE)
}

# IMPORTANT: Restrict .libPaths() to ONLY lib_path and R's base library.
# Without this, install.packages(dependencies=TRUE) sees packages in the
# build machine's user/system library (e.g. digest, rlang, cli) and skips
# installing them into lib_path.  The portable bundle then ships without
# these transitive dependencies and fails at runtime.
r_base_lib <- file.path(R.home(), "library")
.libPaths(c(lib_path, r_base_lib))

# --vanilla is the primary boundary. This guard makes accidental invocation
# from an activated source-development renv session fail loudly instead of
# allowing renv's install shim to redirect portable package installation.
renv_project <- Sys.getenv("RENV_PROJECT", unset = "")
renv_option <- getOption("renv.project.path", NULL)
if ("renv:shims" %in% search() || nzchar(renv_project) ||
    (!is.null(renv_option) && nzchar(as.character(renv_option)))) {
  stop(
    paste0(
      "Portable package installation must run outside the MiraProt renv environment. ",
      "Invoke Rscript with --vanilla."
    ),
    call. = FALSE
  )
}

options(repos = c(CRAN = "https://cloud.r-project.org"))
cat("Installing MiraProt packages to:", lib_path, "\n\n")

get_missing_pkgs <- function(pkgs, lib.loc = lib_path) {
  available <- vapply(pkgs, function(pkg) {
    installed_here <- nzchar(system.file(package = pkg, lib.loc = lib.loc))
    installed_here && requireNamespace(pkg, quietly = TRUE)
  }, FUN.VALUE = logical(1))
  pkgs[!available]
}

install_cran <- function(pkgs, dependencies = TRUE) {
  if (!length(pkgs)) return(invisible(NULL))
  if (.Platform$OS.type != "windows") {
    utils::install.packages(pkgs, lib = lib_path, dependencies = dependencies)
    return(invisible(NULL))
  }

  binary_index <- tryCatch(
    utils::available.packages(repos = getOption("repos"), type = "binary"),
    error = function(e) NULL
  )
  binary_pkgs <- if (is.null(binary_index)) character() else intersect(pkgs, rownames(binary_index))
  if (length(binary_pkgs)) {
    utils::install.packages(binary_pkgs, lib = lib_path, dependencies = dependencies, type = "binary")
  }
  source_pkgs <- setdiff(pkgs, binary_pkgs)
  if (length(source_pkgs)) {
    message(
      "No compatible Windows binary is available for: ",
      paste(source_pkgs, collapse = ", "),
      ". Trying source installation."
    )
    utils::install.packages(source_pkgs, lib = lib_path, dependencies = dependencies, type = "source")
  }
  invisible(NULL)
}

expected_rtools <- function() {
  version <- getRversion()
  if (version >= "4.5.0") return("Rtools45")
  if (version >= "4.4.0") return("Rtools44")
  if (version >= "4.3.0") return("Rtools43")
  if (version >= "4.2.0") return("Rtools42")
  "the Rtools release compatible with this R version"
}


## 1) BiocManager -----------------------------------------------------------

cat("--- Step 1/5: BiocManager bootstrap ---\n")
if (!requireNamespace("BiocManager", quietly = TRUE)) {
  install_cran("BiocManager", dependencies = FALSE)
}
if (!requireNamespace("pkgbuild", quietly = TRUE)) {
  install_cran("pkgbuild", dependencies = TRUE)
}



ensure_user_library_writable <- function() {
  # Despite the historical helper name, portable installation must never add
  # the build user's library. The caller-supplied portable library is the only
  # writable package destination.
  if (file.access(lib_path, 2) != 0) {
    stop("Portable R library is not writable: ", lib_path, call. = FALSE)
  }
  .libPaths(c(lib_path, r_base_lib))
  invisible(lib_path)
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
  if (!has_tools) {
    tool_hint <- if (.Platform$OS.type == "windows") expected_rtools() else "R build tools"
    stop(
      paste0(
        "AnnotationHub could not be installed. Binary packages for one or more dependencies are unavailable ",
        "for this R/platform, and local build tools were not detected.\n",
        "Install ", tool_hint, " and rerun this script."
      ),
      call. = FALSE
    )
  }

  stop("AnnotationHub installation failed even with build tools. Check network/repo configuration and retry.", call. = FALSE)
}

## 2) Bioconductor packages -------------------------------------------------

cat("--- Step 2/5: Bioconductor packages ---\n")
bioc_packages <- c(
  "AnnotationHub",
  "clusterProfiler",
  "enrichplot",
  "STRINGdb",
  "DEqMS",
  "BiocFileCache",
  "limma",          # Data Wizard statistical and batch operations
  "fgsea",          # selected GSEA engine
  "BiocParallel",   # GSEA execution parallelization
  "sva",
  "GO.db",
  "AnnotationDbi",
  "biomaRt"
)

bioc_missing <- bioc_packages[!bioc_packages %in% rownames(installed.packages(lib.loc = lib_path))]
if (length(bioc_missing)) {
  cat("Installing", length(bioc_missing), "Bioconductor packages...\n")
  options(repos = BiocManager::repositories())
  BiocManager::install(bioc_missing, lib = lib_path, ask = FALSE, update = FALSE)
} else {
  cat("All Bioconductor packages already installed.\n")
}
bioc_missing_after <- get_missing_pkgs(bioc_packages)
if (length(bioc_missing_after)) {
  cat("Retrying missing Bioconductor packages:", paste(bioc_missing_after, collapse = ", "), "\n")
  options(repos = BiocManager::repositories())
  BiocManager::install(bioc_missing_after, lib = lib_path, ask = FALSE, update = FALSE, force = TRUE)
}

install_annotationhub_reliably(lib = lib_path, update = FALSE)
bioc_missing_after_retry <- get_missing_pkgs(bioc_packages)
if (length(bioc_missing_after_retry) && !isTRUE(pkgbuild::has_build_tools(debug = FALSE))) {
  stop(
    paste0(
      "Bioconductor packages still missing after retry: ",
      paste(bioc_missing_after_retry, collapse = ", "),
      "\nLikely source-only packages for this R/platform with no local build tools."
    ),
    call. = FALSE
  )
}


## 3) Required CRAN packages ------------------------------------------------

cat("--- Step 3/5: CRAN runtime packages ---\n")
cran_packages <- c(
  "shiny",
  "htmltools",
  "shinyjs",
  "magrittr",
  "shinydashboard",
  "rhandsontable",
  "plotly",
  "DT",
  "readxl",
  "writexl",
  "reshape2",
  "ggplot2",
  "ggrepel",
  "viridis",
  "stringr",
  "gridExtra",
  "doParallel",
  "foreach",
  "cowplot",
  "igraph",
  "visNetwork",
  "dplyr",
  "RColorBrewer",
  "ComplexUpset",
  "VennDiagram",
  "circlize",
  "missForest",
  "mice",
  "pracma",
  "colourpicker",
  "scales",
  "shinyalert",
  "svglite",
  "tidyverse",
  "purrr",
  "openxlsx",
  "ggupset",
  "ggridges",
  "umap",
  "sortable",
  "png",
  "rsvg",
  "xml2",
  "bslib",
  "later",
  "patchwork",
  "promises",
  "Rcpp",
  "rlang"#,
  # "qs"
)

cran_missing <- cran_packages[!cran_packages %in% rownames(installed.packages(lib.loc = lib_path))]
if (length(cran_missing)) {
  cat("Installing", length(cran_missing), "CRAN packages...\n")
  install_cran(cran_missing, dependencies = TRUE)
} else {
  cat("All CRAN runtime packages already installed.\n")
}
cran_missing_after <- get_missing_pkgs(cran_packages)
if (length(cran_missing_after)) {
  cat("Retrying missing CRAN packages:", paste(cran_missing_after, collapse = ", "), "\n")
  install_cran(cran_missing_after, dependencies = TRUE)
}


## 4) Optional / developer packages ----------------------------------------

cat("--- Step 4/5: Optional CRAN packages ---\n")
optional_packages <- c(
  "tidyr",
  "tibble",
  "jsonlite",
  "yaml",
  "knitr",
  "rmarkdown",
  "janitor",
  "fs",
  "glue",
  "withr",
  "progress",
  "pillar",
  "cli",
  "checkmate",
  "httr",
  "curl",
  "remotes",
  "gtable"
)

opt_missing <- optional_packages[!optional_packages %in% rownames(installed.packages(lib.loc = lib_path))]
if (length(opt_missing)) {
  cat("Installing", length(opt_missing), "optional packages...\n")
  install_cran(opt_missing, dependencies = TRUE)
} else {
  cat("All optional packages already installed.\n")
}
opt_missing_after <- get_missing_pkgs(optional_packages)
if (length(opt_missing_after)) {
  cat("Retrying missing optional packages:", paste(opt_missing_after, collapse = ", "), "\n")
  install_cran(opt_missing_after, dependencies = TRUE)
}


## 5) GitHub packages -------------------------------------------------------

cat("--- Step 5/5: GitHub packages ---\n")
if (!requireNamespace("shinyTree", quietly = TRUE)) {
  if (!requireNamespace("pak", quietly = TRUE)) {
    install_cran("pak", dependencies = TRUE)
  }
  has_build_tools <- isTRUE(pkgbuild::has_build_tools(debug = FALSE))
  if (has_build_tools) {
    pak::pak("shinyTree/shinyTree", lib = lib_path, upgrade = FALSE)
  } else {
    cat("Build tools not detected. Falling back to CRAN install for shinyTree.\n")
    install_cran("shinyTree", dependencies = TRUE)
  }
} else {
  cat("shinyTree already installed.\n")
}


## Done ---------------------------------------------------------------------

required_all <- unique(c(bioc_packages, cran_packages, "shinyTree"))
missing_final <- get_missing_pkgs(required_all, lib.loc = lib_path)
if (length(missing_final)) {
  stop(
    paste0(
      "Portable dependency installation incomplete. Missing: ",
      paste(missing_final, collapse = ", ")
    ),
    call. = FALSE
  )
}
cat("\n=== All packages installed successfully ===\n")
cat("Library path:", lib_path, "\n")
cat("Total packages:", nrow(installed.packages(lib.loc = lib_path)), "\n")
