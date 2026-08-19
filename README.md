# MiraProt

MiraProt is a modular Shiny application for proteomics analysis with two supported execution modes:

1. **Developer mode (local R / RStudio):** run directly from source.
2. **Portable mode (locally generated desktop package):** build from this source repository, then run via the bundled launcher + R runtime.

This README is a practical entry point for both audiences and is aligned with the in-app documentation architecture (`Documentation/MiraProt_doc_ui.R`, `Documentation/MiraProt_doc_user.R`, `Documentation/MiraProt_doc_tech.R`).

---

## Choose your mode

### A) Developer mode (R/RStudio, from source)

Choose this when you:

- want to inspect or change code,
- already work in R or RStudio,
- need module-level debugging and development workflows.

### B) Portable mode (standalone app)

Choose this when you:

- want a repeatable local desktop artifact,
- need a reproducible desktop bundle with launcher and preinstalled packages,
- want to bundle the package environment once and launch it repeatedly.

---

## Developer mode (local R / RStudio)

### 1) Prerequisites

- R installed locally (portable build tooling reads its maintained R runtime default from `portable/R_VERSION`; source mode should use a modern compatible R release).
- System libraries as required by your OS for Bioconductor/CRAN packages.
- Git clone or downloaded source tree.

### 2) Install dependencies

From repository root:

```bash
Rscript install.R
```

This script installs Bioconductor + CRAN + GitHub dependencies, includes fallbacks for common build-tool issues, and can be rerun safely.

### 3) Run the app

```bash
Rscript -e "shiny::runApp('.')"
```

or in RStudio console:

```r
shiny::runApp('.')
```

### 4) Startup behavior (repository-verified)

- `app.R` initializes session lock/cleanup behavior.
- A shared `modEnv` is created/cleared.
- Source files are loaded dynamically from:
  - `modules/*.R` wrappers,
  - most `R/*.R` infrastructure files,
  - all `Documentation/*.R` documentation renderers.
- UI is built through `build_ui(modEnv)`.
- Server initializes shared `rv`, module outputs, coordination, exports, diagnostics, and session save/restore wiring.

---

## Portable mode (standalone edition)

Portable mode is implemented under `portable/` and uses a Go launcher that starts the bundled R runtime and opens the Shiny UI in your browser.

For complete operational details:

- **End users:** `GUIDE_PORTABLE_USER.md`
- **Developers/release maintainers:** `GUIDE_PORTABLE_DEV.md`

### Portable quick facts

- Includes launcher + bundled R + bundled packages + app source. Linux/macOS builds require the matching native R to be preinstalled; the Windows bundler downloads R automatically.
- Intended to run without requiring local R/RStudio installation.
- Is normally built locally from the source repository; public portable binaries are not the authoritative installation path.
- Provides platform-specific packaging (Windows installer, macOS app/dmg, Linux AppImage/archive) via scripts under `portable/installers/` and `portable/scripts/`.

### R runtime version versus MiraProt version

The bundlers' optional `-RVersion` (Windows) and `--r-version` (Linux/macOS)
arguments select the **R runtime placed in `r-portable`**. They do not select
the MiraProt application version. Ordinary users should omit these arguments;
`portable/R_VERSION` is the maintained default. An override is intended for a
maintainer deliberately testing a different, exactly matching R installation.

On Windows, runtime validation does not depend on a `VERSION` file. The
bundler uses the portable `R.exe --version` and `Rscript.exe --version` as
startup probes, then obtains the authoritative version from the absolute-path
portable `Rscript.exe` running `getRversion()`. It isolates inherited R
configuration and repeats these checks after promotion, before replacing the
previous runtime permanently.

MiraProt application versioning is derived independently from Git/build
metadata and the product-version logic in `R/version_info.R`. It is not the R
version, launcher version, Windows installer version, or saved-session schema
version; each of those has its own compatibility and release purpose.

---

## Internet-dependent vs offline features

Most plotting and data-processing workflows can run offline once dependencies are installed.

Internet is typically required for:

- **GO module** (AnnotationHub retrieval/caching).
- **STRING module** (remote STRING database queries).
- **Annotation workflows via biomaRt** (remote Ensembl access).

---

## Project layout (high-level)

```text
MiraProt/
  app.R                       # entrypoint and runtime orchestration
  install.R                   # dependency installation script
  update.R                    # dependency update workflow
  R/                          # app infrastructure (UI/server wiring, coordination, diagnostics, export)
  modules/                    # module wrappers + per-module implementation trees
  Documentation/              # in-app user and technical documentation modules
  portable/                   # launcher, bundling scripts, installer definitions
  GSEA/                       # bundled gene set resources
  AutoAssign/                 # auto-assign templates/resources
  GUIDE_PORTABLE_USER.md      # end-user standalone guide
  GUIDE_PORTABLE_DEV.md       # standalone build/release guide
```

---

## Documentation model in this repository

MiraProt keeps documentation responsibilities explicit:

- `Documentation/MiraProt_doc_ui.R`: routing/navigation only.
- `Documentation/MiraProt_doc_user.R`: user-facing content.
- `Documentation/MiraProt_doc_tech.R`: developer-facing content.

If you update architecture or workflows, update the corresponding documentation owner file to keep in-app docs and README consistent.

---

## Developer notes for safe extension

When adding or refactoring modules:

1. Add/adjust wrapper entry points under `modules/*_module.R`.
2. Keep module internals in module-specific subdirectories.
3. Register integration points in app-level UI/server orchestration (`R/ui.R`, `R/server_modules.R`, `R/server_coordination.R`).
4. Keep cross-module data flow explicit via shared reactive contracts, not hidden direct dependencies.

---

## Support files to read next

- `README.md` (this file): orientation + mode selection.
- `GUIDE_PORTABLE_USER.md`: standalone usage details.
- `GUIDE_PORTABLE_DEV.md`: build, packaging, and release details.
- `Documentation/MiraProt_doc_user.R`: in-app user workflow documentation.
- `Documentation/MiraProt_doc_tech.R`: in-app technical architecture documentation.
