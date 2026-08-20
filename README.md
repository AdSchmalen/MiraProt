<p align="center">
  <img src="MiraProt_icon.png" alt="MiraProt logo" width="180">
</p>

# MiraProt

**Interactive downstream proteomics analysis in R Shiny**

MiraProt is a modular R Shiny application for the interactive downstream analysis of **processed protein-level proteomics data**.

It connects data preparation, metadata handling, statistical analysis, dimensionality reduction, functional enrichment, gene-set enrichment, protein-interaction analysis, visualization, export, and session management in one application.

The central idea is to reduce repeated manual transfer of intermediate results between separate tools. Data prepared in the **Data Wizard** are made available to the downstream MiraProt modules, and analysis results and selections can be used across the connected workflow.

MiraProt is intended for downstream analysis of processed proteomics tables. It does **not** perform raw mass-spectrometry data processing, peptide-spectrum matching, protein identification, or primary quantitative processing of raw MS files.

---

## What data does MiraProt use?

A typical MiraProt input is a table in which:

- each row represents a protein;
- one or more columns contain stable protein identifiers;
- sample abundance measurements are stored in separate columns;
- additional columns may contain annotations, ratios, statistics, or other protein-level information.

CSV, TSV, and Excel-based data can be imported through the Data Wizard.

Additional tables can be loaded as secondary data and merged with the primary dataset when required.

The Data Wizard can then be used to organize metadata, reshape data, filter proteins, transform or normalize measurements, handle missing values, calculate ratios and statistics, correct batch effects, and prepare the final dataset for the downstream analysis modules.

---

## Analysis workflow

MiraProt is organized as a connected workflow rather than a collection of independent applications.

| Workflow stage | MiraProt functionality |
|---|---|
| **Data preparation** | Data Wizard, table import, metadata assignment, filtering, transformation, normalization, imputation, pivoting, merging, batch correction, ratios and statistics |
| **Data inspection** | Table views, abundance distributions, sample information |
| **Dimensionality reduction** | PCA and UMAP |
| **Differential analysis and visualization** | Statistical contrasts and volcano plots |
| **Functional analysis** | Gene Ontology enrichment and Gene Set Enrichment Analysis |
| **Interaction analysis** | STRING protein-protein interaction networks |
| **Visualization** | Heatmaps, dot plots, Venn/set visualization and plot-grid composition |
| **Export and reproducibility** | Multi-module Excel export and session save/restore |

A typical workflow is:

1. load a processed protein-level dataset in the **Data Wizard**;
2. define identifiers, sample metadata, conditions, and other relevant annotations;
3. clean, transform, filter, impute, merge, or otherwise prepare the data;
4. inspect the prepared dataset using abundance, sample, PCA/UMAP, and table views;
5. perform statistical, enrichment, network, and visualization analyses;
6. export results or save the MiraProt session for later continuation.

The exact analysis path depends on the experimental design and the structure of the imported data.

---

## Getting started

MiraProt can be used in two ways:

1. **Source mode** — run MiraProt directly from the source repository using a local R environment.
2. **Self-built portable mode** — build a standalone local distribution containing MiraProt, R, the required packages, and the native launcher.

Official MiraProt releases distribute the **source code**. Portable bundles and platform installers are generated locally from that source.

### Source mode

Clone the repository and enter it:

```text
git clone https://github.com/AdSchmalen/MiraProt.git; cd MiraProt
```

For a reproducible environment, install `renv` if necessary and restore the package versions recorded in `renv.lock`:

```text
Rscript -e "if (!requireNamespace('renv', quietly = TRUE)) install.packages('renv', repos = 'https://cloud.r-project.org'); renv::restore()"
```

Then start MiraProt from the repository root:

```text
Rscript -e "shiny::runApp('.')"
```

Alternatively, from an R or RStudio console opened in the repository root:

```r
shiny::runApp(".")
```

The project `.Rprofile` activates the MiraProt `renv` environment automatically when the project is started normally.

For published or reproducible analyses, use a tagged MiraProt release and the corresponding `renv.lock`.

### Convenience dependency installation

`install.R` provides an alternative dependency installer for setting up the packages required by MiraProt:

```text
Rscript install.R
```

Use `renv::restore()` when reproducing the package environment associated with a specific release. Use `install.R` when a convenient installation of the required dependencies is preferred over restoration of the exact lockfile state.

---

## Portable mode

MiraProt includes portable-build tooling for Windows, Ubuntu/Debian-family Linux, and macOS.

The portable build combines the application with an R runtime, its required packages, the MiraProt launcher, and optional locally available cache and GSEA resources.

### Platform status

| Platform | Portable build status |
|---|---|
| **Windows x86-64** | Manually built and tested. Optional Inno Setup installer packaging is available. |
| **Linux, Ubuntu/Debian family** | Build and AppImage tooling is implemented, but the current portable distribution has not yet been manually validated end-to-end on a native Linux installation. |
| **macOS, Intel / Apple Silicon** | Build and DMG tooling is implemented, but the current portable distribution has not yet been manually validated end-to-end on native macOS systems. |

Linux and macOS portable packaging should therefore be considered **experimental** until complete native builds and representative runtime workflows have been verified on those platforms.

This qualification applies to the portable desktop packaging. It does not imply that the R/Shiny source application itself is inherently Windows-only.

Detailed build instructions are maintained separately:

- [GUIDE_PORTABLE_USER.md](GUIDE_PORTABLE_USER.md) — step-by-step instructions for building and running the portable edition;
- [GUIDE_PORTABLE_DEV.md](GUIDE_PORTABLE_DEV.md) — portable architecture, packaging, validation, and developer details.

### Windows

The Windows stage-1 builder can create a portable directory containing:

- the MiraProt application;
- a portable R runtime;
- the required R package library;
- the native MiraProt launcher;
- optional prebuilt GO, AnnotationHub, and BioMart cache data;
- optional locally supplied GSEA GMT files;
- available root documentation and licensing files.

After the portable bundle has been built and tested, it can optionally be packaged as a conventional Windows setup executable using **Inno Setup 6**.

The installer packages the already-built portable distribution. It does not rebuild MiraProt, reinstall R packages, or regenerate scientific caches.

### Linux and macOS

The Linux/macOS stage-1 builder creates a flat portable distribution from a compatible native R installation and native build environment.

Optional stage-2 tooling can then create:

- a Linux AppImage;
- a macOS application bundle and DMG.

These paths are currently provided for development and testing and should be regarded as experimental until native end-to-end validation has been completed.

### Build dependencies versus runtime dependencies

Build tools such as Go, PowerShell, Git, compilers, Rtools, Inno Setup, or platform packaging tools are used only when assembling particular portable artifacts.

They are not automatically runtime requirements of the finished application.

The portable bundle contains its own R runtime and R package library. Linux and macOS builds can still depend on compatible operating-system shared libraries.

Portable artifacts generated locally remain subject to the licenses and redistribution conditions of their bundled third-party components.

---

## GSEA gene-set files

MiraProt supports Gene Set Enrichment Analysis using gene-set collections in GMT format.

**GMT gene-set files are not distributed with the MiraProt source code.**

Users can obtain suitable gene-set collections from resources such as the [Molecular Signatures Database (MSigDB)](https://www.gsea-msigdb.org/gsea/msigdb/) or another source whose terms permit their use.

### Source mode

Place lowercase `.gmt` files directly in:

```text
GSEA/
```

MiraProt searches the immediate contents of this directory.

For example:

```text
MiraProt/
├── GSEA/
│   ├── h.all.v2026.1.Hs.symbols.gmt
│   └── c5.go.bp.v2026.1.Hs.symbols.gmt
├── modules/
├── R/
├── app.R
└── ...
```

### Portable builds

The runtime GSEA directory of a flat portable bundle is:

```text
shiny-app/GSEA/
```

Locally present immediate source files matching:

```text
GSEA/*.gmt
```

are deliberately ignored by Git but can be copied into:

```text
shiny-app/GSEA/
```

by the portable builder.

If no GMT files are present, the portable build continues normally.

Platform packagers preserve GMT files that are already present in the tested stage-1 portable bundle.

Typical packaged locations are:

| MiraProt mode | GSEA directory |
|---|---|
| Source mode | `<MiraProt source>/GSEA/` |
| Flat portable bundle | `<portable bundle>/shiny-app/GSEA/` |
| Windows installer | `<installation>/shiny-app/GSEA/` |
| macOS app | `MiraProt.app/Contents/Resources/app/GSEA/` |
| Linux AppImage | packaged `usr/bin/shiny-app/GSEA/` |

Adding a GMT file to the source checkout after a portable bundle has already been created does not change that existing bundle. Rebuild the portable version or copy the file into the appropriate writable runtime directory before packaging.

### MSigDB and redistribution

MSigDB and other GMT collections are independent third-party scientific resources.

Downloaded gene-set files:

- are not part of the MiraProt source distribution;
- are not licensed under the MiraProt MIT License;
- should not be committed to the MiraProt repository;
- remain subject to the terms, attribution requirements, and citation requirements of their respective providers.

The presence of a GMT file in a locally generated MiraProt bundle does not grant redistribution rights.

Anyone redistributing a bundle, installer, DMG, or AppImage containing third-party GMT files is responsible for ensuring that such redistribution is permitted.

For reproducible GSEA analyses, record the exact resource, release, and collection used.

For example:

```text
MSigDB release: 2026.1.Hs
Collection: Hallmark gene sets
Identifier type: Gene Symbol
```

---

## External resources and internet access

Most local data preparation, statistical analysis, and visualization can run without an internet connection once the required software and data are available.

Some MiraProt functions access external scientific resources:

| Resource | Used for | Local reuse |
|---|---|---|
| **Bioconductor / AnnotationHub** | Organism-specific annotation and Gene Ontology resources | Downloaded resources can be cached |
| **Ensembl BioMart** | Identifier mapping and cross-species annotation | Metadata and mapping results can be cached |
| **STRING** | Protein-protein interaction information | New queries require access to the STRING service |
| **GSEA gene sets** | Gene-set enrichment analysis | Supplied as local `.gmt` files |

Portable builds can reuse compatible locally available GO, AnnotationHub, and BioMart cache data as seed resources when those data are present.

Caches are optional. If required cached information is absent, MiraProt can retrieve the corresponding resource when that functionality is first used.

The first operation requiring an uncached external resource may therefore be slower and require an internet connection.

### Portable cache behavior

A flat portable bundle uses its adjacent writable `go-cache/` directory.

Packaged formats such as the Windows installer, macOS app, and Linux AppImage can contain prebuilt cache data as seed resources. Writable runtime cache data are then maintained in the user's application-data directory rather than modifying packaged resources.

Existing user cache contents are not overwritten merely because a newly packaged build contains seed data.

Detailed cache architecture is documented in [GUIDE_PORTABLE_DEV.md](GUIDE_PORTABLE_DEV.md).

---

## Reproducibility

MiraProt is designed to make an analysis easier to continue and reproduce, but software state is only one part of reproducibility.

For an analysis intended for publication or long-term reuse, retain at least:

- the MiraProt release/version;
- the R and package environment represented by `renv.lock`;
- the original input data;
- the relevant Data Wizard preprocessing decisions;
- analysis settings and statistical choices;
- external database/resource versions where applicable;
- the exact GMT collection and release when GSEA is used.

For a tagged source release, the reference package environment can be restored with:

```text
Rscript -e "if (!requireNamespace('renv', quietly = TRUE)) install.packages('renv', repos = 'https://cloud.r-project.org'); renv::restore()"
```

### Session save and restore

MiraProt supports session save and restore so that work can be continued later without recreating the entire analysis manually.

Depending on the selected save level, session files can retain:

- processed data and metadata;
- Data Wizard state;
- analysis results;
- visualization settings;
- other supported module configuration.

A saved session complements rather than replaces the original input data, software environment, and external-resource information needed for a reproducible analysis.

For long-term reproducibility, retain both the session file and the MiraProt release with which the analysis was performed.

---

## Documentation

MiraProt contains user-facing and technical documentation directly inside the application.

The in-app documentation covers:

- data preparation and metadata handling;
- individual analysis modules;
- analysis options and interpretation;
- import and export workflows;
- GSEA setup;
- session save and restore;
- technical implementation details for developers.

Portable build documentation is maintained separately:

- [GUIDE_PORTABLE_USER.md](GUIDE_PORTABLE_USER.md)
- [GUIDE_PORTABLE_DEV.md](GUIDE_PORTABLE_DEV.md)

The user guide is intended for people who want to build and run a local portable version.

The developer guide documents the portable architecture, launcher, caches, packaging, and platform-specific implementation details.

---

## Citation

If you use MiraProt in research, please cite the software version used for the analysis and the associated MiraProt publication when available.

The associated manuscript is currently in preparation:

> **Interactive downstream proteomics analysis with MiraProt using Müller cell proteomes from equine recurrent uveitis**  
> Adrian Schmalen, Amelie B. Fleischer, Barbara M. Riedel, Cornelia A. Deeg

Machine-readable software citation metadata are provided in:

```text
CITATION.cff
```

A version-specific archival DOI and the publication DOI can be added to the citation metadata when they become available.

When external resources or methods such as MSigDB, STRING, Ensembl, AnnotationHub, or individual R/Bioconductor packages make a substantive contribution to an analysis, cite those resources as appropriate.

---

## License and third-party software

MiraProt is distributed under the **MIT License**.

Copyright (c) 2026 Adrian Schmalen

See [LICENSE.md](LICENSE.md) for the complete license.

The MIT License applies to the original MiraProt software and associated material for which the copyright holder is entitled to grant that license.

MiraProt depends on R, CRAN packages, Bioconductor packages, Go dependencies, and external scientific databases and services.

These components and resources remain subject to their respective licenses and terms.

Additional information is provided in [THIRD_PARTY_NOTICES.md](THIRD_PARTY_NOTICES.md).

The MiraProt MIT License does not relicense:

- R;
- CRAN packages;
- Bioconductor packages;
- Go dependencies;
- MSigDB gene-set collections;
- AnnotationHub resources;
- Ensembl/BioMart data;
- STRING data;
- other externally obtained scientific resources.

If a locally generated portable bundle or installer is redistributed, the person redistributing it is responsible for complying with the applicable licenses and redistribution requirements of the bundled third-party components and resources.

---

## Research use

MiraProt is research software intended for scientific data analysis and education.

It has not been validated as a clinical diagnostic or medical decision-making system.

Results generated by MiraProt should be interpreted in the context of:

- the experimental design;
- data quality;
- preprocessing choices;
- selected statistical methods;
- analysis parameters;
- external database versions;
- relevant biological evidence.

MiraProt is provided without warranty under the terms of the MIT License.

---

## Development

MiraProt is organized as a modular Shiny application.

The main implementation areas are:

- `R/` — application-wide infrastructure and coordination;
- `modules/` — analysis and visualization modules;
- `Documentation/` — in-app user and technical documentation;
- `AutoAssign/` — Data Wizard assignment-rule resources;
- `GSEA/` — local user-supplied gene-set resources;
- `portable/` — native launcher, portable builders, and platform packaging.

The root `modules/` directory contains the main module entry points, while detailed implementations are organized in module-specific subdirectories.

Development changes should:

- preserve explicit interfaces between modules;
- keep application-wide coordination separate from module-specific logic;
- update user or technical documentation when behavior changes;
- avoid committing generated caches;
- avoid committing third-party scientific resources;
- never commit locally supplied GSEA `.gmt` files;
- preserve reproducibility of scientific calculations where possible.

See [GUIDE_PORTABLE_DEV.md](GUIDE_PORTABLE_DEV.md) for portable-build and packaging architecture.

---

## Contributing

Changes affecting scientific calculations, statistical behavior, data transformation, annotation, or enrichment should be validated carefully.

Contributions should avoid unnecessary architectural coupling between modules and should preserve the existing explicit reactive interfaces used to transfer data and analysis state across MiraProt.

When adding or changing externally sourced resources, packages, or services, also consider whether licensing, attribution, documentation, or reproducibility information needs to be updated.

---

## Repository

MiraProt source code:

https://github.com/AdSchmalen/MiraProt