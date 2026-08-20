# MiraProt

**MiraProt** is a modular R Shiny application for interactive downstream proteomics analysis.

It brings commonly used proteomics analysis and visualization workflows into a connected graphical environment, allowing results and selections to be transferred between analysis modules without repeatedly exporting and re-importing intermediate data.

MiraProt includes tools for data preparation, differential expression analysis, PCA and UMAP, heatmaps, functional enrichment, gene set enrichment analysis (GSEA), STRING protein-interaction networks, volcano and dot plots, set visualization, and figure composition.

## Getting started

MiraProt can be used in two ways:

1. **Source mode** — run MiraProt directly from the repository using a local R installation.
2. **Self-built portable mode** — build a standalone local MiraProt distribution from the provided source and build scripts.

Official MiraProt releases distribute **source code rather than precompiled portable application bundles**.

---

# Source mode

## Requirements

To run MiraProt directly from source, you need:

- R;
- the R packages required by MiraProt;
- an internet connection for installation and for features that access online databases.

RStudio is optional.

## Download MiraProt

Clone the repository:

```text
git clone https://github.com/AdSchmalen/MiraProt.git
cd MiraProt
```

Alternatively, download a tagged MiraProt source release from GitHub and extract it to a local directory.

For published analyses, use a tagged release rather than an arbitrary development version.

## Install the R environment

For reproducible use of the publication release, restore the package versions recorded in `renv.lock`:

```text
install.packages("renv")
renv::restore()
```

MiraProt also provides `install.R` as a convenience installer for the required CRAN, Bioconductor, and GitHub dependencies:

```text
Rscript install.R
```

The `renv.lock` file should be preferred when reproducing the software environment of a specific MiraProt release.

## Run MiraProt

From the repository root:

```text
Rscript -e "shiny::runApp('.')"
```

or from an R/RStudio console:

```text
shiny::runApp(".")
```

---

# Self-built portable mode

MiraProt includes tools for assembling a standalone local distribution.

The portable build can combine the MiraProt source code, an R runtime, the required R packages, optional prebuilt annotation caches, and a launcher into a locally generated application.

Precompiled portable distributions, bundled R runtimes, bundled R package libraries, installers, DMG files, AppImages, and Windows setup executables are **not distributed as official MiraProt release assets**.

Users who want a portable edition build it locally from the tagged MiraProt source release.

Detailed instructions are provided in the source repository:

- `GUIDE_PORTABLE_USER.md` — step-by-step build instructions, including optional Windows installer creation;
- `GUIDE_PORTABLE_DEV.md` — architecture, packaging, validation, and developer details.

These guides are build documentation in the source repository and are not required at runtime.

## Optional Windows installer

After a Windows portable bundle has been built and tested, it can optionally be packaged into a normal Windows setup executable using **Inno Setup 6**.

The installer stage consumes the already-built portable directory. It does not rebuild R, reinstall R packages, rebuild MiraProt, or refresh caches.

A Windows installer build requires these stage-1 components:

- `MiraProt-launcher.exe`;
- `shiny-app/`;
- `r-portable/`;
- `r-library/`.

The following are optional and are included when present:

- `go-cache/`;
- `LICENSE.md`;
- `README.md`;
- `THIRD_PARTY_NOTICES.md`;
- `citation.cff`.

When a portable cache is present, the installer preserves it as packaged seed data. The installed launcher uses a writable per-user runtime cache under `%LOCALAPPDATA%\MiraProt\cache`. Existing user cache contents are not overwritten by the shipped seed. If no cache was packaged, MiraProt remains functional and downloads required resources on demand.

The exact installer procedure is documented in `GUIDE_PORTABLE_USER.md` and `GUIDE_PORTABLE_DEV.md`.

Third-party components included in a locally generated portable build remain subject to their respective upstream licenses.

If a locally generated bundle or installer is redistributed, the person redistributing it is responsible for complying with the applicable licenses and redistribution requirements of R, R packages, and other bundled third-party components.

---

# Typical workflow

A typical MiraProt workflow begins in the **Data Wizard**, where proteomics tables can be imported, organized, annotated, filtered, transformed, and prepared for downstream analysis.

The **Primary Data** together with its associated metadata are then made available to the downstream MiraProt analysis modules.

**Secondary Data** can be used within the Data Wizard to extend or modify the Primary Data, for example through merging or pivot merging, but are not passed directly to downstream analysis modules.

Once the data are prepared, analysis can continue across the different MiraProt modules. Depending on the workflow, results or selected identifiers can be transferred between modules to support connected downstream exploration.

---

# Major analysis features

MiraProt provides interconnected functionality for common downstream proteomics workflows, including:

- data import, restructuring, filtering, annotation, normalization, imputation, and transformation;
- differential expression analysis;
- volcano plots;
- PCA and UMAP;
- heatmap visualization;
- Gene Ontology enrichment;
- Gene Set Enrichment Analysis;
- STRING protein-protein interaction analysis;
- customizable dot plots;
- Venn and set-based visualization;
- figure and plot-grid composition;
- export of plots and analysis results;
- session saving and restoration.

The available controls and statistical methods depend on the selected module and on the structure and metadata of the imported dataset.

---

# Gene Set Enrichment Analysis and MSigDB

## Gene-set files are not included with MiraProt

MiraProt contains the code required to perform Gene Set Enrichment Analysis, but **does not distribute gene-set database files**.

This separation is intentional.

To perform GSEA, you must provide one or more gene-set files in **GMT format**.

The runtime directory depends on how MiraProt is being used:

| MiraProt mode | GSEA directory |
|---|---|
| Source mode | `<MiraProt source repository>/GSEA/` |
| Flat portable bundle | `<portable bundle>/shiny-app/GSEA/` |
| Windows installer | `<MiraProt installation>/shiny-app/GSEA/` |
| macOS app | `MiraProt.app/Contents/Resources/app/GSEA/` |
| Linux AppImage | `usr/bin/shiny-app/GSEA/` inside the packaged image |

The portable stage-1 builders copy locally present, immediate lowercase
`<MiraProt source repository>/GSEA/*.gmt` files to
`<portable bundle>/shiny-app/GSEA/`. With no matching files, assembly continues
normally. Installers, DMGs, and AppImages made from that stage-1 bundle preserve
those files; adding a source GMT afterward requires rebuilding stage 1 or
copying the file into a writable flat bundle before packaging.

Local GMT files are deliberately ignored by Git, and MiraProt neither downloads
nor redistributes them in the source repository. Anyone distributing a locally
built artifact containing third-party GMT files is responsible for ensuring
that the source, license, and terms permit redistribution.

For source mode, MiraProt searches:

```text
GSEA/
```

For a Windows flat portable build or installed Windows version, the corresponding runtime directory is:

```text
shiny-app/GSEA/
```

Any compatible file ending in:

```text
.gmt
```

that is placed directly in the appropriate runtime `GSEA/` directory can be detected by the GSEA module.

For example, a source checkout may look like:

```text
MiraProt/
├── GSEA/
│   ├── h.all.v2026.1.Hs.symbols.gmt
│   ├── c2.cp.reactome.v2026.1.Hs.symbols.gmt
│   └── c5.go.bp.v2026.1.Hs.symbols.gmt
├── modules/
├── R/
├── app.R
└── ...
```

The exact filenames depend on the database release and collections you choose.

If a packaged installation directory is not writable, add the required GMT files to the portable bundle before packaging, or build/install MiraProt to a user-writable location.

## Using MSigDB

MiraProt is compatible with gene-set collections distributed through the **Molecular Signatures Database (MSigDB)**.

MSigDB files are **not included in the MiraProt repository or its releases**.

To use MSigDB gene sets:

1. Visit the official MSigDB website:
   [https://www.gsea-msigdb.org/gsea/msigdb/](https://www.gsea-msigdb.org/gsea/msigdb/)
2. Register or sign in if required by MSigDB.
3. Download the desired **human gene-symbol GMT collection**.
4. Copy the downloaded `.gmt` file into the appropriate MiraProt runtime `GSEA/` directory shown above.
5. Start MiraProt and open the GSEA module. The available GMT files can then be selected as gene-set resources for the analysis.

You do **not** need to download every MSigDB collection. Download only the collections required for your analysis.

For example, commonly used collections include:

- Hallmark gene sets;
- Reactome pathways;
- Gene Ontology Biological Process;
- Gene Ontology Cellular Component;
- Gene Ontology Molecular Function.

The choice of collection is part of the scientific analysis and should be documented when reporting results.

## Licensing of gene-set files

MSigDB and its gene-set collections are independent third-party scientific resources.

Downloaded MSigDB files:

- are not part of MiraProt;
- are not licensed under the MiraProt MIT License;
- should not be committed to the MiraProt repository;
- remain subject to the license terms, attribution requirements, and citation requirements specified by MSigDB and the underlying collection providers.

For reproducible research, record the exact MSigDB release and collection used in an analysis.

For example:

```text
MSigDB release: 2026.1.Hs
Collection: Hallmark gene sets
Identifier type: Gene Symbol
```

When publishing GSEA results, cite MSigDB and any underlying resources as required by the corresponding database documentation.

---

# Internet-dependent features

Most local data processing and visualization functions can operate without an internet connection once MiraProt and its dependencies have been installed.

Some features require access to external services.

## AnnotationHub

Gene Ontology and annotation workflows can retrieve organism-specific annotation resources through Bioconductor and AnnotationHub.

Downloaded resources can be cached locally for later reuse.

## Ensembl BioMart

The Data Wizard annotation workflow can access Ensembl BioMart for identifier mapping and cross-species annotation.

MiraProt can cache BioMart metadata and mapping tables locally so that previously downloaded information can be reused.

## STRING

The STRING module communicates with the STRING database to retrieve protein-interaction information.

An internet connection is therefore required when new STRING information needs to be retrieved.

## GSEA

The GSEA analysis itself uses the local `.gmt` files supplied by the user.

Once the required gene-set files and R dependencies are available locally, the gene-set database does not need to be downloaded from MSigDB during each analysis.

---

# Caches and downloaded resources

MiraProt can cache information retrieved from external databases to improve performance and reduce repeated network requests.

These caches can include resources obtained from services such as:

- AnnotationHub;
- Ensembl BioMart;
- STRING or related external services where applicable.

Cache files are generated locally and are not part of the official MiraProt source distribution.

A self-built portable bundle may include locally available GO, AnnotationHub, and BioMart caches as seed data. A flat portable bundle uses its adjacent `go-cache/` directly. Packaged formats such as the Windows installer use the shipped cache only as seed data and maintain writable runtime cache files in the user's application-data directory.

Removing a cache does not remove MiraProt itself. Required information will be downloaded again when the corresponding feature needs it.

---

# Project structure

The main repository structure is:

```text
MiraProt/
├── app.R
├── install.R
├── renv.lock
├── LICENSE.md
├── THIRD_PARTY_NOTICES.md
├── citation.cff
│
├── R/
│   └── application-wide infrastructure
│
├── modules/
│   ├── main module entry files
│   └── module-specific implementation directories
│
├── Documentation/
│   └── in-app user and technical documentation
│
├── GSEA/
│   └── user-supplied GMT gene-set files
│
├── AutoAssign/
│   └── Data Wizard assignment presets
│
├── portable/
│   └── portable-build, launcher, and optional packaging source
│
├── GUIDE_PORTABLE_USER.md
└── GUIDE_PORTABLE_DEV.md
```

The root `modules/` directory contains the main module entry files. Detailed implementation code is organized in the corresponding module subdirectories.

The Data Wizard uses an additional feature-based level of organization, with its main feature scripts under `modules/Data Wizard/` and detailed feature implementations in corresponding subdirectories.

---

# Reproducibility

MiraProt releases are versioned so that analyses can be associated with a defined software state.

For a reproducible analysis, record at least:

- the MiraProt version;
- the R version;
- the package environment represented by `renv.lock`;
- the input data and relevant preprocessing decisions;
- external database/resource versions where applicable;
- the MSigDB release and collection when GSEA is used.

The package environment of the reference release can be restored with:

```text
install.packages("renv")
renv::restore()
```

A tagged MiraProt software release should be used when reproducing analyses associated with a publication.

---

# Session save and restore

MiraProt supports saving and restoring application sessions.

Session files can preserve analysis state so that work can be continued later without manually recreating all module settings.

Because MiraProt can interact with external resources and locally generated caches, restoration of a session does not replace the need to retain the corresponding input data, compatible software environment, and required external resources.

For long-term reproducibility, retain the MiraProt release version and the files used for the original analysis.

---

# Documentation

MiraProt includes both user-facing and technical documentation inside the application.

The user documentation explains:

- how to prepare data;
- how to use the individual analysis modules;
- available analysis options;
- import and export workflows;
- GSEA resource setup;
- session handling.

Technical documentation describes the internal architecture and is intended primarily for development and maintenance.

Additional information on building a portable local distribution and optional platform packages is available in the source repository:

- `GUIDE_PORTABLE_USER.md`
- `GUIDE_PORTABLE_DEV.md`

---

# Third-party software and resources

MiraProt depends on third-party R and Bioconductor packages and can interact with external scientific databases and services.

These dependencies and resources remain subject to their respective upstream licenses and terms.

The official MiraProt source release does not relicense:

- R;
- CRAN packages;
- Bioconductor packages;
- Go dependencies;
- MSigDB gene-set collections;
- AnnotationHub resources;
- Ensembl/BioMart data;
- STRING data;
- other externally obtained scientific resources.

Additional information is provided in:

```text
THIRD_PARTY_NOTICES.md
```

---

# License

MiraProt is distributed under the **MIT License**.

Copyright (c) 2026 Adrian Schmalen

See:

```text
LICENSE.md
```

for the full license terms.

The MIT License applies to the original MiraProt software and associated material for which the copyright holder is entitled to grant that license.

Third-party packages, databases, downloaded resources, and other externally provided material remain subject to their respective licenses and terms.

---

# Citation

If you use MiraProt in published research, please cite the associated MiraProt publication and the software version used for your analysis.

Machine-readable citation information is provided in:

```text
citation.cff
```

The publication release of MiraProt is archived separately to provide a persistent, version-specific software record.

When external resources such as MSigDB, STRING, Ensembl, AnnotationHub, or individual R/Bioconductor methods make a substantive contribution to an analysis, cite the corresponding resources and methods as appropriate.

---

# Research use and disclaimer

MiraProt is research software intended for scientific data analysis and education.

It has not been validated as a clinical diagnostic or medical decision-making system.

Results generated by MiraProt should be interpreted in the context of the experimental design, data quality, selected statistical methods, analysis parameters, and relevant biological evidence.

MiraProt is provided under the conditions stated in the MIT License and without warranty of any kind.

---

# Contributing and development

MiraProt is organized as a modular Shiny application.

When modifying or extending the software:

- keep module entry points in the root `modules/` directory;
- keep module-specific implementation code in the corresponding module subdirectory;
- preserve explicit cross-module reactive interfaces;
- update technical documentation when implementation architecture changes;
- avoid committing generated caches or third-party scientific resources;
- do not commit MSigDB `.gmt` files.

Changes affecting scientific calculations should be tested carefully to ensure that expected analysis behavior remains reproducible.

---

# Repository

MiraProt source code:

[https://github.com/AdSchmalen/MiraProt](https://github.com/AdSchmalen/MiraProt)
