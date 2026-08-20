# Third-Party Notices

MiraProt is open-source research software developed by Adrian Schmalen.

Original MiraProt source code, documentation, configuration files, and other
original material are distributed under the MIT License. See `LICENSE.md` for
the applicable license terms.

MiraProt depends on and interoperates with third-party software, databases,
web services, and scientific resources. Those third-party components are not
relicensed under the MiraProt MIT License. They remain subject to the
licenses, terms of use, attribution requirements, citation requirements, and
trademark rights of their respective providers.

This notice is provided to clarify those boundaries. It does not replace or
modify the license terms of any third-party component.

## R and Bioconductor software

MiraProt is implemented in R and depends on packages obtained from CRAN,
Bioconductor, GitHub, and other upstream software repositories.

The official MiraProt source release does not distribute copies of those
package source trees or precompiled R package libraries. Dependencies are
installed separately by the user.

Each third-party package remains subject to its own upstream license.

References to packages in MiraProt source files, `install.R`, `renv.lock`, or
other dependency metadata identify software required or supported by
MiraProt. Such references do not relicense those packages under the MiraProt
MIT License.

The `renv.lock` file records package versions and sources for reproducibility.
Its inclusion does not change the licenses of the packages it describes.

## Shiny

MiraProt uses the `shiny` R package and related software maintained by Posit
Software, PBC.

Shiny is a third-party software framework and is not part of the original
MiraProt software covered by the MiraProt copyright notice.

Use of the name Shiny within MiraProt source code or documentation is solely
to identify the software framework and technical dependency with which
MiraProt is implemented.

MiraProt is an independent research software project and is not affiliated
with, sponsored by, endorsed by, or certified by Posit Software, PBC.

## AutoAssign compatibility presets

MiraProt includes AutoAssign preset files that were independently created for
MiraProt to assist with recognizing and configuring data imported from common
proteomics software workflows.

These preset files are original MiraProt material and are distributed under
the MiraProt MIT License.

Some preset filenames or labels contain third-party product names in order to
identify the software output or workflow with which a preset is intended to
be compatible. In particular, MiraProt currently contains presets referring
to Proteome Discoverer and Spectronaut.

Proteome Discoverer is third-party proteomics software provided by Thermo
Fisher Scientific under the Thermo Scientific product family.

Spectronaut is third-party proteomics software provided by Biognosys.

References to these product names are made solely for identification,
compatibility, and interoperability purposes. They do not imply that the
respective third-party software is included with MiraProt.

The AutoAssign presets do not distribute copies of Proteome Discoverer or
Spectronaut software.

MiraProt does not grant any rights to third-party product names, trademarks,
logos, software, or other proprietary material.

MiraProt is not affiliated with, sponsored by, endorsed by, or certified by
Thermo Fisher Scientific or Biognosys.

Users remain responsible for obtaining and using third-party proteomics
software in accordance with the licenses and terms provided by the respective
vendors.

## Go dependencies and portable builds

The MiraProt repository contains source code for an optional portable launcher.

The launcher references external Go modules through the Go module system.
Those modules remain subject to their respective upstream licenses and are not
relicensed under the MiraProt MIT License.

Official MiraProt releases distribute source code rather than precompiled
portable application bundles containing third-party runtimes or package
libraries.

Users may build a portable MiraProt distribution locally using the provided
source and build infrastructure.

A locally generated portable distribution can contain third-party components,
including an R runtime, R packages, Go dependencies, system libraries, and
other software. Those components retain their respective licenses and
redistribution conditions.

Anyone redistributing such a locally generated bundle is responsible for
complying with the applicable third-party licenses and redistribution
requirements.

## MSigDB gene-set resources

MiraProt supports Gene Set Enrichment Analysis using gene-set collections in
GMT format, including collections obtained from the Molecular Signatures
Database (MSigDB).

MSigDB GMT files are not distributed with the official MiraProt source
release.

Users who wish to perform GSEA with MSigDB collections must obtain the desired
gene-set files independently from MSigDB and comply with the applicable
MSigDB terms, collection-specific conditions, attribution requirements, and
citation requirements.

Downloaded MSigDB files remain third-party scientific resources. They are not
part of MiraProt and are not covered by the MiraProt MIT License.

MiraProt documentation explains where compatible GMT files can be placed so
that they can be detected by the GSEA module.

Users should not commit downloaded MSigDB GMT files to the MiraProt source
repository.

## AnnotationHub and Bioconductor annotation resources

MiraProt can retrieve organism-specific annotation resources using
Bioconductor infrastructure including AnnotationHub.

Resources downloaded through AnnotationHub or related Bioconductor services
remain subject to the licenses, copyright notices, terms, and citation
requirements applicable to the respective packages and underlying annotation
resources.

Downloaded databases and cache files are local runtime resources and are not
part of the official MiraProt source release.

The MiraProt MIT License does not apply to third-party annotation data merely
because MiraProt downloads, caches, reads, or processes those data.

## Ensembl and BioMart

MiraProt can communicate with Ensembl BioMart for identifier annotation,
identifier mapping, and related biological-data retrieval.

Data obtained from Ensembl or BioMart are provided by external services and
remain subject to the applicable terms, licenses, attribution requirements,
and citation requirements of their providers.

MiraProt can cache some retrieved BioMart information locally to reduce
repeated network requests. Creating such a local cache does not make the
underlying third-party data part of MiraProt or subject it to the MiraProt MIT
License.

MiraProt does not claim ownership of data obtained from Ensembl or BioMart.

## STRING

MiraProt can communicate with the STRING database and associated services for
protein-protein interaction analyses.

Information retrieved from STRING remains third-party data and is subject to
the applicable STRING terms, licenses, attribution requirements, and citation
requirements.

MiraProt does not claim ownership of data obtained from STRING and does not
relicense such data under the MiraProt MIT License.

## Other external services and scientific resources

MiraProt may interact with additional external scientific resources through
its R package dependencies or through services selected by the user.

Unless explicitly stated otherwise, data obtained from an external provider
remain subject to the provider's own terms and are not covered by the
MiraProt MIT License.

Users should consult the documentation and licensing information supplied by
the relevant provider when using or redistributing externally obtained data.

## User-provided data

Datasets, annotation files, gene-set files, session inputs, and other material
supplied by users are not covered by the MiraProt MIT License merely because
they are imported, processed, analyzed, visualized, cached, or exported using
MiraProt.

Users are responsible for ensuring that they have the necessary rights to use,
process, share, and redistribute data supplied to MiraProt.

## Trademarks and product names

All third-party company names, product names, software names, service names,
logos, trademarks, and registered trademarks remain the property of their
respective owners.

References to third-party names in MiraProt are made only when useful to
describe dependencies, compatibility, interoperability, supported input
formats, external services, or scientific resources.

Such references do not imply affiliation with or endorsement of MiraProt by
the respective trademark or product owners.

MiraProt does not grant any license or other rights to third-party trademarks.

## No endorsement

Mention of a third-party product, package, database, service, organization, or
scientific resource is provided solely to describe MiraProt functionality,
dependencies, compatibility, or research workflows.

Unless explicitly stated otherwise, no third-party provider has sponsored,
endorsed, certified, or approved MiraProt.