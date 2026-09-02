# Contributing to MiraProt

Thank you for your interest in improving MiraProt.

MiraProt is research software for interactive downstream proteomics analysis. Contributions that improve reliability, usability, documentation, portability, or scientific transparency are welcome.

## Before opening an issue

Please:

1. Check the README and the documentation in the `Documentation/` directory.
2. Search existing GitHub Issues to see whether the problem or suggestion has already been reported.
3. Make sure you are using a current MiraProt version when practical.

Please do not upload unpublished, confidential, patient-identifying, personally identifying, proprietary, or otherwise sensitive data to a public GitHub Issue. Use a small synthetic or properly anonymized example whenever possible.

## Reporting bugs

Use the **Bug report** issue form.

A useful bug report should include:

- MiraProt version
- installation mode (source or portable build)
- operating system
- R version, when relevant
- affected MiraProt module or workflow step
- clear steps to reproduce the problem
- expected behavior
- observed behavior
- relevant error messages or logs

A minimal reproducible example is especially helpful.

## Suggesting features

Use the **Feature request** issue form.

Please explain the problem you want to solve rather than only describing a proposed interface change.

If the request changes statistical, bioinformatic, or other scientific behavior, please clearly state this and, where appropriate, provide a short rationale or relevant reference.

## Development setup

For source-based development:

```r
renv::restore()
shiny::runApp(".")
```

The repository uses `renv` to record the R package environment. Avoid changing `renv.lock` unless the dependency change is intentional.

Additional development information is available in `GUIDE_PORTABLE_DEV.md` and the technical documentation under `Documentation/`.

## Pull requests

Small, focused pull requests are preferred.

Before submitting a pull request:

- explain what problem the change addresses
- keep unrelated changes out of the same pull request
- update documentation when user-visible behavior changes
- state whether scientific or statistical output can change
- state whether dependencies or `renv.lock` changed
- avoid committing generated, temporary, confidential, or sensitive files

For changes to the portable Go launcher, run the existing Go tests when applicable:

```powershell
cd portable/launcher; go test ./...
```

For R changes, test the affected workflow manually and describe what you tested in the pull request. MiraProt does not currently claim a comprehensive automated R test suite.

## Changes to scientific behavior

Changes that may alter numerical results, statistical interpretation, enrichment results, filtering, normalization, imputation, differential analysis, or other scientific outputs require particular care.

Please describe:

- what behavior changes
- why the change is needed
- which workflows may be affected
- how the change was checked
- relevant methodological references, when applicable

Such changes may require additional review before they are merged.

## Dependencies and external resources

If your change adds or changes a dependency:

- explain why it is required
- update the relevant installation or environment files
- check whether licensing or redistribution terms affect MiraProt
- update `THIRD_PARTY_NOTICES.md` when appropriate

Do not add third-party datasets or resources unless their redistribution terms permit it.

## Documentation contributions

Documentation fixes and clarifications are welcome. Please keep terminology consistent with the README and existing MiraProt documentation.

## License

By contributing to this repository, you agree that your contribution will be distributed under the repository's MIT License.
