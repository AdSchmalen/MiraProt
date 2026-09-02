## Summary

Briefly describe the change and the problem it addresses.

## Type of change

- [ ] Bug fix
- [ ] Documentation
- [ ] User interface or usability
- [ ] New feature
- [ ] Scientific or statistical behavior
- [ ] Portable build or launcher
- [ ] Dependency or infrastructure
- [ ] Other

## Scientific impact

Does this change affect numerical results, statistical behavior, filtering, normalization, imputation, enrichment, differential analysis, or other scientific output?

- [ ] No
- [ ] Yes — explained below
- [ ] Unsure — explained below

If yes or unsure, describe the expected impact and provide methodological references when appropriate.

## Testing performed

Describe how you tested the change.

For portable Go launcher changes, run the existing Go tests when applicable:

```powershell
cd portable/launcher; go test ./...
```

For R changes, describe the affected MiraProt workflow that you tested manually.

## Reproducibility and dependencies

- [ ] `renv.lock` is unchanged, or any change to it is intentional and explained.
- [ ] New or changed third-party dependencies and redistribution implications have been considered.
- [ ] `THIRD_PARTY_NOTICES.md` has been updated if required.

## Documentation

- [ ] User-facing documentation has been updated if behavior changed.
- [ ] No documentation update is required.

## Final checklist

- [ ] The change is focused and does not include unrelated modifications.
- [ ] I have not committed confidential, identifying, proprietary, or otherwise sensitive data.
- [ ] I searched for related Issues and Pull Requests.
