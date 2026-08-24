# Windows Portable R 4.6.1 session-restore acceptance

Run this manual acceptance check on a clean Windows machine for every Portable
release candidate. It exercises event-loop timing and launcher process behavior
that cannot be represented faithfully by the Linux unit-test environment.

## Preconditions

1. Build/install the Windows Portable artifact and run
   `resources\R\bin\Rscript.exe -e "cat(as.character(getRversion()))"`; it must
   report **4.6.1**.
2. Start `MiraProt.exe`, retain the launcher and R log files, and open the URL
   shown by the launcher.
3. Import a dataset with a stable identifier column, at least two abundance
   columns, and matching sample metadata.
4. Configure and render Data Wizard, Abundances, SampleIDs, PCA, Volcano,
   Dotplot, and Heatmap, then save a **Full Session** file.

## Restore procedure

1. Close MiraProt and verify its launcher and child R process exit. Start it
   again so the restore begins in a clean R process.
2. Upload the Full Session file. Do not interact with controls while replay is
   in progress.
3. Wait until the log contains the final report for the uploaded generation.
   The final state must be `SETTLED`; an upload-progress message or first plot
   render is not completion.
4. Record the generation number and retain every restore job line from the
   first `HYDRATED`/`REPLAYING` line through the final report.

## Required evidence

- **Process liveness:** after `SETTLED`, refresh the health URL and make one UI
  change that causes a plot render. The same launcher-owned R process must
  remain alive and the health check must continue to succeed.
- **Canonical pairing:** compare restored Data Wizard data and metadata with
  the pre-save values. Each abundance column must retain its canonical metadata
  row/sample identity; there must be no cross-pairing with another cached data
  revision. Re-save the restored session and repeat one restore to demonstrate
  the pairing survives a round trip.
- **Complete reporting:** the final generation report must list resolved work
  for Data Wizard, Abundances, SampleIDs, PCA, Volcano, Dotplot, and Heatmap.
  Confirm the report is emitted only after the last required render/replay job
  resolves and that no job for that generation remains outstanding.
- **Context safety:** search both launcher and R logs (case-insensitively) for
  `Operation not allowed without an active reactive context`,
  `Can't access reactive value`, `current reactive context`, and
  `REACTIVE_CONTEXT_VIOLATION`. All searches must return zero matches.

## Acceptance record

Record the Portable build/version, Windows version, R version, session-file
checksum, restore generation, final state, R process ID before/after restore,
health result, canonical-pair comparison result, owner/job report, and the four
context-safety search results. Attach the logs. The candidate passes only when
the state is `SETTLED`, the process is live, pairing is unchanged, reporting is
complete, no required jobs remain, and every context-safety search is empty.
