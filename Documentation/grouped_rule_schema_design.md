# Grouped Auto-Assign rule schema and migration design

**Status:** normative design for the next persisted rule-format revision. The
current implementation must not emit a grouped payload until every loader,
editor, executor, session serializer, and transfer path implements this
contract. Capitalized requirement words are normative.

## 1. Goals and non-goals

This design makes a rule row independently identifiable, groups content and
downstream rules by a stable variant identity, makes precedence data explicit,
and records which content selector owns a variant's transformation. It also
provides a lossless, deterministic upgrade for the three legacy data frames.

It does **not** change matching, regex dialect, slash escaping, extraction
methods, NA conventions, or the current last-matching-content-rule behavior.
It does not infer grouping from regex equality: regex text is mutable rule data,
not identity.

## 2. Version and capability gate

A persisted or transferred rule collection MUST be an envelope with these
top-level fields before grouped variants may be present:

| Field | Storage class | Required value for this design |
|---|---|---|
| `RuleFormatVersion` | integer scalar | `2L` |
| `RequiredCapabilities` | character vector | includes `"grouped-variants-v1"`, `"stable-rule-id-v1"`, `"explicit-priority-v1"`, and `"transformation-owner-v1"` |
| `table` | data frame | content rules below |
| `condition` | data frame | condition rules below |
| `ratio` | data frame | ratio rules below |

Version and capabilities are semantic input, not debug metadata. They MUST be
inside every import, export, session snapshot, transfer snapshot, and rollback
snapshot. A loader MUST inspect them before selecting or coercing the frames.
It MUST reject atomically when `RuleFormatVersion` is greater than its maximum
supported version or any required capability is unknown. Warning and
best-effort loading are forbidden. In particular, a runtime that lacks
`grouped-variants-v1` MUST NOT discard `VariantId` and run condition, ratio, or
transformation rules against every row in a `Content` class.

An unversioned collection is, by definition, legacy version 1 and enters only
the migration in section 5. Version 2 is never inferred merely because columns
with familiar names happen to exist.

## 3. Canonical relational schema

All identity columns are nonblank character vectors. All priorities are
positive, non-missing integers, unique within their component. Data-frame row
names are canonical automatic row names and have no identity or precedence
meaning.

### 3.1 Content selectors (`table`)

Canonical column order and classes are:

```text
RuleId              character
VariantId           character
Priority            integer
Content              character
Include              character
Exclude              character
Transformation       character
TransformationOwner logical
```

`RuleId` is immutable identity for one selector row. `VariantId` is immutable
identity for the logical variant produced by one or more selector rows. Several
selectors MAY share a variant; this represents alternative matches with common
downstream behavior. `Priority` defines content execution order, ascending,
with the later matching selector retaining the existing last-match-wins
behavior. Physical row order MUST equal ascending priority in canonical storage.

Exactly one content selector per `VariantId` MUST have
`TransformationOwner == TRUE`. Its `Transformation` is the variant's effective
transformation. Non-owner transformation cells are retained as typed source
data for lossless legacy round trips but MUST NOT execute. New version-2 rows
MUST write the owner's effective value into every selector in that variant;
validators reject disagreement for non-legacy version-2 content. This explicit
owner rule prevents repeated selectors from repeatedly transforming the full
Content class and also gives migrated legacy conflicts an exact owner.

`Content` is a label, not a key. Different variants MAY have the same Content.
`RuleId` is globally unique in the complete envelope; `VariantId` is unique as
a logical variant identity but intentionally repeats in related rows.

### 3.2 Condition rules (`condition`)

Canonical column order and classes are:

```text
RuleId character; VariantId character; Priority integer; Content character;
Method character; Before character; After character; Separators character;
Pos integer
```

### 3.3 Ratio rules (`ratio`)

Canonical column order and classes are:

```text
RuleId character; VariantId character; Priority integer; Content character;
Method character; Separators character; Invert logical;
NumBefore character; NumAfter character; DenBefore character;
DenAfter character; NumPos integer; DenPos integer
```

For both downstream components, `VariantId` is a required foreign key to a
content variant and `Content` is redundant, human-readable integrity data: it
MUST equal that variant's Content. `Priority` orders rules only within that
component. The executor selects rows by the winning content `VariantId`, then
applies matching downstream rows in ascending priority using the existing
condition or ratio method semantics. It MUST NOT fall back to Content matching.

`RuleId` survives editing, sorting, exporting, importing, and restoration.
Cloning creates a new RuleId; editing never does. Moving a row changes
priorities, not identities. Deleting a row deletes its RuleId. A grouped edit
that changes a row's variant changes `VariantId` but not `RuleId`.

### 3.4 Canonical Row Index

There is exactly one Row Index content selector. It has `Content == "Row
Index"`, `Include == "Row Index"`, `Exclude == ""`, `Transformation ==
NA_character_`, and `TransformationOwner == TRUE`. It has its own RuleId and
VariantId. It remains at the same relative position during legacy migration;
no migration may silently move it to the end. Downstream frames MUST NOT
reference its VariantId. Normal user editing MAY reorder it only if current
Auto-Assign permits that operation; canonicalization then records that order in
Priority rather than imposing a new special-case order.

## 4. Identity creation

New identities SHOULD be lowercase UUIDv4 strings generated once at row or
variant creation. Identity comparison is byte-for-byte and case-sensitive.
Neither a displayed row number, Content, regex, method fields, nor Priority is
part of an identity.

Legacy migration needs reproducible identities. Define `LegacyPayloadId` as
lowercase SHA-256 of a canonical, length-prefixed byte encoding of all three
original frames in order: component name; ordered column names; each column's
exact R storage class; row count; and every value with distinct encodings for
NA, empty text, NaN, infinities, raw bytes, and text encoded as UTF-8. Attributes
other than class and names are excluded. Implementations MUST publish test
vectors before release; they MUST NOT use locale-sensitive printing, R row
serialization, randomized hashes, regex text normalization, or current time.

Deterministic migrated identities are UUIDv5 in the fixed application namespace
`miraprot:auto-assign:legacy:v2`:

* rule: `LegacyPayloadId + "/" + component + "/" + original one-based row`;
* variant: `LegacyPayloadId + "/variant/" + UTF-8 length-prefixed Content`.

The namespace string MUST first be converted to a published fixed namespace
UUID in the implementation test vectors; it is not an instruction to choose a
namespace dynamically. Once written, IDs are persisted and never recomputed.
The payload scope makes identical duplicate rows distinct while repeated import
of the same legacy payload produces identical IDs.

## 5. Deterministic legacy migration

Migration is a pure transaction over a deep copy. Any failure returns the
untouched legacy object and an error; partially upgraded frames are forbidden.

1. Require data-frame `table`, `condition`, and `ratio` components and capture
   their names, storage classes, values, attributes required by the existing
   loader, and physical row order. Compute `LegacyPayloadId` before coercion.
2. Validate the legacy object with the legacy validator. Do not repair regex,
   replace NA, trim strings, normalize slash escaping, normalize methods, sort,
   deduplicate, or run current version-2 coercion.
3. Walk content rows in their original physical order. Assign `Priority =
   seq_len(nrow(table))`, a deterministic per-row RuleId, and the deterministic
   VariantId for that row's exact Content value. Thus legacy alternative
   selectors for one Content remain one logical variant and retain legacy
   Content-wide downstream behavior.
4. For each legacy Content group, mark the physically last selector as its
   `TransformationOwner`. This exactly models the legacy transformation loop's
   last rule for the final Content class. Retain every original Transformation
   cell unchanged, including NA. Do not apply the version-2 agreement rule to a
   migrated payload; record `MigrationProfile = "legacy-v1-exact"` in the
   envelope until a user explicitly resolves any disagreement.
5. Walk condition and ratio frames independently in original order. Assign
   component-local sequential priorities and deterministic RuleIds. Resolve
   each exact Content to the single legacy VariantId created in step 3. A
   downstream Content absent from the content frame makes migration fail rather
   than inventing an unreachable variant.
6. Insert new columns at the canonical positions without reconstructing any
   original data column. Original character, integer, and logical storage
   classes and every regex-bearing value remain byte-equivalent at the value
   level. New IDs are character, Priority is integer, and
   TransformationOwner is logical. Reset only data-frame row names to canonical
   automatic row names after proving the input row names carried no application
   semantics.
7. Preserve the Row Index row and its position. Validate its exact canonical
   values; do not create, remove, merge, or relocate it.
8. Execute both payloads against a migration fixture representing all relevant
   source columns. Compare, in row order, winning Content, effective
   transformation including typed NA, extracted condition, generated Sample,
   numerator, denominator, conflict diagnostics, and errors/warnings. A mismatch
   aborts migration. Then set version 2, required capabilities, the migration
   profile, and persist the complete envelope atomically.

The fixture in step 8 MUST be the importing session's available working data
when present. Export/session upgrades without working data rely on schema test
vectors and defer the first authoritative application until replay verification
is possible; they MUST NOT claim data-specific replay equivalence.

## 6. Execution contract

Content rules are evaluated in ascending content Priority and write both
Content and the winning VariantId. Transformations execute once, using the
winning variant's owner, after content selection. Condition and ratio rules
filter exclusively on that winning VariantId and execute in their own ascending
priority. Existing regex conversion and matching functions receive the stored
values unchanged. Existing condition, sample naming, ratio extraction,
overwrite, warning, and failure behavior otherwise remains unchanged.

Priorities are serialized state. Canonicalization MAY compact a component's
priorities to `1L..n` after a reorder or deletion, but only in current physical
order and in the same atomic edit transaction. It MUST NOT use RuleId, Content,
VariantId, lexical sorting, or regex values to break a tie. Duplicate or invalid
priorities in an imported version-2 payload are an error, not an invitation to
guess order.

## 7. Import, export, edit, session, and deletion invariants

Every path handles the envelope and all three frames as one aggregate:

* import validates capability/version first, then schema, foreign keys,
  identities, priorities, Row Index, ownership, and method-specific fields;
* export writes the same identities, priorities, ownership, version, and
  capabilities and verifies a read-back aggregate before offering the file;
* sorting in a view is presentation-only; an explicit reorder atomically writes
  priorities for the affected component;
* editing preserves RuleId and uses VariantId, never Content, for joins;
* deleting a variant atomically deletes all its content, condition, and ratio
  rows; deleting its owner alone is rejected unless the same transaction elects
  exactly one replacement owner;
* session save and restoration serialize and validate the complete envelope;
  restoration never merges it with rule rows from the current session.

## 8. Exact transfer verification and rollback

The unit of transfer is the **complete grouped payload**: version,
capabilities, migration profile if present, all three frames, their ordered
columns, all rows, RuleIds, VariantIds, priorities, ownership flags, values,
storage classes, typed missing values, and required data-frame attributes.
Debug or provenance fields may be excluded only by an explicit allowlist shared
by writer and reader; rule-bearing fields can never be excluded.

Before mutation, the transfer coordinator obtains one deep authoritative
snapshot through the public rule interface and validates it. It stages and
validates the candidate, rechecks source/run generation, and invokes one quiet
public aggregate loader. After the loader returns success, it reads one new
aggregate snapshot and requires exact structural identity with the staged
payload (`identical()` in the R implementation after canonical automatic row
names). Counts, per-frame hashes, Content labels, or regex equality alone are
insufficient. An optional SHA-256 digest over the canonical encoding may detect
a mismatch early, but cannot replace exact comparison.

Loader error, false return, validation failure, exact mismatch, or stale run
after loading triggers rollback through that same aggregate loader using the
untouched deep snapshot. Rollback succeeds only when a fresh aggregate read is
exactly identical to the snapshot. Until that succeeds, the transaction is
reported failed and the UI MUST NOT claim that previous rules were preserved.
No code may restore individual frames or private reactives. Transfer and
rollback audit records include the before, candidate, observed, and restored
aggregate digests plus the failure stage, but digests are diagnostics rather
than the source of truth.

## 9. Sidecar evaluation

A sidecar that stores IDs, priority, ownership, or capabilities separately from
the rule frames is **rejected for this revision**. Positional pairing cannot
prove synchronization after sorting; Content/regex keys are neither unique nor
stable after editing; and separate writes introduce torn states during import,
export, deletion, session restoration, or rollback.

A future sidecar proposal is admissible only with a single transactional
aggregate API that (1) reads and writes frames plus sidecar under one revision
token, (2) uses immutable RuleId foreign keys rather than row positions or
mutable values, (3) enforces referential integrity and cascading deletion, (4)
round-trips exact identities, priorities, and ownership through import/export,
(5) keeps presentation sorting non-mutating, (6) restores one complete session
snapshot, and (7) proves exact aggregate rollback under injected failure at
every write boundary. The proof requires automated tests for sorting, every
editable field, insertion, cloning, reorder, row and variant deletion,
import/export round trips, session restoration, stale transfers, loader errors,
and rollback mismatches. Meeting those conditions would make the sidecar part
of the same logical payload, not an independently maintained representation;
there is therefore no present advantage over columns in the canonical frames.

## 10. Required conformance tests

Release is blocked until tests cover duplicate Content selectors, multiple
variants sharing Content, duplicate-identical regex rows, conflicting legacy
transformations, typed NA and empty strings, literal backslashes and escaped
slashes, non-ASCII Content, every extraction method, Row Index in first/middle/
last positions, view sorting versus explicit reorder, deletion and owner
replacement, export/import and session round trips, rejection by a version-1
runtime, unknown capabilities, stale-run rollback, and failures after each
component mutation. Golden legacy fixtures MUST assert unchanged physical row
order, original-column classes and values, effective outputs, deterministic IDs,
and exact complete-payload rollback.
