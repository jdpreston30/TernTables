# Feature: Test-Selection Table Terminology Update

## What changed in the package

Documentation-only change — no code or statistical logic was modified.

### Affected files
- `README.md`
- `vignettes/getting-started.Rmd`
- `R/TernTables-package.R` (`@section Statistical tests applied`)
- `development/review/statistical-review/statistical-review-request.Rmd`

### What changed

The test-selection table was restructured based on a recommendation from the
statistical reviewer (Maria Aslam), citing Bensken WP, Ho VP, Pieracci FM
(Surgical Infections, 2021): because continuous and ordinal variables use the
exact same set of tests, collapsing them into a single "Numeric" category with
a normality sub-distinction is statistically cleaner and avoids implying a
meaningful algorithmic difference that doesn't exist.

**Old table structure:**

| Variable type | 2 groups | 3+ groups | Post-hoc |
|---|---|---|---|
| Continuous, normal | Welch's t-test | Welch ANOVA | Games-Howell |
| Continuous, non-normal | Wilcoxon rank-sum | Kruskal-Wallis | Dunn's + Holm |
| Binary / Categorical | Fisher's exact or Chi-squared | Fisher's exact or Chi-squared | — |
| Ordinal (`force_ordinal`) | Wilcoxon rank-sum | Kruskal-Wallis | Dunn's + Holm |

**New table structure:**

| Variable type | 2 groups | 3+ groups | Post-hoc |
|---|---|---|---|
| Binary / Categorical | Fisher's exact or Chi-squared | Fisher's exact or Chi-squared | — |
| Numeric, normal | Welch's t-test | Welch ANOVA | Games-Howell |
| Numeric, non-normal† | Wilcoxon rank-sum | Kruskal-Wallis | Dunn's + Holm |

†Includes variables designated as ordinal via `force_ordinal`, which bypass
normality testing and always use non-parametric methods.

**Specific changes:**
- "Continuous, normal" → "Numeric, normal"
- "Continuous, non-normal" → "Numeric, non-normal†"
- "Ordinal (`force_ordinal`)" row removed; collapsed into the Numeric,
  non-normal row via the † footnote
- Binary / Categorical moved to the top row (from third)
- `TernTables-package.R` `@section Statistical tests applied` rewritten to
  match: separate items for Numeric, normal and Numeric, non-normal by group
  count; Ordinal (forced) item removed; unified under non-normal description

---

## Web app integration required

### Likely UI changes

The web app has several places where it may surface variable-type language to
the user (e.g. the Normality Preview step, tooltips, the generated R script,
and the methods document). All instances of "Continuous" and "Ordinal" used
to describe variable classification should be reviewed and updated to match the
new "Numeric" terminology.

Specific places to check:

1. **Normality Preview step** — The table shown to users classifying each
   variable as "Normal" or "Non-normal" likely uses the word "Continuous" in
   its column or row headers. Update to "Numeric."

2. **Variable type display** — Any place in the Step 2 UI where detected
   variable type is shown to the user (e.g. a badge, tooltip, or label saying
   "Continuous" or "Ordinal") should be updated to "Numeric."

3. **Methods document** — `write_methods_doc.R` generates the boilerplate
   methods paragraph. If it uses "continuous" or "ordinal" in its written
   prose, those passages should be reviewed for consistency. (Note:
   `write_methods_doc.R` was not updated in this change — check whether its
   prose needs updating separately.)

4. **Reproducible R script** — `R/helpers/rscript.R` in the service repo
   generates the `_reproduce.R` download. If it includes any explanatory
   comments about variable types, update "continuous"/"ordinal" to "numeric."

### Plumber API changes (R/plumber.R)

No API parameter changes. The underlying function arguments (`force_ordinal`,
`consider_normality`, etc.) are unchanged. Only surface-level display text
needs updating.

### Frontend API changes (client/src/api/client.js)

No changes needed — no new parameters introduced.

### State changes (App.jsx / Step2.jsx)

No state changes needed. If the Normality Preview component renders a variable
type label from a data field returned by `/api/classify-normality`, check
whether that field value is "Continuous" — if so, the service-side label
should be updated to "Numeric" to match the new terminology.

---

## Notes / open questions

- `write_methods_doc.R` prose was **not** updated in this change. The methods
  paragraph it generates may still say "continuous" in its written text. This
  should be reviewed as a follow-up — either leave as-is (methods language is
  more verbose and "continuous" is still correct English) or update for full
  consistency.
- The `force_ordinal` parameter name itself is unchanged and does not need to
  change — it is an argument name, not displayed terminology.
- The statistical review document
  (`development/review/statistical-review/statistical-review-request.Rmd`) was
  also updated for archival consistency, but it is not user-facing.
