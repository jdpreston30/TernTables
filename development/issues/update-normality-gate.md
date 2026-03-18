# Package Integration: ROBUST Normality Gate — Kurtosis Addition + External Module

Package version: 1.6.3.9023

---

## What changed in the package

### 1. Gate 2 now includes excess kurtosis

The ROBUST four-gate normality decision previously evaluated only skewness at
Gate 2. It now evaluates both skewness and excess kurtosis:

**Before:**
> Gate 2: |skewness| > 2 in any group → non-parametric

**After:**
> Gate 2: |skewness| > 2 **or** |excess kurtosis| > 7 in any group → non-parametric

The full four-gate algorithm is now:

| Gate | Condition | Routes to |
|---|---|---|
| 1 | Any group n < 3 | Non-parametric |
| 2 | \|skewness\| > 2 **or** \|excess kurtosis\| > 7 in any group | Non-parametric |
| 3 | All groups n ≥ 30 | Parametric (CLT) |
| 4 | Shapiro-Wilk p > 0.05 in all groups (valid 3 ≤ n ≤ 5,000) | Parametric / Non-parametric |

Both statistics use all-population-moments formulas (divided by n, not n−1),
which are internally consistent and appropriate for a binary gate decision at
clinical sample sizes.

### 2. Algorithm extracted to a shared external module

Previously, the four-gate logic — including `calc_skewness()` and
`calc_kurtosis()` as local closures — was duplicated verbatim inside both
`ternG.R` and `ternD.R`. This created a single-source-of-truth problem: any
future algorithm change required editing two separate locations.

The algorithm has been extracted to **`R/utils_normality.R`**, which defines
three package-internal (dot-prefixed, non-exported) functions:

- **`.calc_skewness(x)`** — population-moments skewness of a numeric vector
- **`.calc_kurtosis(x)`** — population-moments excess kurtosis of a numeric vector
- **`.robust_normality(group_list)`** — the full four-gate decision

`.robust_normality()` accepts a **named list of numeric vectors** (one per
group). `ternG` passes its per-group vectors directly; `ternD` wraps its single
variable vector in `list(x)` — making ternD a degenerate one-group case of the
same logic with no mode-switching argument needed.

Return value of `.robust_normality()`:
```r
list(
  is_normal  = TRUE/FALSE,   # route to parametric or non-parametric
  sw_pvalues = named numeric vector or NULL,  # Shapiro-Wilk p-values (Gate 4 only)
  gate       = integer 1–4   # which gate made the decision
)
```

`sw_pvalues` is `NULL` at Gates 1–3 (decision made before Shapiro-Wilk) and a
named vector at Gate 4, matching the `"SW_p_<group_name>"` convention used by
`ternG`'s `print_normality` column.

### 3. Files modified

| File | Change |
|---|---|
| `R/utils_normality.R` | **New file.** Defines `.calc_skewness`, `.calc_kurtosis`, `.robust_normality` |
| `R/ternG.R` | ROBUST block replaced with 5-line call to `.robust_normality()` |
| `R/ternD.R` | ROBUST block replaced with 6-line call to `.robust_normality()` |
| `R/write_methods_doc.R` | All four Gate 2 description strings updated to include kurtosis |
| `R/TernTables-package.R` | Package-level help page Gate 2 description updated |
| `README.md` | ROBUST description + four-gate table added |
| `vignettes/getting-started.Rmd` | Three locations updated; four-gate table added |

### 4. Validation

A capture/verify test script was written and run before and after the
refactoring: `development/tests/test-normality-extraction.R`.

Results: **30/30 tests passed**, including:
- Byte-identical `identical()` comparison of ternD, ternG 2-group, and ternG
  3-group output tibbles against pre-change references
- Unit tests covering all four gates in single-vector and group-list mode
- Edge cases: constant vector, all-NA vector, n=30 CLT boundary, n=29

No behavioral change was introduced by the extraction — only Gate 2's kurtosis
addition changes algorithm output compared to the version prior to this session.

---

## Web app integration required

### Likely UI changes
- **None required.** The ROBUST algorithm is applied automatically under the
  hood. No new user-facing parameter was added. The user does not select or
  configure the gate thresholds.

### About page / documentation changes (likely needed)

The web app's About page (or equivalent "How it works" / methodology section)
currently describes the ROBUST normality gating algorithm. The Gate 2
description needs to be updated to reflect the kurtosis addition:

**Find text like:**
> "absolute skewness exceeding 2.0"

or

> "extreme skewness (|skew| > 2)"

**Update to:**
> "absolute skewness exceeding 2.0 or excess kurtosis exceeding 7.0"

This affects any static about/methodology text in the frontend that describes
the normality algorithm — typically rendered as a bullet list or table. The
auto-generated `TernTables_methods.docx` (produced by the R package) is already
updated and will be correct when the server pulls the new package version.

### Smoke test after server package update

When deploying the updated package to the server (`./scripts/deploy.sh --update-pkg`),
do a quick sanity check that the externalized module loads and runs correctly
in the server's R environment:

- Run a test analysis through the app (both grouped and descriptive) and confirm
  the normality column behaves as expected — variables with large sample sizes
  should route parametric, visibly skewed/heavy-tailed variables should route
  non-parametric.
- Check the server R logs for any `could not find function ".robust_normality"`
  or similar namespace errors, which would indicate the new `utils_normality.R`
  file was not included in the installed package on the server.
- The functions are dot-prefixed internal helpers — they are **not** exported
  and will not appear in `ls("package:TernTables")`, which is expected and correct.
  The concern is only whether the package installed cleanly and `ternG()`/`ternD()`
  continue to produce correct output.

This is expected to be a non-issue since `devtools::check()` passed 0 errors /
0 warnings locally, but worth confirming on first deploy.

### Plumber API changes (`R/plumber.R`)
- **None.** The normality routing is internal to `ternG()`/`ternD()`. No new
  parameter is read from `req$body`.

### Frontend API changes (`client/src/api/client.js`)
- **None.**

### State changes (`App.jsx` / `Step2.jsx`)
- **None.**

### Reproducible R script (`R/helpers/rscript.R`)
- **No change needed.** `consider_normality = "ROBUST"` is and remains the
  default; the generated `_reproduce.R` script does not need to emit it unless
  overridden. The kurtosis change is transparent to the user's script.

---

## Notes

- The `R/utils_normality.R` module is not exported and is not user-accessible.
  If a future feature exposes normality gating as a standalone function (e.g.,
  `classify_normality()` for single variables), it would be a thin wrapper
  around `.robust_normality()`.
- The kurtosis threshold of 7 (excess) is a conservative choice calibrated for
  clinical data. It targets genuinely heavy-tailed distributions (e.g.,
  leptokurtic counts, long-tailed lab values) while leaving normally-shaped
  distributions unaffected. It was chosen alongside the existing skewness
  threshold of 2, which is an established guideline in biostatistical practice.
