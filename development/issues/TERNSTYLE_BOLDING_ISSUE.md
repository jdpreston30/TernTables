# TernTables: ternStyle Conditional Bolding — Issue & Proposed Fix

**Filed by:** HGG-2026 pipeline  
**Date:** 2026-04-22  
**Addressed to:** TernTables package developer / implementing agent

---

## Problem Description

`ternStyle()` currently supports `bold_rows` — an integer vector of row indices to bold. This is row-level bolding only: the entire row (Variable name, HR, CI, and p-value cells) is bolded together. There is no mechanism for cell-level or column-conditional bolding.

This creates two real problems when using `ternStyle` to produce Cox regression tables:

### Problem 1: Significant p-values do not bold

When a predictor has a statistically significant LRT p-value (e.g., p = 0.022 for Surgery), the bolding cannot be applied selectively to that row without also bolding every other column in the row. Since `ternStyle` receives pre-formatted strings (not numeric values), it has no way to evaluate significance internally.

### Problem 2: Non-significant p-values bold spuriously (or you must choose)

The workaround of using `bold_rows` to bold only the rows where p < 0.05 partially works — but it bolds the *entire* row. For predictor header rows (e.g., "Surgery", "Molecular Subtype") this looks acceptable. For sub-level rows (e.g., "STR/Biopsy", "Female") it is wrong: bolding the Variable name cell of a sub-level row is non-standard and visually confusing.

---

## Concrete Example

This pipeline produces Cox regression tables (T8a OS, T8b PFS) with the following tibble structure passed to `ternStyle`:

```
Variable                Uni HR (95% CI)     Uni p   Multi HR (95% CI)   Multi p
Surgery                 ""                  0.022   ""                  0.117     ← bold entire row? OK
GTR                     Reference           -       Reference           -
STR/Biopsy              2.63 (1.03–6.70)    0.043   2.13 (0.77–5.88)    0.143     ← bold this? WRONG
```

Desired output:
- "Surgery" row: **Variable** bold (p < 0.05 at LRT level), **Uni p** bold
- "STR/Biopsy" row: Variable stays italic-indented normal weight, **Uni HR** and **Uni p** bold (p = 0.043)

Neither is achievable with the current `bold_rows` integer vector.

---

## Proposed Fix

### Approach: `bold_sig` argument with column-level p-value evaluation

Add a new argument to `ternStyle()`:

```r
bold_sig = NULL
```

Where `bold_sig` accepts a named list specifying which columns contain numeric p-values or formatted p-value strings, and what threshold to bold at:

```r
bold_sig = list(
  p_cols     = c("Uni p", "Multi p"),   # column names containing p-values
  hr_cols    = c("Uni HR (95% CI)", "Multi HR (95% CI)"),  # paired HR cols to also bold
  threshold  = 0.05
)
```

Internally, `ternStyle` would:
1. Parse the p-value strings back to numeric (strip `<`, handle `< 0.001`, etc.) — this logic already exists in `val_p_format()`
2. For each cell where the parsed p < threshold: bold that cell AND its paired HR cell
3. For predictor header rows (where p-value represents overall LRT): bold the Variable cell and the p cell only — not the HR cell (which is blank)

### Why this mirrors ternG

`ternG` already identifies significant cells internally during table construction and applies bold formatting at the flextable level before export. The information needed to replicate this in `ternStyle` is present — it's just been discarded by the time strings reach `ternStyle`. The `bold_sig` argument would allow the caller to re-supply that significance information via the p-value columns themselves, letting `ternStyle` recover it and apply cell-level formatting identically to what `ternG` produces.

### Alternative simpler approach (if cell-level is too complex)

A minimal version: add `bold_sig_cols` as a character vector of column names that contain p-values, and bold only those cells (and their paired HR cells) where the parsed value < 0.05. Row-level bolding for the Variable column would still require the caller to pass `bold_rows` as today — but at least the HR and p cells would format correctly at the cell level without improperly bolding Variable names for sub-level rows.

---

## Current Workaround in This Repo

`bold_rows = os_sig_rows` where `os_sig_rows` contains only the **predictor header row indices** where LRT p < 0.05 — not sub-level rows. This means:

- Significant predictor rows (Surgery, Molecular Subtype) are fully bolded ✅
- Significant sub-level rows (STR/Biopsy Wald p = 0.043) are NOT bolded ⚠️
- Non-significant predictor rows are not bolded ✅

This is the best achievable within current `ternStyle` constraints, but it means individual-level significant HRs go unbolded, which is the standard in ternG output.
