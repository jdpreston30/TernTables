# Resolution: ternStyle Conditional Bolding — bold_sig Parameter

**Resolves:** TERNSTYLE_BOLDING_ISSUE.md  
**Implemented in:** TernTables v1.7.1.9007  
**Date:** 2026-04-22

---

## What Was Added

A new `bold_sig` argument has been added to both `ternStyle()` and `word_export()`.

It accepts a named list with three keys:

| Key | Type | Description |
|---|---|---|
| `p_cols` | `character` vector | Column names in your tibble that contain p-value strings |
| `hr_cols` | `character` vector or `NULL` | Paired effect-size column names to also bold (same length/order as `p_cols`). `NULL` to skip. |
| `threshold` | `numeric` | Significance threshold. Default `0.05`. |

**Behaviour:**
- For each cell in a `p_cols` column where the parsed p-value < `threshold`, that **p-value cell** is bolded.
- If `hr_cols` is supplied, the **paired HR/coefficient cell in the same row** is also bolded.
- The **Variable column is never modified** by `bold_sig`. Use the existing `bold_rows` argument to bold entire predictor-header rows (e.g. where p represents an omnibus LRT p-value).
- Handles `"< 0.001"` style strings (strips `<`) and scientific notation.
- Carried through `ternB_meta`, so `ternB()` replays formatting correctly in combined documents.

---

## How to Fix the Cox Regression Table

Replace the current `bold_rows`-only workaround with the combination of `bold_rows` + `bold_sig`:

```r
# Identify which predictor-header rows have significant LRT p (bold entire row)
# e.g. Surgery (row 1) and Molecular Subtype (row 4)
os_lrt_sig_rows <- c(1L, 4L)

ternStyle(
  tbl             = os_cox_tbl,
  filename        = "T8a_OS.docx",
  subheader_rows  = c("Surgery", "Molecular Subtype"),

  # Entire-row bolding for predictor headers with significant LRT p-value
  bold_rows       = os_lrt_sig_rows,

  # Cell-level bolding: bold p and HR cells where individual Wald p < 0.05
  # This correctly bolds "STR/Biopsy" HR + p WITHOUT bolding its Variable cell
  bold_sig        = list(
    p_cols    = c("Uni p", "Multi p"),
    hr_cols   = c("Uni HR (95% CI)", "Multi HR (95% CI)"),
    threshold = 0.05
  ),

  manual_italic_indent = c("GTR", "STR/Biopsy", "IDH-mutant", "IDH-wildtype"),
  open_doc        = FALSE,
  citation        = TRUE
)
```

### What This Produces

| Variable | Uni HR (95% CI) | Uni p | Multi HR (95% CI) | Multi p |
|---|---|---|---|---|
| **Surgery** | | **0.022** | | 0.117 |
| GTR | *Reference* | *-* | *Reference* | *-* |
| STR/Biopsy | **2.63 (1.03–6.70)** | **0.043** | 2.13 (0.77–5.88) | 0.143 |
| **Molecular Subtype** | | **0.008** | | 0.055 |
| IDH-mutant | *Reference* | *-* | *Reference* | *-* |
| IDH-wildtype | **3.11 (1.12–8.60)** | **0.031** | 2.54 (0.88–7.32) | 0.087 |

- **Surgery** row: fully bold (entire row via `bold_rows`) ✅
- **STR/Biopsy** Variable cell: normal weight (italic-indented) ✅
- **STR/Biopsy** Uni HR + Uni p: bold (cell-level via `bold_sig`) ✅
- **Multi p = 0.143**: not bold (above threshold) ✅
- **Molecular Subtype** row: fully bold (entire row via `bold_rows`) ✅

---

## Notes

- `bold_sig` and `bold_rows` are fully compatible — they are additive. `bold_rows` runs last and will override `bold_sig` for any cells in those rows if there is a conflict (which is intentional: predictor header rows should be unconditionally bold).
- If you only have one p-value column (e.g. univariable only), pass a length-1 vector: `p_cols = "p"`, `hr_cols = "HR"`.
- `hr_cols` can be `NULL` if you only want p-value cells bolded without touching the HR column.
- The `threshold` defaults to `0.05` and can be omitted unless you need a different cutoff (e.g. `0.10` for exploratory analyses).
