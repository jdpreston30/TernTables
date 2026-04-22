# TernTables bold_sig Follow-Up: HR Bolding Not Firing

## Context

We are using `ternStyle()` with `bold_sig` in a Cox regression results table (T8a/T8b). The table has five columns:

| Variable | HR (95% CI) | P value | Adjusted HR (95% CI) | Adjusted P |
|----------|-------------|---------|----------------------|------------|

We passed:

```r
bold_sig = list(
  p_cols    = c("P value", "Adjusted P"),
  hr_cols   = c("HR (95% CI)", "Adjusted HR (95% CI)"),
  threshold = 0.05
)
```

---

## What Worked

`bold_sig` successfully bolded the **P value** and **Adjusted P** cells when they were below 0.05. The p-column bolding logic is functioning correctly.

---

## What Failed

The paired **HR (95% CI)** cell was **not bolded** even when the corresponding P value was significant and the confidence interval was entirely above 1 (i.e., did not cross the null).

### Test Case (T8a — Overall Survival)

| Variable | HR (95% CI) | P value | Adjusted HR (95% CI) | Adjusted P |
|----------|-------------|---------|----------------------|------------|
| STR/Biopsy | 2.63 [1.03–6.70] | **0.043** | 2.13 [0.77–5.88] | 0.143 |

- P value = 0.043 → correctly bolded ✅
- HR (95% CI) = 2.63 [1.03–6.70], CI entirely above 1 → **not bolded** ❌

---

## Root Cause Discovered (During Debugging)

After inspecting `body(TernTables:::word_export)`, we found that `word_export` renames a column literally called `"P"` to `"P value"` internally:

```r
else if (new_colnames[i] == "P") {
    new_colnames[i] <- "P value"
}
```

This rename happens **before** `bold_sig` iterates over `p_cols`. So when we passed `p_cols = c("P", ...)`, `bold_sig` searched for `"P"` in `colnames(modified_tbl)`, found nothing, and skipped — meaning the paired HR column bolding never fired.

We worked around this by passing `"P value"` instead of `"P"` in `p_cols`. This restored P-cell bolding.

However, even after that fix, the **HR cell is still not being bolded** when its CI does not cross 1 — suggesting the paired HR bolding branch in `bold_sig` is still not firing despite the P fix.

---

## Expected Behavior

When `bold_sig` identifies a row as significant via the P column, the corresponding `hr_cols` cell on that **same row** should also be bolded — regardless of CI bounds. The CI-crossing-1 check should be independent of (or additive to) the P-pairing logic.

We had understood from previous communication that this was implemented in v1.7.1.9006. We are running that version. Please advise whether there is a known issue or if additional configuration is needed.

---

## Version

- TernTables: v1.7.1.9006 (installed via `remotes::install_github("jdpreston30/TernTables")`)
- R: 4.5.1
- Platform: macOS x86_64-apple-darwin23.6.0
