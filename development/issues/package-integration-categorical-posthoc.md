# Package Integration: categorical_posthoc parameter

## What changed in the package

- **Function modified**: `ternG()`
- **New argument**: `categorical_posthoc = FALSE` (logical, opt-in, default off)
- **What it controls**: When `TRUE` and `group_var` has 3+ levels, computes Haberman's adjusted standardized residuals from the global contingency table for each categorical variable following a significant omnibus test (p < 0.05). Cells whose residual exceeds ±1.96 receive an asterisk (*) annotation, indicating a significant deviation from expected frequencies at α = 0.05. Silently ignored for two-group comparisons.
- **Output side effects**:
  - Cell values in group columns may gain a trailing `*` (e.g. `"149 (47%)*"`)
  - `ternB_meta$posthoc_footnote` is populated with a sentence describing the asterisk convention — only when ≥1 categorical variable actually triggered residuals
  - The auto-generated methods doc (`write_methods_doc`) paragraph is updated to describe the residuals approach when `categorical_posthoc = TRUE`
- **No change to output structure** (same column shape, tibble dimensions unchanged)

---

## Web app integration required

### Likely UI changes

- **Where**: Step 2 → Options panel → Advanced section (same area as `post_hoc` toggle)
- **Control type**: Checkbox / toggle, labeled something like "Categorical post-hoc (adjusted residuals)"
- **Default**: `FALSE` (unchecked)
- **Visibility**: Only show when analysis type is Grouped AND the grouping variable has 3+ levels (same condition as the `post_hoc` toggle). If fewer than 3 groups, hide or grey out.
- **Help text suggestion**: "When enabled, cells that significantly deviate from expected frequencies (adjusted standardized residual > ±1.96) are marked with an asterisk (*) following a significant omnibus test. No additional p-value correction is required."

### Plumber API changes (R/plumber.R)

- **Endpoint**: `POST /api/analyze` (the main `ternG()` call)
- **New field**: Read `categorical_posthoc` from `req$body` as logical, default `FALSE`
- **Pass to**:
  - The primary `ternG()` call (download)
  - The citation-free `ternG()` preview call (so the preview table also shows asterisks)
- **Reproducible script** (`rscript.R`): Include `categorical_posthoc` in the generated `_reproduce.R` only when `TRUE` (omit when `FALSE` to keep the script clean)

### Frontend API changes (client/src/api/client.js)

- Add `categorical_posthoc: categoricalPosthoc` to the POST body in `analyzeData()` (or equivalent analysis call)

### State changes (App.jsx / Step2.jsx / OptionsDescriptive is not relevant — this is ternG only)

- New state variable: `categoricalPosthoc` (boolean, default `false`) in Step 2 Options component (grouped analysis path only)
- Wire to the Options panel Advanced section alongside the existing `postHoc` toggle
- Only render when `nGroups >= 3` (same guard as `postHoc`)

---

## Notes / open questions

- `categorical_posthoc` is independent of `post_hoc` — users can enable one, both, or neither. When both are `TRUE`, the footnote in the Word output combines both descriptions in a single sentence block.
- The asterisk annotation is purely visual — it does not add new columns or change the tibble shape, so no backend parsing changes are needed beyond passing the flag.
- `show_p = FALSE` suppresses `categorical_posthoc` automatically (same as `post_hoc`), so no special guard is needed in the API for that case.
- Consider whether the Options panel should group `post_hoc` and `categorical_posthoc` together under a shared "Post-hoc comparisons" sub-heading for clarity.
