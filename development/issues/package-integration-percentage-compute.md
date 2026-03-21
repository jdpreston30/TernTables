# Package Integration: percentage_compute — row vs column categorical percentages

## What changed in the package

- **Function modified:** `ternG()`
- **New argument:** `percentage_compute`
- **Type:** `character`, one of `"column"` or `"row"`
- **Default:** `"column"` (preserves all existing behavior — no breaking change)

### What it controls
Controls the denominator used when computing percentages for **categorical variables** (both binary and multinomial).

- `"column"` (default): each cell count is divided by the **group (column) total**. Percentages describe the composition of each group. This is the standard Table 1 interpretation.
  - Example: "49% of the Recurrence group is Female"
  - Total column: percentage of the overall sample (e.g. 48%)

- `"row"`: each cell count is divided by the **row total** (number of subjects with that category level across all groups). Percentages describe how each category level distributes across groups.
  - Example: "51% of Females had Recurrence"
  - Total column: always 100% for every level

Applies to:
- Binary variables (Y/N format, single-row display)
- Multinomial variables (hierarchical sub-row display)
- Both 2-group and 3+ group comparisons

Does **not** affect:
- Continuous or ordinal variables (mean ± SD, median [IQR])
- The Missing row (always uses column denominator = group N)
- P values, OR, or any other statistic

---

## Web app integration required

### Likely UI changes
- **Location:** Step 2 → Options panel → Advanced section (alongside `show_test`, `print_normality`, etc.)
- **Control type:** Toggle / radio with two options: `"Column %"` (default) and `"Row %"`
- **Label suggestion:** "Categorical percentage basis"
- **Sublabel:** "Column %: % within each group (standard Table 1). Row %: % within each category level."
- Should be visible for both grouped (ternG) analysis types; not relevant for ternD (single cohort, no groups to compare across)
- Can be placed in Advanced — most users will never need it

### Plumber API changes (`R/plumber.R`)
- **Endpoint:** `POST /api/analyze`
- Read `percentage_compute` from `req$body$percentage_compute`, defaulting to `"column"` if absent
- Pass to `ternG(..., percentage_compute = percentage_compute)`
- Add to the preview `ternG()` call as well (so the preview reflects the selection)
- Validate: must be `"column"` or `"row"`; reject with 400 if anything else

### Frontend API changes (`client/src/api/client.js`)
- Add `percentageCompute: options.percentageCompute ?? 'column'` to the POST body in `analyzeData()`

### State changes (`App.jsx` / `Step2.jsx` / `Options.jsx`)
- New state variable: `percentageCompute`, initialized to `'column'`
- Passed down to `Options.jsx` as a prop
- `Options.jsx`: render a two-option radio or segmented toggle in the Advanced section

### Reproducible R script (`R/helpers/rscript.R`)
- Include `percentage_compute` in the generated `_reproduce.R` script **only if it differs from the default** (i.e., only output the line if `percentage_compute == "row"`)

---

## Notes / open questions

- `ternD` does **not** need this parameter — it is a single-cohort descriptive table with no group columns to compute row percentages across.
- The `ternB` replay loop does not need updating — `percentage_compute` affects the computed cell values stored in `meta$tbl`, not the Word rendering. The baked values are replayed correctly via `meta$tbl`.
- "Row %" is an uncommon choice for clinical Table 1s; consider adding a tooltip in the UI explaining when it's appropriate (epidemiological framing where the categorical variable is the exposure and the group variable is the outcome).
