# New features to integrate into app 3/5/25

## BH FDR Correction: `p_adjust` + `p_adjust_display` (v1.6.3.9003)

**What changed in the package:**
- New `ternG()` parameter: `p_adjust = FALSE`
  - When `TRUE`, applies Benjamini-Hochberg (1995) FDR correction to all omnibus P values after testing
  - The correction pool is one P value per variable — sub-rows of multi-level categoricals are excluded from double-counting; post-hoc pairwise P values are excluded entirely
  - The only supported method is BH (no user choice — standardised for reproducibility)
- New `ternG()` parameter: `p_adjust_display = "fdr_only"` (ignored when `p_adjust = FALSE`)
  - `"fdr_only"` — replaces the P value column, renamed to `"P value (FDR corrected)"`
  - `"both"` — keeps original P values in `"P value"` and adds FDR-corrected values immediately to the right in `"P value (FDR corrected)"`
- `write_methods_doc()` / methods paragraph: when `p_adjust = TRUE`, the paragraph automatically appends:
  *"All reported P values were corrected for multiple comparisons using the Benjamini-Hochberg false discovery rate (FDR) procedure (Benjamini & Hochberg, 1995)."*
  and changes the significance line to *"Statistical significance was defined as FDR-corrected p < 0.05."*
- `ternB_meta` now stores `p_adjust` and `p_adjust_display` — the per-table methods doc in `ternB()` picks these up automatically per table

**App changes needed:**

- **UI — add a toggle:** "Apply FDR correction (Benjamini-Hochberg)" — maps to `p_adjust = TRUE/FALSE`
  - Default: off (`FALSE`)
  - Appropriate label: "Multiple testing correction" or "FDR correction (BH)"
  - Position: near the P value / significance options panel
  - Add a tooltip or italicized helper text beneath the toggle: *"Recommended for exploratory analyses. For confirmatory studies with pre-specified endpoints, correction is generally not applied."*

- **UI — add a radio/select (only shown when FDR toggle is on):** "P value display"
  - Option 1: "FDR-corrected only" → `p_adjust_display = "fdr_only"` *(recommended default when FDR is on)*
  - Option 2: "Original + FDR-corrected" → `p_adjust_display = "both"`

- **R call:** pass both args to `ternG()`:
  ```r
  ternG(
    ...,
    p_adjust         = TRUE,          # or FALSE
    p_adjust_display = "fdr_only"     # or "both"
  )
  ```

- **Downloaded script:** both `p_adjust` and `p_adjust_display` must appear explicitly in the generated script whenever `p_adjust = TRUE`, so the user's download is fully reproducible

- **Methods doc:** no extra work — `write_methods_doc()` is called automatically inside `ternG()` and the FDR sentence is injected automatically when `p_adjust = TRUE`

- **ternB:** no extra work — `ternB_meta` carries `p_adjust` per table; per-table methods paragraphs will correctly reflect FDR status per table

---

## FDR Toggle Helper Text — Pool Size Note (UI only, no package change)

**App changes needed:**

- Update the helper text beneath the FDR toggle to the full version:
  *"Recommended for exploratory analyses. For confirmatory studies with pre-specified endpoints, correction is generally not applied. Note: adjusted P values depend on the number of variables in the table — adding or removing variables will change them."*

---

## "About" Page — Three-Group Table Demo Footnote (UI only, no package change)

**App changes needed:**

- Below the three-group example table in the About section, add the following note:

  > **Note:** In randomized trials, *P* values for baseline characteristics are
  > conventionally omitted — any observed differences between arms are attributable
  > to chance, not systematic bias, because randomization was the assignment
  > mechanism. The baseline variables above (Patient Demographics, Surgical
  > Findings, Tumor Characteristics) are included here alongside the Outcomes
  > section solely to demonstrate multi-group Welch ANOVA and Kruskal-Wallis
  > testing across the full range of variable types. The clinically meaningful
  > comparison in this table is the outcome: Recurrence by treatment arm.
