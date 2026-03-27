## New submission — v1.7.0

This is a new feature release building on v1.6.4, which was accepted to CRAN
on 2026-03-26. Both reviewer comments raised during the v1.6.3 review cycle
were fully resolved in the accepted v1.6.4. This submission introduces no
breaking changes.

---

## Summary of changes since v1.6.4

**New exported functions:**

* `ternStyle()`: applies full TernTables Word formatting to any user-supplied
  tibble; output carries `ternB_meta` for direct use in `ternB()`.
* `classify_normality()`: exposes the internal normality routing algorithm as
  a tidy audit tibble, for use in manuscript methods reporting.

**New parameters** (across `ternG`, `ternD`, `ternB`, `ternStyle`,
`word_export`, `write_methods_doc`, `write_cleaning_doc`):
`font_family`, `plain_header`, `show_p`, `show_missing`, `force_continuous`,
`force_normal`, `zero_to_dash`, `percentage_compute`, `round_decimal`,
`p_adjust`, `p_adjust_display`, `citation`, `open_doc`,
`variable_footnote`, `abbreviation_footnote`, `index_style`.

**Statistical improvements:**
* Excess kurtosis added as Gate 2 criterion in the four-gate ROBUST normality
  algorithm; decision logic extracted to `utils_normality.R`.
* CLD center-based letter re-mapping removed; letters now follow standard
  `multcompLetters()` alphabetical ordering.

**Bug fixes:** CLD dependency resolution, Word blank-page and citation-bleed
bugs in `ternB()`, line-break header crash, name-cleaning false positives,
wide-table page overflow.

---

## R CMD check results

* macOS Sonoma 14.3, R 4.5.1 aarch64-apple-darwin23.6.0 (local) — 0 errors | 0 warnings | 1 note
* Windows win-builder (to be run immediately prior to submission)

**NOTE: unable to verify current time**
Standard message; no action required. Occurs in check environments without
reliable network time access.

The win-builder NOTE about possibly misspelled words ('Welch', 'Moertel',
'adjuvant', 'et', 'al') is expected and unchanged — all are correct and
listed in `inst/WORDLIST`.

---

## Additional notes

* The `tern_colon` dataset is derived from `survival::colon`. 'survival'
  is listed in `Suggests` only and is not required at runtime. The
  pre-processed dataset is bundled in `data/tern_colon.rda`.

* Seven exported functions are now available: `ternG()`, `ternD()`, `ternB()`,
  `ternP()`, `ternStyle()`, `classify_normality()`, `write_methods_doc()`,
  `write_cleaning_doc()`, `word_export()`, `val_format()`, `val_p_format()`.
  All follow the same documentation and example standards established in v1.3.1.