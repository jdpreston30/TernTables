## Resubmission

This is a resubmission. The original submission was reviewed as v1.3.1;
this version is v1.6.3. Both CRAN review points have been addressed below.
The fixes have been applied consistently across the original code and all new
code added since v1.3.1.

---

## Responses to CRAN review

**Comment 1 — Software names in single quotes**
Fixed. A full audit of all software and package names in the Description
field was performed. All such names are now in single quotes: 'Word',
'Excel', 'tibble', 'officer', 'flextable', 'writexl', and 'rstatix'.

**Comment 2 — \dontrun{} replaced with \donttest{}**
Fixed. All `\dontrun{}` blocks across the package have been replaced with
`\donttest{}`. These examples write files to `tempdir()` and require no
missing software or API keys. `\donttest{}` is used rather than unwrapping
because the 'Word' export operations (via 'officer' and 'flextable') may
exceed 5 seconds on slower check machines.

---

## R CMD check results

Tested on:
* macOS Sonoma 14.5, R 4.5.1 (local) — 0 errors | 0 warnings | 0 notes
* win-builder (R-devel) — 0 errors | 0 warnings | 1 note

The single win-builder NOTE:
* "Possibly misspelled words in DESCRIPTION: Moertel (37:5), Welch (30:49,
  31:5), adjuvant (36:23), al (37:16), et (37:13)" — all are correct.
  'Welch' and 'Moertel' are proper names (Welch's t-test; Moertel et al.
  1990 NEJM landmark colon cancer trial). 'adjuvant', 'et', and 'al' are
  standard academic English from the citation text. All five are listed in
  inst/WORDLIST.

---

## Additional notes

* The `tern_colon` dataset is derived from `survival::colon`. 'survival'
  is listed in `Suggests` only and is not required at runtime. The
  pre-processed dataset is bundled in `data/tern_colon.rda`.

* Since v1.3.1, three new exported functions have been added: `ternB()` (multi-table
  'Word' export), `ternP()` (data preprocessing with PHI detection), and
  `write_cleaning_doc()` (cleaning audit 'Word' document). All three follow the
  same documentation and example standards as the original functions.