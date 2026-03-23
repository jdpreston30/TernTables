## Resubmission

This is a resubmission addressing the two issues raised by Benjamin Altmann
in the review of v1.6.3 (email dated 2026-03-20). Both points have been
fully resolved. We are submitting as v1.6.4 (rather than a patch to v1.6.3)
because additional improvements have been made since the original submission.

---

## Responses to CRAN review

**Comment 1 — `<<-` modifies the global environment**
Fixed. All uses of `<<-` in `R/ternG.R`, `R/ternD.R`, and `R/ternP.R` have
been eliminated. The accumulator pattern (incrementing counters and building
character vectors across `lapply` iterations) has been replaced with an
explicit `new.env(parent = emptyenv())` environment object that is created
inside the enclosing function and passed by reference to nested closures.
Values are extracted back to local variables before use in the reporting
section. In `ternP.R`, the `dplyr::across()` lambda that used `<<-` to build
a list has been replaced with a plain `for` loop over character column names.

**Comment 2 — `set.seed()` to a specific number inside a function**
Fixed (in v1.6.3.9025, prior to this submission). The bare `set.seed()` call
inside the Monte Carlo Fisher's exact fallback has been replaced with
`withr::with_seed()`, which scopes the seed locally and restores the caller's
RNG state after the call. `withr` has been added to `Imports`.

---

## R CMD check results

(Updated after `devtools::check()` run — to be confirmed before submission)

* macOS Sequoia 15.x, R 4.5.x (local) — 0 errors | 0 warnings | 0 notes

The previously noted win-builder NOTE about possibly misspelled words
('Welch', 'Moertel', 'adjuvant', 'et', 'al') is unchanged — all are correct
and listed in `inst/WORDLIST`.

---

## Additional notes

* The `tern_colon` dataset is derived from `survival::colon`. 'survival'
  is listed in `Suggests` only and is not required at runtime. The
  pre-processed dataset is bundled in `data/tern_colon.rda`.

* Since v1.3.1, five new exported functions have been added: `ternB()` (multi-table
  'Word' export), `ternP()` (data preprocessing with PHI detection),
  `write_cleaning_doc()` (cleaning audit 'Word' document), `ternStyle()`
  (custom tibble Word formatting), and `classify_normality()` (normality
  assessment audit table). All follow the same documentation and example
  standards as the original functions.