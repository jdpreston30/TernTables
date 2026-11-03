## Submission — v1.8.0

This release removes the dependency on epitools, which CRAN has scheduled for
archival on 2026-11-30. The Wald odds ratio previously taken from
`epitools::oddsratio()` is now computed internally with the same formula, so
reported values are unchanged.

Other changes since v1.7.2 are listed in NEWS.md. In brief:

* New exported functions `tern_stats()` and `tern_estimates()`
* New parameters: `plain_tibble` in `ternG()` / `ternD()`; `spanner` and
  `col1_spanner_pos` in `ternStyle()` / `word_export()`
* Bug fix: `force_continuous` in `ternG()` was ignored for variables that
  failed normality assessment. Output changes only for calls that set it.

---

## R CMD check results

* macOS Tahoe 26.6.2, R 4.5.1 aarch64-apple-darwin20 (local) —
  0 errors | 0 warnings | 1 note ("unable to verify current time", from the
  local clock check, not the package)
* win-builder, R-devel (2026-11-02 r90827 ucrt), Windows Server 2022 x64 —
  0 errors | 0 warnings | 0 notes

---

## Downstream dependencies

No packages on CRAN currently depend on TernTables.
