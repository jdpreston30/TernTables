## R CMD check results

0 errors | 0 warnings | 0 notes

Tested on:
* macOS Sonoma 14.5, R 4.5.1 (local) — 0 errors | 0 warnings | 0 notes
* win-builder (R-devel, 2026-02-26 r89489) — 0 errors | 0 warnings | 1 note

The single win-builder NOTE:
* "Possibly misspelled words in DESCRIPTION: Welch (30:49)" — "Welch" is
  the proper name of the statistician (Welch's t-test) and is correctly
  spelled. It is listed in inst/WORDLIST.

## Notes

* This is the first CRAN submission of TernTables.

* The `tern_colon` dataset is derived from `survival::colon`, which
  ships with every R installation. `survival` is listed in `Suggests` only;
  it is not required at runtime. The pre-processed dataset is bundled in
  `data/tern_colon.rda` so users do not need `survival` installed.

* All exported functions include `\examples{}` sections. Examples that write
  files to disk use `\dontrun{}` or pass `methods_doc = FALSE` so they do
  not produce side-effects during `R CMD check`.
