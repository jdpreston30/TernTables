## R CMD check results

-- To be completed after running `devtools::check()` --

0 errors | 0 warnings | 0 notes  (expected after final check)

## Test environments

* macOS (local), R 4.x
* win-builder (release and devel) — pending
* R-hub (ubuntu-latest) — pending

## Notes

* This is the first CRAN submission of TernTables.

* The `tern_colon` dataset is derived from `survival::colon`, which
  ships with every R installation. `survival` is listed in `Suggests` only;
  it is not required at runtime. The pre-processed dataset is bundled in
  `data/tern_colon.rda` so users do not need `survival` installed.

* The `Outputs/` directory and `TernTables_methods.docx` at the repository
  root are developer artefacts excluded from the package tarball via
  `.Rbuildignore`.

* All exported functions include `\examples{}` sections. Examples that write
  files to disk use `\dontrun{}` or write to `tempdir()` so they do not
  produce side-effects during `R CMD check`.
