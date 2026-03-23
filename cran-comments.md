## Resubmission (v1.6.4)

This is a patch resubmission addressing the two policy issues raised by
CRAN reviewer Benjamin Altmann on v1.6.3:

1. **`<<-` (super-assignment)** in `R/ternD.R`, `R/ternG.R`, and
   `R/ternP.R` — All occurrences removed. Counter and tracker variables
   are now accumulated inside a local environment created with
   `new.env(parent = emptyenv())`, which does not alter any parent frame.

2. **`set.seed()` inside a function** in `R/ternG.R` — Replaced with
   `withr::with_seed()`, which restores the RNG state on exit and does
   not permanently modify the global RNG. **withr** has been added to
   `Imports` in `DESCRIPTION`.

No other changes were made relative to v1.6.3.

---

## Test environments

* macOS (local): R 4.4.x
* win-builder: R-devel

## R CMD check results

0 errors | 0 warnings | 0 notes
