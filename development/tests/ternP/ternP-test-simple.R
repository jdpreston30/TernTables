# ternP() — simple smoke test for print method and write_cleaning_doc()
# Run from project root: setwd("/Users/jdp2019/Desktop/Repos/TernTables")
# Safe to run in the same R session as ternP-test.R — uses load_all() instead
# of install(), so no restart required between runs.

devtools::load_all(".")
library(readr)

if (!file.exists("inst/extdata/csv/tern_colon_messy.csv")) {
  source("data-raw/tern_colon_extdata.R")
}

out_dir <- "development/tests/ternP/tables"

# ==============================================================================
# SCENARIO 1: messy data — all five transformations should fire in the printout
# ==============================================================================

cat("\n\n====  SCENARIO 1: Messy data  ====\n\n")

raw_messy <- readr::read_csv(
  "inst/extdata/csv/tern_colon_messy.csv",
  show_col_types = FALSE
)

# Feedback prints automatically at the moment ternP() runs — no extra line needed.
# Expected printout:
#   ── ternP Preprocessing Summary ──────────────────────────────────────────────
#   i String NA values ("NA", "na", "Na", "unk") converted to `NA`.
#   i 1 empty column dropped: "Metastasis".
#   i 3 completely blank rows removed.
#   i Capitalization normalized in 1 column: "Sex".
#   ! 3 sparse rows flagged (>50% missing, retained in clean_data).
#   ─────────────────────────────────────────────────────────────────────────────
#   i Cleaned data: 932 rows × 12 columns.

result_messy <- ternP(raw_messy)

# Write cleaning audit doc — should contain one paragraph per transformation.
write_cleaning_doc(
  result   = result_messy,
  filename = file.path(out_dir, "cleaning_summary_messy.docx")
)

# ==============================================================================
# SCENARIO 2: clean data — no transformations, minimal printout
# ==============================================================================

cat("\n\n====  SCENARIO 2: Clean data (tern_colon)  ====\n\n")

data(tern_colon)

# Expected printout:
#   ── ternP Preprocessing Summary ──────────────────────────────────────────────
#   ✔ No transformations required. Data passed through unchanged.
#   ─────────────────────────────────────────────────────────────────────────────
#   i Cleaned data: 929 rows × 14 columns.

result_clean <- ternP(tern_colon)

# Write cleaning audit doc — should contain only the "no transformations" sentence.
write_cleaning_doc(
  result   = result_clean,
  filename = file.path(out_dir, "cleaning_summary_clean.docx")
)

# Typing the object later re-displays the summary on demand:
# result_clean



