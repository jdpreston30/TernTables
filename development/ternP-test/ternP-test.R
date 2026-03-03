# ternP() test script
# Run from project root: setwd("/Users/jdp2019/Desktop/Repos/TernTables")
# Reinstall and load the package fresh before each test run.

devtools::document()
devtools::install(pkg = ".", quick = TRUE, upgrade = "never", quiet = TRUE)
library(TernTables)
library(readr)
library(readxl)

# ==============================================================================
# SETUP: generate example data files if not already present
# ==============================================================================

if (!file.exists("inst/extdata/csv/tern_colon_messy.csv")) {
  source("data-raw/tern_colon_extdata.R")
}

# ==============================================================================
# TEST 1: Full cleaning pipeline — tern_colon_messy.csv
#   Expected behaviour:
#     - string_na_converted  = TRUE  (NA / na / Na / unk injected)
#     - blank_rows_removed   = 3     (3 all-NA rows appended)
#     - sparse_rows_flagged  = 3     (3 rows with >50% missing)
#     - case_normalized_vars = "Sex" (fEMALE / MAle injected)
#     - dropped_empty_cols   = "Metastasis" (100% blank named column)
# ==============================================================================

cat("\n\n=== TEST 1: Full cleaning pipeline (CSV) ===\n")

raw_messy <- readr::read_csv(
  "inst/extdata/csv/tern_colon_messy.csv",
  show_col_types = FALSE
)

cat("Raw dimensions:", nrow(raw_messy), "rows x", ncol(raw_messy), "cols\n")

result <- ternP(raw_messy)

cat("\n--- clean_data ---\n")
cat("Dimensions:", nrow(result$clean_data), "rows x", ncol(result$clean_data), "cols\n")
print(head(result$clean_data, 5))

cat("\n--- sparse_rows ---\n")
cat("Sparse rows retained:", nrow(result$sparse_rows), "\n")
print(result$sparse_rows)

cat("\n--- feedback ---\n")
cat("string_na_converted :", result$feedback$string_na_converted,  "\n")
cat("blank_rows_removed  :", result$feedback$blank_rows_removed,   "\n")
cat("sparse_rows_flagged :", result$feedback$sparse_rows_flagged,  "\n")
cat("case_normalized_vars:", result$feedback$case_normalized_vars, "\n")
cat("dropped_empty_cols  :", result$feedback$dropped_empty_cols,   "\n")

# Quick assertions
stopifnot(isTRUE(result$feedback$string_na_converted))
stopifnot(result$feedback$blank_rows_removed == 3)
stopifnot(result$feedback$sparse_rows_flagged == 3)
stopifnot("Sex" %in% result$feedback$case_normalized_vars)
stopifnot("Metastasis" %in% result$feedback$dropped_empty_cols)
cat("\nTEST 1 PASSED\n")

# ==============================================================================
# TEST 2: Full cleaning pipeline — tern_colon_messy.xlsx
#   Same expectations as Test 1 but loading from XLSX.
# ==============================================================================

cat("\n\n=== TEST 2: Full cleaning pipeline (XLSX) ===\n")

raw_messy_xlsx <- readxl::read_excel("inst/extdata/xlsx/tern_colon_messy.xlsx")
result_xlsx    <- ternP(raw_messy_xlsx)

cat("Dimensions:", nrow(result_xlsx$clean_data), "rows x", ncol(result_xlsx$clean_data), "cols\n")
cat("string_na_converted :", result_xlsx$feedback$string_na_converted,  "\n")
cat("blank_rows_removed  :", result_xlsx$feedback$blank_rows_removed,   "\n")
cat("case_normalized_vars:", result_xlsx$feedback$case_normalized_vars, "\n")
cat("dropped_empty_cols  :", result_xlsx$feedback$dropped_empty_cols,   "\n")

stopifnot(isTRUE(result_xlsx$feedback$string_na_converted))
stopifnot("Metastasis" %in% result_xlsx$feedback$dropped_empty_cols)
cat("\nTEST 2 PASSED\n")

# ==============================================================================
# TEST 3: Hard stop — PHI columns (CSV)
#   Expected: stop() with a message mentioning the PHI column names.
# ==============================================================================

cat("\n\n=== TEST 3: PHI hard stop (CSV) ===\n")

raw_phi <- readr::read_csv(
  "inst/extdata/csv/tern_colon_phi.csv",
  show_col_types = FALSE
)

phi_error <- tryCatch(
  ternP(raw_phi),
  error = function(e) e
)

cat("Error caught:", conditionMessage(phi_error), "\n")
stopifnot(inherits(phi_error, "error"))
stopifnot(grepl("PHI", conditionMessage(phi_error), ignore.case = TRUE))
cat("\nTEST 3 PASSED\n")

# ==============================================================================
# TEST 4: Hard stop — PHI columns (XLSX)
# ==============================================================================

cat("\n\n=== TEST 4: PHI hard stop (XLSX) ===\n")

raw_phi_xlsx <- readxl::read_excel("inst/extdata/xlsx/tern_colon_phi.xlsx")

phi_error_xlsx <- tryCatch(
  ternP(raw_phi_xlsx),
  error = function(e) e
)

cat("Error caught:", conditionMessage(phi_error_xlsx), "\n")
stopifnot(inherits(phi_error_xlsx, "error"))
stopifnot(grepl("PHI", conditionMessage(phi_error_xlsx), ignore.case = TRUE))
cat("\nTEST 4 PASSED\n")

# ==============================================================================
# TEST 5: Hard stop — unnamed column with data (CSV)
#   Expected: stop() with a message mentioning unnamed column(s).
# ==============================================================================

cat("\n\n=== TEST 5: Unnamed column hard stop (CSV) ===\n")

raw_unnamed <- readr::read_csv(
  "inst/extdata/csv/tern_colon_unnamed.csv",
  show_col_types = FALSE
)

unnamed_error <- tryCatch(
  ternP(raw_unnamed),
  error = function(e) e
)

cat("Error caught:", conditionMessage(unnamed_error), "\n")
stopifnot(inherits(unnamed_error, "error"))
stopifnot(grepl("unnamed", conditionMessage(unnamed_error), ignore.case = TRUE))
cat("\nTEST 5 PASSED\n")

# ==============================================================================
# TEST 6: Hard stop — unnamed column with data (XLSX)
# ==============================================================================

cat("\n\n=== TEST 6: Unnamed column hard stop (XLSX) ===\n")

raw_unnamed_xlsx <- readxl::read_excel("inst/extdata/xlsx/tern_colon_unnamed.xlsx")

unnamed_error_xlsx <- tryCatch(
  ternP(raw_unnamed_xlsx),
  error = function(e) e
)

cat("Error caught:", conditionMessage(unnamed_error_xlsx), "\n")
stopifnot(inherits(unnamed_error_xlsx, "error"))
stopifnot(grepl("unnamed", conditionMessage(unnamed_error_xlsx), ignore.case = TRUE))
cat("\nTEST 6 PASSED\n")

# ==============================================================================
# TEST 7: Clean data passes through without triggering any feedback
#   Use tern_colon directly — it is already clean.
# ==============================================================================

cat("\n\n=== TEST 7: Clean data — no feedback triggered ===\n")

clean_result <- ternP(tern_colon)

cat("Dimensions:", nrow(clean_result$clean_data), "rows x",
    ncol(clean_result$clean_data), "cols\n")
cat("feedback$string_na_converted :", clean_result$feedback$string_na_converted,  "\n")
cat("feedback$blank_rows_removed  :", clean_result$feedback$blank_rows_removed,   "\n")
cat("feedback$sparse_rows_flagged :", clean_result$feedback$sparse_rows_flagged,  "\n")
cat("feedback$case_normalized_vars:", clean_result$feedback$case_normalized_vars, "\n")
cat("feedback$dropped_empty_cols  :", clean_result$feedback$dropped_empty_cols,   "\n")

stopifnot(is.null(clean_result$feedback$string_na_converted))
stopifnot(is.null(clean_result$feedback$blank_rows_removed))
stopifnot(is.null(clean_result$feedback$case_normalized_vars))
stopifnot(is.null(clean_result$feedback$dropped_empty_cols))
cat("\nTEST 7 PASSED\n")

# ==============================================================================
cat("\n\n=== ALL TESTS PASSED ===\n")
