# test-row-percentages.R
# Validates percentage_compute = "row" in ternG().
# Run from package root: source("development/tests/row_percentages/test-row-percentages.R")

suppressPackageStartupMessages({
  devtools::load_all()
  library(dplyr)
})

data(tern_colon)

pass <- 0
fail <- 0

chk <- function(label, condition) {
  if (isTRUE(condition)) {
    cat("  PASS:", label, "\n")
    pass <<- pass + 1
  } else {
    cat("  FAIL:", label, "\n")
    fail <<- fail + 1
  }
}

cat("\n-- Test 1: 2-group binary (Recurrence x Colonic_Obstruction) --\n")
t1 <- ternG(
  data               = tern_colon,
  vars               = c("Colonic_Obstruction"),
  group_var          = "Recurrence",
  percentage_compute = "row",
  open_doc           = FALSE,
  citation           = FALSE
)
print(t1)
chk("T1: Total column absent", !any(grepl("Total", names(t1))))

cat("\n-- Test 2: 2-group multi-level (Recurrence x Tumor_Differentiation) --\n")
t2 <- ternG(
  data               = tern_colon,
  vars               = c("Tumor_Differentiation"),
  group_var          = "Recurrence",
  percentage_compute = "row",
  open_doc           = FALSE,
  citation           = FALSE
)
print(t2)
chk("T2: Total column absent", !any(grepl("Total", names(t2))))

cat("\n-- Test 3: 3-group binary (Treatment_Arm x Colonic_Obstruction) --\n")
t3 <- ternG(
  data               = tern_colon,
  vars               = c("Colonic_Obstruction"),
  group_var          = "Treatment_Arm",
  percentage_compute = "row",
  open_doc           = FALSE,
  citation           = FALSE
)
print(t3)
chk("T3: Total column absent", !any(grepl("Total", names(t3))))

cat("\n-- Test 4: 3-group multi-level (Treatment_Arm x Extent_of_Local_Spread) --\n")
t4 <- ternG(
  data               = tern_colon,
  vars               = c("Extent_of_Local_Spread"),
  group_var          = "Treatment_Arm",
  percentage_compute = "row",
  open_doc           = FALSE,
  citation           = FALSE
)
print(t4)
chk("T4: Total column absent", !any(grepl("Total", names(t4))))

cat("\n-- Test 5: Regression - column % default unchanged --\n")
t5 <- ternG(
  data               = tern_colon,
  vars               = c("Colonic_Obstruction"),
  group_var          = "Recurrence",
  percentage_compute = "column",
  open_doc           = FALSE,
  citation           = FALSE
)
print(t5)
chk("T5: Total column PRESENT with column%", any(grepl("Total", names(t5))))

cat("\n-- Test 6: Row percentages sum to ~100 across groups --\n")
t6 <- ternG(
  data               = tern_colon,
  vars               = c("Tumor_Differentiation"),
  group_var          = "Treatment_Arm",
  percentage_compute = "row",
  show_p             = FALSE,
  open_doc           = FALSE,
  citation           = FALSE
)
print(t6)

grp_cols <- names(t6)[!names(t6) %in% c("Variable", "Value", "p", "Test", "Normal", "Total")]
extract_pct <- function(x) {
  m <- regmatches(x, regexpr("[0-9]+(?=%\\))", x, perl = TRUE))
  as.numeric(ifelse(length(m) == 0, NA, m))
}
if (length(grp_cols) >= 2) {
  pct_mat <- sapply(grp_cols, function(col) sapply(t6[[col]], extract_pct))
  row_sums <- rowSums(pct_mat, na.rm = TRUE)
  data_rows <- row_sums[row_sums > 0]
  cat("  Row % sums (expect ~100 per level):", data_rows, "\n")
  chk("T6: Row percentages sum to ~100", all(abs(data_rows - 100) < 2))
}

cat("\n-------------------------------------------------------------------------\n")
cat("Results:", pass, "passed,", fail, "failed\n")
