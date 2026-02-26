# TernTablesR Local Testing Script
# Run from the repo root: source("inst/examples/example_usage.r")
# or: Rscript inst/examples/example_usage.r

devtools::load_all(".")
library(CardioDataSets)
data("heart_transplant_df", package = "CardioDataSets")

# --- ternD: descriptive summary ---
cat("Running ternD (descriptive only)...\n")
TernDesc <- ternD(
  data = heart_transplant_df,
  consider_normality = TRUE
)
print(TernDesc)

# --- ternG: 2-level comparison ---
cat("\nRunning ternG (2-level: transplant status)...\n")
Tern2v <- ternG(
  data = heart_transplant_df,
  group_var = "transplant",
  consider_normality = TRUE,
  show_test = TRUE,
  insert_subheads = TRUE
)
print(Tern2v)

cat("\nDone.\n")
