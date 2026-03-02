# =============================================================================
# Manual vs ternG() Welch ANOVA validation
# Purpose: confirm that the switch from aov() to oneway.test(var.equal = FALSE)
#          was truly implemented and that ternG() p-values match manual calc.
# Run from repo root with: source("development/test-welch-anova.R")
# =============================================================================

library(devtools)
devtools::load_all(".")   # load current package source without installing
data(tern_colon)

# tern_colon grouping variable for 3+ group tests:
#   Treatment_Arm: "Levamisole + 5FU" / "Levamisole" / "Observation"
GROUP <- "Treatment_Arm"
cat("Group levels:", levels(factor(tern_colon[[GROUP]])), "\n")
cat("Group sizes:\n")
print(table(tern_colon[[GROUP]]))
cat("\n")

# =============================================================================
# TEST 1: Welch ANOVA path (normally distributed variable -> Age_Years)
# =============================================================================
cat("=== TEST 1: Welch ANOVA (Age_Years ~ Treatment_Arm) ===\n\n")

# --- Manual calculation ---
manual_welch <- stats::oneway.test(Age_Years ~ Treatment_Arm,
                                   data = tern_colon,
                                   var.equal = FALSE)
cat("Manual oneway.test(var.equal = FALSE):\n")
print(manual_welch)
cat("Manual p-value:", manual_welch$p.value, "\n\n")

# Check what aov() would have given (should differ to confirm we're NOT using it)
manual_aov <- summary(stats::aov(Age_Years ~ Treatment_Arm, data = tern_colon))
cat("For reference -- old aov() p-value:", manual_aov[[1]][["Pr(>F)"]][1], "\n\n")

# --- ternG result ---
cat("ternG() output (show_test = TRUE so we can see test name):\n")
tbl <- ternG(
  data       = tern_colon,
  group_var  = GROUP,
  vars       = c("Age_Years"),
  show_test  = TRUE,
  consider_normality = "ASSESS"   # default: let Shapiro-Wilk decide
)
print(tbl)
cat("\n")

# Extract the p-value row for Age_Years
p_row <- tbl[tbl$Variable == "Age_Years", ]
cat("ternG P column:  ", p_row$P, "\n")
cat("ternG test column:", p_row$test, "\n\n")

# Verify test name is "Welch ANOVA" not "ANOVA"
stopifnot("FAIL: test name should be 'Welch ANOVA'" = p_row$test == "Welch ANOVA")
cat("PASS: test name is 'Welch ANOVA'\n\n")

# Verify p-value matches manual calculation (formatted)
manual_p_formatted <- TernTables::val_p_format(manual_welch$p.value)
cat("Manual formatted p:", manual_p_formatted, "\n")
cat("ternG formatted p: ", p_row$P, "\n")
stopifnot("FAIL: p-values do not match" = p_row$P == manual_p_formatted)
cat("PASS: p-values match\n\n")

# =============================================================================
# TEST 2: Kruskal-Wallis path (non-normal variable -> Positive_Lymph_Nodes_n)
#         Confirms the non-normal branch is unaffected by the change
# =============================================================================
cat("=== TEST 2: Kruskal-Wallis (Positive_Lymph_Nodes_n ~ Treatment_Arm) ===\n\n")

manual_kw <- stats::kruskal.test(Positive_Lymph_Nodes_n ~ Treatment_Arm,
                                  data = tern_colon)
cat("Manual kruskal.test():\n")
print(manual_kw)
cat("Manual p-value:", manual_kw$p.value, "\n\n")

cat("ternG() output (forcing non-normal path via consider_normality = 'FORCE'):\n")
tbl2 <- ternG(
  data       = tern_colon,
  group_var  = GROUP,
  vars       = c("Positive_Lymph_Nodes_n"),
  show_test  = TRUE,
  consider_normality = "FORCE"   # force non-normal regardless of Shapiro-Wilk
)
print(tbl2)
cat("\n")

p_row2 <- tbl2[tbl2$Variable == "Positive_Lymph_Nodes_n", ]
cat("ternG P column:  ", p_row2$P, "\n")
cat("ternG test column:", p_row2$test, "\n\n")

stopifnot("FAIL: test name should be 'Kruskal-Wallis'" = p_row2$test == "Kruskal-Wallis")
cat("PASS: test name is 'Kruskal-Wallis'\n\n")

manual_kw_formatted <- TernTables::val_p_format(manual_kw$p.value)
stopifnot("FAIL: Kruskal-Wallis p-values do not match" = p_row2$P == manual_kw_formatted)
cat("PASS: Kruskal-Wallis p-values match\n\n")

# =============================================================================
# TEST 3: Welch t-test still intact for 2-group comparisons
#         (Positive_Lymph_Nodes_n normally on Recurrence: 2 groups)
# =============================================================================
cat("=== TEST 3: Welch t-test still intact (Age_Years ~ Recurrence, 2 groups) ===\n\n")

manual_ttest <- stats::t.test(Age_Years ~ Recurrence, data = tern_colon)
cat("Manual t.test():\n")
print(manual_ttest)
cat("Manual p-value:", manual_ttest$p.value, "\n\n")

tbl3 <- ternG(
  data      = tern_colon,
  group_var = "Recurrence",
  vars      = c("Age_Years"),
  show_test = TRUE,
  consider_normality = "ASSESS"
)
print(tbl3)
cat("\n")

p_row3 <- tbl3[tbl3$Variable == "Age_Years", ]
cat("ternG P column:  ", p_row3$P, "\n")
cat("ternG test column:", p_row3$test, "\n\n")

stopifnot("FAIL: test name should be 'Welch t-test'" = p_row3$test == "Welch t-test")
cat("PASS: test name is 'Welch t-test'\n\n")

manual_t_formatted <- TernTables::val_p_format(manual_ttest$p.value)
stopifnot("FAIL: t-test p-values do not match" = p_row3$P == manual_t_formatted)
cat("PASS: Welch t-test p-values match\n\n")

# =============================================================================
# All tests passed
# =============================================================================
cat("============================================================\n")
cat("All validation tests PASSED.\n")
cat("Welch ANOVA (oneway.test) is confirmed live in ternG().\n")
cat("============================================================\n")
