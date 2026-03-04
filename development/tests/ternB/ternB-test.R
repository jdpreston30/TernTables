# Fresh setup
{
devtools::install(pkg = ".", quick = TRUE, upgrade = "never", quiet = TRUE)
library(TernTables)
data(tern_colon)

# Build tables
T1 <- ternD(
  data = tern_colon,
  exclude_vars = c("ID"),
  methods_doc = FALSE,
  table_caption = "Table 1. Overall patient characteristics.",
  category_start = c(
    "Patient Demographics"  = "Age (yr)",
    "Tumor Characteristics" = "Positive Lymph Nodes (n)"
  ),
  table_footnote = "Continuous variables expressed as mean \u00b1 SD or median [IQR] based on Shapiro-Wilk normality testing."
)

T2 <- ternG(
  data = tern_colon,
  exclude_vars = c("ID"),
  group_var = "Recurrence",
  OR_col = TRUE,
  methods_doc = FALSE,
  table_caption = "Table 2. Characteristics by recurrence status.",
  table_footnote = c(
    "Abbreviations: OR, odds ratio; CI, confidence interval.",
    "† P values from chi-square test (categorical) or Wilcoxon rank-sum test (continuous).",
    "‡ ORs from unadjusted logistic regression."
  ),
  category_start = c(
    "Patient Demographics"  = "Age (yr)",
    "Tumor Characteristics" = "Positive Lymph Nodes (n)"
  )
)

T3 <- ternG(
  data          = tern_colon,
  exclude_vars  = c("ID"),
  group_var     = "Treatment_Arm",
  methods_doc   = FALSE,
  table_caption = "Table 3. Characteristics by treatment arm.",
  table_footnote = c(
    "† Continuous variables expressed as median [IQR].",
    "‡ P values from Kruskal-Wallis test with Dunn post-hoc correction."
  )
)

ternB(
  tables           = list(T1, T2, T3),
  output_docx      = "development/tests/ternB/tables/T1-T3.docx",
  methods_doc      = TRUE,
  methods_filename = "development/tests/ternB/tables/T1-T3-methods.docx"
)
}
