## test-posthoc-cld.R
## Run ternG grouped by Tumor_Differentiation (Well / Moderate / Poor)
## to exercise post_hoc = TRUE and inspect CLD output in Word.

devtools::load_all(".")
data(tern_colon)

out_dir <- "development/tests/posthoc-cld/tables"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

tbl <- ternG(
  data               = tern_colon,
  exclude_vars       = c("ID", "Treatment_Arm"),
  group_var          = "Tumor_Differentiation",
  group_order        = c("Well", "Moderate", "Poor"),
  consider_normality = "ROBUST",
  post_hoc           = TRUE,
  show_test          = TRUE,
  output_docx        = file.path(out_dir, "test_posthoc_cld.docx"),
  methods_filename   = file.path(out_dir, "test_posthoc_cld_methods.docx"),
  table_caption      = "Table. Characteristics by tumor differentiation.",
  category_start     = c(
    "Patient Demographics"  = "Age (yr)",
    "Surgical Findings"     = "Colonic Obstruction",
    "Tumor Characteristics" = "Positive Lymph Nodes (n)",
    "Outcomes"              = "Recurrence"
  )
)

print(tbl, n = 50)
