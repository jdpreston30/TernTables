# ── 1. Load package and example dataset ─────────────────────────────────────
# colon_recurrence is bundled with TernTables (see ?colon_recurrence).
# It is derived from survival::colon (etype == 1), one row per patient,
# with clinically labelled factors and renamed columns.
library(TernTables)
data(colon_recurrence)

# --- ternD: descriptive summary ---
TernDesc <- ternD(
  data           = colon_recurrence,
  exclude_vars   = c("ID"),
  output_docx    = "Outputs/Tern_descriptive.docx",
  category_start = c(
    "Patient Demographics"   = "Age (yr)",
    "Surgical Findings"      = "Colonic Obstruction",
    "Tumor Characteristics" = "Positive Lymph Nodes (n)",
    "Outcomes" = "Recurrence"
  )
)

# --- ternG: 2-group comparison (recurrence) ---
Tern2v <- ternG(
  data            = colon_recurrence,
  exclude_vars    = c("ID"),
  group_var       = "Recurrence",
  output_docx     = "Outputs/Tern_2_group.docx",
  OR_col          = TRUE,
  insert_subheads = TRUE,
  category_start  = c(
    "Patient Demographics"   = "Age (yr)",
    "Surgical Findings"      = "Colonic Obstruction",
    "Tumor Characteristics" = "Positive Lymph Nodes (n)",
    "Treatment Details" = "Treatment Arm"
  )
)

# --- ternG: 3-group comparison (treatment arm) ---
Tern3v <- ternG(
  data               = colon_recurrence,
  exclude_vars       = c("ID"),
  group_var          = "Treatment_Arm",
  group_order        = c("Observation", "Levamisole", "Levamisole + 5FU"),
  output_docx        = "Outputs/Tern_3_group.docx",
  consider_normality = TRUE,
  category_start     = c(
    "Patient Demographics"   = "Age (yr)",
    "Surgical Findings"      = "Colonic Obstruction",
    "Tumor Characteristics" = "Positive Lymph Nodes (n)",
    "Outcomes" = "Recurrence"
  )
)

