# ── 1. Pre-process example dataset ───────────────────────────────────────────
# Filter to the recurrence endpoint (etype == 1) first so colon1 contains
# exactly one row per patient — the dataset of interest for all analyses.
colon1 <- as_tibble(survival::colon) |>
  dplyr::filter(etype == 1) |>
  dplyr::mutate(
    rx       = factor(rx, levels = c("Obs", "Lev", "Lev+5FU")),
    sex      = factor(sex,      levels = c(0, 1), labels = c("Female", "Male")),
    obstruct = factor(obstruct, levels = c(0, 1), labels = c("N", "Y")),
    perfor   = factor(perfor,   levels = c(0, 1), labels = c("N", "Y")),
    adhere   = factor(adhere,   levels = c(0, 1), labels = c("N", "Y")),
    node4    = factor(node4,    levels = c(0, 1), labels = c("N", "Y")),
    status   = factor(status,   levels = c(0, 1), labels = c("N", "Y")),
    etype    = factor(etype,    levels = c(1, 2),  labels = c("Recurrence", "Death")),
    surg     = factor(surg,     levels = c(0, 1),  labels = c("Short", "Long")),
    differ   = factor(differ,   levels = c(1, 2, 3),
                                labels = c("Well", "Moderate", "Poor")),
    extent   = factor(extent,   levels = c(1, 2, 3, 4),
                                labels = c("Submucosa", "Muscle", "Serosa", "Contiguous Structures"))
  ) |>
  dplyr::rename(
    "ID"                  = id,
    "Study"                       = study,
    "Treatment_Arm"               = rx,
    "Sex"                         = sex,
    "Age_Years"                   = age,
    "Colonic_Obstruction"         = obstruct,
    "Bowel_Perforation"           = perfor,
    "Tumour_Adherence"            = adhere,
    "Positive_Lymph_Nodes"        = nodes,
    "Recurrence"                  = status,
    "Tumour_Differentiation"      = differ,
    "Extent_of_Local_Spread"      = extent,
    "Time_to_Registration_days"    = surg,
    "More_Than_4_Positive_Nodes"  = node4,
    "Time_to_Event_days"          = time,
    "Event_Type"                  = etype
  ) |>
  dplyr::select(-Study, -Event_Type)

# ── 2. Load package ───────────────────────────────────────────────────────────
devtools::load_all(".")

# Columns to exclude from all table analyses (identifiers / survival vars)

# --- ternD: descriptive summary ---
TernDesc <- ternD(
  data = colon1,
  output_docx = "TernDesc.docx",
  exclude_vars = c("ID", "Time_to_Event_days")
)

# --- ternG: 2-group comparison (recurrence) ---
Tern2v <- ternG(
  data = colon1,
  group_var = "Recurrence",
  exclude_vars = c("ID", "Time_to_Event_days"),
  output_docx = "Tern_2group.docx",
  insert_subheads = TRUE
)

# --- ternG: 3-group comparison (treatment arm) ---
Tern3v <- ternG(
  data = colon1,
  group_var = "Treatment_Arm",
  exclude_vars = c("ID", "Time_to_Event_days"),
  consider_normality = TRUE,
  show_test = TRUE
)
print(Tern3v)

cat("\nDone.\n")