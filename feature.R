#+ 1.2: Create descriptive statistics table
T1 <- ternD(
  data = T1_data,
  consider_normality = "ROBUST",
  round_intg = FALSE,
  open_doc = TRUE,
  table_font_size = 9,
  force_ordinal = c("AAST Grade"),
  methods_doc = FALSE,
  table_caption = "Table 1. Descriptive statistics of the cohort.",
  abbreviation_footnote = "Abbreviations: AAST, American Association for the Surgery of Trauma; GCS, Glasgow Coma Scale; MAP, mean arterial pressure; SBP, systolic blood pressure; DBP, diastolic blood pressure; HR, heart rate; ISS, Injury Severity Score; MTP, massive transfusion protocol; RBC, red blood cells; FFP, fresh frozen plasma; TXA, tranexamic acid; AKI, acute kidney injury; Cr, creatinine; ICU, intensive care unit; LOS, length of stay; ED, emergency department.",
  category_start = c(
    "Demographics"                    = "Age",
    "Injury Features and Vital Signs" = "AAST Grade",
    "Lab Values"                      = "Initial Lactate",
    "Blood Products (24 h)"           = "MTP",
    "Clinical Course"                 = "Survival"
  )
)
T2 <- ternG(
  data = T2_data,
  group_var = "Index Management Strategy",
  round_intg = FALSE,
  open_doc = TRUE,
  force_ordinal = c("AAST Grade"),
  consider_normality = "ROBUST",
  methods_doc = FALSE,
  show_total = FALSE,
  group_order = c("Conservative Management", "Interventional Radiology", "Operative Management"),
  table_font_size = 9,
  table_caption = "Table 2. Demographic, clinical, and clinical course data in high-grade penetrating kidney injury patients stratified by index management strategy. All values are displayed as n (%), median [IQR], or mean ± SD as appropriate. Nonoperative management included interventional radiology and serial monitoring/conservative management. Calculations for Renal Salvage, AKI, ventilator days, ICU LOS, hospital LOS, and return to ED excluded patients who did not survive the index hospitalization. p-values < 0.05 are printed in bold. *Defined as no further procedures or death following the index management strategy; however, nephrectomy at index procedure was not counted as successful index management.",
  abbreviation_footnote = c("Abbreviations: AAST, American Association for the Surgery of Trauma; GCS, Glasgow Coma Scale; MAP, mean arterial pressure; SBP, systolic blood pressure; DBP, diastolic blood pressure; HR, heart rate; ISS, Injury Severity Score; MTP, massive transfusion protocol; RBC, red blood cells; FFP, fresh frozen plasma; TXA, tranexamic acid; AKI, acute kidney injury; Cr, creatinine; ICU, intensive care unit; LOS, length of stay; ED, emergency department."),
  variable_footnote = c(
    "Index Management Success" = "Defined as no further kidney-directed procedures or death following the index management strategy; however, nephrectomy at index procedure was not counted as successful index management."
  ),
  category_start = c(
    "Demographics"                    = "Age",
    "Injury Features and Vital Signs" = "AAST Grade",
    "Lab Values"                      = "Initial Lactate",
    "Blood Products (24 h)"           = "MTP",
    "Clinical Course"                 = "Survival"
  )
)
#+ 3.2: Create Table 3 stratified by AAST grade
T3 <- ternG(
  data = T3_data,
  group_var = "AAST Grade",
  group_order = c("Grade III", "Grade IV", "Grade V"),
  round_intg = FALSE,
  force_ordinal = c("AAST Grade"),
  open_doc = TRUE,
  consider_normality = "ROBUST",
  methods_doc = FALSE,
  show_total = FALSE,
  table_font_size = 9,
  table_caption = "Table 3. Demographic, clinical, and outcomes data in high-grade penetrating kidney injury patients stratified by AAST injury grade. All values are displayed as n (%), median [IQR], or mean ± SD as appropriate. Calculations for Renal Salvage, AKI, ventilator days, ICU LOS, hospital LOS, and return to ED excluded patients who did not survive the index hospitalization. p-values < 0.05 are printed in bold.",
  abbreviation_footnote = c("Abbreviations: AAST, American Association for the Surgery of Trauma; GCS, Glasgow Coma Scale; MAP, mean arterial pressure; SBP, systolic blood pressure; DBP, diastolic blood pressure; HR, heart rate; ISS, Injury Severity Score; MTP, massive transfusion protocol; RBC, red blood cells; FFP, fresh frozen plasma; TXA, tranexamic acid; AKI, acute kidney injury; Cr, creatinine; ICU, intensive care unit; LOS, length of stay; ED, emergency department."),
  variable_footnote = c(
    "Index Management Success" = "Defined as no further kidney-directed procedures or death following the index management strategy; however, nephrectomy at index procedure was not counted as successful index management.",
    "Operative Management" = "Refers to index management strategy."
  ),
  category_start = c(
    "Demographics" = "Age",
    "Injury Features and Vital Signs" = "GCS",
    "Lab Values" = "Initial Lactate",
    "Blood Products (24 h)" = "MTP",
    "Index Management Details" = "Index Management Success",
    "Clinical Course" = "Survival"
  )
)
