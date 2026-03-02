# Test: auto line-break for multi-word column headers + inline caption formatting
# Verifies that group names like "Conservative Management" wrap automatically
# without manually embedding \n in the group label.
# Also verifies that Table captions bold only the first two sentences.
{
  devtools::load_all(".", quiet = TRUE)  # load_all so changes in R/ are live immediately
  data(tern_colon)

  # Create a version of tern_colon with long multi-word group names
  # that mirror realistic clinical scenarios (e.g. management pathways)
  test_data <- tern_colon %>%
    dplyr::mutate(
      Management_Group = dplyr::case_when(
        Treatment_Arm == "Observation"       ~ "Conservative Management",
        Treatment_Arm == "Levamisole"        ~ "Interventional Radiology",
        Treatment_Arm == "Levamisole + 5FU"  ~ "Operative Management"
      )
    )

  # --- 3-group: multi-word headers + long real-world caption (sentences 1-2 bold, rest plain) ---
  T_long <- ternG(
    data         = test_data,
    exclude_vars = c("ID", "Treatment_Arm"),
    group_var    = "Management_Group",
    group_order  = c("Conservative Management", "Interventional Radiology", "Operative Management"),
    methods_doc  = FALSE,
    show_test    = TRUE,
    output_docx  = "development/linebreak-3group.docx",
    table_caption = "Table 2. Demographic, clinical, and clinical course data in high-grade penetrating kidney injury patients stratified by index management strategy. All values are displayed as n (%), median [IQR], or mean \u00b1 SD as appropriate. Nonoperative management included interventional radiology and serial monitoring/conservative management. Calculations for Renal Salvage, AKI, ventilator days, ICU LOS, hospital LOS, and return to ED excluded patients who did not survive the index hospitalization. p-values < 0.05 are printed in bold. *Defined as no further procedures or death following the index management strategy; however, nephrectomy at index procedure was not counted as successful index management."
  )

  # --- 2-group: two-word headers with OR column + short caption (whole thing bold) ---
  test_data_2g <- tern_colon %>%
    dplyr::mutate(
      Recurrence_Status = dplyr::if_else(Recurrence == "Recurrence", "Disease Recurrence", "No Recurrence")
    )

  T_long_2g <- ternG(
    data         = test_data_2g,
    exclude_vars = c("ID", "Recurrence"),
    group_var    = "Recurrence_Status",
    group_order  = c("No Recurrence", "Disease Recurrence"),
    OR_col       = TRUE,
    methods_doc  = FALSE,
    show_test    = TRUE,
    output_docx  = "development/linebreak-2group.docx",
    table_caption = "Table 1. Patient characteristics stratified by recurrence status. Single-sentence subtitle so the whole caption stays bold."
  )

  message("Done — check development/linebreak-3group.docx and development/linebreak-2group.docx")
}
