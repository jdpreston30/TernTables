## code to prepare the `tern_colon` dataset

library(dplyr)
library(tibble)

tern_colon <- as_tibble(survival::colon) |>
  dplyr::filter(etype == 1) |>
  dplyr::mutate(
    rx       = factor(rx, levels = c("Lev+5FU", "Lev", "Obs"),
                      labels = c("Levamisole + 5FU", "Levamisole", "Observation")),
    sex      = factor(sex,      levels = c(0, 1), labels = c("Female", "Male")),
    obstruct = factor(obstruct, levels = c(0, 1), labels = c("N", "Y")),
    perfor   = factor(perfor,   levels = c(0, 1), labels = c("N", "Y")),
    adhere   = factor(adhere,   levels = c(0, 1), labels = c("N", "Y")),
    node4    = factor(node4,    levels = c(0, 1), labels = c("N", "Y")),
    status   = factor(status,   levels = c(0, 1), labels = c("No Recurrence", "Recurrence")),
    etype    = factor(etype,    levels = c(1, 2),  labels = c("Recurrence", "Death")),
    surg     = factor(surg,     levels = c(0, 1),  labels = c("Short", "Long")),
    differ   = factor(differ,   levels = c(1, 2, 3),
                                labels = c("Well", "Moderate", "Poor")),
    extent   = factor(extent,   levels = c(1, 2, 3, 4),
                                labels = c("Submucosa", "Muscle", "Serosa", "Contiguous Structures"))
  ) |>
  dplyr::rename(
    "ID"                     = id,
    "Study"                  = study,
    "Treatment_Arm"          = rx,
    "Sex"                    = sex,
    "Age_Years"              = age,
    "Colonic_Obstruction"    = obstruct,
    "Bowel_Perforation"      = perfor,
    "Tumor_Adherence"        = adhere,
    "Positive_Lymph_Nodes_n" = nodes,
    "Recurrence"             = status,
    "Tumor_Differentiation"  = differ,
    "Extent_of_Local_Spread" = extent,
    "Time_to_Registration"   = surg,
    ">_4_Positive_Nodes"     = node4,
    "Time_to_Event_days"     = time,
    "Event_Type"             = etype
  ) |>
  dplyr::select(-Study, -Event_Type) |>
  dplyr::select(
    ID,
    Age_Years, Sex,
    Colonic_Obstruction, Bowel_Perforation,
    Positive_Lymph_Nodes_n, `>_4_Positive_Nodes`, Tumor_Adherence,
    Tumor_Differentiation, Extent_of_Local_Spread,
    Recurrence, Treatment_Arm
  )

usethis::use_data(tern_colon, overwrite = TRUE)
