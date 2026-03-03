## code to prepare inst/extdata/csv/ and inst/extdata/xlsx/ example files
##
## These files are intentionally "messy" versions of the colon dataset derived
## from survival::colon (LGPL license). The same transformations applied in
## tern_colon.R are reproduced here in full so that this script is a completely
## self-contained paper trail. Synthetic messiness is then added on top to
## exercise the ternP() preprocessing function.
##
## Output files written:
##   inst/extdata/csv/tern_colon_messy.csv    — cleaning demo (no hard stops)
##   inst/extdata/xlsx/tern_colon_messy.xlsx
##   inst/extdata/csv/tern_colon_phi.csv      — PHI hard stop demo
##   inst/extdata/xlsx/tern_colon_phi.xlsx
##   inst/extdata/csv/tern_colon_unnamed.csv  — unnamed column hard stop demo
##   inst/extdata/xlsx/tern_colon_unnamed.xlsx
##
## Synthetic modifications in tern_colon_messy (NOT present in original data):
##   - String NA injection: "NA", "na", "Na", "unk" inserted in Sex and Tumor_Differentiation
##   - Whitespace injection: leading/trailing spaces on some Sex values
##   - Capitalization inconsistency: "fEMALE" and "MAle" mixed into Sex
##   - "Metastasis" column: 100% blank named column inserted in the middle of the dataset
##     (demonstrates silent empty-column removal in ternP; flanked by real columns on both sides)
##   - Completely blank rows: 3 rows where every cell is empty
##   - Sparse rows: 3 rows where >50% of cells are NA (but not 100%)
##
## A fixed random seed is used throughout so the output is fully reproducible.

library(dplyr)
library(tibble)
library(readr)    # write_csv
library(writexl)  # write_xlsx (no Java dependency)

set.seed(20260302)  # reproducibility seed — date this script was written

# ------------------------------------------------------------------------------
# Step 1: Reproduce all transformations from tern_colon.R
#   Starting from survival::colon raw so this script is self-contained.
# ------------------------------------------------------------------------------

base <- as_tibble(survival::colon) |>
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
    "Over_4_Positive_Nodes"  = node4,
    "Time_to_Event_days"     = time,
    "Event_Type"             = etype
  ) |>
  dplyr::select(-Study, -Event_Type) |>
  dplyr::select(
    ID,
    Age_Years, Sex,
    Colonic_Obstruction, Bowel_Perforation,
    Positive_Lymph_Nodes_n, Over_4_Positive_Nodes, Tumor_Adherence,
    Tumor_Differentiation, Extent_of_Local_Spread,
    Recurrence, Treatment_Arm
  )

# ------------------------------------------------------------------------------
# Step 2: Convert all columns to character
#   Required so that synthetic string values ("NA", "unk", whitespace, etc.)
#   can be written cleanly to CSV/XLSX without factor level conflicts.
# ------------------------------------------------------------------------------

messy <- base |>
  mutate(across(everything(), as.character))

n <- nrow(messy)

# ------------------------------------------------------------------------------
# Step 3: Inject string NA values
#   "NA", "na", "Na", and "unk" are inserted into Sex and Tumor_Differentiation
#   to trigger the string-NA conversion step in ternP().
#   ~5% of rows affected per column across all string-NA variants.
# ------------------------------------------------------------------------------

# Sex: inject "NA", "na", and "Na" across non-overlapping row subsets
remaining_idx      <- seq_len(n)
sex_NA_idx         <- sample(remaining_idx, size = round(n * 0.02))
remaining_idx      <- setdiff(remaining_idx, sex_NA_idx)
sex_na_idx         <- sample(remaining_idx, size = round(n * 0.02))
remaining_idx      <- setdiff(remaining_idx, sex_na_idx)
sex_Na_idx         <- sample(remaining_idx, size = round(n * 0.01))
messy$Sex[sex_NA_idx] <- "NA"
messy$Sex[sex_na_idx] <- "na"
messy$Sex[sex_Na_idx] <- "Na"

# Tumor_Differentiation: inject "unk"
differ_unk_idx <- sample(seq_len(n), size = round(n * 0.05))
messy$Tumor_Differentiation[differ_unk_idx] <- "unk"

# Combined index of all string-NA-injected Sex rows (used in Step 4)
all_sex_na_idx <- c(sex_NA_idx, sex_na_idx, sex_Na_idx)

# ------------------------------------------------------------------------------
# Step 4: Inject whitespace
#   Leading and trailing spaces added to some Sex values to trigger the
#   whitespace-trim step in ternP().
# ------------------------------------------------------------------------------

ws_idx <- sample(setdiff(seq_len(n), all_sex_na_idx), size = round(n * 0.04))
messy$Sex[ws_idx] <- paste0(" ", messy$Sex[ws_idx], " ")

# ------------------------------------------------------------------------------
# Step 5: Inject capitalization inconsistencies
#   "fEMALE" and "MAle" are mixed into Sex to trigger case-normalization in
#   ternP(). Only rows with clean "Female"/"Male" values are targeted.
# ------------------------------------------------------------------------------

clean_female_idx <- which(messy$Sex == "Female")
clean_male_idx   <- which(messy$Sex == "Male")

cap_female_idx <- sample(clean_female_idx, size = round(length(clean_female_idx) * 0.06))
cap_male_idx   <- sample(clean_male_idx,   size = round(length(clean_male_idx)   * 0.06))

messy$Sex[cap_female_idx] <- "fEMALE"
messy$Sex[cap_male_idx]   <- "MAle"

# ------------------------------------------------------------------------------
# Step 6: Add a 100% empty named column ("Metastasis") in the middle
#   This column has a real name but no data — ternP() should silently drop it
#   as part of the empty-column removal step. It is placed between
#   Bowel_Perforation and Positive_Lymph_Nodes_n so it is flanked on both
#   sides by real columns, as a realistic user spreadsheet might look.
# ------------------------------------------------------------------------------

messy <- messy |>
  dplyr::mutate(Metastasis = NA_character_) |>
  dplyr::relocate(Metastasis, .after = Bowel_Perforation)

# ------------------------------------------------------------------------------
# Step 7: Add 3 completely blank rows
#   These are 100% empty rows that ternP() should detect and remove.
#   They are appended at the end of the data frame.
# ------------------------------------------------------------------------------

blank_rows <- tibble::tibble(
  ID                     = NA_character_,
  Age_Years              = NA_character_,
  Sex                    = NA_character_,
  Colonic_Obstruction    = NA_character_,
  Bowel_Perforation      = NA_character_,
  Metastasis             = NA_character_,
  Positive_Lymph_Nodes_n = NA_character_,
  Over_4_Positive_Nodes  = NA_character_,
  Tumor_Adherence        = NA_character_,
  Tumor_Differentiation  = NA_character_,
  Extent_of_Local_Spread = NA_character_,
  Recurrence             = NA_character_,
  Treatment_Arm          = NA_character_
)

blank_rows <- dplyr::bind_rows(blank_rows, blank_rows, blank_rows)  # 3 blank rows

# ------------------------------------------------------------------------------
# Step 8: Add 3 sparse rows (>50% missing, but NOT 100%)
#   These rows have most cells empty but at least one value, so they are
#   retained by ternP() but flagged in the preprocessing feedback.
# ------------------------------------------------------------------------------

sparse_rows <- tibble::tibble(
  ID                     = c("9901", "9902", "9903"),
  Age_Years              = c("67",   NA,     NA),
  Sex                    = c(NA,     "Male", NA),
  Colonic_Obstruction    = c(NA,     NA,     NA),
  Bowel_Perforation      = c(NA,     NA,     NA),
  Metastasis             = c(NA,     NA,     NA),
  Positive_Lymph_Nodes_n = c(NA,     NA,     "3"),
  Over_4_Positive_Nodes  = c(NA,     NA,     NA),
  Tumor_Adherence        = c(NA,     NA,     NA),
  Tumor_Differentiation  = c(NA,     NA,     NA),
  Extent_of_Local_Spread = c(NA,     NA,     NA),
  Recurrence             = c(NA,     NA,     NA),
  Treatment_Arm          = c(NA,     NA,     NA)
)

# ------------------------------------------------------------------------------
# Step 8: Combine all rows into the final messy dataset
# ------------------------------------------------------------------------------

tern_colon_messy <- dplyr::bind_rows(messy, blank_rows, sparse_rows)

# ------------------------------------------------------------------------------
# Step 9: Write outputs to inst/extdata/csv/ and inst/extdata/xlsx/
# ------------------------------------------------------------------------------

dir.create("inst/extdata/csv",  showWarnings = FALSE, recursive = TRUE)
dir.create("inst/extdata/xlsx", showWarnings = FALSE, recursive = TRUE)

readr::write_csv(
  tern_colon_messy,
  file = "inst/extdata/csv/tern_colon_messy.csv",
  na   = ""   # write NA as blank cell, as a real export would look
)

writexl::write_xlsx(
  tern_colon_messy,
  path = "inst/extdata/xlsx/tern_colon_messy.xlsx"
)

message("inst/extdata/csv/tern_colon_messy.csv and xlsx/tern_colon_messy.xlsx written successfully.")
message(paste0("Rows: ", nrow(tern_colon_messy), "  |  Columns: ", ncol(tern_colon_messy)))

# ------------------------------------------------------------------------------
# HARD STOP DEMO 1: PHI columns
#   tern_colon_phi.csv / .xlsx
#   Purpose: trigger the PHI hard stop in ternP().
#   Contains fake_MRN, fake_firstname, and fake_lastname columns alongside a
#   small clean slice of the colon data. All PHI values are fabricated and
#   clearly labelled as fake. This file is intentionally kept small (30 rows)
#   since its only purpose is to demonstrate the hard stop, not cleaning.
# ------------------------------------------------------------------------------

set.seed(20260302)

phi_demo <- base |>
  dplyr::slice(1:30) |>
  mutate(across(everything(), as.character)) |>
  mutate(
    fake_MRN       = formatC(sample(100000:999999, size = 30, replace = FALSE), width = 6),
    fake_firstname = sample(
      c("James", "Mary", "John", "Patricia", "Robert", "Jennifer",
        "Michael", "Linda", "William", "Barbara", "David", "Susan"),
      size = 30, replace = TRUE
    ),
    fake_lastname  = sample(
      c("Smith", "Johnson", "Williams", "Brown", "Jones", "Garcia",
        "Miller", "Davis", "Rodriguez", "Martinez", "Hernandez", "Lopez"),
      size = 30, replace = TRUE
    ),
    fake_DOB       = format(
      as.Date("1940-01-01") + sample(0:20000, size = 30, replace = FALSE),
      "%m/%d/%Y"
    )
  ) |>
  dplyr::select(fake_MRN, fake_firstname, fake_lastname, fake_DOB, everything())

readr::write_csv(phi_demo, file = "inst/extdata/csv/tern_colon_phi.csv", na = "")
writexl::write_xlsx(phi_demo, path = "inst/extdata/xlsx/tern_colon_phi.xlsx")

message("inst/extdata/csv/tern_colon_phi.csv and xlsx/tern_colon_phi.xlsx written successfully.")
message(paste0("Rows: ", nrow(phi_demo), "  |  Columns: ", ncol(phi_demo)))

# ------------------------------------------------------------------------------
# HARD STOP DEMO 2: Unnamed column with data
#   tern_colon_unnamed.csv / .xlsx
#   Purpose: trigger the unnamed column hard stop in ternP().
#   A column with a blank header ("") but real data values is present.
#   This file is intentionally small (30 rows).
#   Note: the unnamed column is named "" in the data frame and written as-is
#   to CSV so the header cell is blank but the column contains data — exactly
#   the condition ternP() must detect.
# ------------------------------------------------------------------------------

unnamed_demo <- base |>
  dplyr::slice(1:30) |>
  mutate(across(everything(), as.character))

# Add a column with a blank name containing random Y/N data, inserted between
# Bowel_Perforation and Positive_Lymph_Nodes_n so it is flanked on both sides
# by real columns — as a realistic user spreadsheet might look.
# dplyr::relocate() cannot select empty-string column names, so base R
# column reordering is used instead.
unnamed_demo[[""]] <- sample(c("Y", "N"), size = 30, replace = TRUE)
insert_after   <- which(names(unnamed_demo) == "Bowel_Perforation")
col_order      <- c(
  seq_len(insert_after),
  ncol(unnamed_demo),
  seq(insert_after + 1, ncol(unnamed_demo) - 1)
)
unnamed_demo <- unnamed_demo[, col_order]

readr::write_csv(unnamed_demo, file = "inst/extdata/csv/tern_colon_unnamed.csv", na = "")
writexl::write_xlsx(unnamed_demo, path = "inst/extdata/xlsx/tern_colon_unnamed.xlsx")

message("inst/extdata/csv/tern_colon_unnamed.csv and xlsx/tern_colon_unnamed.xlsx written successfully.")
message(paste0("Rows: ", nrow(unnamed_demo), "  |  Columns: ", ncol(unnamed_demo)))
