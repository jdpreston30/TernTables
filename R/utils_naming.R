# Internal helpers for variable name cleaning.
# Used by ternG() and ternD() for all display name formatting.

#' Clean a variable name for table display
#'
#' Strips internal suffixes, detects time unit suffixes and appends them as
#' parentheticals, replaces underscores with spaces, applies title case, and
#' expands a small set of common medical abbreviations.
#' Used automatically by ternG() and ternD() for every variable header.
#' @noRd
.clean_variable_name_for_header <- function(var_name) {
  clean_name <- var_name

  # Strip internal suffixes
  clean_name <- gsub("_simplified$", "", clean_name)
  clean_name <- gsub("_calc$",       "", clean_name)
  clean_name <- gsub("_tpx$",        "", clean_name)

  # Detect and extract time-unit suffix before underscores are removed
  unit_suffix <- ""
  if (grepl("_[nN]$", clean_name)) {
    clean_name <- sub("_[nN]$", "", clean_name)
    unit_suffix <- " (n)"
  } else if (grepl("_(yr|year|years)$", clean_name, ignore.case = TRUE)) {
    clean_name <- sub("_(yr|year|years)$", "", clean_name, ignore.case = TRUE)
    unit_suffix <- " (yr)"
  } else if (grepl("_(d|day|days)$", clean_name, ignore.case = TRUE)) {
    clean_name <- sub("_(d|day|days)$", "", clean_name, ignore.case = TRUE)
    unit_suffix <- " (days)"
  } else if (grepl("_(mo|month|months)$", clean_name, ignore.case = TRUE)) {
    clean_name <- sub("_(mo|month|months)$", "", clean_name, ignore.case = TRUE)
    unit_suffix <- " (months)"
  } else if (grepl("_(wk|wks|week|weeks)$", clean_name, ignore.case = TRUE)) {
    clean_name <- sub("_(wk|wks|week|weeks)$", "", clean_name, ignore.case = TRUE)
    unit_suffix <- " (weeks)"
  }

  # Convert Over_ prefix to "> " for display (e.g. Over_4_Positive_Nodes -> > 4 Positive Nodes)
  clean_name <- sub("^Over_", "> ", clean_name)

  clean_name <- gsub("_", " ", clean_name)
  clean_name <- tools::toTitleCase(clean_name)
  clean_name <- paste0(clean_name, unit_suffix)

  # Common abbreviation fixes post title-case
  clean_name <- gsub("\\bCod\\b",     "Cause of Death",       clean_name)
  clean_name <- gsub("\\bDbd Dcd\\b", "Mode of Organ Donation", clean_name)
  clean_name <- gsub("\\bPhm\\b",     "Predicted Heart Mass",  clean_name)
  clean_name <- gsub("\\bPhs\\b",     "PHS",                   clean_name)
  clean_name <- gsub("\\bLvef\\b",    "LVEF",                  clean_name)

  return(clean_name)
}

# Rule-based renaming helpers (used when smart_rename = TRUE)

#' Apply comprehensive cleaning rules to a single variable name
#' @noRd
.apply_cleaning_rules <- function(var_name) {

  # Remove trailing : Y, : N patterns (binary indicators)
  clean_name <- gsub(": [YN]$", "", var_name)

  # If there are no underscores the name has already been cleaned — return as-is
  # to avoid .capitalize_medical_term destroying title case
  if (!grepl("_", clean_name)) return(trimws(clean_name))

  # Split by underscores and process each part
  parts <- strsplit(clean_name, "_")[[1]]

  # Common abbreviation mappings
  abbrev_map <- list(
    "demographics" = "",
    "age" = "Age",
    "BMI" = "Body Mass Index",
    "sex" = "Sex",
    "race" = "Race",
    "preop" = "Preoperative",
    "postop" = "Postoperative",
    "operative" = "Operative",
    "intraop" = "Intraoperative",
    "LVEF" = "LVEF",
    "CVP" = "CVP",
    "IABP" = "Intra-Aortic Balloon Pump",
    "ECMO" = "ECMO",
    "VA" = "VA",
    "LVAD" = "LVAD",
    "RVAD" = "RVAD",
    "MCS" = "Mechanical Circulatory Support",
    "ICU" = "ICU",
    "LOS" = "LOS",
    "CRRT" = "CRRT",
    "RRT" = "RRT",
    "CVA" = "Cerebrovascular Accident",
    "COD" = "Cause of Death",
    "DBD" = "DBD",
    "DCD" = "DCD",
    "PHS" = "PHS",
    "UNOS" = "UNOS",
    "PVR" = "PVR",
    "GFR" = "GFR",
    "Hgb" = "Hemoglobin",
    "WBC" = "WBC",
    "Plt" = "Platelets",
    "ALT" = "ALT",
    "AST" = "AST",
    "PHM" = "Predicted Heart Mass",
    "ACR" = "Acute Cellular Rejection",
    "minutes" = "(minutes)",
    "days" = "(days)",
    "calc" = "Score",
    "median" = "",
    "tpx" = "",
    "smoking" = "Smoking",
    "hx" = "History",
    "DM" = "Diabetes Mellitus",
    "prior" = "Prior",
    "cardiac" = "Cardiac",
    "surg" = "Surgery",
    "temp" = "Temporary",
    "inotrope" = "Inotrope",
    "stroke" = "Stroke",
    "hospital" = "Hospital",
    "survival" = "Survival",
    "needed" = "Needed",
    "index" = "Index",
    "score" = "Score",
    "status" = "Status",
    "recipient" = "Recipient",
    "donor" = "Donor",
    "mismatch" = "Mismatch",
    "drug" = "Drug",
    "use" = "Use",
    "risk" = "Risk",
    "simplified" = "",
    "greater" = "Greater",
    "or" = "",
    "IT" = "Ischemic Time",
    "CPB" = "CPB Time",
    "labs" = "",
    "albumin" = "Albumin",
    "alkphos" = "Alkaline Phosphatase",
    "bilirubin" = "Bilirubin",
    "creatinine" = "Creatinine",
    "sodium" = "Sodium",
    "30" = "30-day",
    "90" = "90-day",
    "2R" = ">=2R",
    "5.5" = "5.5",
    "Impella5.5" = "Impella 5.5"
  )

  processed_parts <- character(length(parts))
  for (i in seq_along(parts)) {
    part <- parts[i]
    if (part %in% names(abbrev_map)) {
      mapped <- abbrev_map[[part]]
      if (mapped != "") processed_parts[i] <- mapped
    } else {
      processed_parts[i] <- .capitalize_medical_term(part)
    }
  }

  processed_parts <- processed_parts[processed_parts != ""]
  result <- paste(processed_parts, collapse = " ")

  result <- gsub("\\s+", " ", result)
  result <- gsub("^\\s+|\\s+$", "", result)

  result <- gsub("Postoperative Mechanical Circulatory Support", "Postoperative", result)
  result <- gsub("Survival 90-day", "90-day Survival", result)
  result <- gsub("Postoperative 30-day Day", "Postoperative 30-day", result)
  result <- gsub("Acute Cellular Rejection >=2R Or Greater", ">=2R Acute Cellular Rejection", result)
  result <- gsub("Acute Cellular Rejection >=2R Greater", ">=2R Acute Cellular Rejection", result)
  result <- gsub("Postoperative Ischemic Time", "Ischemic Time", result)
  result <- gsub("Postoperative CPB Time", "CPB Time", result)
  result <- gsub("Operative CPB Time", "CPB Time", result)
  result <- gsub("Operative Ischemic Time", "Ischemic Time", result)
  result <- gsub(": y$", ": Y", result)
  result <- gsub(": n$", ": N", result)
  result <- gsub("Iabp", "IABP", result)
  result <- gsub("Ecmo", "ECMO", result)
  result <- gsub("Rvad", "RVAD", result)
  result <- gsub("Lvad", "LVAD", result)

  if (grepl("(Time|LOS)$", result) && !grepl("\\(", result)) {
    if (grepl("Time", result)) result <- paste0(result, " (minutes)")
    else if (grepl("LOS", result)) result <- paste0(result, " (days)")
  }

  return(result)
}

#' Capitalize medical terms appropriately
#' @noRd
.capitalize_medical_term <- function(term) {

  if (grepl("^[0-9.]+$", term)) return(term)

  if (nchar(term) >= 2 && term == toupper(term) && grepl("^[A-Z]+[0-9]*$", term)) return(term)

  if (grepl("[A-Z]$", term) && nchar(term) <= 5) return(term)

  upper_terms <- c(
    "IT", "CPB", "NICM", "ICM", "IABP", "VA", "LVEF", "CVP",
    "MCS", "ICU", "LOS", "CRRT", "RRT", "UNOS", "PVR", "GFR",
    "WBC", "ALT", "AST", "BMI", "DBD", "DCD", "PHS", "PHM",
    "ACR", "LVAD", "RVAD", "ECMO", "AKI", "GCS", "ISS", "ED",
    "SBP", "DBP", "MAP", "HR", "OR", "SM", "IR", "BUN", "CRP",
    "ESR", "CBC", "BNP", "LDH", "PT", "PTT", "INR", "DNA", "RNA",
    "HIV", "HCV", "HBV", "CMV", "EBV", "COVID", "COPD", "CHF",
    "CAD", "PVD", "DM", "HTN", "AF", "VTE", "PE", "DVT", "MI",
    "CVA", "TIA", "CABG", "PCI", "ICD", "CRT", "EKG", "ECG",
    "CT", "MRI", "PET", "US", "CXR", "ECHO", "TEE", "TTE"
  )

  if (term %in% upper_terms) return(term)

  paste0(toupper(substr(term, 1, 1)), tolower(substr(term, 2, nchar(term))))
}
