# Internal helpers for ternP() preprocessing.
# Both functions are hard stops — they call stop() if a condition is detected.

# ------------------------------------------------------------------------------
# PHI column name patterns
#   Case-insensitive regex patterns. Any column name matching one or more of
#   these patterns triggers the PHI hard stop.
#
#   Design intent: flag genuine personal identifiers while avoiding false
#   positives on de-identified research columns that researchers routinely use.
#   Specifically:
#     - Patient/subject/participant IDs are NOT flagged (de-identified in use).
#     - Clinical-event dates (admission, discharge, visit, etc.) are NOT flagged.
#     - Only personal-identity dates (DOB, DOD) are flagged.
# ------------------------------------------------------------------------------

.phi_patterns <- c(
  # Names
  "first.?name", "last.?name", "middle.?name", "full.?name",
  "given.?name", "family.?name", "surname", "maiden.?name",
  "^fname$", "^lname$", "^mname$", "^name$", "^patient.?name$",

  # Identifiers
  # No word-boundary anchors on short abbreviations: underscores are word
  # characters in regex, so \\b would NOT fire on e.g. fake_MRN or fake_DOB.
  # Note: patient_id / subject_id / participant_id are intentionally NOT flagged
  # — researchers commonly use these as de-identified study identifiers.
  "mrn", "medical.?record", "record.?number", "record.?no",
  "npi", "national.?provider",
  "ssn", "social.?security", "\\bsin\\b",
  "account.?number", "insurance.?id", "member.?id", "policy.?number",
  "health.?plan", "beneficiary",

  # Dates — only PERSONAL identifiers (birth / death); clinical-event dates
  # (admission, discharge, visit, procedure, surgery, etc.) are NOT flagged
  # because they are routinely present in de-identified research datasets.
  "dob", "date.?of.?birth", "birth.?date", "birthdate", "birthday",
  "date.?of.?death", "dod", "death.?date",

  # Contact / location
  # No \\b anchors — underscores are word characters and would block matching
  # on column names like patient_phone, mailing_address, cell_number, etc.
  # "tel" and "city" use word-boundary anchors to prevent false positives on
  # legitimate clinical column names such as "Platelet", "Ethnicity", "Velocity".
  "phone", "telephone", "\\btel\\b", "mobile", "cell.?num", "cell.?phone",
  "fax",
  "email", "e.?mail",
  "address", "street", "\\bcity\\b", "zip", "zip.?code", "postal.?code",

  # Geography smaller than state
  "county", "district", "precinct",

  # Biometrics / photos
  "fingerprint", "retina", "voiceprint", "photo", "image", "biometric",

  # Device / IP
  # "device.?id" uses a more specific pattern to avoid matching medical implant
  # fields like "Unique Device Identifier (UDI)" which are not patient identifiers.
  "device.?id(?!entifier)", "serial.?number", "ip.?address", "mac.?address", "url"
)

#' Check a data frame for PHI column names
#'
#' Scans column names for patterns associated with protected health information.
#' Stops with a descriptive error if any matches are found.
#' @param data A data frame or tibble.
#' @noRd
.check_phi <- function(data) {
  col_names <- names(data)

  matched <- vapply(col_names, function(nm) {
    any(grepl(paste(.phi_patterns, collapse = "|"), nm, ignore.case = TRUE,
              perl = TRUE))
  }, logical(1))

  if (any(matched)) {
    flagged <- col_names[matched]
    stop(
      "Potential PHI detected. The following column name(s) may contain ",
      "protected health information and cannot be processed:\n  ",
      paste(flagged, collapse = ", "), "\n\n",
      "Please remove or rename these columns before proceeding. ",
      "If this is a false positive (e.g. a column named 'Address' that does ",
      "not contain real addresses), rename it to something that does not ",
      "match a PHI identifier.",
      call. = FALSE
    )
  }

  invisible(NULL)
}

#' Check a data frame for unnamed columns that contain data
#'
#' Stops with a descriptive error if any column has a blank, whitespace-only,
#' or auto-renamed (e.g. \code{...6} from readr/readxl) name AND contains at
#' least one non-NA value. Completely empty unnamed columns are silently ignored
#' here — they are dropped downstream by the empty-column removal step.
#'
#' readr and readxl auto-rename blank headers to \code{...N} (e.g. \code{...6})
#' before the data frame reaches ternP. These are treated as unnamed columns.
#' @param data A data frame or tibble.
#' @noRd
.check_unnamed_cols <- function(data) {
  col_names <- names(data)

  # A column is considered "unnamed" if its name is:
  #   (a) blank / whitespace-only, OR
  #   (b) the readr/readxl auto-rename pattern: ...N (e.g. "...6", "...12")
  is_unnamed <- trimws(col_names) == "" |
                grepl("^\\.\\.\\.[0-9]+$", col_names)

  if (!any(is_unnamed)) return(invisible(NULL))

  # Only flag unnamed columns that actually have data
  has_data <- vapply(which(is_unnamed), function(i) {
    !all(is.na(data[[i]]))
  }, logical(1))

  if (any(has_data)) {
    n_flagged <- sum(has_data)
    stop(
      n_flagged, " unnamed column(s) with data were detected. ",
      "Column names must appear in the first row of your file.\n\n",
      "Please open your file and ensure every column that contains data ",
      "has a name in the header row, then re-upload.",
      call. = FALSE
    )
  }

  invisible(NULL)
}

# ── Shared missing-value utilities (single source of truth) ──────────────────
# Used by ternP(), ternG() and ternD() to ensure consistent detection of
# string representations of missing data.

#' Canonical set of string values treated as missing
#'
#' Returns the same list that ternP() uses when converting string NAs to real
#' NA values. Matching is always case-insensitive (callers should apply
#' \code{tolower(trimws(x))} before comparing).
#' @return A character vector of lowercase, trimmed string NA patterns.
#' @noRd
.tern_missing_strings <- function() {
  c(
    "na", "n/a", "n\\a", "nan",
    "missing", "unknown", "unk",
    "not available", "not applicable",
    "none", "null", "nil",
    "-", "--", "---",
    ".", "?"
  )
}

#' Test whether each element of a vector counts as "missing"
#'
#' An element is missing if \code{is.na(x)} is \code{TRUE} OR if the
#' case-insensitive trimmed value matches one of the string patterns in
#' \code{indicators} (or the ternP canonical list when \code{indicators} is
#' \code{NULL}).  Non-character columns only trigger the \code{is.na()} check.
#'
#' @param x A vector (any type).
#' @param indicators Optional character vector of string patterns to treat as
#'   missing.  When supplied, \strong{replaces} (does not supplement) the
#'   default ternP list.  Case is ignored; leading/trailing whitespace is
#'   trimmed before comparison.
#' @return A logical vector the same length as \code{x}.
#' @noRd
.is_missing_value <- function(x, indicators = NULL) {
  miss_strings <- if (!is.null(indicators)) {
    tolower(trimws(as.character(indicators)))
  } else {
    .tern_missing_strings()
  }
  is.na(x) | (is.character(x) & !is.na(x) & tolower(trimws(x)) %in% miss_strings)
}
