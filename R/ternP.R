#' Preprocess a raw data frame for use with ternG or ternD
#'
#' \code{ternP()} cleans a raw data frame loaded from a CSV or XLSX file,
#' applying a standardized set of transformations and performing validation
#' checks before the data is passed to \code{\link{ternG}} or
#' \code{\link{ternD}}.
#'
#' @section Cleaning pipeline (in order):
#' \enumerate{
#'   \item Date columns are detected (R \code{Date}/\code{POSIXct} types, or
#'     character columns where ≥80\% of values match a common date pattern) and
#'     reported in \code{feedback$date_cols_detected}. They are not dropped
#'     automatically — the caller decides whether to exclude or keep them.
#'   \item String NA values (\code{"NA"}, \code{"na"}, \code{"Na"},
#'     \code{"unk"}) are converted to \code{NA}.
#'   \item Leading and trailing whitespace is trimmed from all character
#'     columns.
#'   \item Columns that are 100\% empty (all \code{NA}) are silently dropped.
#'   \item Rows where every cell is \code{NA} are removed.
#'   \item Character columns where values differ only by capitalization
#'     (e.g. \code{"Male"} vs \code{"MAle"}) are standardized to title case.
#' }
#'
#' @section Validation hard stops:
#' \code{ternP()} stops with a descriptive error if:
#' \itemize{
#'   \item Any column name matches a protected health information (PHI) pattern
#'     (e.g. \code{MRN}, \code{DOB}, \code{FirstName}). De-identified research
#'     identifiers such as \code{patient_id}, \code{subject_id}, and
#'     \code{participant_id} are explicitly excluded, as are clinical-event
#'     dates (admission date, discharge date, visit date, etc.). Only
#'     personal-identity dates such as DOB and DOD are flagged.
#'   \item Any column with a blank or whitespace-only header contains data.
#'     Completely empty unnamed columns are silently dropped and do not trigger
#'     this error.
#' }
#'
#' @param data A data frame or tibble as loaded from a CSV or XLSX file (e.g.
#'   via \code{readr::read_csv()} or \code{readxl::read_excel()}). All
#'   character columns are processed; numeric and logical columns are passed
#'   through unchanged by the string-cleaning steps.
#'
#' @return A named list with three elements:
#' \describe{
#'   \item{\code{clean_data}}{A tibble containing the fully cleaned dataset,
#'     ready to pass to \code{ternG()} or \code{ternD()}.}
#'   \item{\code{sparse_rows}}{A tibble of rows from \code{clean_data} where
#'     more than 50\% of values are \code{NA}. These rows are \emph{retained}
#'     in \code{clean_data} but extracted here for optional review or download.
#'     An empty tibble if no sparse rows exist.}
#'   \item{\code{feedback}}{A named list of feedback items. Each element is
#'     \code{NULL} if the corresponding transformation was not triggered, or a
#'     value describing what changed:
#'     \describe{
  #'       \item{\code{string_na_converted}}{A named list with elements
  #'         \code{total} (integer count of values converted) and \code{cols}
  #'         (character vector of affected column names), or \code{NULL} if no
  #'         string NA values were found.}
#'       \item{\code{blank_rows_removed}}{A named list with elements
#'         \code{count} (integer) and \code{row_indices} (integer vector of
#'         original row positions removed), or \code{NULL} if none.}
#'       \item{\code{sparse_rows_flagged}}{A named list with elements
#'         \code{count} (integer) and \code{row_indices} (integer vector of
#'         row positions in \code{clean_data} with >50\% missingness),
#'         or \code{NULL} if none.}
#'       \item{\code{case_normalized_vars}}{A named list with elements
#'         \code{cols} (character vector of affected column names) and
#'         \code{detail} (a named list per column, each with
#'         \code{changed_from} and \code{changed_to} character vectors
#'         showing the exact value changes), or \code{NULL} if none.}  #'       \item{\code{dropped_empty_cols}}{Character vector of column names
  #'         (or \code{""} for unnamed columns) that were dropped because they
  #'         were 100\% empty, or \code{NULL} if none.}
  #'       \item{\code{date_cols_detected}}{Character vector of column names
  #'         that appear to contain date values — either R \code{Date}/\code{POSIXct}
  #'         types (from Excel) or character columns where ≥80\% of non-NA values
  #'         match a common date pattern (from CSV). These columns are \emph{not}
  #'         dropped automatically; the caller should decide whether to exclude
  #'         them or keep them as categorical variables.}
  #'     }}
#' }
#'
#' @seealso \code{\link{ternG}} for grouped comparisons, \code{\link{ternD}} for descriptive statistics.
#'
#' @examples
#' \donttest{
#' # Load a messy CSV and preprocess it
#' path   <- system.file("extdata/csv", "tern_colon_messy.csv",
#'                       package = "TernTables")
#' raw    <- read.csv(path, stringsAsFactors = FALSE)
#' result <- ternP(raw)
#'
#' # Access cleaned data
#' result$clean_data
#'
#' # Review preprocessing feedback
#' result$feedback
#'
#' # Sparse rows flagged (>50% missing), retained but not removed
#' result$sparse_rows
#' }
#'
#' @export
ternP <- function(data) {

  # --- Input validation -------------------------------------------------------
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame or tibble.", call. = FALSE)
  }
  if (nrow(data) == 0) {
    stop("`data` has no rows.", call. = FALSE)
  }
  if (ncol(data) == 0) {
    stop("`data` has no columns.", call. = FALSE)
  }

  # --- Hard stops (before any cleaning) ---------------------------------------
  .check_phi(data)
  .check_unnamed_cols(data)

  # --- Initialise feedback trackers -------------------------------------------
  feedback <- list(
    string_na_converted  = NULL,
    blank_rows_removed   = NULL,
    sparse_rows_flagged  = NULL,
    case_normalized_vars = NULL,
    dropped_empty_cols   = NULL,
    date_cols_detected   = NULL
  )

  # ---------------------------------------------------------------------------
  # Step 0: Detect date columns
  #   Flags columns that are R Date/POSIXct types (from Excel) or character
  #   columns where ≥80% of non-NA values match a common date pattern (CSV).
  #   Detected columns are reported in feedback but NOT dropped here — the
  #   caller decides what to do with them (exclude or keep as categorical).
  # ---------------------------------------------------------------------------
  .date_patterns <- c(
    "^\\d{4}-\\d{2}-\\d{2}$",                          # YYYY-MM-DD
    "^\\d{2}/\\d{2}/\\d{4}$",                          # MM/DD/YYYY or DD/MM/YYYY
    "^\\d{2}-\\d{2}-\\d{4}$",                          # MM-DD-YYYY or DD-MM-YYYY
    "^\\d{1,2}[/-]\\d{1,2}[/-]\\d{2,4}$",             # M/D/YY variants
    "^\\d{1,2}\\s+[A-Za-z]{3,9}\\s+\\d{4}$",          # 15 March 2021
    "^[A-Za-z]{3,9}\\s+\\d{1,2},?\\s+\\d{4}$",        # March 15, 2021
    "^\\d{1,2}-[A-Za-z]{3}-\\d{4}$",                   # 15-Mar-2021
    "^[A-Za-z]{3}-\\d{1,2}-\\d{4}$",                   # Mar-15-2021
    "^\\d{8}$"                                          # YYYYMMDD compact
  )

  .is_date_type  <- function(col) inherits(col, c("Date", "POSIXct", "POSIXlt"))
  .is_date_chars <- function(col) {
    if (!is.character(col)) return(FALSE)
    non_na <- col[!is.na(col)]
    if (length(non_na) == 0) return(FALSE)
    matched <- vapply(.date_patterns, function(p) {
      mean(grepl(p, non_na)) >= 0.80
    }, logical(1))
    any(matched)
  }

  date_col_names <- names(data)[vapply(names(data), function(nm) {
    .is_date_type(data[[nm]]) || .is_date_chars(data[[nm]])
  }, logical(1))]

  if (length(date_col_names) > 0) {
    feedback$date_cols_detected <- date_col_names
  }

  # ---------------------------------------------------------------------------
  # Step 1: Convert string NA values to NA
  #   Common text representations of missing data in any character column are
  #   coerced to NA. Matching is case-insensitive via a vectorised tolower()
  #   comparison so the list below covers all capitalisation variants.
  # ---------------------------------------------------------------------------
  string_na_values <- c(
    "na", "n/a", "n\\a", "nan",
    "missing", "unknown", "unk",
    "not available", "not applicable",
    "none", "null", "nil",
    "-", "--", "---",
    ".", "?", "99", "999", "9999", "-9", "-99", "-999"
  )

  # Count total occurrences and record which columns are affected before cleaning.
  # Matching is case-insensitive: compare tolower(value) against the lowercase list.
  .is_sna <- function(x) !is.na(x) & (tolower(trimws(x)) %in% string_na_values)

  sna_cols  <- Filter(function(nm) {
    col <- data[[nm]]
    is.character(col) && any(.is_sna(col))
  }, names(data))
  sna_total <- sum(vapply(sna_cols, function(nm) {
    sum(.is_sna(data[[nm]]))
  }, integer(1)))

  data <- dplyr::mutate(
    data,
    dplyr::across(
      dplyr::where(is.character),
      ~ dplyr::if_else(.is_sna(.), NA_character_, .)
    )
  )

  if (length(sna_cols) > 0) {
    feedback$string_na_converted <- list(total = sna_total, cols = sna_cols)
  }

  # ---------------------------------------------------------------------------
  # Step 2: Trim leading and trailing whitespace from all character columns
  # ---------------------------------------------------------------------------
  data <- dplyr::mutate(
    data,
    dplyr::across(dplyr::where(is.character), stringr::str_trim)
  )

  # ---------------------------------------------------------------------------
  # Step 3: Drop 100% empty columns (including empty-named ones)
  #   Named empty columns are reported in feedback; they are soft-dropped.
  #   This is distinct from the hard stop in .check_unnamed_cols(), which
  #   fires only for unnamed columns that contain data.
  # ---------------------------------------------------------------------------
  empty_col_idx  <- which(vapply(data, function(col) all(is.na(col)), logical(1)))

  if (length(empty_col_idx) > 0) {
    empty_col_names <- names(data)[empty_col_idx]
    feedback$dropped_empty_cols <- empty_col_names
    data <- data[, -empty_col_idx, drop = FALSE]
  }

  # ---------------------------------------------------------------------------
  # Step 4: Remove completely blank rows (every cell is NA)
  # ---------------------------------------------------------------------------
  blank_row_idx <- which(apply(data, 1, function(r) all(is.na(r))))
  data          <- dplyr::filter(data, !dplyr::if_all(dplyr::everything(), is.na))
  n_removed     <- length(blank_row_idx)

  if (n_removed > 0) {
    feedback$blank_rows_removed <- list(count = n_removed, row_indices = blank_row_idx)
  }

  # ---------------------------------------------------------------------------
  # Step 5: Case-inconsistency normalization
  #   For each character column, if any values are letter-for-letter identical
  #   when lowercased (i.e. only differ by capitalization), collapse them all
  #   to title case. Track which columns were affected.
  # ---------------------------------------------------------------------------
  collapsed_detail <- list()

  data <- dplyr::mutate(
    data,
    dplyr::across(
      dplyr::where(is.character),
      ~ {
        lowered <- stringr::str_to_lower(.)
        if (dplyr::n_distinct(lowered, na.rm = TRUE) <
            dplyr::n_distinct(.,      na.rm = TRUE)) {
          col_nm      <- dplyr::cur_column()
          unique_vals <- unique(.[!is.na(.)])
          title_vals  <- stringr::str_to_title(unique_vals)
          changed_idx <- which(unique_vals != title_vals)
          collapsed_detail[[col_nm]] <<- list(
            changed_from = unique_vals[changed_idx],
            changed_to   = title_vals[changed_idx]
          )
          stringr::str_to_title(.)
        } else {
          .
        }
      }
    )
  )

  if (length(collapsed_detail) > 0) {
    feedback$case_normalized_vars <- list(
      cols   = names(collapsed_detail),
      detail = collapsed_detail
    )
  }

  # ---------------------------------------------------------------------------
  # Step 6: Identify sparse rows (>50% missing values)
  #   Sparse rows are RETAINED in clean_data but extracted separately so the
  #   caller can surface them to the user (e.g. as a download button).
  # ---------------------------------------------------------------------------
  sparse_row_idx <- which(rowSums(is.na(data)) > (ncol(data) / 2))
  sparse_rows    <- data[sparse_row_idx, , drop = FALSE]
  n_sparse       <- length(sparse_row_idx)

  if (n_sparse > 0) {
    feedback$sparse_rows_flagged <- list(count = n_sparse, row_indices = sparse_row_idx)
  }

  # ---------------------------------------------------------------------------
  # Return
  # ---------------------------------------------------------------------------
  result <- list(
    clean_data  = tibble::as_tibble(data),
    sparse_rows = tibble::as_tibble(sparse_rows),
    feedback    = feedback
  )
  class(result) <- "ternP_result"

  # Emit feedback immediately — fires whether or not the result is assigned.
  .ternP_emit_feedback(result)

  result
}

# ── Internal feedback emitter ────────────────────────────────────────────────
# Shared by ternP() (runs automatically on every call) and print.ternP_result
# (re-displays the summary on demand). Kept internal — not exported.

.ternP_emit_feedback <- function(x) {
  fb      <- x$feedback
  n_clean <- nrow(x$clean_data)
  n_cols  <- ncol(x$clean_data)

  cli::cli_rule("ternP Preprocessing Summary")

  clean_flag <- all(vapply(fb, is.null, logical(1)))

  if (clean_flag) {
    cli::cli_alert_success(
      "No transformations required. Data passed through unchanged."
    )
  } else {
    if (!is.null(fb$date_cols_detected)) {
      dc <- fb$date_cols_detected
      cli::cli_alert_warning(
        "{length(dc)} potential date column{?s} detected: {.val {dc}}. \\
Review these columns — date values cannot be summarised statistically. \\
Exclude them or keep as categorical."
      )
    }
    if (!is.null(fb$string_na_converted)) {
      sna <- fb$string_na_converted
      cli::cli_alert_info(
        "{sna$total} string NA value{?s} converted to {.code NA} across \
{length(sna$cols)} column{?s}: {.val {sna$cols}}."
      )
    }
    if (!is.null(fb$dropped_empty_cols)) {
      n_d <- length(fb$dropped_empty_cols)
      cli::cli_alert_info(
        "{n_d} empty column{?s} dropped: {.val {fb$dropped_empty_cols}}."
      )
    }
    if (!is.null(fb$blank_rows_removed)) {
      br <- fb$blank_rows_removed
      cli::cli_alert_info(
        "{br$count} completely blank row{?s} removed. Original row number{?s}: {br$row_indices}."
      )
    }
    if (!is.null(fb$case_normalized_vars)) {
      cn  <- fb$case_normalized_vars
      n_n <- length(cn$cols)
      cli::cli_alert_info(
        "Capitalization normalized in {n_n} column{?s}: {.val {cn$cols}}."
      )
    }
    if (!is.null(fb$sparse_rows_flagged)) {
      sp <- fb$sparse_rows_flagged
      cli::cli_alert_warning(
        "{sp$count} sparse row{?s} flagged (>50% missing, retained in clean_data). Row number{?s}: {sp$row_indices}."
      )
    }
  }

  cli::cli_rule()
  cli::cli_alert_info(
    "Cleaned data: {n_clean} row{?s} \u00d7 {n_cols} column{?s}."
  )
  invisible(x)
}

# ── S3 print method ─────────────────────────────────────────────────────────

#' Print method for ternP_result objects
#'
#' Re-displays the preprocessing summary for a \code{ternP_result} object.
#' Note that \code{\link{ternP}} already emits this summary automatically at
#' the time it is called, so this method is most useful for reviewing the
#' summary after the fact (e.g. typing \code{result} at the console later
#' in a session).
#'
#' @param x A \code{ternP_result} object returned by \code{\link{ternP}}.
#' @param ... Currently unused; included for S3-method compatibility.
#' @return Invisibly returns \code{x}.
#' @method print ternP_result
#' @export
print.ternP_result <- function(x, ...) {
  .ternP_emit_feedback(x)
}
