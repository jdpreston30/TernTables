#' Generate descriptive summary table (optionally normality-aware)
#'
#' Creates a descriptive summary table with a single "Total" column format.
#' By default (\code{consider_normality = TRUE}), continuous variables are shown
#' as mean +/- SD or median [IQR] based on Shapiro-Wilk testing. This can be
#' overridden via \code{consider_normality} and \code{force_ordinal}.
#'
#' @param data Tibble with variables.
#' @param vars Character vector of variables to summarize. Defaults to all except \code{exclude_vars}.
#' @param exclude_vars Character vector to exclude from the summary.
#' @param force_ordinal Character vector of variables to treat as ordinal (i.e., use median [IQR]) 
#'   regardless of the \code{consider_normality} setting. This parameter takes priority over 
#'   normality testing when \code{consider_normality = TRUE}.
#' @param output_xlsx Optional Excel filename to export the table.
#' @param output_docx Optional Word filename to export the table.
#' @param consider_normality Logical; if \code{TRUE} (default), uses Shapiro-Wilk test to choose between
#'   mean +/- SD (for normal data) vs median [IQR] (for non-normal data) for numeric variables.
#'   If \code{FALSE}, defaults to mean +/- SD for all numeric variables unless specified in
#'   \code{force_ordinal}.
#' @param print_normality Logical; if \code{TRUE}, includes Shapiro-Wilk P values as an
#'   additional column in the output. Default is \code{FALSE}.
#' @param round_intg Logical; if \code{TRUE}, rounds all means, medians, IQRs, and standard 
#'   deviations to nearest integer (0.5 rounds up). Default is \code{FALSE}.
#' @param smart_rename Logical; if \code{TRUE}, automatically cleans variable names and
#'   subheadings for publication-ready output using built-in rule-based pattern matching for
#'   common medical abbreviations and prefixes. Default is \code{TRUE}.
#' @param insert_subheads Logical; if \code{TRUE} (default), creates a hierarchical structure with a header row and
#'   indented sub-category rows for categorical variables with 3 or more levels. Binary variables
#'   (Y/N, YES/NO, or numeric 1/0 -- which are auto-detected and treated as Y/N) are always displayed
#'   as a single row showing the positive/yes count regardless of this setting. Two-level categorical
#'   variables whose values are not Y/N, YES/NO, or 1/0 (e.g. Male/Female) use the hierarchical
#'   sub-row format, showing both levels as indented rows.
#'   If \code{FALSE}, all categorical variables use a single-row flat format. Default is \code{TRUE}.
#' @param factor_order Character; controls the ordering of factor levels in the output. If
#'   \code{"levels"} (default), respects the original factor level ordering as defined in the data;
#'   if the variable is not a factor, falls back to frequency ordering. If \code{"frequency"},
#'   orders levels by decreasing frequency (most common first).
#' @param methods_doc Logical; if \code{TRUE} (default), generates a methods document
#'   describing the statistical presentation used. The document contains boilerplate
#'   text for all three table types so the relevant section can be copied directly
#'   into a manuscript.
#' @param methods_filename Character; filename for the methods document.
#'   Default is \code{"TernTables_methods.docx"}.
#' @param category_start Named character vector specifying where to insert category headers.
#'   Names are the header label text to display; values are the anchor variable -- either the
#'   original column name (e.g. \code{"Age_Years"}) or the cleaned display name
#'   (e.g. \code{"Age (yr)"}). Both forms are accepted.
#'   Example: \code{c("Demographics" = "Age_Years", "Clinical Measures" = "bmi")}.
#'   Default is \code{NULL} (no category headers).
#' @param table_font_size Numeric; font size for Word document output tables. Default is 9.
#' @param manual_italic_indent Character vector of display variable names (post-cleaning) that should be
#'   formatted as italicized and indented in Word output -- matching the appearance of factor sub-category
#'   rows. Has no effect on the returned tibble; only applies when \code{output_docx} is specified.
#'   Default is \code{NULL}.
#' @param manual_underline Character vector of display variable names (post-cleaning) that should be
#'   formatted as underlined in Word output -- matching the appearance of multi-category variable headers.
#'   Has no effect on the returned tibble; only applies when \code{output_docx} is specified.
#'   Default is \code{NULL}.
#'
#' @details
#' The function always returns a tibble with a single \code{Total (N = n)} column format, regardless of the
#' \code{consider_normality} setting. The behavior for numeric variables follows this priority:
#' \enumerate{
#'   \item Variables in \code{force_ordinal}: Always use median [IQR]
#'   \item When \code{consider_normality = TRUE}: Use Shapiro-Wilk test to choose format
#'   \item When \code{consider_normality = FALSE}: Default to mean +/- SD
#' }
#'
#' For categorical variables, the function shows frequencies and percentages. When
#' \code{insert_subheads = TRUE}, categorical variables with 3 or more levels are displayed with
#' hierarchical formatting (main variable as header, levels as indented sub-rows). Binary variables
#' (Y/N, YES/NO, or numeric 1/0 auto-detected as Y/N) always use a single-row format showing
#' only the positive/yes count, regardless of this setting. Two-level categorical variables whose
#' values are not Y/N, YES/NO, or 1/0 (e.g. Male/Female) also use the hierarchical sub-row format.
#'
#' @return A tibble with one row per variable (multi-row for factors), containing:
#' \describe{
#'   \item{Variable}{Variable names with appropriate indentation}
#'   \item{Total (N = n)}{Summary statistics (mean +/- SD, median [IQR], or n (\%) as appropriate)}
#'   \item{SW_p}{Shapiro-Wilk P values (only if \code{print_normality = TRUE})}
#' }
#'
#' @examples
#' data(tern_colon)
#'
#' # Basic descriptive summary
#' ternD(tern_colon, exclude_vars = c("ID"))
#'
#' # With normality-aware formatting and category section headers
#' ternD(tern_colon, exclude_vars = c("ID"),
#'       category_start = c("Patient Demographics"  = "Age (yr)",
#'                          "Tumor Characteristics" = "Positive Lymph Nodes (n)"))
#'
#' # Force specific variables to ordinal (median [IQR]) display
#' ternD(tern_colon, exclude_vars = c("ID"),
#'       force_ordinal = c("Positive_Lymph_Nodes_n"))
#'
#' # Export to Word (writes a file -- not run during automated checks)
#' \dontrun{
#' ternD(tern_colon,
#'       exclude_vars     = c("ID"),
#'       output_docx      = file.path(tempdir(), "descriptive.docx"),
#'       category_start   = c("Patient Demographics"  = "Age (yr)",
#'                            "Surgical Findings"     = "Colonic Obstruction",
#'                            "Tumor Characteristics" = "Positive Lymph Nodes (n)",
#'                            "Outcomes"              = "Recurrence"))
#' }
#' @export
ternD <- function(data, vars = NULL, exclude_vars = NULL, force_ordinal = NULL,
                  output_xlsx = NULL, output_docx = NULL,
                  consider_normality = TRUE, print_normality = FALSE,
                  round_intg = FALSE, smart_rename = TRUE, insert_subheads = TRUE,
                  factor_order = "levels", methods_doc = TRUE,
                  methods_filename = "TernTables_methods.docx", category_start = NULL,
                  table_font_size = 9, manual_italic_indent = NULL, manual_underline = NULL) {
  stopifnot(is.data.frame(data))
  
  # Store total N for column header
  total_n <- nrow(data)

  # Helper function for proper rounding (0.5 always rounds up)
  round_up_half <- function(x, digits = 0) {
    if (digits == 0) {
      floor(x + 0.5)
    } else {
      factor <- 10^digits
      floor(x * factor + 0.5) / factor
    }
  }

  if (is.null(vars)) {
    vars <- setdiff(names(data), exclude_vars)
  }

  fmt_mean_sd <- function(x) {
    m <- mean(x, na.rm = TRUE)
    s <- stats::sd(x, na.rm = TRUE)
    if (round_intg) {
      paste0(round_up_half(m, 0), " \u00b1 ", round_up_half(s, 0))
    } else {
      paste0(round(m, 1), " \u00b1 ", round(s, 1))
    }
  }

  fmt_median_iqr <- function(x) {
    q <- stats::quantile(x, probs = c(0.25, 0.5, 0.75), na.rm = TRUE, names = FALSE)
    if (round_intg) {
      paste0(round_up_half(q[2], 0), " [", round_up_half(q[1], 0), "\u2013", round_up_half(q[3], 0), "]")
    } else {
      paste0(round(q[2], 1), " [", round(q[1], 1), "\u2013", round(q[3], 1), "]")
    }
  }

  shapiro_p <- function(x) {
    x <- x[!is.na(x)]
    # Shapiro requires n >= 3 and not all equal
    if (length(x) < 3 || stats::var(x) == 0) {
      return(NA_real_)
    }
    out <- tryCatch(stats::shapiro.test(x)$p.value, error = function(e) NA_real_)
    out
  }

  summarize_variable <- function(df, var) {
    v <- df[[var]]

    # Auto-detect binary numeric (0/1) as categorical Y/N
    if (is.numeric(v) && length(unique(stats::na.omit(v))) == 2 && all(stats::na.omit(v) %in% c(0, 1))) {
      v <- factor(v, levels = c(0, 1), labels = c("N", "Y"))
    }

    # ---------- CATEGORICAL ----------
    if (is.factor(v) || is.character(v) || is.logical(v)) {
      v <- factor(v) # ensure levels
      tab <- table(v, useNA = "no")
      if (length(tab) == 0) {
        # all missing
        out <- tibble::tibble(Variable = .clean_variable_name_for_header(var), .indent = 2, Summary = "0 (0%)")
        if (print_normality) out$SW_p <- NA_real_
        return(out)
      }
      pct <- round(100 * prop.table(tab))
      
      # Sort levels by frequency (descending order - most common first) or respect factor levels
      if (factor_order == "levels" && is.factor(v)) {
        # Respect original factor level ordering
        sorted_levels <- levels(v)
        # Filter to only include levels that actually appear in the data
        sorted_levels <- sorted_levels[sorted_levels %in% names(tab)]
      } else {
        # Default: sort by frequency (descending order)
        sorted_levels <- names(sort(tab, decreasing = TRUE))
      }
      
      # Determine if this should use simple format or hierarchical subheads
      # Always use simple format for Y/N variables or when insert_subheads is FALSE
      # Otherwise use hierarchical format for multi-level categorical variables
      upper_levels   <- toupper(sorted_levels)
      is_yes_no      <- all(c("Y", "N")   %in% upper_levels)
      is_yes_no_full <- all(c("YES", "NO") %in% upper_levels)
      is_binary <- is_yes_no || is_yes_no_full
      use_hierarchical <- !is_binary && insert_subheads && length(sorted_levels) > 1
      
      if (use_hierarchical) {
        # Create header row for the main variable
        header_row <- tibble::tibble(
          Variable = .clean_variable_name_for_header(var),
          .indent  = 2L,
          Summary  = ""
        )
        if (print_normality) header_row$SW_p <- NA_real_
        
        # Create sub-category rows (indented)
        sub_rows <- lapply(sorted_levels, function(level) {
          n <- as.integer(tab[[level]])
          p <- pct[[level]]
          row <- tibble::tibble(
            Variable = level,
            .indent  = 6L,
            Summary  = paste0(n, " (", p, "%)"))
          if (print_normality) row$SW_p <- NA_real_
          return(row)
        })
        
        # Combine header and sub-rows
        out <- dplyr::bind_rows(list(header_row), sub_rows)
      } else {
        # For Y/N variables or when insert_subheads=FALSE, use simple format with 2-space indentation
        # For Y/N, show only the "Y" level; for Yes/No, show only "Yes"; for others, show most common level
        if (is_yes_no) {
          # Y/N variables (any case): show only the positive level
          ref_level <- sorted_levels[toupper(sorted_levels) == "Y"]
        } else if (is_yes_no_full) {
          # Yes/No variables (any case): show only the positive level
          ref_level <- sorted_levels[toupper(sorted_levels) == "YES"]
        } else {
          # Other variables: show most common level or first level based on factor_order
          if (factor_order == "levels" && is.factor(v)) {
            # Use the first level that actually appears in the data
            available_levels <- levels(v)[levels(v) %in% names(tab)]
            ref_level <- available_levels[1]
          } else {
            # Default: show most common level
            ref_level <- sorted_levels[1]  # Already sorted by frequency
          }
        }
        
        row <- tibble::tibble(
          Variable = .clean_variable_name_for_header(var),
          .indent  = 2L,
          Summary  = paste0(as.integer(tab[[ref_level]]), " (", pct[[ref_level]], "%)")
        )
        if (print_normality) row$SW_p <- NA_real_
        rows <- list(row)
        out <- dplyr::bind_rows(rows)
      }
      return(out)
    }

    # ---------- NUMERIC ----------
    x <- suppressWarnings(as.numeric(v))
    # Always run Shapiro-Wilk for informational reporting regardless of consider_normality
    sw <- shapiro_p(x)
    norm_tested <<- norm_tested + 1
    if (is.na(sw) || sw < 0.05) norm_failed <<- norm_failed + 1

    # Check if variable is forced to be ordinal
    if (!is.null(force_ordinal) && var %in% force_ordinal) {
      # Force ordinal: use median/IQR regardless of consider_normality setting
      summary_str <- fmt_median_iqr(x)
    } else if (isTRUE(consider_normality)) {
      # choose mean +- SD if normal; else median [IQR]
      if (!is.na(sw) && sw > 0.05) {
        summary_str <- fmt_mean_sd(x)
      } else {
        summary_str <- fmt_median_iqr(x)
      }
    } else {
      # Default behavior when consider_normality = FALSE: use mean +/- SD
      summary_str <- fmt_mean_sd(x)
    }
    
    out <- tibble::tibble(
      Variable = .clean_variable_name_for_header(var),
      .indent  = 2L,
      Summary  = summary_str
    )
    if (print_normality) out$SW_p <- sw
    return(out)
  }

  norm_tested <- 0
  norm_failed <- 0
  out_tbl <- dplyr::bind_rows(lapply(vars, function(v) summarize_variable(data, v)))

  # -- Report normality results -----------------------------------------------
  if (norm_tested > 0) {
    norm_passed <- norm_tested - norm_failed
    passed_pct  <- round((norm_passed / norm_tested) * 100, 1)
    cli::cli_rule(left = "Normality Assessment (Shapiro-Wilk) \u2014 ternD")
    if (isTRUE(consider_normality)) {
      cli::cli_alert_info("{norm_passed} of {norm_tested} continuous variable{?s} normally distributed ({passed_pct}%)")
      cli::cli_bullets(c(
        ">" = "Normally distributed \u2192 mean \u00b1 SD",
        ">" = "Non-normal           \u2192 median [IQR]"
      ))
    } else {
      cli::cli_alert_info("{norm_passed} of {norm_tested} continuous variable{?s} normally distributed ({passed_pct}%)")
      cli::cli_alert_warning("consider_normality = FALSE: all continuous variables displayed as mean \u00b1 SD")
    }
  }

  # Rename Summary column to match ternG Total column format
  names(out_tbl)[names(out_tbl) == "Summary"] <- paste0("Total\n(N = ", total_n, ")")
  
  # Apply smart variable name cleaning if requested
  if (smart_rename) {
    for (i in seq_len(nrow(out_tbl))) {
      current_var <- out_tbl$Variable[i]

      if (grepl("^\\s+", current_var)) {
        padding <- stringr::str_extract(current_var, "^\\s+")
        trimmed_var <- trimws(current_var)

        if (grepl(": [A-Za-z0-9]+$", trimmed_var)) {
          parts <- strsplit(trimmed_var, ": ")[[1]]
          cleaned_var <- paste0(.apply_cleaning_rules(parts[1]), ": ", parts[2])
        } else {
          cleaned_var <- .apply_cleaning_rules(trimmed_var)
        }

        out_tbl$Variable[i] <- paste0(padding, cleaned_var)
      } else {
        out_tbl$Variable[i] <- .apply_cleaning_rules(current_var)
      }
    }
  }

  # Replace "0 (NaN%)" with "-" for structurally impossible cells
  # (e.g. a subgroup that cannot logically have any observations in a given column)
  out_tbl <- out_tbl %>%
    dplyr::mutate(dplyr::across(dplyr::where(is.character), ~ gsub("0 \\(NaN%\\)", "-", .x)))

  if (!is.null(output_xlsx)) export_to_excel(out_tbl, output_xlsx)
  if (!is.null(output_docx)) word_export(out_tbl, output_docx, font_size = table_font_size,
                                         category_start = category_start,
                                         manual_italic_indent = manual_italic_indent,
                                         manual_underline = manual_underline)
  if (methods_doc) write_methods_doc(out_tbl, methods_filename, source = "ternD")

  out_tbl <- dplyr::select(out_tbl, -dplyr::any_of(".indent"))
  out_tbl
}
