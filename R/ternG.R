#' Generate grouped summary table with appropriate statistical tests
#'
#' Creates a grouped summary table with optional statistical testing for group
#' comparisons. Supports numeric and categorical variables; numeric variables
#' can be treated as ordinal via \code{force_ordinal}. Includes options to
#' calculate P values and odds ratios. For descriptive
#' (ungrouped) tables, use \code{ternD}.
#'
#' @param data Tibble containing all variables.
#' @param vars Character vector of variables to summarize. Defaults to all except \code{group_var} and \code{exclude_vars}.
#' @param exclude_vars Character vector of variable(s) to exclude. \code{group_var} is automatically excluded.
#' @param group_var Character, the grouping variable (factor or character with >=2 levels).
#' @param force_ordinal Character vector of variables to treat as ordinal (i.e., use medians/IQR and nonparametric tests).
#' @param force_continuous Character vector of variables to force treatment as continuous (mean \eqn{\pm} SD and
#'   parametric tests), bypassing the automatic binary \code{0/1} detection that would otherwise convert
#'   them to categorical Y/N. Useful when a numeric variable with only two unique values (e.g. \code{0}/\code{1}
#'   dose levels) should be analysed as a continuous measurement rather than a dichotomous category.
#'   Takes priority over automatic type detection but does not override \code{force_ordinal} (if a variable
#'   appears in both, \code{force_ordinal} wins). Default is \code{NULL}.
#' @param group_order Optional character vector to specify a custom group level order.
#' @param output_xlsx Optional filename to export the table as an Excel file.
#' @param output_docx Optional filename to export the table as a Word document.
#' @param OR_col Logical; if \code{TRUE}, adds unadjusted odds ratios with 95\% CI for binary categorical variables
#'   (Y/N, YES/NO, or numeric 0/1) and two-level categorical variables (e.g. Male/Female). For
#'   two-level categoricals displayed with sub-rows, the reference level (factor level 1, or
#'   alphabetical first for non-factors) shows \code{"1.00 (ref.)"}; the non-reference level
#'   shows the computed OR with 95\% CI. Variables with three or more levels show \code{"-"}.
#'   Only valid when \code{group_var} has exactly 2 levels; an error is raised for 3+ group comparisons.
#'   Default is \code{FALSE}.
#' @param OR_method Character; controls how odds ratios are calculated when \code{OR_col = TRUE}.
#'   If \code{"dynamic"} (default), uses Fisher's exact method when any expected cell count is < 5
#'   (Cochran criterion), otherwise uses the Wald method. If \code{"wald"}, forces the Wald method
#'   regardless of expected cell counts.
#' @param consider_normality Character or logical; controls how continuous variables are routed to
#'   parametric vs. non-parametric tests.
#'   \code{"ROBUST"} (default) applies a four-gate decision consistent with standard biostatistical
#'   practice: (1) any group n < 3 is a conservative fail-safe to non-parametric; (2) absolute skewness
#'   > 2 in any group routes to non-parametric regardless of sample size (catches LOS, counts, etc.);
#'   (3) all groups n \eqn{\geq} 30 routes to parametric via the Central Limit Theorem; (4) otherwise
#'   Shapiro-Wilk p > 0.05 in all groups routes to parametric. Normal variables use mean \eqn{\pm} SD
#'   and Welch t-test (2 groups) or Welch ANOVA (3+ groups); non-normal variables use median [IQR] and
#'   Wilcoxon rank-sum (2 groups) or Kruskal-Wallis (3+ groups).
#'   If \code{TRUE}, uses Shapiro-Wilk alone (p > 0.05 in all groups = normal). Conservative at large n.
#'   If \code{FALSE}, all numeric variables are treated as normally distributed regardless of distribution.
#'   If \code{"FORCE"}, all numeric variables are treated as non-normal (median [IQR], nonparametric tests).
#' @param print_normality Logical; if \code{TRUE}, includes Shapiro-Wilk P values in the output. Default is \code{FALSE}.
#' @param show_test Logical; if \code{TRUE}, includes the statistical test name as a column in the output. Default is \code{FALSE}.
#' @param p_digits Integer; number of decimal places for P values (default 3).
#' @param round_intg Logical; if \code{TRUE}, rounds all means, medians, IQRs, and standard deviations to nearest integer (0.5 rounds up). Default is \code{FALSE}.
#' @param smart_rename Logical; if \code{TRUE}, automatically cleans variable names and subheadings for publication-ready output using built-in rule-based pattern matching for common medical abbreviations and prefixes. Default is \code{TRUE}.
#' @param insert_subheads Logical; if \code{TRUE} (default), creates a hierarchical structure with a header row and
#'   indented sub-category rows for categorical variables with 3 or more levels. Binary variables
#'   (Y/N, YES/NO, or numeric 1/0 -- which are auto-detected and treated as Y/N) are always displayed
#'   as a single row showing the positive/yes count regardless of this setting. Two-level categorical
#'   variables whose values are not Y/N, YES/NO, or 1/0 (e.g. Male/Female) use the hierarchical
#'   sub-row format, showing both levels as indented rows.
#'   If \code{FALSE}, all categorical variables use a single-row flat format. Default is \code{TRUE}.
#' @param factor_order Character; controls the ordering of factor levels in the output.
#'   \code{"mixed"} (default) applies level-aware ordering for two-level categorical variables and
#'   frequency ordering for variables with three or more levels: for any factor, factor level order
#'   is always respected regardless of the number of levels; for non-factor two-level variables
#'   (e.g. Male/Female), levels are sorted alphabetically; for non-factor variables with three or
#'   more levels, levels are sorted by decreasing frequency.
#'   \code{"levels"} respects the original factor level ordering for all variables; if the variable
#'   is not a factor, falls back to frequency ordering.
#'   \code{"frequency"} orders all levels by decreasing frequency (most common first).
#' @param table_font_size Numeric; font size for Word document output tables. Default is 9.
#' @param methods_doc Logical; if \code{TRUE} (default), generates a methods document describing the statistical tests used.
#' @param methods_filename Character; filename for the methods document. Default is \code{"TernTables_methods.docx"}.
#' @param category_start Named character vector specifying where to insert category headers.
#'   Names are the header label text to display; values are the anchor variable -- either the
#'   original column name (e.g. \code{"Age_Years"}) or the cleaned display name
#'   (e.g. \code{"Age (yr)"}). Both forms are accepted.
#'   Example: \code{c("Demographics" = "Age_Years", "Clinical" = "bmi")}.
#'   Default is \code{NULL} (no category headers).
#' @param plain_header Named character vector, same interface as \code{category_start}. Names are
#'   the label text; values are the anchor variable to insert before. Inserts a label-only row
#'   with underline formatting and no bold, merge, or border treatments. Default \code{NULL}.
#' @param manual_italic_indent Character vector of display variable names (post-cleaning) that should be
#'   formatted as italicized and indented in Word output -- matching the appearance of factor sub-category
#'   rows. Has no effect on the returned tibble; only applies when \code{output_docx} is specified or when
#'   the tibble is passed to \code{word_export}.
#' @param manual_underline Character vector of display variable names (post-cleaning) that should be
#'   formatted as underlined in Word output -- matching the appearance of multi-category variable headers.
#'   Has no effect on the returned tibble; only applies when \code{output_docx} is specified or when
#'   the tibble is passed to \code{word_export}.
#' @param show_total Logical; if \code{TRUE}, adds a "Total" column showing the aggregate summary statistic across all groups (e.g., for a publication Table 1 that includes both per-group and overall columns). Default is \code{TRUE}.
#' @param table_caption Optional character string for a table caption to display above the table in
#'   the Word document. Rendered as size 11 Arial bold, single-spaced with a small gap before the table.
#'   Default is \code{NULL} (no caption).
#'   Example: \code{"Table 2. Comparison of recurrence vs. no recurrence."}
#' @param table_footnote Optional character string for a footnote to display below the table in the
#'   Word document. Rendered as size 6 Arial italic with a double-bar border above and below.
#'   Default is \code{NULL} (no footnote).
#' @param abbreviation_footnote Optional character string listing abbreviations. Always printed
#'   first in the footnote block. Default \code{NULL}.
#' @param variable_footnote Optional named character vector. Names are display variable names
#'   (case-insensitive); values are the footnote definition text. Each variable gets the next
#'   symbol appended to its name in the table, and the footnote block lists each definition
#'   below the abbreviation line. Default \code{NULL}.
#' @param index_style Character; \code{"symbols"} (default) uses *, dagger, double-dagger ...
#'   \code{"alphabet"} uses Unicode superscript letters. See \code{word_export} for details.
#' @param line_break_header Logical; if \code{TRUE} (default), column headers are wrapped with
#'   \code{\\n} -- group names break on spaces, sample size counts move to a second line, and
#'   the first column header reads \code{"Category / Variable"}. Set to \code{FALSE} to suppress
#'   all header line breaks. Can also be set package-wide via
#'   \code{options(TernTables.line_break_header = FALSE)}.
#' @param post_hoc Logical; if \code{TRUE}, runs pairwise post-hoc tests for continuous and ordinal
#'   variables in three or more group comparisons and annotates each group column value with a compact
#'   letter display (CLD) superscript. Groups sharing a letter are not significantly different at
#'   \eqn{\alpha = 0.05}. For normally distributed variables (Welch ANOVA path), Games-Howell
#'   pairwise tests are used. For non-normal and ordinal variables (Kruskal-Wallis path), Dunn's test
#'   with Holm correction is used. Post-hoc testing is never applied to categorical variables.
#'   Only valid when \code{group_var} has three or more levels; silently ignored for two-group
#'   comparisons. Requires the \code{rstatix} package. Default is \code{FALSE}.
#' @param indent_info_column Logical; if \code{FALSE} (default), the internal \code{.indent} helper column
#'   is dropped from the returned tibble. Set to \code{TRUE} to retain it -- this is necessary when you
#'   intend to post-process the tibble and later pass it to \code{word_export} directly, as
#'   \code{word_export} uses the \code{.indent} column to apply correct indentation and italic formatting
#'   in the Word table.
#' @param p_adjust Logical; if \code{TRUE}, applies the Benjamini-Hochberg (BH) false discovery rate
#'   correction to all omnibus P values after testing. The correction pool is one P value per
#'   variable — sub-rows of multi-level categoricals share the parent P value and are not
#'   double-counted. Post-hoc pairwise P values (which already carry within-variable Holm
#'   correction) are excluded from the correction pool. Default is \code{FALSE}.
#' @param p_adjust_display Character; controls how BH-corrected P values appear in the output
#'   when \code{p_adjust = TRUE}. \code{"fdr_only"} (default) replaces the standard P value
#'   column with the corrected values, renaming the column to \code{"P value (FDR corrected)"}.
#'   \code{"both"} retains the original P values in a column named \code{"P value"} and adds
#'   FDR-corrected values immediately to its right in a column named
#'   \code{"P value (FDR corrected)"}. Ignored when \code{p_adjust = FALSE}.
#' @param open_doc Logical; if \code{TRUE} (default), automatically opens the written Word document
#'   in the system default application after saving. Set to \code{FALSE} to suppress.
#'   Has no effect when \code{output_docx} is \code{NULL}.
#' @param citation Logical; if \code{TRUE} (default), appends a citation line at the bottom
#'   of the table footnote block and at the end of the methods document: package version, authors,
#'   and links to the GitHub repository and web interface. Set to \code{FALSE} to suppress.
#' @param font_family Character; font family name used for all Word output (table,
#'   captions, footnotes, methods document). Any font installed on the system that
#'   renders the document may be used. Popular options include \code{"Arial"},
#'   \code{"Helvetica"}, \code{"Times New Roman"}, \code{"Garamond"}, and
#'   \code{"Calibri"}. Defaults to \code{getOption("TernTables.font_family", "Arial")}.
#' @param show_missing Logical; if \code{TRUE}, appends a \code{"Missing"} row after each
#'   variable's data rows showing the count and percentage of missing observations per group
#'   (denominator is each group's total N). Missing rows display \code{"-"} in the P and OR
#'   columns. A footnote is automatically appended noting that missing values are reported.
#'   Default is \code{FALSE}.
#'
#' @return A tibble with one row per variable (multi-row for multi-level factors), showing summary statistics by group,
#' P values, test type, and optionally odds ratios and total summary column.
#'
#' @examples
#' data(tern_colon)
#'
#' # 2-group comparison
#' ternG(tern_colon, exclude_vars = c("ID"), group_var = "Recurrence",
#'       methods_doc = FALSE)
#'
#' # 2-group comparison with odds ratios
#' ternG(tern_colon, exclude_vars = c("ID"), group_var = "Recurrence",
#'       OR_col = TRUE, methods_doc = FALSE)
#'
#' # 3-group comparison
#' ternG(tern_colon, exclude_vars = c("ID"), group_var = "Treatment_Arm",
#'       group_order = c("Observation", "Levamisole", "Levamisole + 5FU"),
#'       methods_doc = FALSE)
#'
#' # 2-group comparison with BH FDR correction (fdr_only — default display)
#' ternG(tern_colon, exclude_vars = c("ID"), group_var = "Recurrence",
#'       p_adjust = TRUE, methods_doc = FALSE)
#'
#' # 2-group comparison with BH FDR correction (show raw + corrected side by side)
#' ternG(tern_colon, exclude_vars = c("ID"), group_var = "Recurrence",
#'       p_adjust = TRUE, p_adjust_display = "both", methods_doc = FALSE)
#'
#' # Export to Word (writes a file to tempdir)
#' \donttest{
#' ternG(tern_colon,
#'       exclude_vars   = c("ID"),
#'       group_var      = "Recurrence",
#'       OR_col         = TRUE,
#'       methods_doc    = FALSE,
#'       open_doc       = FALSE,
#'       output_docx    = file.path(tempdir(), "comparison.docx"),
#'       category_start = c("Patient Demographics"  = "Age (yr)",
#'                          "Tumor Characteristics" = "Positive Lymph Nodes (n)"))
#' }
#'
#' @export
ternG <- function(data,
                  vars = NULL,
                  exclude_vars = NULL,
                  group_var,
                  force_ordinal = NULL,
                  force_continuous = NULL,
                  group_order = NULL,
                  output_xlsx = NULL,
                  output_docx = NULL,
                  OR_col = FALSE,
                  OR_method = "dynamic",
                  consider_normality = "ROBUST",
                  print_normality = FALSE,
                  show_test = FALSE,
                  p_digits = 3,
                  round_intg = FALSE,
                  smart_rename = TRUE,
                  insert_subheads = TRUE,
                  factor_order = "mixed",
                  table_font_size = 9,
                  methods_doc = TRUE,
                  methods_filename = "TernTables_methods.docx",
                  category_start = NULL,
                  plain_header = NULL,
                  manual_italic_indent = NULL,
                  manual_underline = NULL,
                  indent_info_column = FALSE,
                  show_total = TRUE,
                  table_caption = NULL, table_footnote = NULL,
                  abbreviation_footnote = NULL, variable_footnote = NULL,
                  index_style = "symbols",
                  line_break_header = getOption("TernTables.line_break_header", TRUE),
                  post_hoc = FALSE,
                  p_adjust = FALSE,
                  p_adjust_display = "fdr_only",
                  open_doc = TRUE, citation = TRUE,
                  font_family = getOption("TernTables.font_family", "Arial"),
                  show_missing = FALSE) {

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
    vars <- setdiff(names(data), unique(c(exclude_vars, group_var)))
  }

  if (!is.factor(data[[group_var]])) {
    data[[group_var]] <- factor(data[[group_var]])
  }
  if (!is.null(group_order)) {
    data[[group_var]] <- factor(data[[group_var]], levels = group_order)
  }

  data <- data %>% filter(!is.na(.data[[group_var]]))
  n_levels <- length(unique(data[[group_var]]))

  if (isTRUE(OR_col) && n_levels != 2) {
    stop("`OR_col = TRUE` is only valid for 2-group comparisons. ",
         group_var, " has ", n_levels, " levels. Set `OR_col = FALSE` or use a binary group variable.")
  }

  group_counts <- data %>% count(.data[[group_var]]) %>% deframe()
  group_levels <- names(group_counts)
  group_labels <- setNames(
    paste0(group_levels, " (n = ", group_counts, ")"),
    group_levels
  )
  
  # Calculate total N for the Total column header
  total_n <- nrow(data)

  # Initialize normality tracking variables
  normality_results <- list()
  numeric_vars_tested <- 0
  numeric_vars_failed <- 0

  # Track which variables actually had post-hoc tests run (omnibus significant)
  posthoc_ran_display <- character(0)
  # Track which variables used Monte Carlo Fisher's exact (workspace limit fallback)
  fisher_sim_display  <- character(0)

  .summarize_var_internal <- function(df, var, force_ordinal = NULL, force_continuous = NULL, show_test = FALSE, round_intg = FALSE, show_total = FALSE) {

    # Count missings per group BEFORE the NA filter below (used when show_missing = TRUE)
    .missing_row <- function(result_tbl) {
      if (!isTRUE(show_missing)) return(result_tbl)
      miss_n   <- sapply(group_levels, function(g_lvl) {
        grp_data <- df[!is.na(df[[group_var]]) & df[[group_var]] == g_lvl, ]
        sum(is.na(grp_data[[var]]))
      })
      grp_n    <- sapply(group_levels, function(g_lvl) {
        sum(!is.na(df[[group_var]]) & df[[group_var]] == g_lvl)
      })
      miss_pct <- round(miss_n / grp_n * 100)
      # Only emit the row when at least one group has missing values
      if (!any(miss_n > 0)) return(result_tbl)
      mrow <- tibble(Variable = "Missing", .indent = 6L)
      for (g_lvl in group_levels) {
        mrow[[group_labels[g_lvl]]] <- paste0(miss_n[g_lvl], " (", miss_pct[g_lvl], "%)")
      }
      mrow$P      <- "-"
      mrow$.p_raw <- NA_real_
      if (show_test)  mrow$test      <- "-"
      if (OR_col)     mrow$OR        <- "-"
      if (show_test && OR_col) mrow$OR_method <- "-"
      if (show_total) {
        total_miss <- sum(miss_n)
        total_grp  <- sum(grp_n)
        mrow$Total <- paste0(total_miss, " (", round(total_miss / total_grp * 100), "%)")
      }
      bind_rows(result_tbl, mrow)
    }

    g <- df %>% filter(!is.na(.data[[var]]), !is.na(.data[[group_var]]))
    if (nrow(g) == 0) return(NULL)
    v <- g[[var]]

    # Auto-detect binary numeric (0/1) as categorical Y/N
    # Skipped when the variable is listed in force_continuous
    if (is.numeric(v) && length(unique(stats::na.omit(v))) == 2 && all(stats::na.omit(v) %in% c(0, 1)) &&
        !(var %in% force_continuous)) {
      v <- factor(v, levels = c(0, 1), labels = c("N", "Y"))
      g[[var]] <- v
    }

    # ----- Categorical -----
    if (is.character(v) || is.factor(v)) {
      g[[var]] <- factor(g[[var]])
      tab <- table(g[[group_var]], g[[var]])
      tab_pct <- as.data.frame.matrix(round(prop.table(tab, 1) * 100))
      tab_n   <- as.data.frame.matrix(tab)
      tab_total_n   <- colSums(tab)
      tab_total_pct <- round(prop.table(tab_total_n) * 100)

      # Cochran (1954) criterion: use Fisher's exact when any *expected* cell count < 5
      fisher_flag <- any(suppressWarnings(stats::chisq.test(tab)$expected) < 5)
      test_result <- tryCatch({
        if (fisher_flag) {
          sim_used <- FALSE
          ft <- tryCatch(
            stats::fisher.test(tab),
            error = function(e) {
              # Workspace limit exceeded for large tables — fall back to Monte Carlo
              sim_used <<- TRUE
              set.seed(getOption("TernTables.seed", 42L))
              stats::fisher.test(tab, simulate.p.value = TRUE, B = 10000L)
            }
          )
          list(p_value = ft$p.value,
               test_name = if (sim_used) "Fisher exact (simulated)" else "Fisher exact",
               error = NULL)
        } else {
          list(p_value = stats::chisq.test(tab)$p.value, test_name = "Chi-squared", error = NULL)
        }
      }, error = function(e) {
        # Determine the reason for failure
        if (nrow(tab) < 2 || ncol(tab) < 2) {
          reason <- "insufficient variation"
        } else if (sum(tab) == 0) {
          reason <- "no observations"
        } else if (any(rowSums(tab) == 0) || any(colSums(tab) == 0)) {
          reason <- "empty cells"
        } else {
          reason <- "test failure"
        }
        list(p_value = NA_real_, test_name = if (fisher_flag) "Fisher exact" else "Chi-squared",
             error = reason)
      })
      
      
      # Determine if this should use simple format or hierarchical subheads
      # Always use simple format for Y/N variables or when insert_subheads is FALSE
      # Otherwise use hierarchical format for categorical variables
      upper_cols     <- toupper(colnames(tab))
      is_yes_no      <- all(c("Y", "N")   %in% upper_cols)
      is_yes_no_full <- all(c("YES", "NO") %in% upper_cols)
      is_binary <- is_yes_no || is_yes_no_full
      use_simple_format <- is_binary || !insert_subheads
      
      if (use_simple_format) {
        # Simple format: single row showing the positive/yes level
        if (is_yes_no) {
          ref_level <- colnames(tab)[toupper(colnames(tab)) == "Y"]
        } else if (is_yes_no_full) {
          ref_level <- colnames(tab)[toupper(colnames(tab)) == "YES"]
        } else {
          if ((factor_order == "levels" || factor_order == "mixed") && is.factor(g[[var]])) {
            # Factor: use first level that actually appears in the data
            available_levels <- levels(g[[var]])[levels(g[[var]]) %in% colnames(tab)]
            ref_level <- available_levels[1]
          } else if (factor_order == "mixed") {
            # Non-factor: 2-level → alphabetical first; 3+ → most frequent
            if (length(colnames(tab)) == 2) {
              ref_level <- sort(colnames(tab))[1]
            } else {
              ref_level <- names(sort(colSums(tab), decreasing = TRUE))[1]
            }
          } else {
            # "levels" non-factor or "frequency": use most frequent level
            ref_level <- names(sort(colSums(tab), decreasing = TRUE))[1]
          }
        }
        result <- tibble(Variable = .clean_variable_name_for_header(var), .indent = 2)
        for (g_lvl in group_levels) {
          result[[group_labels[g_lvl]]] <- paste0(
            tab_n[g_lvl, ref_level], " (", tab_pct[g_lvl, ref_level], "%)"
          )
        }

        if (!is.null(test_result$error)) {
          result$P      <- paste0("NA (", test_result$error, ")")
          result$.p_raw <- NA_real_
        } else {
          result$P      <- val_p_format(test_result$p_value, p_digits)
          result$.p_raw <- test_result$p_value
        }
        if (show_test) {
          result$test <- test_result$test_name
        }

        if (OR_col && ncol(tab) == 2 && nrow(tab) == 2) {
          if (OR_method == "dynamic") {
            if (fisher_flag) {
              fisher_obj <- tryCatch(stats::fisher.test(tab), error = function(e) NULL)
              if (!is.null(fisher_obj)) {
                result$OR <- sprintf("%.2f [%.2f\u2013%.2f]", fisher_obj$estimate, fisher_obj$conf.int[1], fisher_obj$conf.int[2])
                if (show_test) result$OR_method <- "Fisher"
              } else {
                result$OR <- "NA (calculation failed)"
                if (show_test) result$OR_method <- "Fisher"
              }
            } else {
              or_obj <- tryCatch(epitools::oddsratio(tab, method = "wald")$measure, error = function(e) NULL)
              result$OR <- if (!is.null(or_obj)) sprintf("%.2f [%.2f\u2013%.2f]", or_obj[2,1], or_obj[2,2], or_obj[2,3]) else "NA (calculation failed)"
              if (show_test) result$OR_method <- "Wald"
            }
          } else if (OR_method == "wald") {
            or_obj <- tryCatch(epitools::oddsratio(tab, method = "wald")$measure, error = function(e) NULL)
            result$OR <- if (!is.null(or_obj)) sprintf("%.2f [%.2f\u2013%.2f]", or_obj[2,1], or_obj[2,2], or_obj[2,3]) else "NA (calculation failed)"
            if (show_test) result$OR_method <- "Wald"
          }
        } else if (OR_col) {
          result$OR <- "-"
          if (show_test) result$OR_method <- "-"
        }
        if (show_total) {
          result$Total <- paste0(tab_total_n[ref_level], " (", tab_total_pct[ref_level], "%)")
        }

      } else {
        # Hierarchical format: header + indented sub-categories
        # tab_total_n is already a vector of total counts per level
        if ((factor_order == "levels" || factor_order == "mixed") && is.factor(g[[var]])) {
          # Factor: always respect original factor level ordering
          sorted_levels <- levels(g[[var]])
          sorted_levels <- sorted_levels[sorted_levels %in% names(tab_total_n)]
        } else if (factor_order == "mixed") {
          # Non-factor: 2-level → alphabetical; 3+ → frequency
          available <- names(tab_total_n)
          if (length(available) == 2) {
            sorted_levels <- sort(available)
          } else {
            sorted_levels <- names(sort(tab_total_n, decreasing = TRUE))
          }
        } else {
          # "levels" non-factor fallback or "frequency": sort by frequency
          sorted_levels <- names(sort(tab_total_n, decreasing = TRUE))
        }
        
        # Create header row for the main variable (no data, just variable name)
        header_row <- tibble(Variable = .clean_variable_name_for_header(var), .indent = 2)
        for (g_lvl in group_levels) {
          header_row[[group_labels[g_lvl]]] <- ""
        }
        header_row$P      <- ""
        header_row$.p_raw <- NA_real_
        if (show_test) {
          header_row$test <- ""
        }
        if (OR_col) {
          header_row$OR <- ""
          if (show_test) header_row$OR_method <- ""
        }
        if (show_total) {
          header_row$Total <- ""
        }
        
        # For two-level categorical variables, compute OR once before the row loop.
        # Reference level: factor level 1, or alphabetical first for non-factors.
        hier_or_vals <- NULL
        if (OR_col && length(sorted_levels) == 2) {
          ref_level_hier <- if (is.factor(g[[var]])) {
            lvls <- levels(g[[var]])
            lvls[lvls %in% sorted_levels][1]
          } else {
            sort(sorted_levels)[1]
          }
          non_ref_level_hier <- sorted_levels[sorted_levels != ref_level_hier]
          # Reorder columns so reference level is first (groups stay as rows)
          tab_hier2 <- tab[, c(ref_level_hier, non_ref_level_hier), drop = FALSE]
          if (nrow(tab_hier2) == 2 && ncol(tab_hier2) == 2) {
            or_string_hier <- tryCatch({
              if (OR_method == "dynamic") {
                if (fisher_flag) {
                  fisher_obj2 <- stats::fisher.test(tab_hier2)
                  sprintf("%.2f [%.2f\u2013%.2f]", fisher_obj2$estimate,
                          fisher_obj2$conf.int[1], fisher_obj2$conf.int[2])
                } else {
                  or_obj2 <- epitools::oddsratio(tab_hier2, method = "wald")$measure
                  sprintf("%.2f [%.2f\u2013%.2f]", or_obj2[2, 1], or_obj2[2, 2], or_obj2[2, 3])
                }
              } else if (OR_method == "wald") {
                or_obj2 <- epitools::oddsratio(tab_hier2, method = "wald")$measure
                sprintf("%.2f [%.2f\u2013%.2f]", or_obj2[2, 1], or_obj2[2, 2], or_obj2[2, 3])
              } else {
                "NA"
              }
            }, error = function(e) "NA (calculation failed)")
            hier_or_vals <- list(
              ref_level = ref_level_hier,
              or_string  = or_string_hier,
              or_method  = if (fisher_flag) "Fisher" else "Wald"
            )
          }
        }

        # Create sub-category rows (indented)
        sub_rows <- lapply(sorted_levels, function(level) {
          out <- tibble(Variable = level, .indent = 6)
          for (g_lvl in group_levels) {
            val <- if (g_lvl %in% rownames(tab_n) && level %in% colnames(tab_n)) {
              paste0(tab_n[g_lvl, level], " (", tab_pct[g_lvl, level], "%)")
            } else {
              "NA (NA%)"
            }
            out[[group_labels[g_lvl]]] <- val
          }
          
          # Only first sub-row gets P value, others get "-"
          if (level == sorted_levels[1]) {
            if (!is.null(test_result$error)) {
              out$P      <- paste0("NA (", test_result$error, ")")
              out$.p_raw <- NA_real_
            } else {
              out$P      <- val_p_format(test_result$p_value, p_digits)
              out$.p_raw <- test_result$p_value
            }
            if (show_test) {
              out$test <- test_result$test_name
            }
          } else {
            out$P      <- "-"
            out$.p_raw <- NA_real_
            if (show_test) {
              out$test <- "-"
            }
          }
          
          if (OR_col) {
            if (!is.null(hier_or_vals)) {
              # Two-level categorical: reference row shows "1.00 (ref.)", other row shows computed OR
              out$OR <- if (level == hier_or_vals$ref_level) "1.00 (ref.)" else hier_or_vals$or_string
              if (show_test) out$OR_method <- hier_or_vals$or_method
            } else {
              out$OR <- "-"
              if (show_test) out$OR_method <- "-"
            }
          }
          if (show_total) {
            out$Total <- paste0(tab_total_n[level], " (", tab_total_pct[level], "%)")
          }
          out
        })
        
        # Combine header and sub-rows
        result <- bind_rows(list(header_row), sub_rows)
      }
      if (grepl("simulated", test_result$test_name, fixed = FALSE))
        fisher_sim_display <<- c(fisher_sim_display, result$Variable[1])
      return(.missing_row(result))
    }

    # ----- Force ordinal -----
    if (!is.null(force_ordinal) && var %in% force_ordinal) {
      stats <- g %>% group_by(.data[[group_var]]) %>% summarise(
        Q1 = if (round_intg) round_up_half(quantile(.data[[var]], 0.25, na.rm = TRUE), 0) else round(quantile(.data[[var]], 0.25, na.rm = TRUE), 1),
        med = if (round_intg) round_up_half(median(.data[[var]], na.rm = TRUE), 0) else round(median(.data[[var]], na.rm = TRUE), 1),
        Q3 = if (round_intg) round_up_half(quantile(.data[[var]], 0.75, na.rm = TRUE), 0) else round(quantile(.data[[var]], 0.75, na.rm = TRUE), 1), .groups = "drop")
      result <- tibble(Variable = .clean_variable_name_for_header(var), .indent = 2)
      for (g_lvl in group_levels) {
        val <- stats %>% filter(.data[[group_var]] == g_lvl)
        result[[group_labels[g_lvl]]] <- if (nrow(val) == 1) {
          paste0(val$med, " [", val$Q1, "\u2013", val$Q3, "]")
        } else {
          "NA [NA\u2013NA]"
        }
      }
      
      test_result <- tryCatch({
        if (n_levels == 2) {
          p_val <- stats::wilcox.test(g[[var]] ~ g[[group_var]])$p.value
          list(p_value = p_val, test_name = "Wilcoxon rank-sum", error = NULL)
        } else {
          p_val <- stats::kruskal.test(g[[var]] ~ g[[group_var]])$p.value
          list(p_value = p_val, test_name = "Kruskal-Wallis", error = NULL)
        }
      }, error = function(e) {
        # Determine reason for test failure
        group_sizes <- table(g[[group_var]])
        if (any(group_sizes < 2)) {
          reason <- "insufficient group sizes"
        } else if (length(unique(g[[var]])) < 2) {
          reason <- "no variation in values"
        } else {
          reason <- "test failure"
        }
        test_name <- if (n_levels == 2) "Wilcoxon rank-sum" else "Kruskal-Wallis"
        list(p_value = NA_real_, test_name = test_name, error = reason)
      })
      
      if (!is.null(test_result$error)) {
        result$P      <- paste0("NA (", test_result$error, ")")
        result$.p_raw <- NA_real_
      } else {
        result$P      <- val_p_format(test_result$p_value, p_digits)
        result$.p_raw <- test_result$p_value
      }
      if (show_test) {
        result$test <- test_result$test_name
      }
      
      if (OR_col) result$OR <- "-"
      if (show_total) {
        val_total <- g %>% dplyr::summarise(
          Q1  = if (round_intg) round_up_half(quantile(.data[[var]], 0.25, na.rm = TRUE), 0) else round(quantile(.data[[var]], 0.25, na.rm = TRUE), 1),
          med = if (round_intg) round_up_half(median(.data[[var]], na.rm = TRUE), 0) else round(median(.data[[var]], na.rm = TRUE), 1),
          Q3  = if (round_intg) round_up_half(quantile(.data[[var]], 0.75, na.rm = TRUE), 0) else round(quantile(.data[[var]], 0.75, na.rm = TRUE), 1))
        result$Total <- paste0(val_total$med, " [", val_total$Q1, "\u2013", val_total$Q3, "]")
      }
      if (print_normality) {
        for (g_lvl in group_levels) {
          sw_p <- tryCatch({
            x <- g %>% filter(.data[[group_var]] == g_lvl) %>% pull(.data[[var]])
            if (length(x) >= 3 && length(x) <= 5000) stats::shapiro.test(x)$p.value else NA_real_
          }, error = function(e) NA_real_)
          result[[paste0("SW_p_", g_lvl)]] <- formatC(sw_p, format = "f", digits = 4)
        }
      }
      # Post-hoc pairwise testing for 3+ groups: Dunn's test with Holm correction
      # Only runs when omnibus (Kruskal-Wallis) is significant at alpha = 0.05
      if (post_hoc && n_levels >= 3L && is.null(test_result$error) &&
          !is.na(test_result$p_value) && test_result$p_value < 0.05) {
        # Sanitize column names for rstatix (spaced/special names break its formula parser)
          safe_var      <- make.names(var)
          safe_group    <- make.names(group_var)
          g_safe        <- g
          names(g_safe)[names(g_safe) == var]       <- safe_var
          names(g_safe)[names(g_safe) == group_var] <- safe_group
          ph_formula <- stats::as.formula(paste0("`", safe_var, "` ~ `", safe_group, "`"))
          ph_res <- tryCatch(
            rstatix::dunn_test(g_safe, ph_formula, p.adjust.method = "holm"),
            error = function(e) NULL
          )
          if (!is.null(ph_res)) {
            centers_df  <- g %>% dplyr::group_by(.data[[group_var]]) %>%
              dplyr::summarise(ctr = median(.data[[var]], na.rm = TRUE), .groups = "drop")
            centers_vec <- setNames(centers_df$ctr, as.character(centers_df[[group_var]]))
            ph_df <- data.frame(
              group1 = as.character(ph_res$group1),
              group2 = as.character(ph_res$group2),
              p.adj  = ph_res$p.adj,
              stringsAsFactors = FALSE
            )
            cld <- .compute_cld(as.character(group_levels), centers_vec[as.character(group_levels)], ph_df)
            for (g_lvl in group_levels) {
              sup <- .superscript_letters(cld[as.character(g_lvl)])
              if (!is.null(sup) && nzchar(sup))
                result[[group_labels[g_lvl]]] <- paste0(result[[group_labels[g_lvl]]], sup)
            }
            posthoc_ran_display <<- c(posthoc_ran_display, result$Variable[1])
          }
      }
      if (grepl("simulated", test_result$test_name, fixed = FALSE))
        fisher_sim_display <<- c(fisher_sim_display, result$Variable[1])
      return(.missing_row(result))
    }

    # ----- Normality assessment -----
    sw_p_all <- list()
    is_normal <- TRUE
    
    # Handle different consider_normality options
    if (consider_normality == "FORCE") {
      # Test normality for baseline statistics but force all to be treated as ordinal
      sw_p_all <- tryCatch({
        out <- lapply(group_levels, function(g_lvl) {
          x <- g %>% filter(.data[[group_var]] == g_lvl) %>% pull(.data[[var]])
          pval <- if (length(x) >= 3 && length(x) <= 5000) stats::shapiro.test(x)$p.value else NA_real_
          setNames(pval, paste0("SW_p_", g_lvl))
        })
        do.call(c, out)
      }, error = function(e) rep(NA, n_levels))
      baseline_normal <- all(sw_p_all > 0.05, na.rm = TRUE)
      
      # Track baseline normality results for reporting
      numeric_vars_tested <<- numeric_vars_tested + 1
      if (!baseline_normal) {
        numeric_vars_failed <<- numeric_vars_failed + 1
      }
      
      # Force all to be treated as ordinal regardless of normality
      is_normal <- FALSE
    } else if (consider_normality == "ROBUST") {
      # ROBUST: four-gate decision tree
      #   Gate 1 (fail-safe): any group n < 3 → non-parametric (explicit)
      #   Gate 2: |skewness| > 2 in any group → non-parametric
      #   Gate 3: all groups n >= 30 → CLT → parametric
      #   Gate 4: small-sample fallback → Shapiro-Wilk
      calc_skewness <- function(x) {
        x <- x[!is.na(x)]
        n <- length(x)
        if (n < 3) return(NA_real_)
        m <- mean(x)
        s <- stats::sd(x)
        if (s == 0) return(NA_real_)
        (sum((x - m)^3) / n) / s^3
      }
      group_vals <- lapply(group_levels, function(g_lvl) {
        g %>% filter(.data[[group_var]] == g_lvl) %>% pull(.data[[var]])
      })
      group_ns       <- sapply(group_vals, function(v) sum(!is.na(v)))
      group_skewness <- sapply(group_vals, calc_skewness)

      numeric_vars_tested <<- numeric_vars_tested + 1

      if (any(group_ns < 3)) {
        # Gate 1: any group too small to test — non-parametric (conservative fail-safe)
        is_normal <- FALSE
        numeric_vars_failed <<- numeric_vars_failed + 1
      } else if (any(abs(group_skewness) > 2, na.rm = TRUE)) {
        # Gate 2: extreme skewness — non-parametric regardless of n
        is_normal <- FALSE
        numeric_vars_failed <<- numeric_vars_failed + 1
      } else if (all(group_ns >= 30)) {
        # Gate 3: CLT applies — parametric
        is_normal <- TRUE
      } else {
        # Gate 4: at least one small group — use Shapiro-Wilk
        sw_p_all <- tryCatch({
          out <- lapply(seq_along(group_levels), function(i) {
            x <- group_vals[[i]]
            pval <- if (length(x) >= 3 && length(x) <= 5000) stats::shapiro.test(x)$p.value else NA_real_
            setNames(pval, paste0("SW_p_", group_levels[i]))
          })
          do.call(c, out)
        }, error = function(e) rep(NA_real_, n_levels))
        is_normal <- all(!is.na(sw_p_all) & sw_p_all > 0.05)
        if (!is_normal) numeric_vars_failed <<- numeric_vars_failed + 1
      }
    } else if (isTRUE(consider_normality)) {
      # TRUE: Shapiro-Wilk only (conservative at large n)
      sw_p_all <- tryCatch({
        out <- lapply(group_levels, function(g_lvl) {
          x <- g %>% filter(.data[[group_var]] == g_lvl) %>% pull(.data[[var]])
          pval <- if (length(x) >= 3 && length(x) <= 5000) stats::shapiro.test(x)$p.value else NA_real_
          setNames(pval, paste0("SW_p_", g_lvl))
        })
        do.call(c, out)
      }, error = function(e) rep(NA, n_levels))
      is_normal <- all(!is.na(sw_p_all) & sw_p_all > 0.05)

      numeric_vars_tested <<- numeric_vars_tested + 1
      if (!is_normal) {
        numeric_vars_failed <<- numeric_vars_failed + 1
      }
    } else {
      # consider_normality = FALSE: still run Shapiro-Wilk for informational reporting,
      # but always treat variables as normally distributed for display.
      sw_p_all <- tryCatch({
        out <- lapply(group_levels, function(g_lvl) {
          x <- g %>% filter(.data[[group_var]] == g_lvl) %>% pull(.data[[var]])
          pval <- if (length(x) >= 3 && length(x) <= 5000) stats::shapiro.test(x)$p.value else NA_real_
          setNames(pval, paste0("SW_p_", g_lvl))
        })
        do.call(c, out)
      }, error = function(e) rep(NA_real_, n_levels))
      numeric_vars_tested <<- numeric_vars_tested + 1
      if (!all(sw_p_all > 0.05, na.rm = TRUE)) numeric_vars_failed <<- numeric_vars_failed + 1
    }

    if (!is_normal) {
      return(.summarize_var_internal(df, var = var, force_ordinal = var, show_test = show_test, round_intg = round_intg, show_total = show_total))
    }

    # ----- Normally distributed numeric -----
    result <- tibble(Variable = .clean_variable_name_for_header(var), .indent = 2)
    stats <- g %>% group_by(.data[[group_var]]) %>% summarise(
      mean = mean(.data[[var]], na.rm = TRUE),
      sd = sd(.data[[var]], na.rm = TRUE), .groups = "drop")

    for (g_lvl in group_levels) {
      val <- stats %>% filter(.data[[group_var]] == g_lvl)
      result[[group_labels[g_lvl]]] <- if (nrow(val) == 1) {
        if (round_intg) {
          paste0(round_up_half(val$mean, 0), " \u00b1 ", round_up_half(val$sd, 0))
        } else {
          paste0(round(val$mean, 1), " \u00b1 ", round(val$sd, 1))
        }
      } else {
        "NA \u00b1 NA"
      }
    }

    test_result <- tryCatch({
      if (n_levels == 2) {
        p_val <- stats::t.test(g[[var]] ~ g[[group_var]], var.equal = FALSE)$p.value
        list(p_value = p_val, test_name = "Welch t-test", error = NULL)
      } else {
        p_val <- stats::oneway.test(g[[var]] ~ g[[group_var]], var.equal = FALSE)$p.value
        list(p_value = p_val, test_name = "Welch ANOVA", error = NULL)
      }
    }, error = function(e) {
      # Determine reason for test failure
      group_sizes <- table(g[[group_var]])
      if (any(group_sizes < 2)) {
        reason <- "insufficient group sizes"
      } else if (all(is.na(g[[var]]))) {
        reason <- "all values missing"
      } else if (stats::var(g[[var]], na.rm = TRUE) == 0) {
        reason <- "no variation in values"
      } else {
        reason <- paste0("test failure: ", conditionMessage(e))
      }
      test_name <- if (n_levels == 2) "Welch t-test" else "Welch ANOVA"
      list(p_value = NA_real_, test_name = test_name, error = reason)
    })
    
    if (!is.null(test_result$error)) {
      result$P      <- paste0("NA (", test_result$error, ")")
      result$.p_raw <- NA_real_
    } else {
      result$P      <- val_p_format(test_result$p_value, p_digits)
      result$.p_raw <- test_result$p_value
    }
    if (show_test) {
      result$test <- test_result$test_name
    }
    
    if (OR_col) result$OR <- "-"
    if (show_total) {
      val_total <- g %>% dplyr::summarise(mean = mean(.data[[var]], na.rm = TRUE), sd = sd(.data[[var]], na.rm = TRUE))
      if (round_intg) {
        result$Total <- paste0(round_up_half(val_total$mean, 0), " \u00b1 ", round_up_half(val_total$sd, 0))
      } else {
        result$Total <- paste0(round(val_total$mean, 1), " \u00b1 ", round(val_total$sd, 1))
      }
    }

    if (print_normality && length(sw_p_all) > 0) {
      for (nm in names(sw_p_all)) {
        result[[nm]] <- formatC(sw_p_all[[nm]], format = "f", digits = 4)
      }
    }
    # Post-hoc pairwise testing for 3+ groups: Games-Howell
    # Only runs when omnibus (Welch ANOVA) is significant at alpha = 0.05
    if (post_hoc && n_levels >= 3L && is.null(test_result$error) &&
        !is.na(test_result$p_value) && test_result$p_value < 0.05) {
        # Sanitize column names for rstatix (spaced/special names break its formula parser)
        safe_var      <- make.names(var)
        safe_group    <- make.names(group_var)
        g_safe        <- g
        names(g_safe)[names(g_safe) == var]       <- safe_var
        names(g_safe)[names(g_safe) == group_var] <- safe_group
        gh_formula <- stats::as.formula(paste0("`", safe_var, "` ~ `", safe_group, "`"))
        ph_res <- tryCatch(
          rstatix::games_howell_test(g_safe, gh_formula),
          error = function(e) NULL
        )
        if (!is.null(ph_res)) {
          centers_vec <- setNames(stats$mean, as.character(stats[[group_var]]))
          ph_df <- data.frame(
            group1 = as.character(ph_res$group1),
            group2 = as.character(ph_res$group2),
            p.adj  = ph_res$p.adj,
            stringsAsFactors = FALSE
          )
          cld <- .compute_cld(as.character(group_levels), centers_vec[as.character(group_levels)], ph_df)
          for (g_lvl in group_levels) {
            sup <- .superscript_letters(cld[as.character(g_lvl)])
            if (!is.null(sup) && nzchar(sup))
              result[[group_labels[g_lvl]]] <- paste0(result[[group_labels[g_lvl]]], sup)
          }
          posthoc_ran_display <<- c(posthoc_ran_display, result$Variable[1])
        }
    }
    return(.missing_row(result))
  }

  out_tbl <- suppressWarnings({
    result <- bind_rows(lapply(vars, function(v) .summarize_var_internal(data, v, force_ordinal, force_continuous, show_test, round_intg, show_total)))
    cli::cli_alert_info("Multi-level categorical variables occupy more than one row in the output table.")
    result
  })
                        
  # -- Standardize group headers and enforce final column order (level-agnostic)
  # Keep the group column names with "n = x" format as requested

  # Collect any normality cols
  normality_cols <- grep("^SW_p_", names(out_tbl), value = TRUE)

  # The group columns should already have the "n = x" format from group_labels
  existing_group_cols <- intersect(unname(group_labels), names(out_tbl))

  # Desired column order (keeping group columns with n = x format)
  if (show_test) {
    desired <- c("Variable", existing_group_cols, "Total", "OR", "p", "test", "OR_method", normality_cols)
  } else {
    desired <- c("Variable", existing_group_cols, "Total", "OR", "p", normality_cols)
    # Remove test and OR_method columns if they exist
    if ("test" %in% names(out_tbl)) {
      out_tbl <- dplyr::select(out_tbl, -test)
    }
    if ("OR_method" %in% names(out_tbl)) {
      out_tbl <- dplyr::select(out_tbl, -OR_method)
    }
  }

  # Reorder: put desired first (when they exist), then everything else
  out_tbl <- dplyr::select(out_tbl, dplyr::any_of(desired), dplyr::everything())
  
  # Rename Total column to include N with uppercase N and line break
  if ("Total" %in% names(out_tbl)) {
    names(out_tbl)[names(out_tbl) == "Total"] <- paste0("Total\n(N = ", total_n, ")")
  }

  # ── Benjamini-Hochberg FDR correction ─────────────────────────────────────
  if (isTRUE(p_adjust) && ".p_raw" %in% names(out_tbl)) {
    raw_vals <- out_tbl[[".p_raw"]]
    adj_idx  <- which(!is.na(raw_vals))
    if (length(adj_idx) > 0) {
      if (length(adj_idx) < 2) {
        cli::cli_alert_info(
          "p_adjust = TRUE: only {length(adj_idx)} P value found in the correction pool. \\
           BH adjustment requires multiple tests to be meaningful; \\
           the reported value is unchanged from the raw P value."
        )
      }
      fdr_vals <- p.adjust(raw_vals[adj_idx], method = "BH")
      fdr_fmt  <- vapply(fdr_vals, val_p_format, character(1), digits = p_digits)
      # Template from P column: preserves "-" (sub-rows) and "" (header rows) as-is
      fdr_col         <- out_tbl[["P"]]
      fdr_col[adj_idx] <- fdr_fmt
      if (p_adjust_display == "fdr_only") {
        out_tbl[["P"]] <- fdr_col
        names(out_tbl)[names(out_tbl) == "P"] <- "P value (FDR corrected)"
      } else {
        names(out_tbl)[names(out_tbl) == "P"] <- "P value"
        out_tbl <- dplyr::mutate(out_tbl, `P value (FDR corrected)` = fdr_col,
                                 .after = dplyr::all_of("P value"))
      }
    }
  }
  out_tbl <- dplyr::select(out_tbl, -dplyr::any_of(".p_raw"))

  if (!is.null(output_xlsx)) export_to_excel(out_tbl, output_xlsx)
  
  # Write methods document if requested
  if (methods_doc) {
    write_methods_doc(out_tbl, methods_filename, n_levels = n_levels, OR_col = OR_col, OR_method = OR_method, source = "ternG", post_hoc = post_hoc, p_adjust = p_adjust, open_doc = open_doc, citation = citation, font_family = font_family)
  }

  # -- Report normality test results -----------------------------------------
  if (numeric_vars_tested > 0) {
    numeric_vars_passed <- numeric_vars_tested - numeric_vars_failed
    passed_pct  <- round((numeric_vars_passed / numeric_vars_tested) * 100, 1)
    param_test    <- if (n_levels == 2) "Welch's independent samples t-test" else "Welch's one-way ANOVA"
    nonparam_test <- if (n_levels == 2) "Wilcoxon rank-sum" else "Kruskal-Wallis"

    cli::cli_rule(left = "Normality Assessment (Shapiro-Wilk) \u2014 ternG")
    if (consider_normality == "FORCE") {
      cli::cli_alert_info("{numeric_vars_passed} of {numeric_vars_tested} continuous variable{?s} normally distributed ({passed_pct}%)")
      cli::cli_alert_warning("consider_normality = 'FORCE': all continuous variables \u2192 median [IQR], tested with {nonparam_test}")
    } else if (consider_normality == "ROBUST") {
      cli::cli_alert_info("{numeric_vars_passed} of {numeric_vars_tested} continuous variable{?s} routed to parametric ({passed_pct}%)")
      cli::cli_bullets(c(
        ">" = "Routing: skewness>2 \u2192 non-param; all-n\u226530 \u2192 CLT parametric; else Shapiro-Wilk",
        ">" = "Parametric   \u2192 mean \u00b1 SD, tested with {param_test}",
        ">" = "Non-parametric \u2192 median [IQR], tested with {nonparam_test}"
      ))
    } else if (isTRUE(consider_normality)) {
      cli::cli_alert_info("{numeric_vars_passed} of {numeric_vars_tested} continuous variable{?s} normally distributed ({passed_pct}%)")
      cli::cli_bullets(c(
        ">" = "Normally distributed \u2192 mean \u00b1 SD, tested with {param_test}",
        ">" = "Non-normal           \u2192 median [IQR], tested with {nonparam_test}"
      ))
    } else {
      cli::cli_alert_info("{numeric_vars_passed} of {numeric_vars_tested} continuous variable{?s} normally distributed ({passed_pct}%)")
      cli::cli_alert_warning("consider_normality = FALSE: all continuous variables \u2192 mean \u00b1 SD, tested with {param_test}")
    }
  }

  # Insert category header rows if specified
  # Replace "0 (NaN%)" with "-" for structurally impossible cells
  # (e.g. a subgroup that cannot logically have any observations in a given column)
  out_tbl <- out_tbl %>%
    dplyr::mutate(dplyr::across(dplyr::where(is.character), ~ gsub("0 \\(NaN%\\)", "-", .x)))

  # Apply smart variable name cleaning if requested
  if (smart_rename) {
    for (i in seq_len(nrow(out_tbl))) {
      # Factor level sub-rows (.indent == 6) are user data — never run through
      # name-cleaning rules or their casing/content will be corrupted.
      if (out_tbl$.indent[i] == 6) next
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

  # Save with .indent intact for ternB multi-table export metadata
  out_tbl_with_indent <- out_tbl

  # Export to Word AFTER smart_rename so docx gets clean names
  # Auto-prepend post-hoc footnote only when at least one variable actually ran post-hoc
  # CLD note: positioned after abbreviations and before symbols in the footnote block
  posthoc_note <- NULL
  if (length(posthoc_ran_display) > 0L) {
    vars_listed <- paste(unique(posthoc_ran_display), collapse = ", ")
    posthoc_note <- paste0(
      "Superscript letters indicate pairwise post-hoc comparisons (",
      vars_listed,
      "; \u03b1\u00a0=\u00a00.05); groups sharing a letter are not significantly different."
    )
  }
  # Other auto-notes (Fisher simulation, show_missing) go into table_footnote slot
  effective_footnote <- table_footnote
  notes <- character(0)
  if (length(fisher_sim_display) > 0L) {
    sim_vars <- paste(unique(fisher_sim_display), collapse = ", ")
    notes <- c(notes, paste0(
      "Fisher\u2019s exact test with Monte Carlo simulation (B\u00a0=\u00a010,000; seed\u00a0",
      getOption("TernTables.seed", 42L),
      ") was used where exact enumeration was computationally infeasible: ",
      sim_vars, "."
    ))
  }
  if (isTRUE(show_missing)) {
    notes <- c(notes, "Missing: n (%) of missing observations per group.")
  }
  if (length(notes) > 0L)
    effective_footnote <- if (is.null(table_footnote)) notes else c(notes, table_footnote)

  # Auto-append bold-convention note to abbreviation_footnote
  # The p-value column is named "P" in out_tbl (renamed to "P value" only inside word_export).
  # FDR-corrected variant is "P value (FDR corrected)".
  has_p_col  <- any(c("P", "P value (FDR corrected)") %in% colnames(out_tbl))
  has_or_col <- isTRUE(OR_col) && "OR" %in% colnames(out_tbl)
  bold_note  <- if (has_p_col && has_or_col) {
    "Bold values indicate statistical significance (p\u00a0<\u00a00.05); bold OR indicates 95% CI excludes 1."
  } else if (has_p_col) {
    "Bold p-values indicate statistical significance (p\u00a0<\u00a00.05)."
  } else {
    NULL
  }
  effective_abbr <- if (!is.null(bold_note)) {
    if (is.null(abbreviation_footnote)) bold_note else c(abbreviation_footnote, bold_note)
  } else {
    abbreviation_footnote
  }

  if (!is.null(output_docx)) word_export(out_tbl, output_docx, round_intg = round_intg, font_size = table_font_size, category_start = category_start, plain_header = plain_header, manual_italic_indent = manual_italic_indent, manual_underline = manual_underline, table_caption = table_caption, table_footnote = effective_footnote, abbreviation_footnote = effective_abbr, posthoc_footnote = posthoc_note, variable_footnote = variable_footnote, index_style = index_style, line_break_header = line_break_header, open_doc = open_doc, citation = citation, font_family = font_family)

  if (!indent_info_column) out_tbl <- dplyr::select(out_tbl, -dplyr::any_of(".indent"))

  # Attach word-export metadata so ternB() can reproduce this table in a combined document
  attr(out_tbl, "ternB_meta") <- list(
    tbl                   = out_tbl_with_indent,
    round_intg            = round_intg,
    font_size             = table_font_size,
    category_start        = category_start,
    plain_header          = plain_header,
    manual_italic_indent  = manual_italic_indent,
    manual_underline      = manual_underline,
    table_caption         = table_caption,
    table_footnote        = effective_footnote,
    abbreviation_footnote = effective_abbr,
    posthoc_footnote      = posthoc_note,
    variable_footnote     = variable_footnote,
    index_style           = index_style,
    line_break_header     = line_break_header,
    source                = "ternG",
    n_levels              = n_levels,
    OR_col                = OR_col,
    OR_method             = OR_method,
    post_hoc              = post_hoc,
    p_adjust              = p_adjust,
    p_adjust_display      = p_adjust_display,
    citation              = citation,
    font_family           = font_family,
    force_continuous      = force_continuous,
    show_missing          = show_missing
  )

  return(out_tbl)
}
