#' Generate grouped summary table with appropriate statistical tests
#'
#' Creates a grouped summary table with optional statistical testing for group
#' comparisons. Supports numeric, ordinal, and categorical variables, and
#' includes options to calculate p-values and odds ratios. For descriptive
#' (ungrouped) tables, use \code{ternD}.
#'
#' @param data Tibble containing all variables.
#' @param vars Character vector of variables to summarize. Defaults to all except \code{group_var} and \code{exclude_vars}.
#' @param exclude_vars Character vector of variable(s) to exclude. \code{group_var} is automatically excluded.
#' @param group_var Character, the grouping variable (factor or character with >=2 levels).
#' @param force_ordinal Character vector of variables to treat as ordinal (i.e., use medians/IQR and nonparametric tests).
#' @param group_order Optional character vector to specify a custom group level order.
#' @param output_xlsx Optional filename to export the table as an Excel file.
#' @param output_docx Optional filename to export the table as a Word document.
#' @param OR_col Logical; if \code{TRUE}, adds odds ratios for 2-level categorical variables.
#' @param OR_method Character; if \code{"dynamic"}, uses Fisher/Wald based on test type. If \code{"wald"}, forces Wald method.
#' @param consider_normality Logical or character; if \code{TRUE}, uses Shapiro-Wilk to choose t-test vs. Wilcoxon for numeric vars. If \code{FALSE}, uses variable type and force_ordinal. If \code{"FORCE"}, treats all numeric variables as ordinal (median/IQR, nonparametric tests).
#' @param print_normality Logical; if \code{TRUE}, includes Shapiro-Wilk p-values in the output.
#' @param show_test Logical; if \code{TRUE}, includes the statistical test name as a column in the output. Default is \code{FALSE}.
#' @param p_digits Integer; number of decimal places for p-values (default 3).
#' @param round_intg Logical; if \code{TRUE}, rounds all means, medians, IQRs, and standard deviations to nearest integer (0.5 rounds up). Default is \code{FALSE}.
#' @param smart_rename Logical; if \code{TRUE}, automatically cleans variable names and subheadings for publication-ready output using built-in rule-based pattern matching for common medical abbreviations and prefixes. Default is \code{TRUE}.
#' @param insert_subheads Logical; if \code{TRUE}, creates hierarchical structure with headers and indented sub-categories for multi-level categorical variables (except Y/N). If \code{FALSE}, uses simple flat format. Default is \code{TRUE}.
#' @param factor_order Character; controls the ordering of factor levels in the output. If \code{"frequency"} (default), orders levels by decreasing frequency (most common first). If \code{"levels"}, respects the original factor level ordering as defined in the data; if the variable is not a factor, falls back to \code{"frequency"}.
#' @param table_font_size Numeric; font size for Word document output tables. Default is 9.
#' @param methods_doc Logical; if \code{TRUE} (default), generates a methods document describing the statistical tests used.
#' @param methods_filename Character; filename for the methods document. Default is \code{"TernTables_methods.docx"}.
#' @param category_start Named character vector specifying where to insert category headers. Names should be variable names, and values are the category header labels to insert before those variables. For example, \code{c("age" = "Demographics", "bmi" = "Clinical Measures")}. Default is \code{NULL} (no category headers).
#' @param manual_italic_indent Character vector; optional parameter for specifying which variable names should be italicized and indented in formatted outputs. Used for advanced formatting control.
#' @param manual_underline Character vector; optional parameter for specifying which variable names should be underlined in formatted outputs. Used for advanced formatting control.
#' @param show_total Logical; if \code{TRUE}, adds a "Total" column showing the aggregate summary statistic across all groups (e.g., for a publication Table 1 that includes both per-group and overall columns). Default is \code{TRUE}.
#' @param indent_info_column Logical; if \code{FALSE} (default), the internal \code{.indent} helper column is dropped from the returned tibble. Set to \code{TRUE} to retain it.
#'
#' @return A tibble with one row per variable (multi-row for multi-level factors), showing summary statistics by group,
#' p-values, test type, and optionally odds ratios and total summary column.
#'
#' @examples
#' # Basic usage with built-in mtcars dataset
#' ternG(mtcars, group_var = "am")
#'
#' # With survival::colon (ships with base R)
#' colon1 <- subset(survival::colon, etype == 1)
#' ternG(colon1, group_var = "sex",
#'       exclude_vars = c("id", "study", "etype", "time", "status"))  # 2-group
#' ternG(colon1, group_var = "rx",
#'       exclude_vars = c("id", "study", "etype", "time", "status"))  # 3-group
#'
#' @export
ternG <- function(data,
                  vars = NULL,
                  exclude_vars = NULL,
                  group_var,
                  force_ordinal = NULL,
                  group_order = NULL,
                  output_xlsx = NULL,
                  output_docx = NULL,
                  OR_col = FALSE,
                  OR_method = "dynamic",
                  consider_normality = TRUE,
                  print_normality = FALSE,
                  show_test = FALSE,
                  p_digits = 3,
                  round_intg = FALSE,
                  smart_rename = TRUE,
                  insert_subheads = TRUE,
                  factor_order = "frequency",
                  table_font_size = 9,
                  methods_doc = TRUE,
                  methods_filename = "TernTables_methods.docx",
                  category_start = NULL,
                  manual_italic_indent = NULL,
                  manual_underline = NULL,
                  indent_info_column = FALSE,
                  show_total = TRUE) {

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

  .summarize_var_internal <- function(df, var, force_ordinal = NULL, show_test = FALSE, round_intg = FALSE, show_total = FALSE) {

    g <- df %>% filter(!is.na(.data[[var]]), !is.na(.data[[group_var]]))
    if (nrow(g) == 0) return(NULL)
    v <- g[[var]]

    # Auto-detect binary numeric (0/1) as categorical Y/N
    if (is.numeric(v) && length(unique(na.omit(v))) == 2 && all(na.omit(v) %in% c(0, 1))) {
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

      # Calculate p-value once for the entire contingency table (not per level)
      fisher_flag <- any(tab < 5)
      test_result <- tryCatch({
        if (fisher_flag) {
          list(p_value = stats::fisher.test(tab)$p.value, test_name = "Fisher exact", error = NULL)
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
          if (factor_order == "levels" && is.factor(g[[var]])) {
            # Use the first level that actually appears in the data
            available_levels <- levels(g[[var]])[levels(g[[var]]) %in% colnames(tab)]
            ref_level <- available_levels[1]
          } else {
            # Default: use most frequent level
            ref_level <- names(sort(colSums(tab), decreasing = TRUE))[1]
          }
        }
        result <- tibble(Variable = var, .indent = 2)
        for (g_lvl in group_levels) {
          result[[group_labels[g_lvl]]] <- paste0(
            tab_n[g_lvl, ref_level], " (", tab_pct[g_lvl, ref_level], "%)"
          )
        }

        if (!is.null(test_result$error)) {
          result$p <- paste0("NA (", test_result$error, ")")
        } else {
          result$p <- fmt_p(test_result$p_value, p_digits)
        }
        if (show_test) {
          result$test <- test_result$test_name
        }

        if (OR_col && ncol(tab) == 2 && nrow(tab) == 2) {
          if (OR_method == "dynamic") {
            if (fisher_flag) {
              fisher_obj <- tryCatch(stats::fisher.test(tab), error = function(e) NULL)
              if (!is.null(fisher_obj)) {
                result$OR <- sprintf("%.2f [%.2f–%.2f]", fisher_obj$estimate, fisher_obj$conf.int[1], fisher_obj$conf.int[2])
                if (show_test) result$OR_method <- "Fisher"
              } else {
                result$OR <- "NA (calculation failed)"
                if (show_test) result$OR_method <- "Fisher"
              }
            } else {
              or_obj <- tryCatch(epitools::oddsratio(tab, method = "wald")$measure, error = function(e) NULL)
              result$OR <- if (!is.null(or_obj)) sprintf("%.2f [%.2f–%.2f]", or_obj[2,1], or_obj[2,2], or_obj[2,3]) else "NA (calculation failed)"
              if (show_test) result$OR_method <- "Wald"
            }
          } else if (OR_method == "wald") {
            or_obj <- tryCatch(epitools::oddsratio(tab, method = "wald")$measure, error = function(e) NULL)
            result$OR <- if (!is.null(or_obj)) sprintf("%.2f [%.2f–%.2f]", or_obj[2,1], or_obj[2,2], or_obj[2,3]) else "NA (calculation failed)"
            if (show_test) result$OR_method <- "Wald"
          }
        } else if (OR_col) {
          result$OR <- NA_character_
          if (show_test) result$OR_method <- NA_character_
        }
        if (show_total) {
          result$Total <- paste0(tab_total_n[ref_level], " (", tab_total_pct[ref_level], "%)")
        }

      } else {
        # Hierarchical format: header + indented sub-categories
        # tab_total_n is already a vector of total counts per level
        if (factor_order == "levels" && is.factor(g[[var]])) {
          # Respect original factor level ordering
          sorted_levels <- levels(g[[var]])
          # Filter to only include levels that actually appear in the data
          sorted_levels <- sorted_levels[sorted_levels %in% names(tab_total_n)]
        } else {
          # Default: sort by frequency (descending order)
          sorted_levels <- names(sort(tab_total_n, decreasing = TRUE))
        }
        
        # Create header row for the main variable (no data, just variable name)
        header_row <- tibble(Variable = .clean_variable_name_for_header(var), .indent = 2)
        for (g_lvl in group_levels) {
          header_row[[group_labels[g_lvl]]] <- ""
        }
        header_row$p <- ""
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
          
          # Only first sub-row gets p-value, others get "-"
          if (level == sorted_levels[1]) {
            if (!is.null(test_result$error)) {
              out$p <- paste0("NA (", test_result$error, ")")
            } else {
              out$p <- fmt_p(test_result$p_value, p_digits)
            }
            if (show_test) {
              out$test <- test_result$test_name
            }
          } else {
            out$p <- "-"
            if (show_test) {
              out$test <- "-"
            }
          }
          
          if (OR_col) {
            out$OR <- "-"
            if (show_test) out$OR_method <- "-"
          }
          if (show_total) {
            out$Total <- paste0(tab_total_n[level], " (", tab_total_pct[level], "%)")
          }
          out
        })
        
        # Combine header and sub-rows
        result <- bind_rows(list(header_row), sub_rows)
      }
      return(result)
    }

    # ----- Force ordinal -----
    if (!is.null(force_ordinal) && var %in% force_ordinal) {
      stats <- g %>% group_by(.data[[group_var]]) %>% summarise(
        Q1 = if (round_intg) round_up_half(quantile(.data[[var]], 0.25, na.rm = TRUE), 0) else round(quantile(.data[[var]], 0.25, na.rm = TRUE), 1),
        med = if (round_intg) round_up_half(median(.data[[var]], na.rm = TRUE), 0) else round(median(.data[[var]], na.rm = TRUE), 1),
        Q3 = if (round_intg) round_up_half(quantile(.data[[var]], 0.75, na.rm = TRUE), 0) else round(quantile(.data[[var]], 0.75, na.rm = TRUE), 1), .groups = "drop")
      result <- tibble(Variable = var, .indent = 2)
      for (g_lvl in group_levels) {
        val <- stats %>% filter(.data[[group_var]] == g_lvl)
        result[[group_labels[g_lvl]]] <- if (nrow(val) == 1) {
          paste0(val$med, " [", val$Q1, "–", val$Q3, "]")
        } else {
          "NA [NA–NA]"
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
        result$p <- paste0("NA (", test_result$error, ")")
      } else {
        result$p <- fmt_p(test_result$p_value, p_digits)
      }
      if (show_test) {
        result$test <- test_result$test_name
      }
      
      if (OR_col) result$OR <- NA_character_
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
            if (length(x) >= 3) stats::shapiro.test(x)$p.value else NA_real_
          }, error = function(e) NA_real_)
          result[[paste0("SW_p_", g_lvl)]] <- formatC(sw_p, format = "f", digits = 4)
        }
      }
      return(result)
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
          pval <- if (length(x) >= 3) stats::shapiro.test(x)$p.value else NA_real_
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
    } else if (isTRUE(consider_normality)) {
      # Test normality and track results
      sw_p_all <- tryCatch({
        out <- lapply(group_levels, function(g_lvl) {
          x <- g %>% filter(.data[[group_var]] == g_lvl) %>% pull(.data[[var]])
          pval <- if (length(x) >= 3) stats::shapiro.test(x)$p.value else NA_real_
          setNames(pval, paste0("SW_p_", g_lvl))
        })
        do.call(c, out)
      }, error = function(e) rep(NA, n_levels))
      is_normal <- all(sw_p_all > 0.05, na.rm = TRUE)
      
      # Track normality results for reporting
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
          pval <- if (length(x) >= 3) stats::shapiro.test(x)$p.value else NA_real_
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
    result <- tibble(Variable = var, .indent = 2)
    stats <- g %>% group_by(.data[[group_var]]) %>% summarise(
      mean = mean(.data[[var]], na.rm = TRUE),
      sd = sd(.data[[var]], na.rm = TRUE), .groups = "drop")

    for (g_lvl in group_levels) {
      val <- stats %>% filter(.data[[group_var]] == g_lvl)
      result[[group_labels[g_lvl]]] <- if (nrow(val) == 1) {
        if (round_intg) {
          paste0(round_up_half(val$mean, 0), " ± ", round_up_half(val$sd, 0))
        } else {
          paste0(round(val$mean, 1), " ± ", round(val$sd, 1))
        }
      } else {
        "NA ± NA"
      }
    }

    test_result <- tryCatch({
      if (n_levels == 2) {
        p_val <- stats::t.test(g[[var]] ~ g[[group_var]])$p.value
        list(p_value = p_val, test_name = "t-test", error = NULL)
      } else {
        p_val <- stats::aov(g[[var]] ~ g[[group_var]]) %>% summary() %>% .[[1]] %>% .["Pr(>F)"][[1]][1]
        list(p_value = p_val, test_name = "ANOVA", error = NULL)
      }
    }, error = function(e) {
      # Determine reason for test failure
      group_sizes <- table(g[[group_var]])
      if (any(group_sizes < 2)) {
        reason <- "insufficient group sizes"
      } else if (all(is.na(g[[var]]))) {
        reason <- "all values missing"
      } else if (var(g[[var]], na.rm = TRUE) == 0) {
        reason <- "no variation in values"
      } else {
        reason <- "test failure"
      }
      test_name <- if (n_levels == 2) "t-test" else "ANOVA"
      list(p_value = NA_real_, test_name = test_name, error = reason)
    })
    
    if (!is.null(test_result$error)) {
      result$p <- paste0("NA (", test_result$error, ")")
    } else {
      result$p <- fmt_p(test_result$p_value, p_digits)
    }
    if (show_test) {
      result$test <- test_result$test_name
    }
    
    if (OR_col) result$OR <- NA_character_
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
    return(result)
  }

  out_tbl <- suppressWarnings({
    result <- bind_rows(lapply(vars, function(v) .summarize_var_internal(data, v, force_ordinal, show_test, round_intg, show_total)))
    message("Note: Categorical variables with >2 levels return multiple rows.")
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
    desired <- c("Variable", existing_group_cols, "Total", "p", "OR", "test", "OR_method", normality_cols)
  } else {
    desired <- c("Variable", existing_group_cols, "Total", "p", "OR", normality_cols)
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
                        

  if (!is.null(output_xlsx)) export_to_excel(out_tbl, output_xlsx)
  
  # Write methods document if requested
  if (methods_doc) {
    write_methods_doc(out_tbl, methods_filename, n_levels = n_levels, OR_col = OR_col, source = "ternG")
  }

  # -- Report normality test results -----------------------------------------
  if (numeric_vars_tested > 0) {
    numeric_vars_passed <- numeric_vars_tested - numeric_vars_failed
    passed_pct  <- round((numeric_vars_passed / numeric_vars_tested) * 100, 1)
    param_test    <- if (n_levels == 2) "independent samples t-test" else "one-way ANOVA"
    nonparam_test <- if (n_levels == 2) "Wilcoxon rank-sum" else "Kruskal-Wallis"

    if (consider_normality == "FORCE") {
      message(sprintf(
        "Normality (Shapiro-Wilk): %d of %d continuous variables were normally distributed (%.1f%%).",
        numeric_vars_passed, numeric_vars_tested, passed_pct
      ))
      message(sprintf(
        "consider_normality = 'FORCE': All continuous variables displayed as median [IQR] and tested with nonparametric methods (%s).",
        nonparam_test
      ))
    } else if (isTRUE(consider_normality)) {
      message(sprintf(
        "Normality (Shapiro-Wilk): %d of %d continuous variables were normally distributed (%.1f%%).",
        numeric_vars_passed, numeric_vars_tested, passed_pct
      ))
      message(sprintf(
        "Normally distributed variables: displayed as mean \u00b1 SD, tested with %s. Non-normal variables: displayed as median [IQR], tested with %s.",
        param_test, nonparam_test
      ))
    } else {
      message(sprintf(
        "Normality (Shapiro-Wilk): %d of %d continuous variables were normally distributed (%.1f%%).",
        numeric_vars_passed, numeric_vars_tested, passed_pct
      ))
      message(sprintf(
        "consider_normality = FALSE: All continuous variables displayed as mean \u00b1 SD and tested with %s regardless of distribution.",
        param_test
      ))
    }
  }

  # Insert category header rows if specified
  if (!is.null(category_start) && length(category_start) > 0) {
    
    for (var_name in names(category_start)) {
      # Find the row with this variable name (using trimmed names)
      # For multi-category variables, find the header row (which has empty data in column 2)
      trimmed_vars <- sapply(out_tbl[[1]], function(x) trimws(x, which = "both"))
      var_matches <- which(trimmed_vars == var_name)
      
      # If multiple matches, prefer the one with empty data (the header row)
      if (length(var_matches) > 1) {
        header_match <- var_matches[which(out_tbl[[2]][var_matches] == "" | is.na(out_tbl[[2]][var_matches]))]
        var_idx <- if(length(header_match) > 0) header_match[1] else var_matches[1]
      } else if (length(var_matches) == 1) {
        var_idx <- var_matches[1]
      } else {
        next  # Variable not found, skip
      }
      
      # insert_at is just var_idx - we search the current table each time
      insert_at <- var_idx
      
      # Create category header row with no indentation
      cat_row <- out_tbl[1, ]
      cat_row[[1]][1] <- category_start[var_name]
      # Set all other columns to empty/NA
      for (j in 2:ncol(cat_row)) {
        if (is.numeric(cat_row[[j]])) {
          cat_row[[j]][1] <- NA_real_
        } else {
          cat_row[[j]][1] <- ""
        }
      }
      
      # Insert the category row
      if (insert_at == 1) {
        out_tbl <- rbind(cat_row, out_tbl)
      } else {
        out_tbl <- rbind(
          out_tbl[1:(insert_at - 1), ],
          cat_row,
          out_tbl[insert_at:nrow(out_tbl), ]
        )
      }
    }
  }

  # Replace "0 (NaN%)" with "-" for structurally impossible cells
  # (e.g. a subgroup that cannot logically have any observations in a given column)
  out_tbl <- out_tbl %>%
    dplyr::mutate(dplyr::across(dplyr::where(is.character), ~ gsub("0 \\(NaN%\\)", "-", .x)))

  # Export to Word AFTER category headers are inserted
  if (!is.null(output_docx)) export_to_word(out_tbl, output_docx, round_intg = round_intg, font_size = table_font_size, category_start = category_start, manual_italic_indent = manual_italic_indent, manual_underline = manual_underline)

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

  if (!indent_info_column) out_tbl <- dplyr::select(out_tbl, -dplyr::any_of(".indent"))

  return(out_tbl)
}
