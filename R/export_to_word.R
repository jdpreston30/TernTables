#' Export TernTables output to a formatted Word document
#'
#' @param tbl A tibble created by ternG or ternD
#' @param filename Output file path ending in .docx
#' @param round_intg Logical; if TRUE, adds note about integer rounding. Default is FALSE.
#' @param font_size Numeric; font size for table body. Default is 9.
#' @param category_start Named character vector specifying category headers. Names are header
#'   label text; values are anchor variable names -- either the original column name or the
#'   cleaned display name (both forms accepted).
#' @param manual_italic_indent Character vector of display variable names (post-cleaning) to force into
#'   italicized and indented formatting, matching the appearance of factor sub-category rows (e.g., levels
#'   of a multi-category variable). Use this for rows that should visually appear as sub-items but are not
#'   automatically detected as such.
#' @param manual_underline Character vector of display variable names (post-cleaning) to force into
#'   underlined formatting, matching the appearance of multi-category variable header rows. Use this for
#'   rows that should visually appear as section headers but are not automatically detected as such.
#' @return Invisibly returns the path to the written Word file.
#' @examples
#' \dontrun{
#' data(tern_colon)
#' tbl <- ternD(tern_colon, exclude_vars = c("ID"))
#' word_export(
#'   tbl      = tbl,
#'   filename = file.path(tempdir(), "descriptive.docx"),
#'   category_start = c(
#'     "Patient Demographics"  = "Age (yr)",
#'     "Tumor Characteristics" = "Positive Lymph Nodes (n)"
#'   )
#' )
#' }
#' @export
word_export <- function(tbl, filename, round_intg = FALSE, font_size = 9, category_start = NULL, manual_italic_indent = NULL, manual_underline = NULL) {
  # Keep the table as-is
  modified_tbl <- tbl

  # Insert category header rows before extracting .indent
  if (!is.null(category_start) && length(category_start) > 0) {
    for (header_label in names(category_start)) {
      var_name <- category_start[[header_label]]
      trimmed_vars <- sapply(modified_tbl[[1]], function(x) trimws(x, which = "both"))
      var_matches <- which(trimmed_vars == var_name)

      # Fallback: if raw name not found, try matching the cleaned display form
      # This allows anchors to be either the original column name (e.g. "Age_Years")
      # or the cleaned display name (e.g. "Age (yr)")
      if (length(var_matches) == 0) {
        cleaned_anchor <- .clean_variable_name_for_header(var_name)
        var_matches <- which(trimmed_vars == cleaned_anchor)
      }

      if (length(var_matches) > 1) {
        second_col <- modified_tbl[[2]]
        header_match <- var_matches[which(second_col[var_matches] == "" | is.na(second_col[var_matches]))]
        var_idx <- if (length(header_match) > 0) header_match[1] else var_matches[1]
      } else if (length(var_matches) == 1) {
        var_idx <- var_matches[1]
      } else {
        next
      }

      cat_row <- modified_tbl[1, ]
      cat_row[[1]][1] <- header_label
      for (j in 2:ncol(cat_row)) {
        if (is.numeric(cat_row[[j]])) cat_row[[j]][1] <- NA_real_
        else cat_row[[j]][1] <- ""
      }
      if (".indent" %in% colnames(cat_row)) cat_row[[".indent"]][1] <- 0L

      if (var_idx == 1) {
        modified_tbl <- rbind(cat_row, modified_tbl)
      } else {
        modified_tbl <- rbind(
          modified_tbl[1:(var_idx - 1), ],
          cat_row,
          modified_tbl[var_idx:nrow(modified_tbl), ]
        )
      }
    }
  }

  # Store .indent column for later use, then remove it from display
  indent_col <- if (".indent" %in% colnames(modified_tbl)) modified_tbl[[".indent"]] else NULL
  if (!is.null(indent_col)) {
    modified_tbl <- modified_tbl %>% select(-dplyr::any_of(".indent"))
  }

  # Track which rows are category headers for formatting
  category_rows <- NULL
  if (!is.null(category_start) && length(category_start) > 0) {
    for (category_label in names(category_start)) {
      trimmed_vars <- sapply(modified_tbl[[1]], function(x) trimws(x, which = "both"))
      cat_idx <- which(trimmed_vars == category_label)
      if (length(cat_idx) > 0) category_rows <- c(category_rows, cat_idx)
    }
  }
  
  # Modify column headers to add symbols and line breaks
  original_colnames <- colnames(modified_tbl)
  new_colnames <- original_colnames
  
  # Replace first column header with category hierarchy
  new_colnames[1] <- "Category\n   Variable"
  
  # Add line breaks for sample sizes (no asterisks)
  for (i in 2:length(new_colnames)) {
    if (!new_colnames[i] %in% c("P", "test", "OR", "OR_method") && !grepl("^Total", new_colnames[i])) {
      new_colnames[i] <- gsub(" \\(n = ", "\n(n = ", new_colnames[i])
    } else if (new_colnames[i] == "P") {
      new_colnames[i] <- "P value"
    } else if (grepl("^Total", new_colnames[i])) {
      # Total column already has line break from ternG, no change needed
    }
  }
  colnames(modified_tbl) <- new_colnames
  
  # Detect which statistical tests were actually used
  has_test_column <- "test" %in% colnames(tbl)
  tests_used <- character(0)
  
  if (has_test_column) {
    tests_used <- unique(tbl$test)
    tests_used <- tests_used[!is.na(tests_used) & tests_used != "" & tests_used != "-"]
  }
  
  # Create flextable
  ft <- flextable(modified_tbl) %>%
    font(fontname = "Arial", part = "all") %>%
    fontsize(size = font_size, part = "all") %>%
    bg(bg = "#cdcdcd", part = "header") %>%
    # Set body alignment: Variable column left, all others center
    align(align = "left", j = 1, part = "body") %>%
    align(align = "center", j = 2:ncol(modified_tbl), part = "body") %>%
    # Set header alignment: Variable column left, all others center
    align(align = "left", j = 1, part = "header") %>%
    align(align = "center", j = 2:ncol(modified_tbl), part = "header") %>%
    border_remove() %>%
    border(i = 1, border.bottom = fp_border(color = "black", width = 0.5), part = "header") %>%
    padding(padding.top = 0, padding.bottom = 1, part = "body") %>%
    padding(padding.left = 0, padding.right = 6, part = "body") %>%
    padding(padding.left = 3, padding.right = 6, part = "header")
  
  # Apply variable formatting (use modified_tbl which has category headers inserted)
  # Use the .indent column to determine padding (or spaces if .indent doesn't exist for backwards compat)
  variable_col <- modified_tbl[[1]]
  
  for (i in seq_along(variable_col)) {
    # Skip category header rows - they're formatted separately
    if (!is.null(category_rows) && i %in% category_rows) {
      next
    }
    
    # Determine indentation level
    if (!is.null(indent_col) && i <= length(indent_col) && !is.na(indent_col[i])) {
      indent_level <- indent_col[i]
    } else {
      # Fallback: count spaces (backwards compatibility)
      var_name <- variable_col[i]
      indent_level <- nchar(var_name) - nchar(trimws(var_name, which = "left"))
    }
    
    # Check for manual formatting overrides
    trimmed_var <- trimws(variable_col[i], which = "both")
    manual_italic <- !is.null(manual_italic_indent) && trimmed_var %in% manual_italic_indent
    manual_under <- !is.null(manual_underline) && trimmed_var %in% manual_underline
    
    # Apply formatting based on indentation level
    # Add universal base padding of 3 to all rows
    base_padding <- 3
    
    # Manual formatting overrides natural formatting
    if (manual_italic) {
      # Manually specified to be indented and italicized (like level 6)
      ft <- ft %>% 
        padding(i = i, j = 1, padding.left = 12 + base_padding, part = "body") %>%
        italic(i = i, j = 1, part = "body")
    } else if (manual_under) {
      # Manually specified to be underlined (like multi-category header)
      ft <- ft %>% 
        padding(i = i, j = 1, padding.left = 6 + base_padding, part = "body") %>%
        style(i = i, j = 1, pr_t = fp_text(underlined = TRUE, font.family = "Arial", font.size = font_size), part = "body")
    } else if (indent_level == 0) {
      # Category headers from tibble - just base padding
      ft <- ft %>% padding(i = i, j = 1, padding.left = base_padding, part = "body")
    } else if (indent_level == 2) {
      # Regular variables
      is_empty <- length(modified_tbl) >= 2 && (modified_tbl[[2]][i] == "" || is.na(modified_tbl[[2]][i]))
      if (is_empty) {
        # Multi-category header (underlined)
        ft <- ft %>% 
          padding(i = i, j = 1, padding.left = 6 + base_padding, part = "body") %>%
          style(i = i, j = 1, pr_t = fp_text(underlined = TRUE, font.family = "Arial", font.size = font_size), part = "body")
      } else {
        # Regular variable
        ft <- ft %>% padding(i = i, j = 1, padding.left = 6 + base_padding, part = "body")
      }
    } else if (indent_level == 6) {
      # Stratified variables (italicized)
      ft <- ft %>% 
        padding(i = i, j = 1, padding.left = 12 + base_padding, part = "body") %>%
        italic(i = i, j = 1, part = "body")
    }
  }
  
  # Bold header row - don't change alignment here
  ft <- ft %>% bold(part = "header")
  
  # Format category header rows if they exist
  if (!is.null(category_rows) && length(category_rows) > 0) {
    base_padding <- 3
    for (cat_row_idx in category_rows) {
      ft <- ft %>%
        merge_at(i = cat_row_idx, j = 1:ncol(modified_tbl), part = "body") %>%
        bold(i = cat_row_idx, part = "body") %>%
        bg(i = cat_row_idx, bg = "white", part = "body") %>%
        border(i = cat_row_idx, border.bottom = fp_border(color = "black", width = 0.5), part = "body") %>%
        align(i = cat_row_idx, align = "left", part = "body") %>%
        padding(i = cat_row_idx, j = 1, padding.left = base_padding, padding.top = 2, padding.bottom = 2, part = "body")
    }
  }
  
  # P value header is already set in new_colnames, no special formatting needed
  
  # Bold significant p-values (use modified_tbl which has category headers)
  # Note: column is now called "P value" after renaming
  if ("P value" %in% colnames(modified_tbl)) {
    p_col_index <- which(colnames(modified_tbl) == "P value")
    sig_rows <- which(sapply(modified_tbl[[p_col_index]], function(p_val) {
      if (is.na(p_val) || is.null(p_val) || p_val == "" || p_val == "-") return(FALSE)
      if (grepl("E-", p_val)) return(TRUE)  # Scientific notation with negative exponent
      p_numeric <- suppressWarnings(as.numeric(p_val))
      if (!is.na(p_numeric) && p_numeric < 0.05) return(TRUE)
      return(FALSE)
    }))
    if (length(sig_rows) > 0) {
      ft <- bold(ft, i = sig_rows, j = p_col_index, part = "body")
    }
  }
  
  # Bold significant odds ratios (95% CI excludes 1) - use modified_tbl
  if ("OR" %in% colnames(modified_tbl)) {
    or_col_index <- which(colnames(modified_tbl) == "OR")
    sig_or_rows <- which(sapply(modified_tbl$OR, function(or_val) {
      if (is.na(or_val) || is.null(or_val) || or_val == "" || or_val == "-" || grepl("NA", or_val)) return(FALSE)
      # Parse format: "1.23 [0.45-2.67]"
      match <- regmatches(or_val, regexec("([0-9.]+)\\s*\\[([0-9.]+)\u2013([0-9.]+)\\]", or_val))
      if (length(match[[1]]) == 4) {
        lower_ci <- as.numeric(match[[1]][3])
        upper_ci <- as.numeric(match[[1]][4])
        # Significant if CI doesn't include 1
        return(!is.na(lower_ci) && !is.na(upper_ci) && (lower_ci > 1 || upper_ci < 1))
      }
      return(FALSE)
    }))
    if (length(sig_or_rows) > 0) {
      ft <- bold(ft, i = sig_or_rows, j = or_col_index, part = "body")
    }
  }

  # Shrink all columns to fit their content, then lock row heights exactly.
  # height() and hrule() must come AFTER autofit() -- autofit resets row heights
  # as a side effect of its column-width calculation.
  ft <- autofit(ft)
  ft <- ft %>%
    height(height = font_size / 72 * 1.5, part = "body") %>%
    flextable::hrule(rule = "exact", part = "body")

  # Re-apply category header row heights after the blanket lock
  if (!is.null(category_rows) && length(category_rows) > 0) {
    for (cat_row_idx in category_rows) {
      ft <- ft %>% height(i = cat_row_idx, height = font_size / 72 * 2.05, part = "body")
    }
  }

  # Create Word document
  doc <- read_docx() %>% body_add_flextable(ft)
  dir.create(dirname(filename), recursive = TRUE, showWarnings = FALSE)
  print(doc, target = filename)
  invisible(filename)
}