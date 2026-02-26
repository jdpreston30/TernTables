#' Write methods section document for ternG output
#'
#' @param tbl A tibble created by ternG
#' @param filename Output file path ending in .docx
#' @param n_levels Number of group levels (2 for two-group, 3+ for multi-group)
#' @param OR_col Logical; whether odds ratios were calculated
#' @export
write_methods_doc <- function(tbl, filename, n_levels = 2, OR_col = FALSE) {
  
  # Detect which statistical tests were actually used
  has_test_column <- "test" %in% colnames(tbl)
  tests_used <- character(0)
  
  if (has_test_column) {
    tests_used <- unique(tbl$test)
    tests_used <- tests_used[!is.na(tests_used) & tests_used != "" & tests_used != "-"]
  }
  
  # Determine which tests were used
  has_ttest <- any(grepl("t-test", tests_used, ignore.case = TRUE))
  has_anova <- any(grepl("ANOVA", tests_used, ignore.case = TRUE))
  has_wilcoxon <- any(grepl("Wilcoxon", tests_used, ignore.case = TRUE))
  has_kruskal <- any(grepl("Kruskal", tests_used, ignore.case = TRUE))
  has_fisher <- any(grepl("Fisher", tests_used, ignore.case = TRUE))
  has_chisq <- any(grepl("Chi-squared", tests_used, ignore.case = TRUE))
  
  # Build methods text
  methods_text <- ""
  
  # Always start with descriptive statistics sentence
  methods_text <- paste0(methods_text, "Continuous variables are presented as mean ± SD for normally distributed variables or median [IQR] for non-normally distributed or ordinal variables. Categorical variables are presented as n (%). ")
  
  # Add comparison methods based on number of groups
  if (n_levels == 2) {
    # Two-group comparisons
    if (has_ttest) {
      methods_text <- paste0(methods_text, "For two-group comparisons of continuous variables with normal distributions, Welch's t-test was used. ")
    }
    
    if (has_wilcoxon) {
      methods_text <- paste0(methods_text, "For two-group comparisons of continuous variables with non-normal distributions or ordinal variables, the Wilcoxon rank-sum test was used. ")
    }
    
    if (has_fisher || has_chisq) {
      if (has_fisher && has_chisq) {
        methods_text <- paste0(methods_text, "Categorical variables were compared using Chi-squared tests, or Fisher's exact tests when expected cell counts were less than 5. ")
      } else if (has_fisher) {
        methods_text <- paste0(methods_text, "Categorical variables were compared using Fisher's exact tests. ")
      } else {
        methods_text <- paste0(methods_text, "Categorical variables were compared using Chi-squared tests. ")
      }
    }
  } else {
    # Multi-group comparisons (3+ groups)
    if (has_anova) {
      methods_text <- paste0(methods_text, "For multi-group comparisons of continuous variables with normal distributions, one-way ANOVA was used. ")
    }
    
    if (has_kruskal) {
      methods_text <- paste0(methods_text, "For multi-group comparisons of continuous variables with non-normal distributions or ordinal variables, the Kruskal-Wallis test was used. ")
    }
    
    if (has_fisher || has_chisq) {
      if (has_fisher && has_chisq) {
        methods_text <- paste0(methods_text, "Categorical variables were compared using Chi-squared tests, or Fisher's exact tests when expected cell counts were less than 5. ")
      } else if (has_fisher) {
        methods_text <- paste0(methods_text, "Categorical variables were compared using Fisher's exact tests. ")
      } else {
        methods_text <- paste0(methods_text, "Categorical variables were compared using Chi-squared tests. ")
      }
    }
  }
  
  # Add odds ratio statement if applicable
  if (OR_col) {
    methods_text <- paste0(methods_text, "For binary categorical variables, odds ratios with 95% confidence intervals were computed. ")
  }
  
  # Add significance statement
  methods_text <- paste0(methods_text, "Statistical significance was defined as p ≤ 0.05.")
  
  # Create Word document
  fp_text_props <- fp_text(font.size = 11, font.family = "Arial")

  doc <- read_docx() %>%
    body_add_par("", style = "Normal") %>%
    body_add_fpar(
      fpar(
        ftext(methods_text, prop = fp_text_props)
      )
    )

  # Save document
  print(doc, target = filename)
  message(paste0("Methods document written to: ", filename))
}
