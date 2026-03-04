#' Write a methods section document for use with TernTables output
#'
#' Generates a Word document containing boilerplate methods text for all three
#' table types produced by TernTables (descriptive, two-group comparison, and
#' three-or-more-group comparison). Each section is headed by a clear label so
#' the user can copy the relevant paragraph directly into a manuscript. When
#' called from \code{ternG}, the two-group or multi-group section is populated
#' with the statistical tests that were actually used; all other sections use
#' generic boilerplate. When called from \code{ternD}, all comparison sections
#' use generic boilerplate.
#'
#' @param tbl A tibble created by \code{ternG} or \code{ternD}, or \code{NULL}
#'   when generating a generic document.
#' @param filename Output file path ending in \code{.docx}.
#' @param n_levels Number of group levels used in \code{ternG} (2 for two-group,
#'   3+ for multi-group). Ignored when called from \code{ternD}.
#' @param OR_col Logical; whether odds ratios were calculated. Default \code{FALSE}.
#' @param OR_method Character; the OR calculation method used in \code{ternG}.
#'   \code{"dynamic"} (default) means Wald when all expected cells >= 5, Fisher's exact
#'   otherwise. \code{"wald"} means Wald was forced regardless of cell counts. Controls
#'   the OR description in the generated methods paragraph.
#' @param source Character; \code{"ternG"} or \code{"ternD"}. Controls which
#'   section is populated with dynamic test information. Default \code{"ternG"}.
#' @param post_hoc Logical; whether pairwise post-hoc testing was requested
#'   (\code{post_hoc = TRUE} in \code{ternG}). When \code{TRUE} and
#'   \code{n_levels >= 3}, the three-group methods paragraph is updated to
#'   describe the post-hoc test pairing (Games-Howell or Dunn's + Holm).
#'   Default \code{FALSE}.
#' @return Invisibly returns the methods paragraph text as a character string.
#'   Useful for programmatic inspection or testing without opening the Word file.
#' @examples
#' \donttest{
#' data(tern_colon)
#' tbl <- ternG(tern_colon, exclude_vars = c("ID"), group_var = "Recurrence",
#'             methods_doc = FALSE)
#' write_methods_doc(tbl, filename = file.path(tempdir(), "methods.docx"))
#' }
#' @export
write_methods_doc <- function(tbl, filename, n_levels = 2, OR_col = FALSE,
                              OR_method = "dynamic", source = "ternG", post_hoc = FALSE) {

  # ── Detect tests used (only meaningful when called from ternG) ─────────────
  tests_used <- character(0)
  if (!is.null(tbl) && "test" %in% colnames(tbl)) {
    tests_used <- unique(tbl[["test"]])
    tests_used <- tests_used[!is.na(tests_used) & tests_used != "" & tests_used != "-"]
  }
  has_ttest    <- any(grepl("Welch t-test", tests_used, ignore.case = TRUE))
  has_anova    <- any(grepl("ANOVA",        tests_used, ignore.case = TRUE))
  has_wilcoxon <- any(grepl("Wilcoxon",    tests_used, ignore.case = TRUE))
  has_kruskal  <- any(grepl("Kruskal",     tests_used, ignore.case = TRUE))
  has_fisher     <- any(grepl("Fisher",      tests_used, ignore.case = TRUE))
  has_fisher_sim <- any(grepl("simulated",   tests_used, ignore.case = TRUE))
  has_chisq      <- any(grepl("Chi-squared", tests_used, ignore.case = TRUE))

  # ── Shared descriptive sentence ─────────────────────────────────────────────
  # Phrasing varies slightly: grouped analyses reference "comparison groups";
  # descriptive-only analyses reference "all available observations".
  if (source == "ternG") {
    normality_context_n3       <- "variables with fewer than three observations in any comparison group were treated as non-normally distributed (conservative fail-safe); "
    normality_context_skewness <- "variables where any comparison group had absolute skewness exceeding 2.0 were treated as non-normally distributed regardless of sample size, "
    normality_context_clt      <- "variables where all comparison groups comprised 30 or more observations were treated as normally distributed on the basis of the Central Limit Theorem; "
    normality_context_sw       <- "for all remaining variables, the Shapiro-Wilk test was applied within each comparison group, and p\u2009>\u20090.05 in all groups was used as the threshold for normality."
  } else {
    normality_context_n3       <- "variables with fewer than three available observations were treated as non-normally distributed (conservative fail-safe); "
    normality_context_skewness <- "variables with absolute skewness exceeding 2.0 were treated as non-normally distributed regardless of sample size, "
    normality_context_clt      <- "variables with 30 or more non-missing observations were treated as normally distributed on the basis of the Central Limit Theorem; "
    normality_context_sw       <- "for all remaining variables, the Shapiro-Wilk test was applied and p\u2009>\u20090.05 was used as the threshold for normality."
  }
  desc_sentence <- paste0(
    "Continuous variables are presented as mean \u00b1 SD if normally distributed or ",
    "median [IQR] if non-normally distributed or ordinal. ",
    "Categorical variables are presented as n (%). ",
    "Distribution of each continuous variable was evaluated using a sequential four-gate algorithm: ",
    normality_context_n3,
    normality_context_skewness,
    "as extreme skewness indicates a distribution for which parametric assumptions are inappropriate irrespective of the Central Limit Theorem; ",
    normality_context_clt,
    normality_context_sw
  )

  # ── Helper: categorical comparison sentence ──────────────────────────────────
  cat_sentence <- function(f, c, s = FALSE) {
    base <- if (f && c) {
      "Categorical variables were compared using Chi-squared tests, or Fisher's exact tests when any expected cell count was less than 5 (Cochran criterion). "
    } else if (f) {
      "Categorical variables were compared using Fisher's exact tests. "
    } else if (c) {
      "Categorical variables were compared using Chi-squared tests. "
    } else {
      "Categorical variables were compared using Chi-squared tests, or Fisher's exact tests when any expected cell count was less than 5 (Cochran criterion). "
    }
    if (s) base <- paste0(base,
      "Where the exact algorithm was computationally infeasible (workspace limit exceeded), ",
      "a Monte Carlo simulation (B\u00a0=\u00a010,000 replicates) was used in place of exact enumeration; ",
      "seed was fixed (\u2009\u2009getOption(\"TernTables.seed\"), default 42) to ensure reproducibility. ")
    base
  }

  or_sentence  <- if (OR_col) {
    or_method_detail <- if (OR_method == "wald") {
      "OR and 95% CI were derived using the Wald method for all variables. "
    } else {
      paste0(
        "Where all expected cell counts were five or greater, OR and 95% CI were derived using the Wald method. ",
        "Where any expected cell count was less than five (Cochran criterion), OR and 95% CI were derived from Fisher's exact test. "
      )
    }
    paste0(
      "For binary categorical variables and two-level categorical variables (e.g. Male/Female), ",
      "unadjusted odds ratios (OR) with 95% confidence intervals (CI) were calculated, ",
      "with the reference level (factor level 1, or alphabetical first for non-factors) serving as the reference category. ",
      or_method_detail
    )
  } else ""
  sig_sentence <- "Statistical significance was defined as p < 0.05."

  # ── Section 1: Descriptive ───────────────────────────────────────────────────
  sec1_body <- paste0(desc_sentence, " ", sig_sentence)

  # ── Section 2: Two-group ─────────────────────────────────────────────────────
  if (source == "ternG" && n_levels == 2) {
    s2_cont <- if (has_ttest && has_wilcoxon) {
      "Normally distributed continuous variables were compared between groups using Welch's independent samples t-test; non-normally distributed or ordinal continuous variables were compared using the Wilcoxon rank-sum test. "
    } else if (has_ttest) {
      "Continuous variables were compared between groups using Welch's independent samples t-test. "
    } else if (has_wilcoxon) {
      "Continuous variables were compared between groups using the Wilcoxon rank-sum test. "
    } else {
      "Normally distributed continuous variables were compared using Welch's independent samples t-test; non-normally distributed variables were compared using the Wilcoxon rank-sum test. "
    }
    sec2_body <- paste0(desc_sentence, " ", s2_cont, cat_sentence(has_fisher, has_chisq, has_fisher_sim), or_sentence, sig_sentence)
  } else {
    sec2_body <- paste0(
      desc_sentence, " ",
      "Normally distributed continuous variables were compared between groups using Welch's independent samples t-test; ",
      "non-normally distributed or ordinal continuous variables were compared using the Wilcoxon rank-sum test. ",
      "Categorical variables were compared using Chi-squared tests, or Fisher's exact tests when any expected cell count was less than 5 (Cochran criterion). ",
      sig_sentence
    )
  }

  # ── Section 3: Three-or-more-group ───────────────────────────────────────────
  if (source == "ternG" && n_levels >= 3) {
    s3_cont <- if (has_anova && has_kruskal) {
      "Normally distributed continuous variables were compared across groups using Welch's one-way ANOVA; non-normally distributed or ordinal continuous variables were compared using the Kruskal-Wallis test. "
    } else if (has_anova) {
      "Continuous variables were compared across groups using Welch's one-way ANOVA. "
    } else if (has_kruskal) {
      "Continuous variables were compared across groups using the Kruskal-Wallis test. "
    } else {
      "Normally distributed continuous variables were compared using Welch's one-way ANOVA; non-normally distributed variables were compared using the Kruskal-Wallis test. "
    }
    omnibus_note <- if (post_hoc) {
      paste0(
        "Omnibus P values are reported. ",
        "For variables with a significant omnibus P value (p\u2009<\u20090.05), pairwise post-hoc comparisons were performed: ",
        "normally distributed variables were compared using the Games-Howell test; ",
        "non-normally distributed and ordinal variables were compared using Dunn\u2019s test with Holm correction for multiple comparisons. ",
        "Results are presented using compact letter display (CLD) notation \u2014 superscript letters appended to cell values \u2014 ",
        "whereby groups sharing a superscript letter are not significantly different from each other. ",
        "Categorical variables were not included in post-hoc comparisons. "
      )
    } else {
      "Omnibus P values are reported; pairwise post-hoc comparisons were not performed. "
    }
    sec3_body <- paste0(desc_sentence, " ", s3_cont, omnibus_note, cat_sentence(has_fisher, has_chisq, has_fisher_sim), or_sentence, sig_sentence)
  } else {
    sec3_body <- paste0(
      desc_sentence, " ",
      "Normally distributed continuous variables were compared across groups using Welch's one-way ANOVA; ",
      "non-normally distributed or ordinal continuous variables were compared using the Kruskal-Wallis test. ",
      "Omnibus P values are reported; pairwise post-hoc comparisons were not performed. ",
      "Categorical variables were compared using Chi-squared tests, or Fisher's exact tests when any expected cell count was less than 5 (Cochran criterion). ",
      sig_sentence
    )
  }

  # ── Select the paragraph that matches this specific run ──────────────────────
  methods_body <- if (source == "ternD") {
    sec1_body
  } else if (source == "ternG" && n_levels == 2) {
    sec2_body
  } else {
    sec3_body
  }

  # ── Build Word document ───────────────────────────────────────────────────────
  head_props   <- fp_text(font.size = 11, font.family = "Arial", bold = TRUE)
  body_props   <- fp_text(font.size = 11, font.family = "Arial")
  footer_props <- fp_text(font.size = 9,  font.family = "Arial", italic = TRUE,
                          color = "#555555")

  # Read version from installed package and strip any development tag (.9000 etc.)
  # so the footer always shows the public release number (e.g. "1.6.3").
  raw_ver  <- unclass(utils::packageVersion("TernTables"))[[1]]
  pkg_ver  <- paste(raw_ver[seq_len(min(3L, length(raw_ver)))], collapse = ".")
  footer_txt   <- paste0(
    "Generated by TernTables v", pkg_ver, ". ",
    "This paragraph reflects the specific analysis configuration used in this run. ",
    "For documentation of other configurations (descriptive, two-group, three-or-more-group), ",
    "see the package vignette: vignette(\"getting-started\", package = \"TernTables\")."
  )

  doc <- read_docx() |>
    body_add_fpar(fpar(ftext("Statistical Methods", prop = head_props))) |>
    body_add_par("", style = "Normal") |>
    body_add_fpar(fpar(ftext(methods_body, prop = body_props))) |>
    body_add_par("", style = "Normal") |>
    body_add_fpar(fpar(ftext(footer_txt, prop = footer_props)))

  dir.create(dirname(filename), recursive = TRUE, showWarnings = FALSE)
  print(doc, target = filename)
  cli::cli_alert_success("Methods document written to: {filename}")
  invisible(methods_body)
}
