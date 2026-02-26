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
#' @param source Character; \code{"ternG"} or \code{"ternD"}. Controls which
#'   section is populated with dynamic test information. Default \code{"ternG"}.
#' @export
write_methods_doc <- function(tbl, filename, n_levels = 2, OR_col = FALSE,
                              source = "ternG") {

  # ── Detect tests used (only meaningful when called from ternG) ─────────────
  tests_used <- character(0)
  if (!is.null(tbl) && "test" %in% colnames(tbl)) {
    tests_used <- unique(tbl[["test"]])
    tests_used <- tests_used[!is.na(tests_used) & tests_used != "" & tests_used != "-"]
  }
  has_ttest    <- any(grepl("t-test",       tests_used, ignore.case = TRUE))
  has_anova    <- any(grepl("ANOVA",        tests_used, ignore.case = TRUE))
  has_wilcoxon <- any(grepl("Wilcoxon",    tests_used, ignore.case = TRUE))
  has_kruskal  <- any(grepl("Kruskal",     tests_used, ignore.case = TRUE))
  has_fisher   <- any(grepl("Fisher",      tests_used, ignore.case = TRUE))
  has_chisq    <- any(grepl("Chi-squared", tests_used, ignore.case = TRUE))

  # ── Shared descriptive sentence ─────────────────────────────────────────────
  desc_sentence <- paste0(
    "Continuous variables are presented as mean \u00b1 SD for normally distributed ",
    "variables or median [IQR] for non-normally distributed or ordinal variables. ",
    "Categorical variables are presented as n (%). ",
    "Normality of continuous variables was assessed using the Shapiro-Wilk test; ",
    "variables with a Shapiro-Wilk p > 0.05 were considered normally distributed."
  )

  # ── Helper: categorical comparison sentence ──────────────────────────────────
  cat_sentence <- function(f, c) {
    if (f && c) {
      "Categorical variables were compared using Chi-squared tests, or Fisher's exact tests when expected cell counts were less than 5. "
    } else if (f) {
      "Categorical variables were compared using Fisher's exact tests. "
    } else if (c) {
      "Categorical variables were compared using Chi-squared tests. "
    } else {
      "Categorical variables were compared using Chi-squared tests, or Fisher's exact tests when expected cell counts were less than 5. "
    }
  }

  or_sentence  <- if (OR_col) "For binary categorical variables, odds ratios with 95% confidence intervals were computed. " else ""
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
    sec2_body <- paste0(desc_sentence, " ", s2_cont, cat_sentence(has_fisher, has_chisq), or_sentence, sig_sentence)
  } else {
    sec2_body <- paste0(
      desc_sentence, " ",
      "Normally distributed continuous variables were compared between groups using Welch's independent samples t-test; ",
      "non-normally distributed or ordinal continuous variables were compared using the Wilcoxon rank-sum test. ",
      "Categorical variables were compared using Chi-squared tests, or Fisher's exact tests when expected cell counts were less than 5. ",
      sig_sentence
    )
  }

  # ── Section 3: Three-or-more-group ───────────────────────────────────────────
  if (source == "ternG" && n_levels >= 3) {
    s3_cont <- if (has_anova && has_kruskal) {
      "Normally distributed continuous variables were compared across groups using one-way ANOVA; non-normally distributed or ordinal continuous variables were compared using the Kruskal-Wallis test. "
    } else if (has_anova) {
      "Continuous variables were compared across groups using one-way ANOVA. "
    } else if (has_kruskal) {
      "Continuous variables were compared across groups using the Kruskal-Wallis test. "
    } else {
      "Normally distributed continuous variables were compared using one-way ANOVA; non-normally distributed variables were compared using the Kruskal-Wallis test. "
    }
    sec3_body <- paste0(desc_sentence, " ", s3_cont, cat_sentence(has_fisher, has_chisq), or_sentence, sig_sentence)
  } else {
    sec3_body <- paste0(
      desc_sentence, " ",
      "Normally distributed continuous variables were compared across groups using one-way ANOVA; ",
      "non-normally distributed or ordinal continuous variables were compared using the Kruskal-Wallis test. ",
      "Categorical variables were compared using Chi-squared tests, or Fisher's exact tests when expected cell counts were less than 5. ",
      sig_sentence
    )
  }

  # ── Build Word document ───────────────────────────────────────────────────────
  head_props <- fp_text(font.size = 11, font.family = "Arial", bold = TRUE)
  body_props <- fp_text(font.size = 11, font.family = "Arial")

  make_heading <- function(txt) fpar(ftext(txt, prop = head_props))
  make_body    <- function(txt) fpar(ftext(txt, prop = body_props))

  doc <- read_docx() |>
    body_add_fpar(make_heading("If you used the package to generate descriptive tables (ternD):")) |>
    body_add_par("", style = "Normal") |>
    body_add_fpar(make_body(sec1_body)) |>
    body_add_par("", style = "Normal") |>
    body_add_fpar(make_heading("If you used the package to generate two-group comparison tables (ternG):")) |>
    body_add_par("", style = "Normal") |>
    body_add_fpar(make_body(sec2_body)) |>
    body_add_par("", style = "Normal") |>
    body_add_fpar(make_heading("If you used the package to generate three-or-more-group comparison tables (ternG):")) |>
    body_add_par("", style = "Normal") |>
    body_add_fpar(make_body(sec3_body))

  print(doc, target = filename)
  cli::cli_alert_success("Methods document written to: {filename}")
}
