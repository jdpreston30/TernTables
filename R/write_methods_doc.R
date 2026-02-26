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
#' @return Invisibly returns the path to the written Word file.
#' @examples
#' \dontrun{
#' data(tern_colon)
#' tbl <- ternG(tern_colon, exclude_vars = c("ID"), group_var = "Recurrence")
#' write_methods_doc(tbl, filename = file.path(tempdir(), "methods.docx"))
#' }
#' @export
write_methods_doc <- function(tbl, filename, n_levels = 2, OR_col = FALSE,
                              source = "ternG") {

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
  has_fisher   <- any(grepl("Fisher",      tests_used, ignore.case = TRUE))
  has_chisq    <- any(grepl("Chi-squared", tests_used, ignore.case = TRUE))

  # ── Shared descriptive sentence ─────────────────────────────────────────────
  desc_sentence <- paste0(
    "Continuous variables are presented as mean \u00b1 SD for normally distributed ",
    "variables or median [IQR] for non-normally distributed or ordinal variables. ",
    "Categorical variables are presented as n (%). ",
    "Normality of continuous variables was assessed using the Shapiro-Wilk test applied per group; ",
    "a variable was considered normally distributed only if all groups had a Shapiro-Wilk p > 0.05."
  )

  # ── Helper: categorical comparison sentence ──────────────────────────────────
  cat_sentence <- function(f, c) {
    if (f && c) {
      "Categorical variables were compared using Chi-squared tests, or Fisher's exact tests when any expected cell count was less than 5 (Cochran criterion). "
    } else if (f) {
      "Categorical variables were compared using Fisher's exact tests. "
    } else if (c) {
      "Categorical variables were compared using Chi-squared tests. "
    } else {
      "Categorical variables were compared using Chi-squared tests, or Fisher's exact tests when any expected cell count was less than 5 (Cochran criterion). "
    }
  }

  or_sentence  <- if (OR_col) paste0(
    "For binary categorical variables, unadjusted odds ratios (OR) with 95% confidence intervals (CI) were calculated, ",
    "with the first group serving as the reference category. ",
    "Where all expected cell counts were five or greater, OR and 95% CI were derived using the Wald method. ",
    "Where any expected cell count was less than five (Cochran criterion), OR and 95% CI were derived from Fisher's exact test. "
  ) else ""
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
      "Categorical variables were compared using Chi-squared tests, or Fisher's exact tests when any expected cell count was less than 5 (Cochran criterion). ",
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
    omnibus_note <- "Omnibus P values are reported; pairwise post-hoc comparisons were not performed. "
    sec3_body <- paste0(desc_sentence, " ", s3_cont, omnibus_note, cat_sentence(has_fisher, has_chisq), sig_sentence)
  } else {
    sec3_body <- paste0(
      desc_sentence, " ",
      "Normally distributed continuous variables were compared across groups using one-way ANOVA; ",
      "non-normally distributed or ordinal continuous variables were compared using the Kruskal-Wallis test. ",
      "Omnibus P values are reported; pairwise post-hoc comparisons were not performed. ",
      "Categorical variables were compared using Chi-squared tests, or Fisher's exact tests when any expected cell count was less than 5 (Cochran criterion). ",
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

  dir.create(dirname(filename), recursive = TRUE, showWarnings = FALSE)
  print(doc, target = filename)
  cli::cli_alert_success("Methods document written to: {filename}")
  invisible(filename)
}
