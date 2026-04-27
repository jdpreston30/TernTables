#' Write a methods section Word document for TernTables output
#'
#' Generates a Word document containing a methods paragraph describing the
#' statistical approach used in a specific \code{ternG} or \code{ternD} run.
#' The paragraph is fully dynamic: it reflects the tests that were actually used,
#' the number of comparison groups, whether odds ratios were calculated, and
#' whether post-hoc testing was performed. It is headed by a bold
#' \strong{Statistical Methods} label and followed by a brief attribution footer.
#'
#' When \code{boilerplate = TRUE}, all run-specific arguments are ignored and a
#' comprehensive reference document is written instead, covering all five standard
#' TernTables configurations with package-default phrasing. See the
#' \code{boilerplate} parameter for details.
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
#' @param categorical_posthoc Logical; whether adjusted standardized residuals
#'   were requested (\code{categorical_posthoc = TRUE} in \code{ternG}). When
#'   \code{TRUE} and \code{n_levels >= 3}, the methods paragraph notes that
#'   cells with adjusted standardized residuals exceeding \eqn{\pm 1.96} are
#'   marked with an asterisk following a significant omnibus test. Default
#'   \code{FALSE}.
#' @param cat_posthoc_fisher_vars Character vector of variable names for which
#'   Fisher's exact test was the omnibus test while \code{categorical_posthoc = TRUE}.
#'   When non-empty, a caveat sentence is appended noting that Haberman's adjusted
#'   residuals were derived from the chi-squared contingency table in the absence of a
#'   Fisher's exact equivalent. Populated automatically when called from
#'   \code{ternG()}. Default \code{character(0)}.
#' @param show_missingness Logical or character; whether missingness columns were added
#'   to the table (\code{FALSE}, \code{"total"}, or \code{"group"}). When
#'   non-\code{FALSE}, a sentence is appended describing the missingness reporting
#'   approach and the string representations used to flag missing values. Should match
#'   the \code{show_missingness} argument passed to \code{ternG()} or \code{ternD()}.
#'   Default \code{FALSE}.
#' @param missing_indicators Character vector of string values treated as missing in
#'   addition to R \code{NA}, or \code{NULL} to use TernTables defaults. Should match
#'   the \code{missing_indicators} argument passed to \code{ternG()} or \code{ternD()}.
#'   Default \code{NULL}.
#' @param boilerplate Logical; if \code{TRUE}, ignores all other arguments and
#'   writes a single comprehensive Word document covering every possible
#'   TernTables configuration (descriptive, two-group with and without odds
#'   ratios, three-or-more-group with and without post-hoc testing), using
#'   package-default phrasing throughout. Output is written to \code{filename}
#'   if supplied; otherwise falls back to
#'   \code{comprehensive_boilerplate_methods.docx} in the current working
#'   directory. Intended as a reference document, not for inclusion in a
#'   manuscript. Default \code{FALSE}.
#' @param p_adjust Logical; if \code{TRUE}, prepends a sentence to the methods paragraph
#'   stating that P values were corrected using the Benjamini-Hochberg FDR procedure, and
#'   updates the significance threshold wording accordingly. Should match the \code{p_adjust}
#'   argument passed to \code{ternG()}. Default \code{FALSE}.
#' @param open_doc Logical; if \code{TRUE} (default), automatically opens the written Word
#'   document in the system default application after saving. Set to \code{FALSE} to suppress.
#' @param citation Logical; if \code{TRUE} (default), appends a citation line after the
#'   document footer: package version, authors, and links to the GitHub repository and web
#'   interface. Set to \code{FALSE} to suppress.
#' @param font_family Character; font family for the Word document. Default \code{"Arial"}.
#'   Can also be set via \code{options(TernTables.font_family = ...)}.
#' @return Invisibly returns the methods paragraph text as a character string
#'   (or, when \code{boilerplate = TRUE}, invisibly returns the output file path).
#'   Useful for programmatic inspection or testing without opening the Word file.
#' @examples
#' \donttest{
#' data(tern_colon)
#' tbl <- ternG(tern_colon, exclude_vars = c("ID"), group_var = "Recurrence",
#'             methods_doc = FALSE, open_doc = FALSE)
#' write_methods_doc(tbl, filename = file.path(tempdir(), "methods.docx"),
#'                   open_doc = FALSE)
#' }
#' \donttest{
#' # Write a comprehensive reference document covering all configurations.
#' write_methods_doc(tbl = NULL,
#'                   filename = file.path(tempdir(), "boilerplate_methods.docx"),
#'                   boilerplate = TRUE, open_doc = FALSE)
#' }
#' @export
write_methods_doc <- function(tbl, filename, n_levels = 2, OR_col = FALSE,
                              OR_method = "dynamic", source = "ternG", post_hoc = FALSE,
                              categorical_posthoc = FALSE,
                              cat_posthoc_fisher_vars = character(0),
                              show_missingness = FALSE, missing_indicators = NULL,
                              boilerplate = FALSE, p_adjust = FALSE, open_doc = TRUE, citation = TRUE,
                              font_family = getOption("TernTables.font_family", "Arial")) {

  # Resolve NULL filename to the package default before any branching
  if (is.null(filename) || !nzchar(filename)) {
    filename <- "TernTables_methods.docx"
  }

  # ── Boilerplate mode: comprehensive reference document, all configurations ──
  if (isTRUE(boilerplate)) {

    pkg_ver <- .tern_pkg_version()

    hp  <- fp_text(font.size = 11, font.family = font_family, bold = TRUE)
    bp  <- fp_text(font.size = 11, font.family = font_family)
    fp  <- fp_text(font.size = 9,  font.family = font_family, italic = TRUE, color = "#555555")
    sp  <- fp_text(font.size = 10, font.family = font_family, italic = TRUE, color = "#333333")

    hd <- function(txt) fpar(ftext(txt, prop = hp))
    bd <- function(txt) fpar(ftext(txt, prop = bp))
    sb <- function(txt) fpar(ftext(txt, prop = sp))

    # ── Shared fragments ──────────────────────────────────────────────────────
    desc_d <- paste0(
      "Continuous variables are presented as mean \u00b1 SD if normally distributed or ",
      "median [IQR] if non-normally distributed or ordinal. ",
      "Categorical variables are presented as n (%). ",
      "Distribution of each continuous variable was evaluated using a sequential four-gate algorithm: ",
      "variables with fewer than three available observations were treated as non-normally distributed (conservative fail-safe); ",
      "variables with absolute skewness exceeding 2.0 or excess kurtosis exceeding 7.0 were treated as non-normally distributed regardless of sample size, ",
      "as extreme skewness or heavy-tailed distributions indicate departures from normality for which parametric assumptions are inappropriate irrespective of the Central Limit Theorem; ",
      "variables with 30 or more non-missing observations were treated as normally distributed on the basis of the Central Limit Theorem; ",
      "for all remaining variables, the Shapiro-Wilk test was applied and p\u2009>\u20090.05 was used as the threshold for normality."
    )
    desc_g <- paste0(
      "Continuous variables are presented as mean \u00b1 SD if normally distributed or ",
      "median [IQR] if non-normally distributed or ordinal. ",
      "Categorical variables are presented as n (%). ",
      "Distribution of each continuous variable was evaluated using a sequential four-gate algorithm: ",
      "variables with fewer than three observations in any comparison group were treated as non-normally distributed (conservative fail-safe); ",
      "variables where any comparison group had absolute skewness exceeding 2.0 or excess kurtosis exceeding 7.0 were treated as non-normally distributed regardless of sample size, ",
      "as extreme skewness or heavy-tailed distributions indicate departures from normality for which parametric assumptions are inappropriate irrespective of the Central Limit Theorem; ",
      "variables where all comparison groups comprised 30 or more observations were treated as normally distributed on the basis of the Central Limit Theorem; ",
      "for all remaining variables, the Shapiro-Wilk test was applied within each comparison group, and p\u2009>\u20090.05 in all groups was used as the threshold for normality."
    )
    cat_default  <- "Categorical variables were compared using Chi-squared tests, or Fisher's exact tests when any expected cell count was less than 5 (Cochran criterion). "
    sig          <- "Statistical significance was defined as p < 0.05."
    or_dynamic   <- paste0(
      "For binary categorical variables and two-level categorical variables (e.g. Male/Female), ",
      "unadjusted odds ratios (OR) with 95% confidence intervals (CI) were calculated, ",
      "with the reference level (factor level 1, or alphabetical first for non-factors) serving as the reference category. ",
      "Where all expected cell counts were five or greater, OR and 95% CI were derived using the Wald method. ",
      "Where any expected cell count was less than five (Cochran criterion), OR and 95% CI were derived from Fisher's exact test. "
    )
    posthoc_note <- paste0(
      "Omnibus P values are reported. ",
      "For variables with a significant omnibus P value (p\u2009<\u20090.05), pairwise post-hoc comparisons were performed: ",
      "normally distributed variables were compared using the Games-Howell test; ",
      "non-normally distributed and ordinal variables were compared using Dunn\u2019s test with Holm correction for multiple comparisons. ",
      "Results are presented using compact letter display (CLD) notation \u2014 superscript letters appended to cell values \u2014 ",
      "whereby groups sharing a superscript letter are not significantly different from each other. ",
      "Categorical variables were not included in post-hoc comparisons. "
    )

    # ── Five canonical paragraphs ─────────────────────────────────────────────
    p1 <- paste0(desc_d, " ", sig)

    p2 <- paste0(
      desc_g, " ",
      "Normally distributed continuous variables were compared between groups using Welch's independent samples t-test; ",
      "non-normally distributed or ordinal continuous variables were compared using the Wilcoxon rank-sum test. ",
      cat_default, sig
    )

    p3 <- paste0(
      desc_g, " ",
      "Normally distributed continuous variables were compared between groups using Welch's independent samples t-test; ",
      "non-normally distributed or ordinal continuous variables were compared using the Wilcoxon rank-sum test. ",
      cat_default, or_dynamic, sig
    )

    p4 <- paste0(
      desc_g, " ",
      "Normally distributed continuous variables were compared across groups using Welch's one-way ANOVA; ",
      "non-normally distributed or ordinal continuous variables were compared using the Kruskal-Wallis test. ",
      "Omnibus P values are reported; pairwise post-hoc comparisons were not performed. ",
      cat_default, sig
    )

    p5 <- paste0(
      desc_g, " ",
      "Normally distributed continuous variables were compared across groups using Welch's one-way ANOVA; ",
      "non-normally distributed or ordinal continuous variables were compared using the Kruskal-Wallis test. ",
      posthoc_note, cat_default, sig
    )

    footer_bp <- paste0(
      "Comprehensive reference document generated by TernTables v", pkg_ver, ". ",
      "This document describes all standard configurations. ",
      "For run-specific methods text, call write_methods_doc() from ternG() or ternD() directly."
    )

    out_path <- if (!missing(filename) && !is.null(filename) && nzchar(filename)) {
      filename
    } else {
      file.path(getwd(), "comprehensive_boilerplate_methods.docx")
    }

    doc <- read_docx() |>
      body_add_fpar(hd("TernTables \u2014 Comprehensive Statistical Methods Reference")) |>
      body_add_par("", style = "Normal") |>
      body_add_fpar(sb("This document covers all standard TernTables configurations using package-default behaviour. Copy the paragraph that matches your analysis into your manuscript methods section.")) |>
      body_add_par("", style = "Normal") |>

      body_add_fpar(hd("1. Descriptive summary table (ternD)")) |>
      body_add_par("", style = "Normal") |>
      body_add_fpar(bd(p1)) |>
      body_add_par("", style = "Normal") |>

      body_add_fpar(hd("2. Two-group comparison (ternG, two groups, no odds ratios)")) |>
      body_add_par("", style = "Normal") |>
      body_add_fpar(bd(p2)) |>
      body_add_par("", style = "Normal") |>

      body_add_fpar(hd("3. Two-group comparison (ternG, two groups, OR_col = TRUE)")) |>
      body_add_par("", style = "Normal") |>
      body_add_fpar(bd(p3)) |>
      body_add_par("", style = "Normal") |>

      body_add_fpar(hd("4. Three-or-more-group comparison (ternG, post_hoc = FALSE)")) |>
      body_add_par("", style = "Normal") |>
      body_add_fpar(bd(p4)) |>
      body_add_par("", style = "Normal") |>

      body_add_fpar(hd("5. Three-or-more-group comparison (ternG, post_hoc = TRUE)")) |>
      body_add_par("", style = "Normal") |>
      body_add_fpar(bd(p5)) |>
      body_add_par("", style = "Normal") |>

      body_add_fpar(fpar(ftext(footer_bp, prop = fp)))

    if (isTRUE(citation)) {
      cit_props <- fp_text(font.family = font_family, font.size = 7,
                           bold = TRUE, italic = TRUE, color = "black")
      doc <- body_set_default_section(
        doc,
        value = prop_section(
          footer_default = block_list(
            fpar(ftext(.tern_citation_line(), prop = cit_props))
          )
        )
      )
    }

    dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)
    print(doc, target = out_path)
    if (isTRUE(open_doc)) .open_docx(out_path)
    cli::cli_alert_success("Comprehensive boilerplate methods document written to: {out_path}")
    return(invisible(out_path))
  }

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
    normality_context_skewness <- "variables where any comparison group had absolute skewness exceeding 2.0 or excess kurtosis exceeding 7.0 were treated as non-normally distributed regardless of sample size, "
    normality_context_clt      <- "variables where all comparison groups comprised 30 or more observations were treated as normally distributed on the basis of the Central Limit Theorem; "
    normality_context_sw       <- "for all remaining variables, the Shapiro-Wilk test was applied within each comparison group, and p\u2009>\u20090.05 in all groups was used as the threshold for normality."
  } else {
    normality_context_n3       <- "variables with fewer than three available observations were treated as non-normally distributed (conservative fail-safe); "
    normality_context_skewness <- "variables with absolute skewness exceeding 2.0 or excess kurtosis exceeding 7.0 were treated as non-normally distributed regardless of sample size, "
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
    normality_context_sw,
    " This normality routing algorithm is a pragmatic, automated heuristic intended to support consistent and transparent reporting in routine clinical descriptive analyses; it does not constitute a formal distributional inference, and investigators requiring non-standard normality handling may override routing for individual variables using the force_ordinal argument or globally via consider_normality."
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
  fdr_sentence <- if (isTRUE(p_adjust)) {
    paste0(
      "All reported P values were corrected for multiple comparisons using the ",
      "Benjamini-Hochberg false discovery rate (FDR) procedure (Benjamini & Hochberg, 1995). "
    )
  } else {
    ""
  }
  sig_sentence <- if (isTRUE(p_adjust)) {
    "Statistical significance was defined as FDR-corrected p\u2009<\u20090.05."
  } else {
    "Statistical significance was defined as p\u2009<\u20090.05."
  }

  # ── Helper: missingness sentence ────────────────────────────────────────
  miss_sentence <- if (!isFALSE(show_missingness)) {
    miss_vals <- if (!is.null(missing_indicators)) missing_indicators else .tern_missing_strings()
    miss_str <- paste0('"', paste(miss_vals, collapse = '", "'), '"')
    col_desc <- if (isTRUE(show_missingness == "total")) {
      "a dedicated 'Missing (total)' column at the right of the table"
    } else {
      "per-group 'Missing' columns interleaved after each comparison-group column"
    }
    paste0(
      "Missing observations, defined as R NA values or any of the following string ",
      "representations of missingness: ", miss_str, ", are reported as n (\u0025) in ",
      col_desc, " for each variable. "
    )
  } else ""

  # ── Helper: categorical posthoc Fisher's caveat ─────────────────────────────
  cat_posthoc_fisher_sentence <- if (length(cat_posthoc_fisher_vars) > 0L) {
    paste0(
      "Note: Fisher\u2019s exact test was the omnibus test for ",
      if (length(cat_posthoc_fisher_vars) == 1L) "one variable" else paste0(length(cat_posthoc_fisher_vars), " variables"),
      " (", paste(cat_posthoc_fisher_vars, collapse = ", "), "); ",
      "adjusted standardized residuals were nonetheless derived from the global chi-squared contingency table, ",
      "as no established Fisher\u2019s exact equivalent exists for this procedure. ",
      "This approach may be less reliable in cells with very small expected counts. "
    )
  } else ""

  # ── Section 1: Descriptive ────────────────────────────────────────
  sec1_body <- paste0(desc_sentence, " ", miss_sentence, sig_sentence)

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
    sec2_body <- paste0(desc_sentence, " ", s2_cont, cat_sentence(has_fisher, has_chisq, has_fisher_sim), or_sentence, fdr_sentence, miss_sentence, sig_sentence)
  } else {
    sec2_body <- paste0(
      desc_sentence, " ",
      "Normally distributed continuous variables were compared between groups using Welch's independent samples t-test; ",
      "non-normally distributed or ordinal continuous variables were compared using the Wilcoxon rank-sum test. ",
      "Categorical variables were compared using Chi-squared tests, or Fisher's exact tests when any expected cell count was less than 5 (Cochran criterion). ",
      fdr_sentence, miss_sentence, sig_sentence
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
        if (categorical_posthoc) {
          paste0(
            "For categorical variables with a significant omnibus test, post-hoc identification of specific group differences ",
            "was performed by calculating adjusted standardized residuals for each cell of the global contingency table; ",
            "cells with adjusted standardized residuals exceeding \u00b11.96 are marked with an asterisk (*), ",
            "indicating a significant deviation from expected frequencies (\u03b1\u00a0=\u00a00.05). ",
            cat_posthoc_fisher_sentence
          )
        } else {
          "Categorical variables were not included in post-hoc comparisons. "
        }
      )
    } else if (categorical_posthoc) {
      paste0(
        "Omnibus P values are reported; pairwise post-hoc comparisons were not performed for continuous variables. ",
        "For categorical variables with a significant omnibus test, post-hoc identification of specific group differences ",
        "was performed by calculating adjusted standardized residuals for each cell of the global contingency table; ",
        "cells with adjusted standardized residuals exceeding \u00b11.96 are marked with an asterisk (*), ",
        "indicating a significant deviation from expected frequencies (\u03b1\u00a0=\u00a00.05). ",
        cat_posthoc_fisher_sentence
      )
    } else {
      "Omnibus P values are reported; pairwise post-hoc comparisons were not performed. "
    }
    sec3_body <- paste0(desc_sentence, " ", s3_cont, omnibus_note, cat_sentence(has_fisher, has_chisq, has_fisher_sim), or_sentence, fdr_sentence, miss_sentence, sig_sentence)
  } else {
    sec3_body <- paste0(
      desc_sentence, " ",
      "Normally distributed continuous variables were compared across groups using Welch's one-way ANOVA; ",
      "non-normally distributed or ordinal continuous variables were compared using the Kruskal-Wallis test. ",
      "Omnibus P values are reported; pairwise post-hoc comparisons were not performed. ",
      "Categorical variables were compared using Chi-squared tests, or Fisher's exact tests when any expected cell count was less than 5 (Cochran criterion). ",
      fdr_sentence, miss_sentence, sig_sentence
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
  head_props   <- fp_text(font.size = 11, font.family = font_family, bold = TRUE)
  body_props   <- fp_text(font.size = 11, font.family = font_family)
  footer_props <- fp_text(font.size = 9,  font.family = font_family, italic = TRUE,
                          color = "#555555")

  # Read version from installed package and strip any development tag (.9000 etc.)
  # so the footer always shows the public release number (e.g. "1.6.3").
  pkg_ver      <- .tern_pkg_version()
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

  if (isTRUE(citation)) {
    cit_props <- fp_text(font.family = font_family, font.size = 7,
                         bold = TRUE, italic = TRUE, color = "black")
    doc <- body_set_default_section(
      doc,
      value = prop_section(
        footer_default = block_list(
          fpar(ftext(.tern_citation_line(), prop = cit_props))
        )
      )
    )
  }

  dir.create(dirname(filename), recursive = TRUE, showWarnings = FALSE)
  print(doc, target = filename)
  if (isTRUE(open_doc)) .open_docx(filename)
  cli::cli_alert_success("Methods document written to: {filename}")
  invisible(methods_body)
}
