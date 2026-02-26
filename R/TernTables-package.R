#' TernTables: Automated Statistics and Table Generation for Clinical Research
#'
#' TernTables generates publication-ready summary tables for descriptive
#' statistics and group comparisons. It automatically detects variable types
#' (continuous, binary, categorical, or ordinal), selects appropriate
#' statistical tests, and formats results for direct export to Word or Excel.
#'
#' @section Main functions:
#' \describe{
#'   \item{\code{\link{ternG}}}{Grouped comparison table for 2- or 3-level group variables.}
#'   \item{\code{\link{ternD}}}{Descriptive-only summary table (no grouping).}
#'   \item{\code{\link{export_to_word}}}{Export a TernTables tibble to a formatted Word document.}
#'   \item{\code{\link{write_methods_doc}}}{Generate a methods Word document describing tests used.}
#'   \item{\code{\link{fmt_p}}}{Format a p-value for publication.}
#'   \item{\code{\link{format_val}}}{Format a numeric value with rounding rules.}
#' }
#'
#' @section Statistical tests applied:
#' \describe{
#'   \item{Continuous (2 groups)}{Welch's \emph{t}-test or Wilcoxon rank-sum, chosen by Shapiro-Wilk.}
#'   \item{Continuous (3+ groups)}{ANOVA or Kruskal-Wallis, chosen by Shapiro-Wilk and Levene's test.}
#'   \item{Binary / Categorical}{Chi-squared or Fisher's exact, based on expected cell counts.}
#'   \item{Ordinal (forced)}{Wilcoxon rank-sum (2 groups) or Kruskal-Wallis (3+ groups).}
#' }
#'
#' @section Getting started:
#' See \code{vignette("getting-started", package = "TernTables")} for a
#' walkthrough using the \code{colon} dataset from the \pkg{survival} package.
#'
#' @keywords internal
"_PACKAGE"
