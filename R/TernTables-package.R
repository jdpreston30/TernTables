#' TernTables: Automated Statistics and Table Generation for Clinical Research
#'
#' TernTables generates publication-ready summary tables for descriptive
#' statistics and group comparisons. It automatically detects variable types
#' (continuous, binary, or categorical), selects appropriate
#' statistical tests, and formats results for direct export to Word or Excel.
#' Numeric variables can be designated as ordinal via \code{force_ordinal}.
#'
#' @section Main functions:
#' \describe{
#'   \item{\code{\link{ternG}}}{Grouped comparison table for 2- or 3-level group variables.}
#'   \item{\code{\link{ternD}}}{Descriptive-only summary table (no grouping).}
#'   \item{\code{\link{word_export}}}{Export a TernTables tibble to a formatted Word document.}
#'   \item{\code{\link{write_methods_doc}}}{Generate a methods Word document describing tests used.}
#'   \item{\code{\link{val_p_format}}}{Format a P value for publication.}
#'   \item{\code{\link{val_format}}}{Format a numeric value with rounding rules.}
#' }
#'
#' @section Statistical tests applied:
#' \describe{
#'   \item{Continuous (2 groups)}{Welch's \emph{t}-test or Wilcoxon rank-sum, routed by ROBUST logic.}
#'   \item{Continuous (3+ groups)}{Welch ANOVA or Kruskal-Wallis, routed by ROBUST logic per group.}
#'   \item{Binary / Categorical}{Chi-squared or Fisher's exact, based on expected cell counts.}
#'   \item{Ordinal (forced)}{Wilcoxon rank-sum (2 groups) or Kruskal-Wallis (3+ groups).}
#' }
#' ROBUST routing uses three gates: (1) |skewness| > 2 in any group \eqn{\Rightarrow} non-parametric;
#' (2) all groups n \eqn{\geq} 30 \eqn{\Rightarrow} parametric (CLT);
#' (3) otherwise Shapiro-Wilk p > 0.05 in all groups \eqn{\Rightarrow} parametric.
#'
#' @section Getting started:
#' See \code{vignette("getting-started", package = "TernTables")} for a
#' walkthrough using the bundled \code{\link{tern_colon}} dataset.
#'
#' @keywords internal
"_PACKAGE"
