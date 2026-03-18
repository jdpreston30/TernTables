# ── Internal ROBUST normality helpers ────────────────────────────────────────
#
# These functions implement the four-gate ROBUST normality decision tree shared
# by ternG() and ternD().  They are package-internal (dot-prefixed) and not
# exported.
#
#   Gate 1  any group n < 3            → non-parametric (conservative fail-safe)
#   Gate 2  |skewness| > 2 OR
#           |excess kurtosis| > 7      → non-parametric (shape precludes it)
#   Gate 3  all groups n ≥ 30          → parametric    (Central Limit Theorem)
#   Gate 4  Shapiro-Wilk p > 0.05
#           in all groups              → parametric (pass) / non-parametric (fail)
#           valid only for 3 ≤ n ≤ 5000; n outside range → NA → non-parametric
# ─────────────────────────────────────────────────────────────────────────────

#' @noRd
.calc_skewness <- function(x) {
  x <- x[!is.na(x)]
  n <- length(x)
  if (n < 3) return(NA_real_)
  m  <- mean(x)
  m2 <- sum((x - m)^2) / n
  m3 <- sum((x - m)^3) / n
  if (m2 == 0) return(NA_real_)
  m3 / m2^1.5
}

#' @noRd
.calc_kurtosis <- function(x) {
  x <- x[!is.na(x)]
  n <- length(x)
  if (n < 4) return(NA_real_)
  m  <- mean(x)
  m2 <- sum((x - m)^2) / n
  m4 <- sum((x - m)^4) / n
  if (m2 == 0) return(NA_real_)
  m4 / m2^2 - 3  # excess kurtosis (population moments; 0 for normal distribution)
}

#' Four-gate ROBUST normality decision (internal)
#'
#' @param group_list A named list of numeric vectors, one per group.
#'   For \code{ternD()} (single descriptive variable), pass \code{list(x)}.
#'   For \code{ternG()}, pass one element per group level.
#'
#' @return A list with three elements:
#'   \describe{
#'     \item{is_normal}{Logical.  \code{TRUE} = route to parametric
#'       test/display; \code{FALSE} = non-parametric.}
#'     \item{sw_pvalues}{Named numeric vector of Shapiro-Wilk p-values, or
#'       \code{NULL} if the decision was made at Gates 1-3.  Names follow the
#'       pattern \code{"SW_p_<group_name>"}.  Used by \code{ternG()} for the
#'       \code{print_normality} column.}
#'     \item{gate}{Integer 1-4 indicating which gate made the decision.}
#'   }
#' @noRd
.robust_normality <- function(group_list) {
  group_ns       <- sapply(group_list, function(v) sum(!is.na(v)))
  group_skewness <- sapply(group_list, .calc_skewness)
  group_kurtosis <- sapply(group_list, .calc_kurtosis)

  grp_names <- if (!is.null(names(group_list))) names(group_list)
               else as.character(seq_along(group_list))

  # Gate 1: any group too small — conservative non-parametric fail-safe
  if (any(group_ns < 3)) {
    return(list(is_normal = FALSE, sw_pvalues = NULL, gate = 1L))
  }

  # Gate 2: extreme shape — non-parametric regardless of n
  if (any(abs(group_skewness) > 2, na.rm = TRUE) ||
      any(abs(group_kurtosis) > 7, na.rm = TRUE)) {
    return(list(is_normal = FALSE, sw_pvalues = NULL, gate = 2L))
  }

  # Gate 3: Central Limit Theorem — all groups sufficiently large
  if (all(group_ns >= 30)) {
    return(list(is_normal = TRUE, sw_pvalues = NULL, gate = 3L))
  }

  # Gate 4: Shapiro-Wilk (valid when 3 ≤ n ≤ 5000; otherwise conservative NA)
  sw_pvalues <- tryCatch({
    out <- lapply(seq_along(group_list), function(i) {
      x <- group_list[[i]]
      x <- x[!is.na(x)]
      pval <- if (length(x) >= 3 && length(x) <= 5000 && stats::var(x) > 0)
                tryCatch(stats::shapiro.test(x)$p.value, error = function(e) NA_real_)
              else
                NA_real_
      setNames(pval, paste0("SW_p_", grp_names[i]))
    })
    do.call(c, out)
  }, error = function(e) {
    setNames(rep(NA_real_, length(group_list)), paste0("SW_p_", grp_names))
  })

  is_normal <- all(!is.na(sw_pvalues) & sw_pvalues > 0.05)
  list(is_normal = is_normal, sw_pvalues = sw_pvalues, gate = 4L)
}
