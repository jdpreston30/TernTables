# ── classify_normality ────────────────────────────────────────────────────────
#
# Exported wrapper around the internal ROBUST four-gate normality algorithm so
# users can audit variable routing outside of ternG()/ternD() — e.g. to answer
# reviewer questions about normality assessment.
# ─────────────────────────────────────────────────────────────────────────────

#' Classify variables by normality and routing decision
#'
#' Applies the same normality assessment logic used internally by \code{ternG()}
#' and \code{ternD()} and returns a tidy tibble showing per-variable (and
#' per-group) statistics, the gate that triggered the routing decision, and the
#' final parametric / non-parametric routing outcome.
#'
#' Useful for:
#' \itemize{
#'   \item Answering reviewer questions about normality testing ("was Age
#'         normally distributed?").
#'   \item Verifying that a given variable's routing matches your expectation
#'         before running \code{ternG()} or \code{ternD()}.
#'   \item Generating a supplemental normality audit table for a manuscript.
#' }
#'
#' @param data A data frame or tibble.
#' @param vars Optional character vector of variable names to assess. If
#'   \code{NULL} (default), all assessable numeric variables are included
#'   (excluding \code{exclude_vars} and \code{group_var}).
#' @param exclude_vars Optional character vector of variable names to exclude.
#' @param group_var Optional name of the grouping variable (as used in
#'   \code{ternG()}). When provided, the normality assessment is performed
#'   across groups simultaneously — exactly as \code{ternG()} does. When
#'   \code{NULL}, each variable is assessed as a single vector (matching
#'   \code{ternD()}).
#' @param consider_normality Normality assessment mode — must match what was
#'   (or will be) passed to \code{ternG()} / \code{ternD()} to guarantee
#'   identical routing. \code{"ROBUST"} (default), \code{TRUE}, or
#'   \code{FALSE}.
#'
#' @return A tibble with one row per variable \eqn{\times} group (or one row per
#'   variable when \code{group_var = NULL}), containing:
#'   \describe{
#'     \item{variable}{Variable name.}
#'     \item{group}{Group level, or \code{"[all]"} when no \code{group_var} is
#'       supplied.}
#'     \item{n}{Non-missing sample size in this group.}
#'     \item{skewness}{Sample skewness (population moments).}
#'     \item{kurtosis}{Excess kurtosis (population moments; 0 for a normal
#'       distribution).}
#'     \item{sw_p}{Shapiro-Wilk p-value for this group. \code{NA} when the
#'       routing decision was made at Gates 1--3 under \code{"ROBUST"}, when
#'       n is outside the valid range (3--5000), or when
#'       \code{consider_normality = FALSE}.}
#'     \item{gate}{Integer 1--4 indicating which gate made the routing
#'       decision under \code{consider_normality = "ROBUST"}, or \code{NA}
#'       for \code{TRUE} / \code{FALSE} modes.}
#'     \item{gate_reason}{Plain-language explanation of the gate decision,
#'       naming which group(s) triggered the rule where relevant.}
#'     \item{is_normal}{Logical; \code{TRUE} = routed to parametric
#'       (mean \eqn{\pm} SD, t-test / ANOVA); \code{FALSE} = non-parametric
#'       (median [IQR], Wilcoxon / Kruskal-Wallis).}
#'     \item{routing}{Human-readable routing summary:
#'       \code{"Parametric (mean \u00b1 SD)"} or
#'       \code{"Non-parametric (median [IQR])"}.}
#'   }
#'
#' @examples
#' data(tern_colon)
#'
#' # Single-group audit (ternD-style)
#' classify_normality(tern_colon, exclude_vars = "ID")
#'
#' # Grouped audit matching a ternG call
#' classify_normality(tern_colon, exclude_vars = "ID", group_var = "Recurrence")
#'
#' # Specific variables only
#' classify_normality(tern_colon,
#'                    vars      = c("Age", "Positive_Lymph_Nodes_n"),
#'                    group_var = "Recurrence")
#'
#' # Using Shapiro-Wilk only (matches consider_normality = TRUE in ternG/ternD)
#' classify_normality(tern_colon, exclude_vars = "ID",
#'                    group_var          = "Recurrence",
#'                    consider_normality = TRUE)
#' @export
classify_normality <- function(data,
                                vars               = NULL,
                                exclude_vars       = NULL,
                                group_var          = NULL,
                                consider_normality = "ROBUST") {

  # ── Input validation ───────────────────────────────────────────────────────
  if (!is.data.frame(data))
    stop("'data' must be a data frame or tibble.", call. = FALSE)

  if (!is.null(group_var) && !group_var %in% names(data))
    stop("'group_var' \"", group_var, "\" not found in data.", call. = FALSE)

  if (!any(identical(consider_normality, "ROBUST"),
           identical(consider_normality, "FORCE"),
           isTRUE(consider_normality),
           identical(consider_normality, FALSE)))
    stop("'consider_normality' must be \"ROBUST\", TRUE, or FALSE.", call. = FALSE)

  # ── Resolve variable list ──────────────────────────────────────────────────
  if (is.null(vars)) {
    vars <- setdiff(names(data), unique(c(exclude_vars, group_var)))
  } else {
    vars <- setdiff(vars, unique(c(exclude_vars, group_var)))
  }

  # Keep only assessable numerics: same rules as ternG/ternD
  #  - must be numeric
  #  - binary 0/1 are auto-converted to Y/N categorical → skip
  .is_assessable <- function(v) {
    if (!is.numeric(v)) return(FALSE)
    vals <- stats::na.omit(v)
    if (length(unique(vals)) == 2 && all(vals %in% c(0, 1))) return(FALSE)
    TRUE
  }
  num_vars <- vars[vapply(vars, function(v) .is_assessable(data[[v]]), logical(1))]

  if (length(num_vars) == 0) {
    cli::cli_alert_warning("No assessable numeric variables found in the supplied data.")
    return(tibble::tibble(
      variable    = character(),
      group       = character(),
      n           = integer(),
      skewness    = numeric(),
      kurtosis    = numeric(),
      sw_p        = numeric(),
      gate        = integer(),
      gate_reason = character(),
      is_normal   = logical(),
      routing     = character()
    ))
  }

  # ── Prepare group structure ────────────────────────────────────────────────
  if (!is.null(group_var)) {
    data2        <- data[!is.na(data[[group_var]]), ]
    group_levels <- as.character(sort(unique(data2[[group_var]])))
  } else {
    data2        <- data
    group_levels <- "[all]"
  }

  # ── Per-variable assessment ────────────────────────────────────────────────
  rows <- lapply(num_vars, function(var) {

    # Build list of group vectors — matches what ternG/ternD pass internally
    if (!is.null(group_var)) {
      gvecs <- stats::setNames(
        lapply(group_levels, function(g) {
          suppressWarnings(
            as.numeric(data2[[var]][as.character(data2[[group_var]]) == g])
          )
        }),
        group_levels
      )
    } else {
      gvecs <- list(`[all]` = suppressWarnings(as.numeric(data2[[var]])))
    }

    # Per-group summary statistics (always computed for transparency)
    grp_stats <- dplyr::bind_rows(lapply(names(gvecs), function(g) {
      x_raw <- gvecs[[g]]
      x     <- x_raw[!is.na(x_raw)]
      sw_val <- if (length(x) >= 3 && length(x) <= 5000 && stats::var(x) > 0)
        tryCatch(stats::shapiro.test(x)$p.value, error = function(e) NA_real_)
      else
        NA_real_
      tibble::tibble(
        group    = g,
        n        = length(x),
        skewness = round(.calc_skewness(x_raw), 3),
        kurtosis = round(.calc_kurtosis(x_raw), 3),
        sw_p     = sw_val
      )
    }))

    # ── Routing decision — mirrors ternG / ternD exactly ────────────────────
    if (identical(consider_normality, FALSE)) {
      is_norm  <- TRUE
      gate_int <- NA_integer_
      reason   <- "consider_normality = FALSE: all numeric variables treated as parametric"
      grp_stats$sw_p <- NA_real_

    } else if (is.character(consider_normality) && consider_normality == "FORCE") {
      is_norm  <- FALSE
      gate_int <- NA_integer_
      reason   <- "consider_normality = \"FORCE\": all numeric variables treated as non-parametric"
      grp_stats$sw_p <- NA_real_

    } else if (is.character(consider_normality) && consider_normality == "ROBUST") {
      res      <- .robust_normality(gvecs)
      is_norm  <- res$is_normal
      gate_int <- res$gate

      reason <- switch(
        as.character(gate_int),
        "1" = paste0(
          "Gate 1: n < 3 in at least one group",
          " \u2192 non-parametric (conservative fail-safe)"
        ),
        "2" = {
          sk_flag <- grp_stats$group[!is.na(grp_stats$skewness) & abs(grp_stats$skewness) > 2]
          ku_flag <- grp_stats$group[!is.na(grp_stats$kurtosis) & abs(grp_stats$kurtosis) > 7]
          parts <- character(0)
          if (length(sk_flag) > 0)
            parts <- c(parts, paste0("|skewness| > 2 [", paste(sk_flag, collapse = ", "), "]"))
          if (length(ku_flag) > 0)
            parts <- c(parts, paste0("|excess kurtosis| > 7 [", paste(ku_flag, collapse = ", "), "]"))
          paste0("Gate 2: ", paste(parts, collapse = "; "), " \u2192 non-parametric")
        },
        "3" = paste0(
          "Gate 3: all groups n \u2265 30 (Central Limit Theorem)",
          " \u2192 parametric"
        ),
        "4" = if (is_norm) {
          "Gate 4: Shapiro-Wilk p > 0.05 in all groups \u2192 parametric"
        } else {
          failed <- grp_stats$group[is.na(grp_stats$sw_p) | grp_stats$sw_p <= 0.05]
          paste0(
            "Gate 4: Shapiro-Wilk p \u2264 0.05 in: ",
            paste(failed, collapse = ", "),
            " \u2192 non-parametric"
          )
        },
        "Unknown gate"
      )

      # Gate 1-3: sw_p was not computed internally; keep our computed values
      # (still informative, though they did not drive the decision)
      if (gate_int < 4L) grp_stats$sw_p <- NA_real_

    } else if (isTRUE(consider_normality)) {
      # Shapiro-Wilk only — all groups must pass (same rule as ternG TRUE path)
      is_norm  <- all(!is.na(grp_stats$sw_p) & grp_stats$sw_p > 0.05)
      gate_int <- NA_integer_
      reason   <- if (is_norm) {
        "Shapiro-Wilk p > 0.05 in all groups \u2192 parametric"
      } else {
        failed <- grp_stats$group[is.na(grp_stats$sw_p) | grp_stats$sw_p <= 0.05]
        paste0(
          "Shapiro-Wilk p \u2264 0.05 or NA in: ",
          paste(failed, collapse = ", "),
          " \u2192 non-parametric"
        )
      }
    }

    dplyr::mutate(
      grp_stats,
      variable    = var,
      gate        = gate_int,
      gate_reason = reason,
      is_normal   = is_norm,
      routing     = if (is_norm) "Parametric (mean \u00b1 SD)" else "Non-parametric (median [IQR])"
    ) |>
      dplyr::select(variable, group, n, skewness, kurtosis,
                    sw_p, gate, gate_reason, is_normal, routing)
  })

  dplyr::bind_rows(rows)
}
