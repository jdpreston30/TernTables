# ============================================================
# test-methods-doc.R
# Validate that write_methods_doc() generates accurate,
# fully dynamic methods text for every ternG / ternD / ternB
# configuration.
#
# No Word documents are opened or retained. Each call writes
# to a tempfile() that is discarded; the captured return value
# (the paragraph string) is what is inspected.
#
# Scenarios
# ---------
#  1. ternD  – descriptive only
#  2. ternG  – 2-group, no OR column
#  3. ternG  – 2-group, OR_col = TRUE (dynamic Fisher/Wald)
#  4. ternG  – 2-group, OR_col = TRUE, OR_method = "wald"
#  5. ternG  – 3-group, post_hoc = FALSE
#  6. ternG  – 3-group, post_hoc = TRUE
#  7. ternB  – combined pool: ternD + ternG-2group-OR + ternG-3group-posthoc
# ============================================================

devtools::load_all()
data(tern_colon)

sep <- function(label) {
  cat("\n", strrep("=", 70), "\n")
  cat(" ", label, "\n")
  cat(strrep("=", 70), "\n\n")
}

check <- function(text, expected_fragments, forbidden_fragments = character(0)) {
  ok   <- TRUE
  for (f in expected_fragments) {
    if (!grepl(f, text, fixed = TRUE)) {
      cat("  [FAIL] Expected fragment not found:\n    >>", f, "\n")
      ok <- FALSE
    }
  }
  for (f in forbidden_fragments) {
    if (grepl(f, text, fixed = TRUE)) {
      cat("  [FAIL] Forbidden fragment found:\n    >>", f, "\n")
      ok <- FALSE
    }
  }
  if (ok) cat("  [PASS] All checks passed.\n")
  invisible(ok)
}

# Helper: call write_methods_doc and return the paragraph text
wmd <- function(tbl, ...) {
  write_methods_doc(tbl, tempfile(fileext = ".docx"), ...)
}

# ── Build tables (methods_doc = FALSE so no Word file is written by ternG/ternD)

tbl_desc    <- ternD(tern_colon, exclude_vars = c("ID", "Study", "Event_Type"),
                     methods_doc = FALSE)

tbl_2g      <- ternG(tern_colon, group_var = "Recurrence",
                     exclude_vars = c("ID", "Study", "Event_Type"),
                     methods_doc = FALSE)

tbl_2g_or   <- ternG(tern_colon, group_var = "Recurrence",
                     exclude_vars = c("ID", "Study", "Event_Type"),
                     OR_col = TRUE, methods_doc = FALSE)

tbl_2g_wald <- ternG(tern_colon, group_var = "Recurrence",
                     exclude_vars = c("ID", "Study", "Event_Type"),
                     OR_col = TRUE, OR_method = "wald", methods_doc = FALSE)

tbl_3g      <- ternG(tern_colon, group_var = "Treatment_Arm",
                     exclude_vars = c("ID", "Study", "Event_Type"),
                     methods_doc = FALSE)

tbl_3g_ph   <- ternG(tern_colon, group_var = "Treatment_Arm",
                     exclude_vars = c("ID", "Study", "Event_Type"),
                     post_hoc = TRUE, methods_doc = FALSE)

# ============================================================
# 1. ternD – descriptive only
# ============================================================
sep("1. ternD — descriptive only")
txt1 <- wmd(tbl_desc, source = "ternD")
cat(txt1, "\n")
check(txt1,
  expected_fragments = c(
    "mean \u00b1 SD",
    "median [IQR]",
    "four-gate algorithm",
    "fewer than three available observations",        # ternD n<3 phrasing
    "variables with absolute skewness exceeding 2.0", # ternD skewness phrasing
    "with 30 or more non-missing observations",       # ternD CLT phrasing
    "Shapiro-Wilk test was applied and p"             # ternD SW phrasing (no "within each group")
  ),
  forbidden_fragments = c(
    "any comparison group",     # ternG-specific phrasing must NOT appear
    "compared between groups",
    "compared across groups",
    "odds ratio"
  )
)

# ============================================================
# 2. ternG – 2-group, no OR
# ============================================================
sep("2. ternG — 2-group, no OR column")
txt2 <- wmd(tbl_2g, n_levels = 2, OR_col = FALSE, source = "ternG")
cat(txt2, "\n")
check(txt2,
  expected_fragments = c(
    "any comparison group",                      # ternG-specific phrasing
    "any comparison group had absolute skewness" # ternG skewness phrasing
  ),
  forbidden_fragments = c(
    "odds ratio",
    "OR and 95%",
    "Games-Howell",
    "Dunn"
  )
)
# Must contain test-specific language actually observed in this data
if (any(grepl("Welch t-test", tbl_2g[["test"]], ignore.case = TRUE))) {
  check(txt2, expected_fragments = c("Welch's independent samples t-test"))
}
if (any(grepl("Wilcoxon",    tbl_2g[["test"]], ignore.case = TRUE))) {
  check(txt2, expected_fragments = c("Wilcoxon rank-sum test"))
}

# ============================================================
# 3. ternG – 2-group, OR_col = TRUE (dynamic method)
# ============================================================
sep("3. ternG — 2-group, OR_col = TRUE, OR_method = \"dynamic\"")
txt3 <- wmd(tbl_2g_or, n_levels = 2, OR_col = TRUE, OR_method = "dynamic", source = "ternG")
cat(txt3, "\n")
check(txt3,
  expected_fragments = c(
    "unadjusted odds ratios (OR)",
    "Where all expected cell counts were five or greater, OR and 95% CI were derived using the Wald method.",
    "Where any expected cell count was less than five (Cochran criterion), OR and 95% CI were derived from Fisher's exact test."
  ),
  forbidden_fragments = c(
    "Wald method for all variables"   # that's the OR_method="wald" branch
  )
)

# ============================================================
# 4. ternG – 2-group, OR_col = TRUE, OR_method = "wald"
# ============================================================
sep("4. ternG — 2-group, OR_col = TRUE, OR_method = \"wald\"")
txt4 <- wmd(tbl_2g_wald, n_levels = 2, OR_col = TRUE, OR_method = "wald", source = "ternG")
cat(txt4, "\n")
check(txt4,
  expected_fragments = c(
    "unadjusted odds ratios (OR)",
    "Wald method for all variables"
  ),
  forbidden_fragments = c(
    "Where all expected cell counts were five or greater",  # dynamic branch
    "Fisher's exact test."                                  # dynamic branch
  )
)

# ============================================================
# 5. ternG – 3-group, post_hoc = FALSE
# ============================================================
sep("5. ternG — 3-group, post_hoc = FALSE")
txt5 <- wmd(tbl_3g, n_levels = 3, OR_col = FALSE, source = "ternG", post_hoc = FALSE)
cat(txt5, "\n")
check(txt5,
  expected_fragments = c(
    "any comparison group",
    "Welch's one-way ANOVA",
    "Kruskal-Wallis",
    "Omnibus P values are reported",
    "pairwise post-hoc comparisons were not performed."
  ),
  forbidden_fragments = c(
    "odds ratio",
    "Games-Howell",
    "Dunn",
    "compact letter display"
  )
)

# ============================================================
# 6. ternG – 3-group, post_hoc = TRUE
# ============================================================
sep("6. ternG — 3-group, post_hoc = TRUE")
txt6 <- wmd(tbl_3g_ph, n_levels = 3, OR_col = FALSE, source = "ternG", post_hoc = TRUE)
cat(txt6, "\n")
check(txt6,
  expected_fragments = c(
    "any comparison group",
    "Welch's one-way ANOVA",
    "Kruskal-Wallis",
    "Omnibus P values are reported",
    "Games-Howell",
    "Dunn\u2019s test with Holm correction",
    "compact letter display (CLD)",
    "groups sharing a superscript letter are not significantly different"
  ),
  forbidden_fragments = c(
    "odds ratio",
    "pairwise post-hoc comparisons were not performed."
  )
)

# ============================================================
# 7. ternB – combined pool
#    Tables: ternD + ternG-2group-OR + ternG-3group-posthoc
#    Expected: 3-group paragraph, OR from tbl_2g_or, post_hoc
#    from tbl_3g_ph — all recovered from stored ternB_meta.
# ============================================================
sep("7. ternB — combined pool (ternD + ternG-2g-OR + ternG-3g-posthoc)")

all_metas <- lapply(
  list(tbl_desc, tbl_2g_or, tbl_3g_ph),
  function(t) attr(t, "ternB_meta")
)

# Replicate ternB's pooling logic (mirrors R/ternB.r exactly)
all_tests <- unlist(lapply(all_metas, function(m) {
  if (!is.null(m$tbl) && "test" %in% colnames(m$tbl)) m$tbl[["test"]] else character(0)
}))
combined_tbl    <- data.frame(test = all_tests, stringsAsFactors = FALSE)
max_n_levels    <- max(vapply(all_metas, function(m) if (is.null(m$n_levels)) 1L else m$n_levels, integer(1)))
any_or_col      <- any(vapply(all_metas, function(m) isTRUE(m$OR_col), logical(1)))
any_post_hoc    <- any(vapply(all_metas, function(m) isTRUE(m$post_hoc), logical(1)))
or_methods      <- vapply(all_metas, function(m) if (isTRUE(m$OR_col)) as.character(m$OR_method) else NA_character_, character(1))
or_methods      <- or_methods[!is.na(or_methods)]
combined_or_method <- if (length(or_methods) > 0 && all(or_methods == "wald")) "wald" else "dynamic"
combined_source <- if (any(vapply(all_metas, function(m) identical(m$source, "ternG"), logical(1)))) "ternG" else "ternD"

cat("  Pooled: max_n_levels =", max_n_levels,
    "| any_or_col =", any_or_col,
    "| any_post_hoc =", any_post_hoc,
    "| combined_or_method =", combined_or_method,
    "| source =", combined_source, "\n\n")

txt7 <- wmd(combined_tbl,
            n_levels  = max_n_levels, OR_col    = any_or_col,
            OR_method = combined_or_method, post_hoc  = any_post_hoc,
            source    = combined_source)
cat(txt7, "\n")
check(txt7,
  expected_fragments = c(
    "any comparison group",
    "Welch's one-way ANOVA",
    "Kruskal-Wallis",
    "unadjusted odds ratios (OR)",
    "Where all expected cell counts were five or greater",  # dynamic OR branch
    "Games-Howell",
    "Dunn\u2019s test with Holm correction",
    "compact letter display (CLD)"
  ),
  forbidden_fragments = c(
    "pairwise post-hoc comparisons were not performed.",
    "Wald method for all variables"  # should be dynamic, not wald-only
  )
)

# ============================================================
# 8. ternB – Wald-only OR pooling edge case
#    All OR-producing tables used OR_method = "wald"
#    Expected: combined_or_method = "wald" → Wald-only sentence
# ============================================================
sep("8. ternB — Wald-only OR pooling (all tables used OR_method = \"wald\")")

all_metas_wald <- lapply(
  list(tbl_2g_wald),
  function(t) attr(t, "ternB_meta")
)

or_methods_wald  <- vapply(all_metas_wald, function(m) if (isTRUE(m$OR_col)) as.character(m$OR_method) else NA_character_, character(1))
or_methods_wald  <- or_methods_wald[!is.na(or_methods_wald)]
combined_or_wald <- if (length(or_methods_wald) > 0 && all(or_methods_wald == "wald")) "wald" else "dynamic"

cat("  or_methods stored in meta:", paste(or_methods_wald, collapse = ", "), "\n")
cat("  combined_or_method resolved to:", combined_or_wald, "\n\n")

all_tests_wald   <- unlist(lapply(all_metas_wald, function(m) {
  if (!is.null(m$tbl) && "test" %in% colnames(m$tbl)) m$tbl[["test"]] else character(0)
}))
txt8 <- wmd(data.frame(test = all_tests_wald, stringsAsFactors = FALSE),
            n_levels  = 2, OR_col    = TRUE,
            OR_method = combined_or_wald, post_hoc  = FALSE,
            source    = "ternG")
cat(txt8, "\n")
check(txt8,
  expected_fragments = c(
    "unadjusted odds ratios (OR)",
    "Wald method for all variables"
  ),
  forbidden_fragments = c(
    "Where all expected cell counts were five or greater"  # dynamic branch must not appear
  )
)

# ============================================================
cat("\n", strrep("=", 70), "\n")
cat("  All 8 scenarios complete.\n")
cat(strrep("=", 70), "\n\n")
