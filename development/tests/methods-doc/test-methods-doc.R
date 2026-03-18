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
# 7. ternB – mixed bundle (ternD + ternG-2g-OR + ternG-3g-posthoc)
#    All three have different configurations → should produce
#    3 separate labeled sections in the methods document.
# ============================================================
sep("7. ternB — mixed bundle: 3 distinct configs → 3 sections")

tmp7 <- tempfile(fileext = ".docx")
ternB(
  list(tbl_desc, tbl_2g_or, tbl_3g_ph),
  output_docx      = tempfile(fileext = ".docx"),
  methods_doc      = TRUE,
  methods_filename = tmp7
)

# Read text back from the written Word file
doc7_text <- paste(officer::docx_summary(officer::read_docx(tmp7))$text, collapse = " ")
cat("  Extracted text (truncated):\n ")
cat(substr(doc7_text, 1, 300), "...\n\n")

check(doc7_text,
  expected_fragments = c(
    # Section for tbl_desc (ternD)
    "fewer than three available observations",   # ternD n<3 phrasing
    # Section for tbl_2g_or (ternG 2-group)
    "Welch's independent samples t-test",
    "unadjusted odds ratios (OR)",
    "Where all expected cell counts were five or greater",
    # Section for tbl_3g_ph (ternG 3-group posthoc)
    "Welch's one-way ANOVA",
    "Games-Howell",
    "compact letter display (CLD)",
    # Footer
    "Each section reflects the specific configuration"
  ),
  forbidden_fragments = c(
    "pairwise post-hoc comparisons were not performed."
  )
)

# ============================================================
# 8. ternB – identical-config deduplication
#    Two separate ternG 2-group tables (same settings) bundled
#    together → should produce ONE section, not two.
# ============================================================
sep("8. ternB — deduplication: 2 identical configs → 1 section")

# Build a second 2-group-OR table (same settings, same data — paragraph will be identical)
tbl_2g_or_b <- ternG(tern_colon, group_var = "Recurrence",
                     exclude_vars = c("ID", "Study", "Event_Type"),
                     OR_col = TRUE, methods_doc = FALSE,
                     table_caption = "Table 2. Duplicate config.")

tmp8 <- tempfile(fileext = ".docx")
ternB(
  list(tbl_2g_or, tbl_2g_or_b),
  output_docx      = tempfile(fileext = ".docx"),
  methods_doc      = TRUE,
  methods_filename = tmp8
)

doc8_rows <- officer::docx_summary(officer::read_docx(tmp8))
doc8_text <- paste(doc8_rows$text, collapse = " ")

# Count how many times the OR paragraph appears — should be exactly once
or_count <- lengths(regmatches(doc8_text, gregexpr("unadjusted odds ratios \\(OR\\)", doc8_text)))
cat("  OR sentence occurrences (expect 1):", or_count, "\n")
if (or_count == 1L) {
  cat("  [PASS] Deduplication working — identical paragraphs consolidated.\n")
} else {
  cat("  [FAIL] Expected 1 OR sentence, found", or_count, "\n")
}

# The heading should contain both table labels joined by " / "
heading_rows <- doc8_rows[doc8_rows$style_name %in% c("Normal") & nchar(trimws(doc8_rows$text)) > 0, ]
has_combined_label <- any(grepl("/", doc8_rows$text, fixed = TRUE))
if (has_combined_label) {
  cat("  [PASS] Combined label present (tables joined with '/').\n")
} else {
  cat("  [FAIL] Combined label not found — sections may not have been merged.\n")
}

# ============================================================
cat("\n", strrep("=", 70), "\n")
cat("  All 8 scenarios complete.\n")
cat(strrep("=", 70), "\n\n")
