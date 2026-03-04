## --------------------------------------------------------------------
## CLD validation script for .compute_cld() in utils_posthoc.R
##
## For each test case, the expected output is derived by tracing through
## the standard CLD algorithm by hand (Piepho 2004 / connected-letters
## method):
##   1. Sort groups by decreasing centre.
##   2. For each group still without a letter, open a new letter and
##      assign it to that group and all groups NOT significantly
##      different from it.
##   3. Post-processing: remove redundant letters (a letter is redundant
##      when every group holding it also holds another letter).
##
## Additionally, when the 'multcompView' package is installed, each case
## is cross-validated against multcompView::multcompLetters(), which is
## the canonical R implementation of the CLD algorithm.
## --------------------------------------------------------------------

source("R/utils_posthoc.R")

# Helper: build pairwise_df from a named p-value vector.
# Names must be "G1-G2" (any order).
make_pw <- function(pvec) {
  nms <- strsplit(names(pvec), "-")
  data.frame(
    group1 = sapply(nms, `[`, 1),
    group2 = sapply(nms, `[`, 2),
    p.adj  = unname(pvec),
    stringsAsFactors = FALSE
  )
}

# Helper: run one test and report PASS / FAIL with details.
# expected is a named character vector of letter strings.
run_test <- function(label, group_names, centers, pw_df, expected) {
  result <- .compute_cld(group_names, centers, pw_df)
  pass   <- identical(result[names(expected)], expected)
  cat(sprintf("\n[%s] %s\n", if (pass) "PASS" else "FAIL", label))
  if (!pass) {
    cat("  Expected: "); print(expected)
    cat("  Got:      "); print(result[names(expected)])
  } else {
    cat(sprintf("  Result: %s\n",
                paste(names(result), result, sep = "=", collapse = "  ")))
  }
  invisible(pass)
}

# Optional: cross-validate against multcompView::multcompLetters
# Returns NULL silently if multcompView is not installed.
cross_validate <- function(label, pvec, centers) {
  if (!requireNamespace("multcompView", quietly = TRUE)) {
    cat("  [INFO] multcompView not installed; skipping cross-validation.\n")
    return(invisible(NULL))
  }
  mv_raw <- multcompView::multcompLetters(pvec)$Letters
  # multcompView sorts alphabetically; we just compare letter equivalence
  # by checking that groups sharing a letter in ours also share one in mv,
  # and vice versa. We do not require identical letter names (a/b vs b/a).
  group_names <- names(centers)
  ord <- order(centers, decreasing = TRUE)
  ours <- .compute_cld(group_names, centers,
                       make_pw(pvec))[group_names]
  # Build "same group" adjacency from each result and compare
  same_ours <- function(g1, g2) {
    any(strsplit(ours[g1], "")[[1]] %in% strsplit(ours[g2], "")[[1]])
  }
  same_mv <- function(g1, g2) {
    any(strsplit(mv_raw[g1], "")[[1]] %in% strsplit(mv_raw[g2], "")[[1]])
  }
  pairs <- combn(group_names, 2, simplify = FALSE)
  mismatches <- Filter(function(p) same_ours(p[1], p[2]) != same_mv(p[1], p[2]), pairs)
  if (length(mismatches) == 0) {
    cat("  [XV] multcompView cross-validation: MATCH\n")
  } else {
    cat("  [XV] multcompView cross-validation: MISMATCH on pairs:\n")
    for (m in mismatches) cat(sprintf("       %s vs %s\n", m[1], m[2]))
    cat("  multcompView result: "); print(mv_raw)
  }
}

results <- logical(0)
cat("================================================================\n")
cat(" .compute_cld() validation — TernTables utils_posthoc.R\n")
cat("================================================================\n")

# ------------------------------------------------------------------
# TEST 1: Three groups, all pairs significant → fully separated a/b/c
#
# Hand trace (sorted A > B > C):
#   - A no letter → open "a" → A only (B,C all sig from A) → A={a}
#   - B no letter → open "b" → B only → B={b}
#   - C no letter → open "c" → C only → C={c}
#   No redundancies.  Expected: A=a, B=b, C=c
# ------------------------------------------------------------------
results["T1"] <- run_test(
  "3 groups · all pairs sig → a / b / c",
  group_names = c("A","B","C"),
  centers     = c(A=10, B=5, C=1),
  pw_df       = make_pw(c("A-B"=0.001, "A-C"=0.0001, "B-C"=0.01)),
  expected    = c(A="a", B="b", C="c")
)
cross_validate("T1", c("A-B"=0.001, "A-C"=0.0001, "B-C"=0.01),
               c(A=10, B=5, C=1))

# ------------------------------------------------------------------
# TEST 2: Three groups, top group differs, bottom two equal → a / b / b
#
# Hand trace (sorted A > B > C):
#   - A no letter → open "a" → A only (B,C sig from A) → A={a}
#   - B no letter → open "b" → B and C (B-C not sig) → B={b}, C={b}
#   No redundancies.  Expected: A=a, B=b, C=b
# ------------------------------------------------------------------
results["T2"] <- run_test(
  "3 groups · top differs, bottom two equal → a / b / b",
  group_names = c("A","B","C"),
  centers     = c(A=10, B=5, C=4),
  pw_df       = make_pw(c("A-B"=0.001, "A-C"=0.001, "B-C"=0.80)),
  expected    = c(A="a", B="b", C="b")
)
cross_validate("T2", c("A-B"=0.001, "A-C"=0.001, "B-C"=0.80),
               c(A=10, B=5, C=4))

# ------------------------------------------------------------------
# TEST 3: Three groups, no pairs significant → all share "a"
#
# Hand trace:
#   - A no letter → open "a" → A, B, C all not sig from A → all={a}
#   No redundancies.  Expected: A=a, B=a, C=a
# ------------------------------------------------------------------
results["T3"] <- run_test(
  "3 groups · no pairs sig → a / a / a",
  group_names = c("A","B","C"),
  centers     = c(A=10, B=9, C=8),
  pw_df       = make_pw(c("A-B"=0.60, "A-C"=0.50, "B-C"=0.90)),
  expected    = c(A="a", B="a", C="a")
)
cross_validate("T3", c("A-B"=0.60, "A-C"=0.50, "B-C"=0.90),
               c(A=10, B=9, C=8))

# ------------------------------------------------------------------
# TEST 4: Non-transitive case → a / ab / b   (classic CLD ambiguity)
#
# A-B not sig, B-C not sig, A-C sig.
# Hand trace (sorted A > B > C):
#   - A no letter → open "a" → A (itself), B (A-B not sig); NOT C (A-C sig)
#     → A={a}, B={a}
#   - C no letter → open "b" → B (B-C not sig), C (itself); NOT A (A-C sig)
#     → B={a,b}, C={b}
#   Redundancy: grps("a")={A,B}, grps("b")={B,C}. Neither is subset of the other.
#   Expected: A=a, B=ab, C=b
# ------------------------------------------------------------------
results["T4"] <- run_test(
  "3 groups · non-transitive → a / ab / b",
  group_names = c("A","B","C"),
  centers     = c(A=10, B=5, C=1),
  pw_df       = make_pw(c("A-B"=0.20, "A-C"=0.01, "B-C"=0.30)),
  expected    = c(A="a", B="ab", C="b")
)
cross_validate("T4", c("A-B"=0.20, "A-C"=0.01, "B-C"=0.30),
               c(A=10, B=5, C=1))

# ------------------------------------------------------------------
# TEST 5: Four groups, all pairs significant → a / b / c / d
#
# Hand trace: each group gets own letter in sequence.
# Expected: A=a, B=b, C=c, D=d
# ------------------------------------------------------------------
results["T5"] <- run_test(
  "4 groups · all pairs sig → a / b / c / d",
  group_names = c("A","B","C","D"),
  centers     = c(A=20, B=15, C=10, D=5),
  pw_df       = make_pw(c("A-B"=0.001,"A-C"=0.0001,"A-D"=0.0001,
                           "B-C"=0.01, "B-D"=0.001,
                           "C-D"=0.02)),
  expected    = c(A="a", B="b", C="c", D="d")
)
cross_validate("T5",
               c("A-B"=0.001,"A-C"=0.0001,"A-D"=0.0001,
                 "B-C"=0.01, "B-D"=0.001, "C-D"=0.02),
               c(A=20, B=15, C=10, D=5))

# ------------------------------------------------------------------
# TEST 6: Four groups, bottom two equal → a / b / c / c
#
# A-B sig, A-C sig, A-D sig, B-C sig, B-D sig, C-D not sig.
# Hand trace (sorted A > B > C > D):
#   - A no letter → open "a" → A only → A={a}
#   - B no letter → open "b" → B only → B={b}
#   - C no letter → open "c" → C, D (C-D not sig) → C={c}, D={c}
#   No redundancies.  Expected: A=a, B=b, C=c, D=c
# ------------------------------------------------------------------
results["T6"] <- run_test(
  "4 groups · bottom two equal → a / b / c / c",
  group_names = c("A","B","C","D"),
  centers     = c(A=20, B=15, C=8, D=7),
  pw_df       = make_pw(c("A-B"=0.01, "A-C"=0.001,"A-D"=0.001,
                           "B-C"=0.02, "B-D"=0.02,
                           "C-D"=0.80)),
  expected    = c(A="a", B="b", C="c", D="c")
)
cross_validate("T6",
               c("A-B"=0.01,"A-C"=0.001,"A-D"=0.001,
                 "B-C"=0.02,"B-D"=0.02,"C-D"=0.80),
               c(A=20, B=15, C=8, D=7))

# ------------------------------------------------------------------
# TEST 7: Four groups, overlapping middle.
#
# A-B not sig, A-C sig, A-D sig, B-C not sig, B-D sig, C-D not sig.
# Hand trace (sorted A > B > C > D, Piepho 2004 consistency check):
#   - Open "a", anchor=A. A gets "a". holders={A}.
#     B: B-A not sig → B gets "a". holders={A,B}.
#     C: C-A sig → no.  D: D-A sig → no.
#     → A={a}, B={a}, C={}, D={}
#   - Open "b", anchor=C. C gets "b". holders={C}.
#     B: B-C not sig → B gets "b". holders={B,C}.
#     D: D-B sig (0.02) → blocked. D does NOT get "b".
#     → A={a}, B={ab}, C={b}, D={}
#   - Open "c", anchor=D. D gets "c". holders={D}.
#     C: C-D not sig → C gets "c". holders={C,D}.
#     → A={a}, B={ab}, C={bc}, D={c}
#   Redundancy: grps(a)={A,B}, grps(b)={B,C}, grps(c)={C,D} — no subsets.
#   Expected: A=a, B=ab, C=bc, D=c  (matches multcompView exactly)
# ------------------------------------------------------------------
results["T7"] <- run_test(
  "4 groups · overlapping middle → a / ab / bc / c",
  group_names = c("A","B","C","D"),
  centers     = c(A=20, B=16, C=10, D=8),
  pw_df       = make_pw(c("A-B"=0.20, "A-C"=0.01, "A-D"=0.01,
                           "B-C"=0.15, "B-D"=0.02,
                           "C-D"=0.60)),
  expected    = c(A="a", B="ab", C="bc", D="c")
)
cross_validate("T7",
               c("A-B"=0.20,"A-C"=0.01,"A-D"=0.01,
                 "B-C"=0.15,"B-D"=0.02,"C-D"=0.60),
               c(A=20, B=16, C=10, D=8))

# ------------------------------------------------------------------
# TEST 8: Input in non-sorted order — result must respect group_names order
#
# Same as T2 but group_names passed in reverse order.
# Expected: C=b, B=b, A=a  (same letters, different position in output)
# ------------------------------------------------------------------
results["T8"] <- run_test(
  "Input order independence — same as T2, groups reversed",
  group_names = c("C","B","A"),
  centers     = c(C=4, B=5, A=10),
  pw_df       = make_pw(c("A-B"=0.001, "A-C"=0.001, "B-C"=0.80)),
  expected    = c(C="b", B="b", A="a")
)

# ------------------------------------------------------------------
# TEST 9: Two groups (edge case) — should work like a 2-group comparison.
# Both given same letter when not sig, different when sig.
# ------------------------------------------------------------------
results["T9a"] <- run_test(
  "2 groups · not sig → a / a",
  group_names = c("A","B"),
  centers     = c(A=10, B=9),
  pw_df       = make_pw(c("A-B"=0.60)),
  expected    = c(A="a", B="a")
)
results["T9b"] <- run_test(
  "2 groups · sig → a / b",
  group_names = c("A","B"),
  centers     = c(A=10, B=4),
  pw_df       = make_pw(c("A-B"=0.01)),
  expected    = c(A="a", B="b")
)

# ------------------------------------------------------------------
# Summary: hand-built test cases
# ------------------------------------------------------------------
cat("\n================================================================\n")
n_pass <- sum(results)
n_fail <- sum(!results)
cat(sprintf(" TOTAL: %d/%d passed", n_pass, length(results)))
if (n_fail == 0) {
  cat("  ✓ All tests passed\n")
} else {
  cat(sprintf("  ✗ %d test(s) FAILED: %s\n",
              n_fail,
              paste(names(results)[!results], collapse = ", ")))
}
cat("================================================================\n")

# ==================================================================
# REAL-DATA CROSS-VALIDATION
# Uses actual rstatix output (Games-Howell and Dunn's) on tern_colon
# and compares .compute_cld() vs multcompView::multcompLetters()
# on the exact same p-value vectors that ternG() would produce.
# ==================================================================
cat("\n================================================================\n")
cat(" REAL-DATA CROSS-VALIDATION (rstatix p-values vs multcompView)\n")
cat("================================================================\n")

has_rstatix      <- requireNamespace("rstatix",      quietly = TRUE)
has_multcompView <- requireNamespace("multcompView", quietly = TRUE)
has_survival     <- requireNamespace("survival",     quietly = TRUE)

if (!has_rstatix)      cat("[SKIP] rstatix not installed.\n")
if (!has_multcompView) cat("[SKIP] multcompView not installed.\n")
if (!has_survival)     cat("[SKIP] survival not installed (needed for tern_colon).\n")

if (has_rstatix && has_multcompView && has_survival) {

  # Load tern_colon (3-group: Observation / Levamisole / Levamisole + 5FU)
  data(tern_colon, package = "TernTables")
  tc <- tern_colon
  group_var <- "Treatment_Arm"
  group_levels <- c("Observation", "Levamisole", "Levamisole + 5FU")

  # Helper: adjacency comparison between two named letter vectors
  # Returns TRUE if every pair of groups has same shared-letter status in both.
  adj_match <- function(ours_named, mv_named) {
    groups <- names(ours_named)
    pairs  <- combn(groups, 2, simplify = FALSE)
    mismatches <- Filter(function(p) {
      shared_ours <- any(strsplit(ours_named[p[1]], "")[[1]] %in%
                           strsplit(ours_named[p[2]], "")[[1]])
      shared_mv   <- any(strsplit(mv_named[p[1]],   "")[[1]] %in%
                           strsplit(mv_named[p[2]],  "")[[1]])
      shared_ours != shared_mv
    }, pairs)
    list(match = length(mismatches) == 0, mismatches = mismatches)
  }

  real_results <- character(0)

  # ----------------------------------------------------------------
  # RD1: Games-Howell on Age_Years (parametric path)
  # ----------------------------------------------------------------
  tryCatch({
    ph <- rstatix::games_howell_test(tc, Age_Years ~ Treatment_Arm)
    centers <- tapply(tc$Age_Years, tc[[group_var]], mean, na.rm = TRUE)[group_levels]
    ph_df <- data.frame(group1 = as.character(ph$group1),
                        group2 = as.character(ph$group2),
                        p.adj  = ph$p.adj,
                        stringsAsFactors = FALSE)
    pvec <- setNames(ph_df$p.adj,
                     paste(ph_df$group1, ph_df$group2, sep = "-"))
    ours <- .compute_cld(group_levels, centers, ph_df)
    mv   <- multcompView::multcompLetters(pvec)$Letters[group_levels]
    chk  <- adj_match(ours, mv)
    cat(sprintf("\n[RD1] Games-Howell · Age_Years\n"))
    cat(sprintf("  ours: %s\n", paste(names(ours), ours, sep="=", collapse="  ")))
    cat(sprintf("  mv:   %s\n", paste(names(mv),   mv,   sep="=", collapse="  ")))
    cat(sprintf("  adjacency: %s\n", if (chk$match) "MATCH" else "MISMATCH"))
    real_results["RD1"] <- if (chk$match) "MATCH" else "MISMATCH"
  }, error = function(e) {
    cat(sprintf("\n[RD1] ERROR: %s\n", conditionMessage(e)))
    real_results["RD1"] <<- "ERROR"
  })

  # ----------------------------------------------------------------
  # RD2: Dunn's (Holm) on Positive_Lymph_Nodes_n (non-parametric path)
  # ----------------------------------------------------------------
  tryCatch({
    ph <- rstatix::dunn_test(tc, Positive_Lymph_Nodes_n ~ Treatment_Arm,
                              p.adjust.method = "holm")
    centers <- tapply(tc$Positive_Lymph_Nodes_n,
                      tc[[group_var]], median, na.rm = TRUE)[group_levels]
    ph_df <- data.frame(group1 = as.character(ph$group1),
                        group2 = as.character(ph$group2),
                        p.adj  = ph$p.adj,
                        stringsAsFactors = FALSE)
    pvec <- setNames(ph_df$p.adj,
                     paste(ph_df$group1, ph_df$group2, sep = "-"))
    ours <- .compute_cld(group_levels, centers, ph_df)
    mv   <- multcompView::multcompLetters(pvec)$Letters[group_levels]
    chk  <- adj_match(ours, mv)
    cat(sprintf("\n[RD2] Dunn's+Holm · Positive_Lymph_Nodes_n\n"))
    cat(sprintf("  ours: %s\n", paste(names(ours), ours, sep="=", collapse="  ")))
    cat(sprintf("  mv:   %s\n", paste(names(mv),   mv,   sep="=", collapse="  ")))
    cat(sprintf("  adjacency: %s\n", if (chk$match) "MATCH" else "MISMATCH"))
    real_results["RD2"] <- if (chk$match) "MATCH" else "MISMATCH"
  }, error = function(e) {
    cat(sprintf("\n[RD2] ERROR: %s\n", conditionMessage(e)))
    real_results["RD2"] <<- "ERROR"
  })

  # ----------------------------------------------------------------
  # RD3: Dunn's (Holm) on Age_Years (non-parametric path on same var as RD1)
  # ----------------------------------------------------------------
  tryCatch({
    ph <- rstatix::dunn_test(tc, Age_Years ~ Treatment_Arm,
                              p.adjust.method = "holm")
    centers <- tapply(tc$Age_Years,
                      tc[[group_var]], median, na.rm = TRUE)[group_levels]
    ph_df <- data.frame(group1 = as.character(ph$group1),
                        group2 = as.character(ph$group2),
                        p.adj  = ph$p.adj,
                        stringsAsFactors = FALSE)
    pvec <- setNames(ph_df$p.adj,
                     paste(ph_df$group1, ph_df$group2, sep = "-"))
    ours <- .compute_cld(group_levels, centers, ph_df)
    mv   <- multcompView::multcompLetters(pvec)$Letters[group_levels]
    chk  <- adj_match(ours, mv)
    cat(sprintf("\n[RD3] Dunn's+Holm · Age_Years\n"))
    cat(sprintf("  ours: %s\n", paste(names(ours), ours, sep="=", collapse="  ")))
    cat(sprintf("  mv:   %s\n", paste(names(mv),   mv,   sep="=", collapse="  ")))
    cat(sprintf("  adjacency: %s\n", if (chk$match) "MATCH" else "MISMATCH"))
    real_results["RD3"] <- if (chk$match) "MATCH" else "MISMATCH"
  }, error = function(e) {
    cat(sprintf("\n[RD3] ERROR: %s\n", conditionMessage(e)))
    real_results["RD3"] <<- "ERROR"
  })

  # ----------------------------------------------------------------
  # RD4: Games-Howell on Positive_Lymph_Nodes_n (parametric path on same var as RD2)
  # ----------------------------------------------------------------
  tryCatch({
    ph <- rstatix::games_howell_test(tc, Positive_Lymph_Nodes_n ~ Treatment_Arm)
    centers <- tapply(tc$Positive_Lymph_Nodes_n,
                      tc[[group_var]], mean, na.rm = TRUE)[group_levels]
    ph_df <- data.frame(group1 = as.character(ph$group1),
                        group2 = as.character(ph$group2),
                        p.adj  = ph$p.adj,
                        stringsAsFactors = FALSE)
    pvec <- setNames(ph_df$p.adj,
                     paste(ph_df$group1, ph_df$group2, sep = "-"))
    ours <- .compute_cld(group_levels, centers, ph_df)
    mv   <- multcompView::multcompLetters(pvec)$Letters[group_levels]
    chk  <- adj_match(ours, mv)
    cat(sprintf("\n[RD4] Games-Howell · Positive_Lymph_Nodes_n\n"))
    cat(sprintf("  ours: %s\n", paste(names(ours), ours, sep="=", collapse="  ")))
    cat(sprintf("  mv:   %s\n", paste(names(mv),   mv,   sep="=", collapse="  ")))
    cat(sprintf("  adjacency: %s\n", if (chk$match) "MATCH" else "MISMATCH"))
    real_results["RD4"] <- if (chk$match) "MATCH" else "MISMATCH"
  }, error = function(e) {
    cat(sprintf("\n[RD4] ERROR: %s\n", conditionMessage(e)))
    real_results["RD4"] <<- "ERROR"
  })

  cat("\n----------------------------------------------------------------\n")
  n_match <- sum(real_results == "MATCH")
  cat(sprintf(" Real-data cross-validation: %d/%d MATCH\n",
              n_match, length(real_results)))
  if (all(real_results == "MATCH")) {
    cat(" ✓ .compute_cld() and multcompView produce identical adjacency\n")
    cat("   structures on real rstatix output. Safe to switch if desired.\n")
  } else {
    cat(" ✗ MISMATCH(ES) detected — investigate before switching.\n")
  }
  cat("================================================================\n")

} else {
  cat("[SKIP] Real-data cross-validation skipped (missing packages above).\n")
  cat("================================================================\n")
}
