# development/tests/test-normality-extraction.R
#
# Validates that extracting the ROBUST normality gating logic into
# .robust_normality() produces IDENTICAL results to the original
# inline logic in ternG.R and ternD.R.
#
# USAGE
# -----
# Step 1 (BEFORE making any changes):
#   devtools::load_all(); source(this_file)
#   → writes reference RDS files to development/tests/normality-extraction/
#
# Step 2 (AFTER creating helper + modifying ternG/ternD):
#   devtools::load_all(); source(this_file)
#   → compares against reference RDS files; all comparisons must pass
#
# The script detects which mode it is in by checking for the reference files.
# ─────────────────────────────────────────────────────────────────────────────

suppressPackageStartupMessages({
  library(dplyr)
  devtools::load_all(quiet = TRUE)
})

REF_DIR  <- "development/tests/normality-extraction"
dir.create(REF_DIR, showWarnings = FALSE, recursive = TRUE)
REF_MODE <- !file.exists(file.path(REF_DIR, "ref_ternD.rds"))

cat(if (REF_MODE) "\n=== MODE: CAPTURE (before-change baseline) ===\n"
                else "\n=== MODE: VERIFY (after-change comparison)  ===\n")
cat("─────────────────────────────────────────────\n\n")

all_pass <- TRUE
fail <- function(msg) { cat("  FAIL:", msg, "\n"); all_pass <<- FALSE }
pass <- function(msg) { cat("  PASS:", msg, "\n") }

# ═══════════════════════════════════════════════════════════════════════════
# PART 1: Unit tests of calc_skewness / calc_kurtosis / .robust_normality
#   These run in VERIFY mode only (functions only exist after the change).
# ═══════════════════════════════════════════════════════════════════════════

if (!REF_MODE) {
  cat("PART 1: Unit tests — helper functions\n")
  cat("─────────────────────────────────────\n")

  # ─── Reference implementations (verbatim copy from OLD ternG.R / ternD.R) ──

  OLD_calc_skewness <- function(x) {
    x <- x[!is.na(x)]
    n <- length(x)
    if (n < 3) return(NA_real_)
    m  <- mean(x)
    m2 <- sum((x - m)^2) / n
    m3 <- sum((x - m)^3) / n
    if (m2 == 0) return(NA_real_)
    m3 / m2^1.5
  }

  OLD_calc_kurtosis <- function(x) {
    x <- x[!is.na(x)]
    n <- length(x)
    if (n < 4) return(NA_real_)
    m  <- mean(x)
    m2 <- sum((x - m)^2) / n
    m4 <- sum((x - m)^4) / n
    if (m2 == 0) return(NA_real_)
    m4 / m2^2 - 3
  }

  # Reference gate logic for a single vector (mirrors original ternD ROBUST block).
  # Uses shapiro_p-style NA stripping for Gate 4 (ternD behaviour).
  OLD_gate_single <- function(x) {
    sw_val <- {
      x2 <- x[!is.na(x)]
      if (length(x2) < 3 || length(x2) > 5000 || stats::var(x2) == 0) NA_real_
      else tryCatch(stats::shapiro.test(x2)$p.value, error = function(e) NA_real_)
    }
    n_obs    <- sum(!is.na(x))
    skewness <- OLD_calc_skewness(x)
    kurtosis <- OLD_calc_kurtosis(x)
    if (n_obs < 3) {
      list(is_normal = FALSE, gate = 1L)
    } else if ((!is.na(skewness) && abs(skewness) > 2) ||
               (!is.na(kurtosis) && abs(kurtosis) > 7)) {
      list(is_normal = FALSE, gate = 2L)
    } else if (n_obs >= 30) {
      list(is_normal = TRUE, gate = 3L)
    } else {
      list(is_normal = !is.na(sw_val) && sw_val > 0.05, gate = 4L, sw = sw_val)
    }
  }

  # Reference gate logic for a named group list (mirrors original ternG ROBUST block).
  # NOTE: old ternG did NOT strip NAs before shapiro.test — for complete data
  # (no NAs) the result is identical; differences only arise with NA-bearing
  # groups at Gate 4, where the new code is strictly more correct.
  OLD_gate_groups <- function(group_vals) {
    group_ns       <- sapply(group_vals, function(v) sum(!is.na(v)))
    group_skewness <- sapply(group_vals, OLD_calc_skewness)
    group_kurtosis <- sapply(group_vals, OLD_calc_kurtosis)
    n_levels       <- length(group_vals)

    if (any(group_ns < 3)) {
      return(list(is_normal = FALSE, sw_p_all = list(), gate = 1L))
    } else if (any(abs(group_skewness) > 2, na.rm = TRUE) ||
               any(abs(group_kurtosis) > 7, na.rm = TRUE)) {
      return(list(is_normal = FALSE, sw_p_all = list(), gate = 2L))
    } else if (all(group_ns >= 30)) {
      return(list(is_normal = TRUE, sw_p_all = list(), gate = 3L))
    } else {
      # Old code: passes raw vector (with NAs) to shapiro.test — error → all NA
      sw_p_all <- tryCatch({
        out <- lapply(seq_along(group_vals), function(i) {
          x    <- group_vals[[i]]
          pval <- if (length(x) >= 3 && length(x) <= 5000)
                    stats::shapiro.test(x)$p.value
                  else NA_real_
          setNames(pval, paste0("SW_p_", names(group_vals)[i]))
        })
        do.call(c, out)
      }, error = function(e) rep(NA_real_, n_levels))
      is_normal <- all(!is.na(sw_p_all) & sw_p_all > 0.05)
      return(list(is_normal = is_normal, sw_p_all = sw_p_all, gate = 4L))
    }
  }

  # ─── Test vectors (complete, no NAs — ensures old/new are truly identical) ──
  set.seed(42)
  g1       <- c(1, 2)                          # Gate 1: n < 3
  g2_skew  <- c(rep(1, 50), 1000)              # Gate 2: high skewness
  g2_kurt  <- c(rep(0, 48), 10, -10)           # Gate 2: high kurtosis, low skew
  g3       <- rnorm(50, 100, 15)               # Gate 3: n >= 30, normal shape
  g4_pass  <- rnorm(15, 100, 15)               # Gate 4: small n, normal
  g4_fail  <- c(rexp(12, 0.1), rexp(3, 10))   # Gate 4: small n, non-normal

  # ─── calc_skewness / calc_kurtosis ──────────────────────────────────────────
  cat("\n1a. Skewness/kurtosis match:\n")
  for (nm in c("g1","g2_skew","g2_kurt","g3","g4_pass","g4_fail")) {
    x   <- get(nm)
    s_o <- OLD_calc_skewness(x); s_n <- TernTables:::.calc_skewness(x)
    k_o <- OLD_calc_kurtosis(x); k_n <- TernTables:::.calc_kurtosis(x)
    ok_s <- isTRUE(all.equal(s_o, s_n))
    ok_k <- isTRUE(all.equal(k_o, k_n))
    if (ok_s && ok_k) {
      pass(sprintf("%-10s skew=%.3f kurt=%.3f", nm,
                   ifelse(is.na(s_n), NA, s_n), ifelse(is.na(k_n), NA, k_n)))
    } else {
      if (!ok_s) fail(sprintf("%s skewness: old=%.4f new=%.4f", nm, s_o, s_n))
      if (!ok_k) fail(sprintf("%s kurtosis: old=%.4f new=%.4f", nm, k_o, k_n))
    }
  }

  # ─── Single-vector (ternD mode) gate routing ────────────────────────────────
  cat("\n1b. Single-vector gate routing (ternD mode):\n")
  single_tests <- list(
    list(label = "Gate 1 (n<3)",        x = g1),
    list(label = "Gate 2 (high skew)",  x = g2_skew),
    list(label = "Gate 2 (high kurt)",  x = g2_kurt),
    list(label = "Gate 3 (n>=30 norm)", x = g3),
    list(label = "Gate 4 pass",         x = g4_pass),
    list(label = "Gate 4 fail",         x = g4_fail)
  )
  for (t in single_tests) {
    old_r <- OLD_gate_single(t$x)
    new_r <- TernTables:::.robust_normality(list(t$x))
    ok_n  <- identical(old_r$is_normal, new_r$is_normal)
    ok_g  <- identical(old_r$gate,      new_r$gate)
    if (ok_n && ok_g) {
      pass(sprintf("%-30s is_normal=%-5s gate=%d", t$label, new_r$is_normal, new_r$gate))
    } else {
      fail(sprintf("%-30s old(is_normal=%s gate=%d) new(is_normal=%s gate=%d)",
                   t$label, old_r$is_normal, old_r$gate, new_r$is_normal, new_r$gate))
    }
  }

  # ─── Group-list (ternG mode) gate routing — complete data only ──────────────
  set.seed(99)
  g3b <- rnorm(40, 50, 10)   # second normal large-n group
  g4b <- rnorm(20, 50, 10)   # second normal small-n group

  cat("\n1c. Group-list gate routing (ternG mode, complete data):\n")
  group_tests <- list(
    list(label = "Gate 1: one n<3 group",     grps = list(A = g1,      B = g3)),
    list(label = "Gate 2: one skewed group",  grps = list(A = g2_skew, B = g3)),
    list(label = "Gate 2: one heavy-tail grp",grps = list(A = g2_kurt, B = g3)),
    list(label = "Gate 3: all n>=30",         grps = list(A = g3,      B = g3b)),
    list(label = "Gate 4 pass: all SW ok",    grps = list(A = g4_pass, B = g4b)),
    list(label = "Gate 4 fail: one non-norm", grps = list(A = g4_fail, B = g4b))
  )
  for (t in group_tests) {
    old_r <- OLD_gate_groups(t$grps)
    new_r <- TernTables:::.robust_normality(t$grps)
    ok_n  <- identical(old_r$is_normal, new_r$is_normal)
    ok_g  <- identical(old_r$gate,      new_r$gate)
    # SW p-values: compare at Gate 4 for complete data
    ok_sw <- TRUE
    if (new_r$gate == 4L && !is.null(new_r$sw_pvalues)) {
      ok_sw <- isTRUE(all.equal(
        as.numeric(old_r$sw_p_all[order(names(old_r$sw_p_all))]),
        as.numeric(new_r$sw_pvalues[order(names(new_r$sw_pvalues))])
      ))
    }
    if (ok_n && ok_g && ok_sw) {
      pass(sprintf("%-32s is_normal=%-5s gate=%d", t$label, new_r$is_normal, new_r$gate))
    } else {
      fail(sprintf("%-32s old(is_normal=%s gate=%d) new(is_normal=%s gate=%d)%s",
                   t$label,
                   old_r$is_normal, old_r$gate,
                   new_r$is_normal, new_r$gate,
                   if (!ok_sw) " [SW p-values differ]" else ""))
    }
  }

  # ─── Edge cases ──────────────────────────────────────────────────────────────
  cat("\n1d. Edge cases:\n")

  # Constant vector — all values equal → m2=0 → NA skew/kurt → falls to Gate 4
  const_vec <- rep(5, 20)
  ec1 <- TernTables:::.robust_normality(list(const_vec))
  if (ec1$gate == 4L)
    pass("Constant vector reaches Gate 4 (NaN-safe skew/kurt)")
  else
    fail(sprintf("Constant vector: expected gate 4, got %d", ec1$gate))

  # All-NA vector — n_obs=0 < 3 → Gate 1
  na_vec <- rep(NA_real_, 10)
  ec2 <- TernTables:::.robust_normality(list(na_vec))
  if (!ec2$is_normal && ec2$gate == 1L)
    pass("All-NA vector → Gate 1 non-parametric")
  else
    fail(sprintf("All-NA vector: expected gate=1 is_normal=FALSE, got gate=%d is_normal=%s",
                 ec2$gate, ec2$is_normal))

  # n exactly 30 — should hit Gate 3, not Gate 4
  ec3_vec <- rnorm(30, 0, 1)
  ec3 <- TernTables:::.robust_normality(list(ec3_vec))
  if (ec3$gate == 3L)
    pass("n=30 routes to Gate 3 (CLT boundary inclusive)")
  else
    fail(sprintf("n=30: expected gate=3, got gate=%d", ec3$gate))

  # n=29 — must reach Gate 4 (one below CLT threshold)
  ec4_vec <- rnorm(29, 0, 1)
  ec4 <- TernTables:::.robust_normality(list(ec4_vec))
  if (ec4$gate == 4L)
    pass("n=29 routes to Gate 4 (below CLT threshold)")
  else
    fail(sprintf("n=29: expected gate=4, got gate=%d", ec4$gate))

} # end !REF_MODE unit tests

# ═══════════════════════════════════════════════════════════════════════════
# PART 2: Integration tests — full ternD and ternG on tern_colon
# ═══════════════════════════════════════════════════════════════════════════

cat(sprintf("\nPART %d: Integration tests — ternD and ternG on tern_colon\n",
            if (REF_MODE) 1L else 2L))
cat("────────────────────────────────────────────────────────\n")

data(tern_colon, package = "TernTables")

# ─── ternD ──────────────────────────────────────────────────────────────────
cat("\nRunning ternD(tern_colon, consider_normality='ROBUST', print_normality=TRUE)...\n")
td <- suppressMessages(
  ternD(tern_colon,
        consider_normality = "ROBUST",
        print_normality    = TRUE,
        open_doc           = FALSE,
        citation           = FALSE)
)

cat("  nrow:", nrow(td), " ncol:", ncol(td), "\n")

if (REF_MODE) {
  saveRDS(td, file.path(REF_DIR, "ref_ternD.rds"))
  cat("  [SAVED reference ternD output]\n")
} else {
  ref_td <- readRDS(file.path(REF_DIR, "ref_ternD.rds"))

  if (identical(td, ref_td)) {
    pass("ternD output is byte-identical to reference")
  } else {
    # Detailed diff
    if (!identical(dim(td), dim(ref_td))) {
      fail(sprintf("ternD dimensions differ: got %dx%d, expected %dx%d",
                   nrow(td), ncol(td), nrow(ref_td), ncol(ref_td)))
    } else if (!identical(names(td), names(ref_td))) {
      fail(sprintf("ternD column names differ:\n    got:      %s\n    expected: %s",
                   paste(names(td), collapse=", "),
                   paste(names(ref_td), collapse=", ")))
    } else {
      diff_rows <- which(apply(mapply(function(a,b) !identical(a,b), td, ref_td), 1, any))
      if (length(diff_rows) == 0) {
        pass("ternD output matches reference (all cells equal)")
      } else {
        fail(sprintf("ternD: %d row(s) differ from reference", length(diff_rows)))
        for (r in head(diff_rows, 5)) {
          cat(sprintf("    Row %d Variable='%s' Summary(new)='%s' Summary(ref)='%s'\n",
                      r,
                      td$Variable[r],
                      as.character(td$Summary[r]),
                      as.character(ref_td$Summary[r])))
        }
      }
    }
  }
}

# ─── ternG, 2-group ─────────────────────────────────────────────────────────
cat("\nRunning ternG(tern_colon, group_var='Sex', consider_normality='ROBUST', print_normality=TRUE)...\n")
tg2 <- suppressMessages(
  ternG(tern_colon,
        group_var          = "Sex",
        consider_normality = "ROBUST",
        print_normality    = TRUE,
        open_doc           = FALSE,
        citation           = FALSE)
)
cat("  nrow:", nrow(tg2), " ncol:", ncol(tg2), "\n")

if (REF_MODE) {
  saveRDS(tg2, file.path(REF_DIR, "ref_ternG_2grp.rds"))
  cat("  [SAVED reference ternG 2-group output]\n")
} else {
  ref_tg2 <- readRDS(file.path(REF_DIR, "ref_ternG_2grp.rds"))

  if (identical(tg2, ref_tg2)) {
    pass("ternG 2-group output is byte-identical to reference")
  } else {
    if (!identical(dim(tg2), dim(ref_tg2))) {
      fail(sprintf("ternG 2-grp dimensions: got %dx%d, expected %dx%d",
                   nrow(tg2), ncol(tg2), nrow(ref_tg2), ncol(ref_tg2)))
    } else {
      diff_rows <- which(apply(mapply(function(a,b) !identical(a,b), tg2, ref_tg2), 1, any))
      if (length(diff_rows) == 0) {
        pass("ternG 2-group matches reference (all cells equal)")
      } else {
        fail(sprintf("ternG 2-grp: %d row(s) differ from reference", length(diff_rows)))
        for (r in head(diff_rows, 5)) {
          cat(sprintf("    Row %d Variable='%s'\n", r, tg2$Variable[r]))
        }
      }
    }
  }
}

# ─── ternG, 3-group ─────────────────────────────────────────────────────────
cat("\nRunning ternG(tern_colon, group_var='Treatment_Arm', consider_normality='ROBUST', print_normality=TRUE)...\n")
tg3 <- suppressMessages(
  ternG(tern_colon,
        group_var          = "Treatment_Arm",
        consider_normality = "ROBUST",
        print_normality    = TRUE,
        open_doc           = FALSE,
        citation           = FALSE)
)
cat("  nrow:", nrow(tg3), " ncol:", ncol(tg3), "\n")
sw_cols <- grep("^SW_p_", names(tg3), value = TRUE)
cat("  SW p-value columns:", if (length(sw_cols)) paste(sw_cols, collapse=", ") else "none", "\n")

if (REF_MODE) {
  saveRDS(tg3, file.path(REF_DIR, "ref_ternG_3grp.rds"))
  cat("  [SAVED reference ternG 3-group output]\n")
} else {
  ref_tg3 <- readRDS(file.path(REF_DIR, "ref_ternG_3grp.rds"))

  if (identical(tg3, ref_tg3)) {
    pass("ternG 3-group output is byte-identical to reference")
  } else {
    if (!identical(dim(tg3), dim(ref_tg3))) {
      fail(sprintf("ternG 3-grp dimensions: got %dx%d, expected %dx%d",
                   nrow(tg3), ncol(tg3), nrow(ref_tg3), ncol(ref_tg3)))
    } else {
      diff_rows <- which(apply(mapply(function(a,b) !identical(a,b), tg3, ref_tg3), 1, any))
      if (length(diff_rows) == 0) {
        pass("ternG 3-group matches reference (all cells equal)")
      } else {
        fail(sprintf("ternG 3-grp: %d row(s) differ from reference", length(diff_rows)))
        for (r in head(diff_rows, 5)) {
          cat(sprintf("    Row %d Variable='%s'\n", r, tg3$Variable[r]))
        }
      }
    }
  }
}

# ─── consider_normality = TRUE and FALSE (unchanged paths — sanity check) ───
cat("\nRunning ternG with consider_normality=TRUE (Shapiro-Wilk-only path, unchanged)...\n")
tg_sw <- suppressMessages(
  ternG(tern_colon,
        group_var          = "Sex",
        consider_normality = TRUE,
        open_doc           = FALSE,
        citation           = FALSE)
)
if (REF_MODE) {
  saveRDS(tg_sw, file.path(REF_DIR, "ref_ternG_sw.rds"))
  cat("  [SAVED reference ternG Shapiro-Wilk-only output]\n")
} else {
  ref_sw <- readRDS(file.path(REF_DIR, "ref_ternG_sw.rds"))
  if (identical(tg_sw, ref_sw))
    pass("ternG consider_normality=TRUE path unchanged")
  else
    fail("ternG consider_normality=TRUE output differs from reference")
}

# ─────────────────────────────────────────────────────────────────────────────
if (REF_MODE) {
  cat("\n══════════════════════════════════════════\n")
  cat("CAPTURE COMPLETE — reference files saved to:\n")
  cat(sprintf("  %s/\n", REF_DIR))
  cat("\nNow make the code changes, then re-run this script to verify.\n")
  cat("══════════════════════════════════════════\n\n")
} else {
  cat("\n══════════════════════════════════════════\n")
  if (all_pass) {
    cat("ALL TESTS PASSED — refactoring is safe.\n")
  } else {
    cat("SOME TESTS FAILED — review output above before merging.\n")
  }
  cat("══════════════════════════════════════════\n\n")
}
