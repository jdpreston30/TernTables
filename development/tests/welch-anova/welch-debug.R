# =============================================================================
# welch-debug.R
# Purpose: Diagnose the large p-value shift (0.18 -> 0.007) observed for BMI
#          when switching from aov() to oneway.test(var.equal = FALSE).
# Run from repo root: source("development/tests/welch-anova/welch-debug.R")
# =============================================================================

library(readr)
library(ggplot2)
library(rstatix)   # for games_howell_test() — install.packages("rstatix") if needed

d <- read_csv("test.csv", show_col_types = FALSE)

cat("Dataset dimensions:", nrow(d), "rows x", ncol(d), "cols\n\n")

# Two candidate 3-level grouping variables found in this dataset
group_candidates <- c("AAST Grade", "Index Mgmt. Strategy")

for (GROUP in group_candidates) {
  cat("=============================================================\n")
  cat("GROUP VARIABLE:", GROUP, "\n")
  cat("=============================================================\n\n")

  sub <- d[!is.na(d$BMI) & !is.na(d[[GROUP]]), ]

  cat("--- Group sizes and BMI descriptives ---\n")
  grp_stats <- tapply(sub$BMI, sub[[GROUP]], function(x) {
    c(n    = length(x),
      mean = round(mean(x, na.rm = TRUE), 2),
      sd   = round(sd(x,   na.rm = TRUE), 2),
      var  = round(var(x,  na.rm = TRUE), 2),
      min  = round(min(x,  na.rm = TRUE), 2),
      max  = round(max(x,  na.rm = TRUE), 2))
  })
  print(do.call(rbind, grp_stats))
  cat("\n")

  # Max/min variance ratio — key diagnostic
  vars <- sapply(grp_stats, function(x) x["var"])
  cat("Variance ratio (max/min):", round(max(vars) / min(vars), 2), "\n")
  cat("(>4 is a common rule-of-thumb threshold for heterogeneity)\n\n")

  # --- Shapiro-Wilk normality per group ---
  cat("--- Shapiro-Wilk normality per group ---\n")
  for (g in unique(sub[[GROUP]])) {
    vals <- sub$BMI[sub[[GROUP]] == g]
    if (length(vals) >= 3) {
      sw <- shapiro.test(vals)
      cat(sprintf("  %-35s n=%3d  W=%.4f  p=%.4f  %s\n",
                  g, length(vals), sw$statistic, sw$p.value,
                  ifelse(sw$p.value < 0.05, "NON-NORMAL", "normal")))
    } else {
      cat(sprintf("  %-35s n=%3d  (too few for Shapiro-Wilk)\n", g, length(vals)))
    }
  }
  cat("\n")

  # --- Bartlett test (sensitive to non-normality but standard) ---
  cat("--- Bartlett test for equal variances ---\n")
  bt <- bartlett.test(sub$BMI ~ factor(sub[[GROUP]]))
  cat(sprintf("  Bartlett K-sq = %.4f, p = %.4f  %s\n\n",
              bt$statistic, bt$p.value,
              ifelse(bt$p.value < 0.05, "<== VARIANCES ARE UNEQUAL", "variances OK")))

  # --- Head-to-head: aov() vs Welch ---
  cat("--- P-value comparison: aov() vs oneway.test(var.equal=FALSE) ---\n")
  p_aov   <- summary(aov(BMI ~ factor(sub[[GROUP]]), data = sub))[[1]][["Pr(>F)"]][1]
  p_welch <- oneway.test(BMI ~ factor(sub[[GROUP]]), data = sub, var.equal = FALSE)$p.value
  cat(sprintf("  aov()  (assumes equal variance) : p = %.4f\n", p_aov))
  cat(sprintf("  Welch  (no variance assumption) : p = %.4f\n", p_welch))
  cat(sprintf("  Difference: %.4f  (%s)\n\n",
              abs(p_aov - p_welch),
              ifelse(abs(p_aov - p_welch) > 0.05,
                     "LARGE SHIFT — heterogeneous variances driving the difference",
                     "small shift — variances sufficiently similar")))

  # --- Outlier check: any BMI values > 3 SD from group mean? ---
  cat("--- Outlier check (|value - group_mean| > 3 * group_SD) ---\n")
  outliers <- do.call(rbind, lapply(unique(sub[[GROUP]]), function(g) {
    vals  <- sub$BMI[sub[[GROUP]] == g]
    m     <- mean(vals, na.rm = TRUE)
    s     <- sd(vals, na.rm = TRUE)
    idx   <- which(sub[[GROUP]] == g & abs(sub$BMI - m) > 3 * s)
    if (length(idx) > 0) {
      data.frame(group = g, BMI = sub$BMI[idx])
    }
  }))
  if (is.null(outliers) || nrow(outliers) == 0) {
    cat("  No outliers beyond 3 SD in any group.\n\n")
  } else {
    cat("  Outliers found:\n")
    print(outliers)
    cat("\n")
  }

  # --- Post-hoc pairwise comparisons ---
  cat("--- Post-hoc pairwise comparisons ---\n")

  # Games-Howell: correct pairing for Welch ANOVA (no equal variance assumption)
  cat("  Games-Howell (var.equal = FALSE — matches Welch ANOVA):\n")
  sub$.grp <- factor(sub[[GROUP]])
  gh <- rstatix::games_howell_test(
    data       = sub,
    formula    = BMI ~ .grp,
    conf.level = 0.95,
    detailed   = FALSE
  )
  gh_print <- as.data.frame(gh[, c("group1", "group2", "estimate", "conf.low", "conf.high", "p.adj", "p.adj.signif")])
  gh_print$estimate  <- round(gh_print$estimate,  3)
  gh_print$conf.low  <- round(gh_print$conf.low,  3)
  gh_print$conf.high <- round(gh_print$conf.high, 3)
  gh_print$p.adj     <- round(gh_print$p.adj,     4)
  print(gh_print, row.names = FALSE)
  cat("\n")

  # Tukey HSD shown for reference only (assumes equal variances like aov)
  cat("  Tukey HSD (for reference only — assumes equal variance like aov):\n")
  tukey <- TukeyHSD(aov(BMI ~ factor(sub[[GROUP]]), data = sub))
  tukey_df <- as.data.frame(tukey[[1]])
  tukey_df <- round(tukey_df, 4)
  print(tukey_df)
  cat("\n")
}

# =============================================================================
# VISUALISATION: BMI distribution by group (scatter + box)
# =============================================================================
cat("\nGenerating plots — one per grouping variable...\n")

for (GROUP in group_candidates) {
  sub <- d[!is.na(d$BMI) & !is.na(d[[GROUP]]), ]
  sub[[GROUP]] <- factor(sub[[GROUP]])
  sub$.grp     <- sub[[GROUP]]

  gh_plot <- rstatix::games_howell_test(
    data       = sub,
    formula    = BMI ~ .grp,
    conf.level = 0.95
  )
  sig_pairs <- gh_plot[gh_plot$p.adj < 0.05, ]
  sig_label <- if (nrow(sig_pairs) == 0) {
    "Games-Howell: no significant pairs"
  } else {
    paste0("Games-Howell sig. pairs: ",
           paste(sprintf("%s vs %s (p=%s)", sig_pairs$group1, sig_pairs$group2,
                         sig_pairs$p.adj.signif), collapse = "; "))
  }

  p <- ggplot(sub, aes(x = .data[[GROUP]], y = BMI, colour = .data[[GROUP]])) +
    # box shows IQR + median
    geom_boxplot(outlier.shape = NA, width = 0.45, linewidth = 0.7,
                 colour = "grey40", fill = NA) +
    # individual points jittered
    geom_jitter(width = 0.15, size = 2, alpha = 0.65) +
    # group mean as a cross
    stat_summary(fun = mean, geom = "point", shape = 3,
                 size = 5, stroke = 1.5, colour = "black") +
    labs(
      title    = paste("BMI by", GROUP),
      subtitle = sprintf("Welch p = %.4f  |  aov() p = %.4f  |  Var ratio = %.2f\n%s",
                         oneway.test(as.formula(paste0("BMI ~ `", GROUP, "`")), data = sub, var.equal = FALSE)$p.value,
                         summary(aov(as.formula(paste0("BMI ~ `", GROUP, "`")), data = sub))[[1]][["Pr(>F)"]][1],
                         max(tapply(sub$BMI, sub[[GROUP]], var, na.rm = TRUE)) /
                           min(tapply(sub$BMI, sub[[GROUP]], var, na.rm = TRUE)),
                         sig_label),
      x = GROUP, y = "BMI",
      caption = "Box = IQR/median  |  + = group mean  |  Points = individual observations"
    ) +
    theme_bw(base_size = 13) +
    theme(legend.position = "none",
          plot.subtitle  = element_text(size = 10, colour = "grey30"),
          plot.caption   = element_text(size = 9,  colour = "grey50"))

  print(p)
}

cat("=============================================================\n")
cat("Interpretation guide:\n")
cat("  - Large variance ratio + Bartlett p<0.05 = heterogeneous variances\n")
cat("    -> Welch ANOVA is the CORRECT choice; aov() p-value is unreliable\n")
cat("  - Small variance ratio = homogeneous variances\n")
cat("    -> Both tests agree; large p-value shift is unexpected\n")
cat("  - Outliers inflating one group's variance can produce this pattern\n")
cat("    -> Inspect those rows and consider whether they are data errors\n")
cat("=============================================================\n\n")

# =============================================================================
# NORMALITY ROUTING COMPARISON
# How do ternG() results differ when normality is assessed vs forced?
#   consider_normality = TRUE   -> Shapiro-Wilk decides: non-normal -> Kruskal-Wallis
#   consider_normality = FALSE     -> ignore normality: always Welch ANOVA
# =============================================================================
devtools::load_all(".")

cat("=============================================================\n")
cat("NORMALITY ROUTING COMPARISON — ternG() on test data\n")
cat("Variable: BMI   |   Both grouping variables\n")
cat("=============================================================\n\n")

for (GROUP in group_candidates) {
  sub <- d[!is.na(d$BMI) & !is.na(d[[GROUP]]), ]
  sub[[GROUP]] <- factor(sub[[GROUP]])

  cat("-------------------------------------------------------------\n")
  cat("GROUP:", GROUP, "\n")
  cat("-------------------------------------------------------------\n")

  # Per-group Shapiro-Wilk summary for context
  cat("Shapiro-Wilk per group:\n")
  for (g in levels(sub[[GROUP]])) {
    vals <- sub$BMI[sub[[GROUP]] == g]
    sw   <- if (length(vals) >= 3) shapiro.test(vals) else list(p.value = NA)
    cat(sprintf("  %-35s p = %.4f  %s\n", g, sw$p.value,
                ifelse(!is.na(sw$p.value) & sw$p.value < 0.05, "NON-NORMAL -> Kruskal", "normal -> Welch ANOVA")))
  }
  cat("\n")

  tbl_assess <- ternG(
    data               = sub,
    group_var          = GROUP,
    vars               = "BMI",
    show_test          = TRUE,
    consider_normality = TRUE
  )
  cat("consider_normality = TRUE (default — Shapiro-Wilk routes the test):\n")
  print(tbl_assess[, c("Variable", "P", "test")])
  cat("\n")

  tbl_forced <- ternG(
    data               = sub,
    group_var          = GROUP,
    vars               = "BMI",
    show_test          = TRUE,
    consider_normality = FALSE
  )
  cat("consider_normality = FALSE (force parametric \u2014 always Welch ANOVA):\n")
  print(tbl_forced[, c("Variable", "P", "test")])

  p_assess <- tbl_assess$P[tbl_assess$Variable == "BMI"]
  p_forced <- tbl_forced$P[tbl_forced$Variable == "BMI"]
  t_assess <- tbl_assess$test[tbl_assess$Variable == "BMI"]
  t_forced <- tbl_forced$test[tbl_forced$Variable == "BMI"]

  cat(sprintf("\n  TRUE (ASSESS) : %-20s  p = %s\n", t_assess, p_assess))
  cat(sprintf("  FORCED : %-20s  p = %s\n", t_forced, p_forced))
  if (p_assess != p_forced) {
    cat("  *** p-values DIFFER \u2014 normality routing changed the result ***\n\n")
  } else {
    cat("  p-values identical \u2014 routing made no difference here\n\n")
  }
}
cat("=============================================================\n")
