# Wald odds ratio for a 2x2 table, row 2 against row 1 (reference), outcome in
# column 2. Replaces epitools::oddsratio(tab, method = "wald")$measure[2, ]
# (epitools 0.5-10.1, oddsratio.wald with its defaults): same estimate and the
# same normal-approximation CI on the log scale, with no zero-cell correction,
# so a zero cell gives 0, Inf or NaN exactly as epitools did.
# Returns c(estimate, lower, upper).
.wald_or <- function(tab, conf.level = 0.95) {
  tab <- as.matrix(tab)
  if (!identical(dim(tab), c(2L, 2L))) stop("`tab` must be a 2x2 table")
  b0 <- tab[1, 1]
  a0 <- tab[1, 2]
  b1 <- tab[2, 1]
  a1 <- tab[2, 2]
  est <- (b0 * a1) / (a0 * b1)
  se <- sqrt(1 / b0 + 1 / a0 + 1 / b1 + 1 / a1)
  z <- stats::qnorm(0.5 * (1 + conf.level))
  c(est, exp(log(est) + c(-1, 1) * z * se))
}
