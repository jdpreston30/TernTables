# Tests for the internal Wald odds ratio that replaced epitools::oddsratio().
#
# epitools is scheduled for CRAN archival, so ternG() now computes the Wald OR
# itself. The reference values below were produced by
# epitools::oddsratio(tab, method = "wald")$measure[2, ] (epitools 0.5-10.1),
# so these tests pin the replacement to the numbers ternG() reported before.

test_that(".wald_or() reproduces epitools' Wald estimate and CI", {
  expect_equal(.wald_or(matrix(c(10, 20, 30, 40), 2)),
               c(0.666666666666667, 0.272514847543026, 1.630899925092241),
               tolerance = 1e-12)
  expect_equal(.wald_or(matrix(c(12, 7, 5, 9), 2)),
               c(3.085714285714286, 0.733529647137761, 12.980569620075636),
               tolerance = 1e-12)
})

test_that(".wald_or() applies no zero-cell correction, as epitools did", {
  or <- .wald_or(matrix(c(5, 3, 0, 4), 2))
  expect_equal(or[1], Inf)
})

test_that(".wald_or() rejects tables that are not 2x2", {
  expect_error(.wald_or(matrix(1:6, 2)), "2x2")
})
