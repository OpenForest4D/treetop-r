# tests/unit/test_gini_coeff.R
#
# Unit tests for GiniCoeff()
# Source: plot_lorenz_curve_from_treeheights.R
#
# Run standalone:
#   testthat::test_file("tests/unit/test_gini_coeff.R")

library(testthat)

# Source the function under test (adjust path if running from repo root)
source(file.path(here::here(), "plot_lorenz_curve_from_treeheights.R"), local = TRUE)

# ---------------------------------------------------------------------------
# GiniCoeff — correctness
# ---------------------------------------------------------------------------

test_that("GiniCoeff: perfect equality returns 0", {
  # All values identical → zero inequality
  heights <- rep(20, 10)
  gc <- GiniCoeff(heights, finite.sample = TRUE, na.rm = TRUE)
  expect_equal(gc, 0, tolerance = 1e-10)
})

test_that("GiniCoeff: maximum inequality approaches 1", {
  # One very tall tree, many tiny ones → Gini near 1
  heights <- c(rep(1, 99), 1000)
  gc <- GiniCoeff(heights, finite.sample = FALSE, na.rm = TRUE)
  expect_gt(gc, 0.9)
})

test_that("GiniCoeff: result is between 0 and 1 for positive heights", {
  heights <- c(5, 10, 15, 20, 25, 30, 35, 40, 45, 50)
  gc <- GiniCoeff(heights)
  expect_gte(gc, 0)
  expect_lte(gc, 1)
})

test_that("GiniCoeff: finite.sample correction reduces coefficient", {
  heights <- c(10, 20, 30, 40, 50)
  gc_finite   <- GiniCoeff(heights, finite.sample = TRUE)
  gc_infinite <- GiniCoeff(heights, finite.sample = FALSE)
  # Finite-sample correction divides by (n-1) instead of n → smaller value
  expect_lt(gc_finite, gc_infinite)
})

test_that("GiniCoeff: single unique value returns 0", {
  expect_equal(GiniCoeff(c(15)), 0, tolerance = 1e-9)
})

test_that("GiniCoeff: NA values are removed when na.rm = TRUE", {
  heights_clean <- c(10, 20, 30, 40)
  heights_na    <- c(10, NA, 20, 30, NA, 40)
  gc_clean <- GiniCoeff(heights_clean, na.rm = TRUE)
  gc_na    <- GiniCoeff(heights_na,    na.rm = TRUE)
  expect_equal(gc_clean, gc_na, tolerance = 1e-9)
})

test_that("GiniCoeff: returns NA when na.rm = FALSE and NAs present", {
  heights <- c(10, NA, 30)
  result <- GiniCoeff(heights, na.rm = FALSE)
  expect_true(is.na(result))
})

test_that("GiniCoeff: two-element vector produces expected value", {
  # For [1, 3], sorted: [1, 3], G = 2*(1*1 + 3*2)/4 - 3 = 2*7/4 - 3 = 3.5 - 3 = 0.5
  # finite.sample: 0.5 / (2-1) = 0.5
  heights <- c(1, 3)
  gc <- GiniCoeff(heights, finite.sample = TRUE)
  expect_equal(gc, 0.5, tolerance = 1e-9)
})

test_that("GiniCoeff: output is numeric scalar", {
  gc <- GiniCoeff(c(10, 20, 30))
  expect_true(is.numeric(gc))
  expect_length(gc, 1L)
})

test_that("GiniCoeff: monotone increasing inputs give non-zero coefficient", {
  heights <- seq(1, 100, by = 5)
  gc <- GiniCoeff(heights)
  expect_gt(gc, 0)
})
