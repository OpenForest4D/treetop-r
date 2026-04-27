# tests/unit/test_lorenz_curve.R
#
# Unit tests for plot_lorenz_curve()
# Source: plot_lorenz_curve_from_treeheights.R
#
# Run standalone:
#   testthat::test_file("tests/unit/test_lorenz_curve.R")

library(testthat)
library(withr)

# Source fixtures and helpers
source(file.path(here::here(), "tests", "fixtures", "make_fixtures.R"), local = TRUE)

# Source function under test
source(file.path(here::here(), "plot_lorenz_curve_from_treeheights.R"), local = TRUE)

# ---------------------------------------------------------------------------
# plot_lorenz_curve — file output
# ---------------------------------------------------------------------------

test_that("plot_lorenz_curve: writes a valid PNG file", {
  tmp <- withr::local_tempdir()
  out <- file.path(tmp, "lorenz.png")

  expect_no_error(
    plot_lorenz_curve(make_tree_heights(), output_file = out)
  )

  expect_true(file.exists(out))
  expect_gt(file.size(out), 100L)

  # Verify PNG magic bytes
  raw_hdr <- readBin(out, "raw", n = 4L)
  expect_equal(as.integer(raw_hdr), c(0x89L, 0x50L, 0x4EL, 0x47L))
})

test_that("plot_lorenz_curve: output path is respected", {
  tmp  <- withr::local_tempdir()
  out1 <- file.path(tmp, "curve_a.png")
  out2 <- file.path(tmp, "curve_b.png")

  plot_lorenz_curve(make_tree_heights(), output_file = out1)
  plot_lorenz_curve(make_tree_heights(), output_file = out2)

  expect_true(file.exists(out1))
  expect_true(file.exists(out2))
  # Both files should be independent
  expect_false(identical(out1, out2))
})

test_that("plot_lorenz_curve: custom width/height/res are accepted", {
  tmp <- withr::local_tempdir()
  out <- file.path(tmp, "lorenz_custom.png")

  expect_no_error(
    plot_lorenz_curve(make_tree_heights(), output_file = out,
                       width = 1200, height = 900, res = 150)
  )
  expect_true(file.exists(out))
})

# ---------------------------------------------------------------------------
# plot_lorenz_curve — input validation / edge cases
# ---------------------------------------------------------------------------

test_that("plot_lorenz_curve: works with minimum 2 distinct heights", {
  tmp <- withr::local_tempdir()
  out <- file.path(tmp, "lorenz_two.png")

  expect_no_error(
    plot_lorenz_curve(c(10, 50), output_file = out)
  )
  expect_true(file.exists(out))
})

test_that("plot_lorenz_curve: handles uniform heights without error", {
  tmp <- withr::local_tempdir()
  out <- file.path(tmp, "lorenz_uniform.png")

  # All same height → Gini = 0, L.mean = 1
  expect_no_error(
    plot_lorenz_curve(make_uniform_tree_heights(), output_file = out)
  )
  expect_true(file.exists(out))
})

test_that("plot_lorenz_curve: closes graphics device after writing", {
  tmp    <- withr::local_tempdir()
  out    <- file.path(tmp, "lorenz_dev.png")
  dev_before <- dev.cur()

  plot_lorenz_curve(make_tree_heights(), output_file = out)

  dev_after <- dev.cur()
  # The device open count should not have grown
  expect_equal(dev_before, dev_after)
})

test_that("plot_lorenz_curve: large height vector completes without error", {
  tmp <- withr::local_tempdir()
  out <- file.path(tmp, "lorenz_large.png")
  heights <- runif(500, min = 2, max = 60)

  expect_no_error(
    plot_lorenz_curve(heights, output_file = out)
  )
  expect_true(file.exists(out))
})
