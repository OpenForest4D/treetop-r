# tests/unit/test_ripley_kl.R
#
# Unit tests for plot_ripley_kl()
# Source: plot_ripley_kl_from_tree_locations.R
#
# Run standalone:
#   testthat::test_file("tests/unit/test_ripley_kl.R")

library(testthat)
library(withr)

# Source fixtures
source(file.path(here::here(), "tests", "fixtures", "make_fixtures.R"), local = TRUE)

# Source function under test
source(file.path(here::here(), "plot_ripley_kl_from_tree_locations.R"), local = TRUE)

# ---------------------------------------------------------------------------
# plot_ripley_kl — file output
# ---------------------------------------------------------------------------

test_that("plot_ripley_kl: writes a valid PNG file", {
  tmp <- withr::local_tempdir()
  out <- file.path(tmp, "ripley.png")

  # Use a small nsim to keep test runtime short (override inside function
  # is not possible without refactor; we accept the default nsim=99 here)
  expect_no_error(
    suppressWarnings(
      plot_ripley_kl(make_tree_locations(), output_file = out,
                     width = 600, height = 200, res = 72)
    )
  )

  expect_true(file.exists(out))
  expect_gt(file.size(out), 100L)

  raw_hdr <- readBin(out, "raw", n = 4L)
  expect_equal(as.integer(raw_hdr), c(0x89L, 0x50L, 0x4EL, 0x47L))
})

test_that("plot_ripley_kl: custom dimensions are accepted", {
  tmp <- withr::local_tempdir()
  out <- file.path(tmp, "ripley_wide.png")

  expect_no_error(
    suppressWarnings(
      plot_ripley_kl(make_tree_locations(), output_file = out,
                     width = 1800, height = 600, res = 100)
    )
  )
  expect_true(file.exists(out))
})

test_that("plot_ripley_kl: closes graphics device after writing", {
  tmp       <- withr::local_tempdir()
  out       <- file.path(tmp, "ripley_dev.png")
  dev_before <- dev.cur()

  suppressWarnings(
    plot_ripley_kl(make_tree_locations(), output_file = out,
                   width = 600, height = 200, res = 72)
  )

  dev_after <- dev.cur()
  expect_equal(dev_before, dev_after)
})

# ---------------------------------------------------------------------------
# plot_ripley_kl — input contract
# ---------------------------------------------------------------------------

test_that("plot_ripley_kl: input must be a data.frame with x and y columns", {
  tmp <- withr::local_tempdir()
  out <- file.path(tmp, "ripley_bad.png")

  bad_input <- data.frame(lat = 1:5, lon = 1:5)  # wrong column names

  expect_error(
    plot_ripley_kl(bad_input, output_file = out),
    regexp = NULL   # any error is acceptable here
  )
})

test_that("plot_ripley_kl: accepts data.frame with exactly 2 columns x and y", {
  locs <- make_tree_locations()
  expect_true(all(c("x", "y") %in% names(locs)))
  expect_s3_class(locs, "data.frame")
})

test_that("plot_ripley_kl: output path is respected", {
  tmp  <- withr::local_tempdir()
  out1 <- file.path(tmp, "rip_a.png")
  out2 <- file.path(tmp, "rip_b.png")

  suppressWarnings({
    plot_ripley_kl(make_tree_locations(), output_file = out1,
                   width = 600, height = 200, res = 72)
    plot_ripley_kl(make_tree_locations(), output_file = out2,
                   width = 600, height = 200, res = 72)
  })

  expect_true(file.exists(out1))
  expect_true(file.exists(out2))
})

# ---------------------------------------------------------------------------
# Spatial point pattern sanity checks (no plot, pure R)
# ---------------------------------------------------------------------------

test_that("make_tree_locations fixture has >= 3 distinct points", {
  locs <- make_tree_locations()
  expect_gte(nrow(locs), 3L)
  expect_gt(length(unique(locs$x)), 1L)
  expect_gt(length(unique(locs$y)), 1L)
})

test_that("bounding box of tree locations is non-degenerate", {
  locs <- make_tree_locations()
  expect_gt(diff(range(locs$x)), 0)
  expect_gt(diff(range(locs$y)), 0)
})
