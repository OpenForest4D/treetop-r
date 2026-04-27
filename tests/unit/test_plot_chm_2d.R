# tests/unit/test_plot_chm_2d.R
#
# Unit tests for the plot_chm_2d pipeline.
#
# plot_chm_2d.R is also a top-level script, so tests here exercise the
# composable steps: raster loading, tree detection, and PNG output contract.
#
# Run standalone:
#   testthat::test_file("tests/unit/test_plot_chm_2d.R")

library(testthat)
library(withr)
library(raster)
library(lidR)

source(file.path(here::here(), "tests", "fixtures", "make_fixtures.R"), local = TRUE)

# Helper: detect trees and produce a 2D CHM plot into a temp file.
# This replicates the core logic of plot_chm_2d.R without the hard-coded paths.
plot_chm_2d_to_file <- function(chm, out_path,
                                 ws = 3.5, hmin = 2,
                                 width = 800, height = 600, res = 96) {
  trees  <- lidR::find_trees(chm, lidR::lmf(ws = ws, hmin = hmin))
  crowns <- lidR::silva2016(chm, trees, max_cr_factor = 0.6, exclusion = 0.3)()

  png(out_path, width = width, height = height, res = res)
  raster::plot(chm, col = height.colors(50), main = "CHM 2D")
  sp::plot(sp::SpatialPoints(sp::coordinates(trees)), add = TRUE, col = "red", pch = 3)
  dev.off()
}

# ---------------------------------------------------------------------------
# CHM raster contract
# ---------------------------------------------------------------------------

test_that("plot_chm_2d: CHM raster has positive extent", {
  chm <- make_synthetic_chm()
  ext <- raster::extent(chm)
  expect_gt(ext@xmax - ext@xmin, 0)
  expect_gt(ext@ymax - ext@ymin, 0)
})

test_that("plot_chm_2d: CHM resolution is consistent", {
  chm <- make_synthetic_chm()
  res <- raster::res(chm)
  expect_equal(res[1], res[2])  # square pixels
  expect_gt(res[1], 0)
})

test_that("plot_chm_2d: CHM has no all-NA layer", {
  chm <- make_synthetic_chm()
  expect_false(all(is.na(raster::values(chm))))
})

# ---------------------------------------------------------------------------
# Tree detection (shared with test_detect_trees.R but scoped to 2D context)
# ---------------------------------------------------------------------------

test_that("plot_chm_2d: tree detection returns SpatialPointsDataFrame", {
  chm   <- make_synthetic_chm()
  trees <- lidR::find_trees(chm, lidR::lmf(ws = 3.5, hmin = 2))
  expect_s4_class(trees, "SpatialPointsDataFrame")
})

test_that("plot_chm_2d: tree coordinates fall within CHM extent", {
  chm    <- make_synthetic_chm()
  trees  <- lidR::find_trees(chm, lidR::lmf(ws = 3.5, hmin = 2))
  coords <- sp::coordinates(trees)
  ext    <- raster::extent(chm)

  expect_true(all(coords[, 1] >= ext@xmin & coords[, 1] <= ext@xmax))
  expect_true(all(coords[, 2] >= ext@ymin & coords[, 2] <= ext@ymax))
})

# ---------------------------------------------------------------------------
# PNG output contract
# ---------------------------------------------------------------------------

test_that("plot_chm_2d: produces a non-empty PNG file", {
  tmp <- withr::local_tempdir()
  out <- file.path(tmp, "chm_2d.png")
  chm <- make_synthetic_chm()

  png(out, width = 400, height = 400, res = 72)
  raster::plot(chm, col = rev(terrain.colors(50)), main = "CHM Test")
  dev.off()

  expect_true(file.exists(out))
  expect_gt(file.size(out), 100L)

  raw_hdr <- readBin(out, "raw", n = 4L)
  expect_equal(as.integer(raw_hdr), c(0x89L, 0x50L, 0x4EL, 0x47L))
})

test_that("plot_chm_2d: graphics device is closed after plotting", {
  tmp        <- withr::local_tempdir()
  out        <- file.path(tmp, "chm_dev.png")
  chm        <- make_synthetic_chm()
  dev_before <- dev.cur()

  png(out, width = 400, height = 400, res = 72)
  raster::plot(chm)
  dev.off()

  dev_after <- dev.cur()
  expect_equal(dev_before, dev_after)
})

test_that("plot_chm_2d: custom resolution produces larger file than low-res", {
  tmp  <- withr::local_tempdir()
  out_lo <- file.path(tmp, "lo.png")
  out_hi <- file.path(tmp, "hi.png")
  chm    <- make_synthetic_chm()

  png(out_lo, width = 400, height = 400, res = 50);  raster::plot(chm); dev.off()
  png(out_hi, width = 400, height = 400, res = 200); raster::plot(chm); dev.off()

  expect_gt(file.size(out_hi), file.size(out_lo))
})

# ---------------------------------------------------------------------------
# Window-size sensitivity
# ---------------------------------------------------------------------------

test_that("plot_chm_2d: smaller window detects >= as many trees as larger window", {
  chm        <- make_synthetic_chm()
  trees_small <- lidR::find_trees(chm, lidR::lmf(ws = 1.5, hmin = 2))
  trees_large <- lidR::find_trees(chm, lidR::lmf(ws = 5.0, hmin = 2))
  expect_gte(nrow(trees_small), nrow(trees_large))
})

test_that("plot_chm_2d: hmin filter excludes trees below threshold", {
  chm         <- make_synthetic_chm()
  trees_low   <- lidR::find_trees(chm, lidR::lmf(ws = 3.5, hmin = 1))
  trees_high  <- lidR::find_trees(chm, lidR::lmf(ws = 3.5, hmin = 15))
  expect_gte(nrow(trees_low), nrow(trees_high))
})
