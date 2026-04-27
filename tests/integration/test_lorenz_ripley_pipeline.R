# tests/integration/test_lorenz_ripley_pipeline.R
#
# Integration test: tree heights + locations → Lorenz curve + Ripley K/L plots
#
# Exercises the combined analytical pipeline:
#   - GiniCoeff correctness relative to Lorenz curve output
#   - plot_lorenz_curve PNG output
#   - plot_ripley_kl PNG output
#   - Both plots in a single batch workflow
#
# Run standalone:
#   testthat::test_file("tests/integration/test_lorenz_ripley_pipeline.R")

library(testthat)
library(withr)
library(raster)
library(lidR)

source(file.path(here::here(), "tests", "fixtures", "make_fixtures.R"), local = TRUE)
source(file.path(here::here(), "plot_lorenz_curve_from_treeheights.R"),   local = TRUE)
source(file.path(here::here(), "plot_ripley_kl_from_tree_locations.R"),   local = TRUE)

# Helper: PNG magic bytes check
is_valid_png <- function(path) {
  if (!file.exists(path)) return(FALSE)
  raw_hdr <- tryCatch(readBin(path, "raw", n = 4L), error = function(e) raw(0))
  identical(as.integer(raw_hdr), c(0x89L, 0x50L, 0x4EL, 0x47L))
}

# ---------------------------------------------------------------------------
# Lorenz + Gini coherence
# ---------------------------------------------------------------------------

test_that("pipeline: Gini coefficient computed inline matches GiniCoeff function", {
  heights <- make_tree_heights()
  gc_fn   <- GiniCoeff(heights)

  # Replicate the inline computation inside plot_lorenz_curve
  size   <- sort(heights, decreasing = TRUE)
  n      <- length(size)
  G      <- 2 * sum(size * seq_len(n)) / sum(size) - (n + 1L)
  gc_raw <- G / (n - 1L)  # finite.sample = TRUE

  expect_equal(gc_fn, gc_raw, tolerance = 1e-10)
})

test_that("pipeline: L.mean is between 0 and 1 for any positive heights", {
  heights <- make_tree_heights()
  size    <- sort(heights, decreasing = TRUE)
  L.mean  <- max(cumsum(size[size >= mean(size)]) / sum(size))

  expect_gte(L.mean, 0)
  expect_lte(L.mean, 1)
})

test_that("pipeline: uniform heights produce Gini = 0 and L.mean = 1", {
  heights <- make_uniform_tree_heights()
  gc      <- GiniCoeff(heights)
  size    <- sort(heights, decreasing = TRUE)
  L.mean  <- max(cumsum(size[size >= mean(size)]) / sum(size))

  expect_equal(gc,     0, tolerance = 1e-9)
  expect_equal(L.mean, 1, tolerance = 1e-9)
})

# ---------------------------------------------------------------------------
# Lorenz PNG output
# ---------------------------------------------------------------------------

test_that("pipeline: Lorenz PNG is written and valid", {
  tmp <- withr::local_tempdir()
  out <- file.path(tmp, "lorenz_integ.png")

  plot_lorenz_curve(make_tree_heights(), output_file = out)

  expect_true(is_valid_png(out), info = "Lorenz PNG is not a valid PNG file")
})

# ---------------------------------------------------------------------------
# Ripley K/L PNG output
# ---------------------------------------------------------------------------

test_that("pipeline: Ripley K/L PNG is written and valid", {
  tmp <- withr::local_tempdir()
  out <- file.path(tmp, "ripley_integ.png")

  suppressWarnings(
    plot_ripley_kl(make_tree_locations(), output_file = out,
                   width = 600, height = 200, res = 72)
  )

  expect_true(is_valid_png(out), info = "Ripley PNG is not a valid PNG file")
})

# ---------------------------------------------------------------------------
# Batch workflow: both plots generated from the same CHM run
# ---------------------------------------------------------------------------

test_that("pipeline: batch run produces Lorenz and Ripley PNGs without error", {
  tmp <- withr::local_tempdir()

  # Simulate detecting trees from CHM and extracting heights / locations
  chm    <- make_synthetic_chm()
  trees  <- lidR::find_trees(chm, lidR::lmf(ws = 3.5, hmin = 2))
  coords <- sp::coordinates(trees)
  heights <- trees$Z
  locs    <- data.frame(x = coords[, 1], y = coords[, 2])

  lorenz_out <- file.path(tmp, "batch_lorenz.png")
  ripley_out <- file.path(tmp, "batch_ripley.png")

  expect_no_error(
    plot_lorenz_curve(heights, output_file = lorenz_out)
  )
  expect_no_error(
    suppressWarnings(
      plot_ripley_kl(locs, output_file = ripley_out,
                     width = 600, height = 200, res = 72)
    )
  )

  expect_true(is_valid_png(lorenz_out), info = "Batch Lorenz PNG invalid")
  expect_true(is_valid_png(ripley_out), info = "Batch Ripley PNG invalid")
})

test_that("pipeline: no graphics devices left open after batch run", {
  tmp        <- withr::local_tempdir()
  dev_before <- dev.cur()

  plot_lorenz_curve(make_tree_heights(),
                    output_file = file.path(tmp, "l.png"))
  suppressWarnings(
    plot_ripley_kl(make_tree_locations(),
                   output_file = file.path(tmp, "r.png"),
                   width = 600, height = 200, res = 72)
  )

  dev_after <- dev.cur()
  expect_equal(dev_before, dev_after)
})

# ---------------------------------------------------------------------------
# Output directory isolation
# ---------------------------------------------------------------------------

test_that("pipeline: repeated runs to different directories do not overwrite each other", {
  tmpA <- withr::local_tempdir()
  tmpB <- withr::local_tempdir()

  outA <- file.path(tmpA, "lorenz.png")
  outB <- file.path(tmpB, "lorenz.png")

  plot_lorenz_curve(make_tree_heights(), output_file = outA)
  plot_lorenz_curve(c(1, 50, 100),      output_file = outB)

  expect_true(file.exists(outA))
  expect_true(file.exists(outB))
  # Files should differ because inputs differ
  expect_false(identical(readBin(outA, "raw", n = file.size(outA)),
                         readBin(outB, "raw", n = file.size(outB))))
})
