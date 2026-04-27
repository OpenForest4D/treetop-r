# tests/integration/test_detect_to_json.R
#
# Integration test: full pipeline from synthetic CHM raster → trees.json
#
# Exercises the complete detect_trees.R workflow in a single test:
#   CHM raster → find_trees → coordinate transform → JSON export → validation
#
# Run standalone:
#   testthat::test_file("tests/integration/test_detect_to_json.R")

library(testthat)
library(withr)
library(raster)
library(lidR)
library(sf)
library(jsonlite)

source(file.path(here::here(), "tests", "fixtures", "make_fixtures.R"), local = TRUE)

# ---------------------------------------------------------------------------
# Full pipeline helper
# ---------------------------------------------------------------------------
run_detect_pipeline <- function(chm, output_json, window_size = 3.5, hmin = 2) {
  # Step 1 — detect trees
  trees <- lidR::find_trees(chm, lidR::lmf(ws = window_size, hmin = hmin))

  # Step 2 — resolve CRS
  chm_crs <- raster::crs(chm)
  if (is.na(chm_crs)) {
    chm_crs <- "+proj=utm +zone=11 +datum=WGS84 +units=m +no_defs"
  } else {
    chm_crs <- as.character(chm_crs)
  }

  coords  <- sp::coordinates(trees)
  heights <- trees$Z

  # Step 3 — convert to sf and transform to WGS84
  trees_sf <- sf::st_as_sf(
    data.frame(x = coords[, 1], y = coords[, 2], Z = heights),
    coords = c("x", "y"),
    crs    = chm_crs
  )
  trees_wgs84 <- sf::st_transform(trees_sf, crs = 4326)
  latlon      <- sf::st_coordinates(trees_wgs84)

  # Step 4 — build list and export JSON
  tree_list <- lapply(seq_len(nrow(trees)), function(i) {
    list(
      latitude  = round(latlon[i, 2], 8),
      longitude = round(latlon[i, 1], 8),
      height    = round(heights[i], 2),
      utm_x     = round(coords[i, 1], 2),
      utm_y     = round(coords[i, 2], 2)
    )
  })

  jsonlite::write_json(tree_list, output_json,
                       pretty = TRUE, auto_unbox = TRUE, digits = 10)

  invisible(tree_list)
}

# ---------------------------------------------------------------------------
# Integration tests
# ---------------------------------------------------------------------------

test_that("pipeline: JSON file is created and parseable", {
  tmp <- withr::local_tempdir()
  out <- file.path(tmp, "trees.json")
  chm <- make_synthetic_chm()

  run_detect_pipeline(chm, out)

  expect_true(file.exists(out))
  parsed <- tryCatch(jsonlite::read_json(out), error = function(e) NULL)
  expect_false(is.null(parsed), info = "JSON could not be parsed")
})

test_that("pipeline: at least one tree is detected and exported", {
  tmp <- withr::local_tempdir()
  out <- file.path(tmp, "trees.json")
  chm <- make_synthetic_chm()

  run_detect_pipeline(chm, out)
  parsed <- jsonlite::read_json(out)

  expect_gt(length(parsed), 0L)
})

test_that("pipeline: every exported tree has all five required fields", {
  tmp <- withr::local_tempdir()
  out <- file.path(tmp, "trees.json")
  chm <- make_synthetic_chm()

  run_detect_pipeline(chm, out)
  parsed <- jsonlite::read_json(out)

  required <- c("latitude", "longitude", "height", "utm_x", "utm_y")
  for (i in seq_along(parsed)) {
    tree <- parsed[[i]]
    missing_fields <- setdiff(required, names(tree))
    expect_equal(length(missing_fields), 0L,
                 info = paste("Tree", i, "missing:", paste(missing_fields, collapse = ", ")))
  }
})

test_that("pipeline: exported latitudes are in [-90, 90]", {
  tmp <- withr::local_tempdir()
  out <- file.path(tmp, "trees.json")
  run_detect_pipeline(make_synthetic_chm(), out)

  parsed <- jsonlite::read_json(out)
  lats   <- vapply(parsed, `[[`, numeric(1), "latitude")
  expect_true(all(lats >= -90 & lats <= 90))
})

test_that("pipeline: exported longitudes are in [-180, 180]", {
  tmp <- withr::local_tempdir()
  out <- file.path(tmp, "trees.json")
  run_detect_pipeline(make_synthetic_chm(), out)

  parsed <- jsonlite::read_json(out)
  lons   <- vapply(parsed, `[[`, numeric(1), "longitude")
  expect_true(all(lons >= -180 & lons <= 180))
})

test_that("pipeline: exported heights are positive", {
  tmp <- withr::local_tempdir()
  out <- file.path(tmp, "trees.json")
  run_detect_pipeline(make_synthetic_chm(), out)

  parsed   <- jsonlite::read_json(out)
  heights  <- vapply(parsed, `[[`, numeric(1), "height")
  expect_true(all(heights > 0))
})

test_that("pipeline: tallest exported tree matches synthetic CHM peak", {
  tmp <- withr::local_tempdir()
  out <- file.path(tmp, "trees.json")
  run_detect_pipeline(make_synthetic_chm(), out)

  parsed  <- jsonlite::read_json(out)
  heights <- vapply(parsed, `[[`, numeric(1), "height")
  # Peak in fixture is 25 m; allow 0.5 m tolerance for lmf interpolation
  expect_equal(max(heights), 25, tolerance = 0.5)
})

test_that("pipeline: UTM coordinates are within CHM extent", {
  chm <- make_synthetic_chm()
  ext <- raster::extent(chm)
  tmp <- withr::local_tempdir()
  out <- file.path(tmp, "trees.json")
  run_detect_pipeline(chm, out)

  parsed <- jsonlite::read_json(out)
  utm_xs <- vapply(parsed, `[[`, numeric(1), "utm_x")
  utm_ys <- vapply(parsed, `[[`, numeric(1), "utm_y")

  expect_true(all(utm_xs >= ext@xmin & utm_xs <= ext@xmax))
  expect_true(all(utm_ys >= ext@ymin & utm_ys <= ext@ymax))
})

test_that("pipeline: missing CRS falls back gracefully without error", {
  chm_no_crs <- make_synthetic_chm()
  raster::crs(chm_no_crs) <- NA

  tmp <- withr::local_tempdir()
  out <- file.path(tmp, "trees_nocrs.json")

  expect_no_error(
    suppressWarnings(run_detect_pipeline(chm_no_crs, out))
  )
  expect_true(file.exists(out))
})

test_that("pipeline: stricter hmin reduces number of exported trees", {
  chm  <- make_synthetic_chm()
  tmp  <- withr::local_tempdir()
  out1 <- file.path(tmp, "trees_low.json")
  out2 <- file.path(tmp, "trees_high.json")

  run_detect_pipeline(chm, out1, hmin = 2)
  run_detect_pipeline(chm, out2, hmin = 20)

  n1 <- length(jsonlite::read_json(out1))
  n2 <- length(jsonlite::read_json(out2))
  expect_gte(n1, n2)
})
