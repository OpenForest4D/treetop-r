# tests/unit/test_detect_trees.R
#
# Unit tests for the detect_trees pipeline.
#
# Because detect_trees.R is written as a top-level script (not a function),
# the tests here validate:
#   1. The behaviour of the individual pipeline steps that can be called in
#      isolation using the same packages (raster, lidR, sf, jsonlite).
#   2. The shape and contract of the JSON output produced by the pipeline.
#
# Run standalone:
#   testthat::test_file("tests/unit/test_detect_trees.R")

library(testthat)
library(withr)
library(raster)
library(lidR)
library(sf)
library(jsonlite)

source(file.path(here::here(), "tests", "fixtures", "make_fixtures.R"), local = TRUE)

# ---------------------------------------------------------------------------
# CHM loading / raster contract
# ---------------------------------------------------------------------------

test_that("make_synthetic_chm: returns a RasterLayer", {
  chm <- make_synthetic_chm()
  expect_s4_class(chm, "RasterLayer")
})

test_that("make_synthetic_chm: CRS is UTM Zone 11N", {
  chm   <- make_synthetic_chm()
  crs_s <- as.character(raster::crs(chm))
  expect_match(crs_s, "proj=utm", ignore.case = TRUE)
  expect_match(crs_s, "zone=11",  ignore.case = TRUE)
})

test_that("make_synthetic_chm: cell values are in plausible CHM range", {
  chm  <- make_synthetic_chm()
  vals <- raster::values(chm)
  expect_true(all(vals >= 0, na.rm = TRUE))
  expect_true(max(vals, na.rm = TRUE) <= 60)   # no tree > 60 m in fixture
})

test_that("make_synthetic_chm: has expected peak height of 25 m", {
  chm <- make_synthetic_chm()
  expect_equal(max(raster::values(chm), na.rm = TRUE), 25, tolerance = 0.01)
})

# ---------------------------------------------------------------------------
# Tree detection with lidR::find_trees / lidR::lmf
# ---------------------------------------------------------------------------

test_that("find_trees: detects at least one tree on synthetic CHM", {
  chm   <- make_synthetic_chm()
  trees <- lidR::find_trees(chm, lidR::lmf(ws = 3.5, hmin = 2))
  expect_gt(nrow(trees), 0L)
})

test_that("find_trees: detected trees have a Z (height) column", {
  chm   <- make_synthetic_chm()
  trees <- lidR::find_trees(chm, lidR::lmf(ws = 3.5, hmin = 2))
  expect_true("Z" %in% names(trees))
})

test_that("find_trees: all detected heights are >= hmin threshold", {
  hmin  <- 2
  chm   <- make_synthetic_chm()
  trees <- lidR::find_trees(chm, lidR::lmf(ws = 3.5, hmin = hmin))
  expect_true(all(trees$Z >= hmin))
})

test_that("find_trees: tallest detected tree matches synthetic peak", {
  chm   <- make_synthetic_chm()
  trees <- lidR::find_trees(chm, lidR::lmf(ws = 3.5, hmin = 2))
  expect_equal(max(trees$Z), 25, tolerance = 0.5)
})

test_that("find_trees: each detected tree has 2D coordinates", {
  chm    <- make_synthetic_chm()
  trees  <- lidR::find_trees(chm, lidR::lmf(ws = 3.5, hmin = 2))
  coords <- sp::coordinates(trees)
  expect_equal(ncol(coords), 2L)   # x and y
  expect_true(all(is.finite(coords)))
})

# ---------------------------------------------------------------------------
# CRS handling
# ---------------------------------------------------------------------------

test_that("CRS extraction: defined CRS does not produce NA", {
  chm     <- make_synthetic_chm()
  chm_crs <- raster::crs(chm)
  expect_false(is.na(chm_crs))
})

test_that("CRS extraction: result can be coerced to character", {
  chm     <- make_synthetic_chm()
  crs_str <- as.character(raster::crs(chm))
  expect_true(is.character(crs_str))
  expect_true(nchar(crs_str) > 0)
})

# ---------------------------------------------------------------------------
# Coordinate transformation (UTM → WGS84)
# ---------------------------------------------------------------------------

test_that("sf transform: UTM coords fall in expected lon/lat range for UTM 11N", {
  chm    <- make_synthetic_chm()
  trees  <- lidR::find_trees(chm, lidR::lmf(ws = 3.5, hmin = 2))
  coords <- sp::coordinates(trees)
  heights <- trees$Z

  trees_sf <- sf::st_as_sf(
    data.frame(x = coords[, 1], y = coords[, 2], Z = heights),
    coords = c("x", "y"),
    crs    = as.character(raster::crs(chm))
  )
  trees_wgs84 <- sf::st_transform(trees_sf, crs = 4326)
  latlon      <- sf::st_coordinates(trees_wgs84)

  # UTM Zone 11N covers roughly -120° to -114° longitude, 0–84° latitude
  expect_true(all(latlon[, 1] >= -180 & latlon[, 1] <= 180))  # valid longitude
  expect_true(all(latlon[, 2] >= -90  & latlon[, 2] <= 90))   # valid latitude
  expect_true(all(latlon[, 1] < -100))  # Zone 11N is in western North America
})

# ---------------------------------------------------------------------------
# JSON output contract
# ---------------------------------------------------------------------------

test_that("JSON output: required fields are present for each tree", {
  chm    <- make_synthetic_chm()
  trees  <- lidR::find_trees(chm, lidR::lmf(ws = 3.5, hmin = 2))
  coords <- sp::coordinates(trees)
  heights <- trees$Z

  trees_sf    <- sf::st_as_sf(
    data.frame(x = coords[, 1], y = coords[, 2], Z = heights),
    coords = c("x", "y"),
    crs    = as.character(raster::crs(chm))
  )
  trees_wgs84 <- sf::st_transform(trees_sf, crs = 4326)
  latlon      <- sf::st_coordinates(trees_wgs84)

  tree_list <- lapply(seq_len(nrow(trees)), function(i) {
    list(
      latitude  = round(latlon[i, 2], 8),
      longitude = round(latlon[i, 1], 8),
      height    = round(heights[i], 2),
      utm_x     = round(coords[i, 1], 2),
      utm_y     = round(coords[i, 2], 2)
    )
  })

  tmp <- withr::local_tempdir()
  out <- file.path(tmp, "trees.json")
  jsonlite::write_json(tree_list, out, pretty = TRUE, auto_unbox = TRUE, digits = 10)

  parsed <- jsonlite::read_json(out)
  expect_true(is.list(parsed))
  expect_gt(length(parsed), 0L)

  required_fields <- c("latitude", "longitude", "height", "utm_x", "utm_y")
  first_tree <- parsed[[1]]
  for (field in required_fields) {
    expect_true(field %in% names(first_tree),
                info = paste("Missing field:", field))
  }
})

test_that("JSON output: latitude and longitude are numerically valid", {
  chm    <- make_synthetic_chm()
  trees  <- lidR::find_trees(chm, lidR::lmf(ws = 3.5, hmin = 2))
  coords <- sp::coordinates(trees)
  heights <- trees$Z

  trees_sf    <- sf::st_as_sf(
    data.frame(x = coords[, 1], y = coords[, 2], Z = heights),
    coords = c("x", "y"),
    crs    = as.character(raster::crs(chm))
  )
  trees_wgs84 <- sf::st_transform(trees_sf, crs = 4326)
  latlon      <- sf::st_coordinates(trees_wgs84)

  expect_true(all(abs(latlon[, 1]) <= 180))
  expect_true(all(abs(latlon[, 2]) <= 90))
})

test_that("JSON output: heights are rounded to 2 decimal places", {
  heights_raw    <- c(12.3456789, 18.9876, 25.111)
  heights_rounded <- round(heights_raw, 2)
  expect_equal(heights_rounded, c(12.35, 18.99, 25.11))
})
