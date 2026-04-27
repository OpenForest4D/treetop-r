# tests/helpers/helpers.R
#
# Reusable utilities sourced automatically by testthat (anything in
# tests/helpers/ matching helper*.R is auto-sourced when running the full
# suite via testthat::test_dir()).  They are also sourced explicitly in each
# individual test file for standalone runs.

# Source fixture factories
source(file.path(dirname(dirname(sys.frame(1)$ofile %||% ".")), "fixtures", "make_fixtures.R"),
       local = TRUE)

# Silence noisy package startup messages during tests
suppressPackageStartupMessages({
  library(testthat)
})

# ---------------------------------------------------------------------------
# expect_png_written(path)
# ---------------------------------------------------------------------------
# Custom expectation: asserts that a PNG file exists, is non-empty, and
# starts with the PNG magic bytes.
# ---------------------------------------------------------------------------
expect_png_written <- function(path) {
  expect_true(file.exists(path),
              info = paste("Expected PNG at:", path))
  expect_gt(file.size(path), 100L,
             info = "PNG file appears to be empty")
  # Check PNG magic bytes: 0x89 0x50 0x4E 0x47
  raw_header <- readBin(path, what = "raw", n = 4L)
  expect_equal(as.integer(raw_header),
               c(0x89L, 0x50L, 0x4EL, 0x47L),
               info = "File does not start with PNG magic bytes")
}

# ---------------------------------------------------------------------------
# expect_json_written(path)
# ---------------------------------------------------------------------------
# Custom expectation: asserts that a JSON file exists and parses cleanly.
# ---------------------------------------------------------------------------
expect_json_written <- function(path) {
  expect_true(file.exists(path),
              info = paste("Expected JSON at:", path))
  parsed <- tryCatch(jsonlite::read_json(path), error = function(e) NULL)
  expect_false(is.null(parsed),
               info = "JSON file could not be parsed")
  parsed
}

# ---------------------------------------------------------------------------
# within_range(x, lo, hi)
# ---------------------------------------------------------------------------
# Convenience for numeric range assertions.
# ---------------------------------------------------------------------------
within_range <- function(x, lo, hi) {
  isTRUE(x >= lo && x <= hi)
}

# Null-coalescing operator (backport for R < 4.4)
`%||%` <- function(a, b) if (!is.null(a)) a else b
