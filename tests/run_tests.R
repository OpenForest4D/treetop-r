#!/usr/bin/env Rscript
# tests/run_tests.R
#
# Convenience runner — executes the entire test suite and prints a summary.
#
# Usage:
#   Rscript tests/run_tests.R            # from repo root
#   source("tests/run_tests.R")          # from an R session

library(testthat)

# Resolve repo root regardless of working directory
repo_root <- tryCatch(
  here::here(),
  error = function(e) normalizePath(file.path(dirname(sys.frame(1)$ofile), ".."))
)

tests_dir <- file.path(repo_root, "tests")

cat("==========================================================\n")
cat(" treetop-r test suite\n")
cat(" Repo root :", repo_root, "\n")
cat(" Tests dir :", tests_dir, "\n")
cat("==========================================================\n\n")

# Run unit tests
cat("--- Unit tests ---\n")
unit_results <- testthat::test_dir(
  file.path(tests_dir, "unit"),
  reporter = "progress",
  stop_on_failure = FALSE
)

cat("\n--- Integration tests ---\n")
integ_results <- testthat::test_dir(
  file.path(tests_dir, "integration"),
  reporter = "progress",
  stop_on_failure = FALSE
)

# Combined summary
all_results <- c(as.list(unit_results), as.list(integ_results))

cat("\n==========================================================\n")
cat(" SUMMARY\n")
cat("==========================================================\n")
testthat::summary(unit_results)
testthat::summary(integ_results)

# Exit with non-zero status if any test failed (useful for CI)
n_failed <- sum(vapply(all_results, function(r) {
  if (is.list(r) && "failed" %in% names(r)) r$failed else 0L
}, integer(1)))

if (n_failed > 0L) {
  message(n_failed, " test(s) FAILED.")
  quit(status = 1L)
} else {
  message("All tests passed.")
  quit(status = 0L)
}
