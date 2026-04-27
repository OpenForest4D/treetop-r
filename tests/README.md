# treetop-r — Test Suite

Unit and integration tests for the [treetop-r](https://github.com/OpenForest4D/treetop-r) package,
written with the [`testthat`](https://testthat.r-lib.org/) framework.

---

## Directory Structure

```
tests/
├── README.md                          ← you are here
│
├── fixtures/                          ← shared synthetic test data
│   └── make_fixtures.R                ← script that creates all fixture objects
│
├── helpers/                           ← reusable test utilities
│   └── helpers.R                      ← helper functions sourced automatically by testthat
│
├── unit/                              ← one file per source module
│   ├── test_gini_coeff.R              ← GiniCoeff()
│   ├── test_lorenz_curve.R            ← plot_lorenz_curve()
│   ├── test_ripley_kl.R               ← plot_ripley_kl()
│   ├── test_detect_trees.R            ← detect_trees pipeline helpers
│   └── test_plot_chm_2d.R             ← plot_chm_2d output contract
│
├── integration/                       ← end-to-end workflow tests
│   ├── test_detect_to_json.R          ← full CHM → JSON pipeline
│   └── test_lorenz_ripley_pipeline.R  ← heights + locations → plots pipeline
│
└── run_tests.R                        ← convenience runner (sources all suites)
```

---

## Prerequisites

```r
install.packages(c(
  "testthat",   # test framework
  "raster",     # raster I/O
  "lidR",       # tree detection (find_trees / lmf)
  "sf",         # spatial transforms
  "jsonlite",   # JSON I/O
  "sp",         # SpatialPoints
  "spatstat",   # Ripley K/L
  "spatstat.geom",
  "spatstat.explore",
  "withr"       # temporary file/dir helpers
))
```

> All packages except `withr` are already required by the source scripts.
> `withr` provides `withr::local_tempdir()` so tests never write to the project root.

---

## Running the Tests

### Run everything

```r
source("tests/run_tests.R")
```

or from a terminal:

```bash
Rscript tests/run_tests.R
```

### Run a single suite

```r
testthat::test_file("tests/unit/test_gini_coeff.R")
```

### Run via `devtools` (if package structure is added later)

```r
devtools::test()
```

---

## Writing New Tests

1. Add a new file under `tests/unit/` or `tests/integration/` following the naming
   convention `test_<module>.R`.
2. Source shared helpers at the top of each file:
   ```r
   source(here::here("tests/helpers/helpers.R"))
   ```
3. Use `withr::local_tempdir()` for any test that writes files to disk —
   the temp directory is cleaned up automatically after each test block.
4. Keep each `test_that()` block focused on **one** behaviour.

---

## Test Data / Fixtures

Synthetic fixtures are created entirely in R (no external `.tif` files required).
`tests/fixtures/make_fixtures.R` builds:

| Object | Description |
|---|---|
| `make_synthetic_chm()` | Returns a small in-memory `RasterLayer` with a known CRS (UTM 11N) and artificial tree peaks |
| `make_tree_heights()` | Returns a numeric vector of tree heights suitable for Lorenz / Gini tests |
| `make_tree_locations()` | Returns a `data.frame(x, y)` of tree coordinates suitable for Ripley K/L tests |

---

## CI Integration

A minimal GitHub Actions workflow is shown below. Save it as
`.github/workflows/r-tests.yml` in the repository root.

```yaml
name: R Tests
on: [push, pull_request]
jobs:
  test:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      - uses: r-lib/actions/setup-r@v2
      - uses: r-lib/actions/setup-r-dependencies@v2
        with:
          packages: |
            testthat
            raster
            lidR
            sf
            jsonlite
            sp
            spatstat
            spatstat.geom
            spatstat.explore
            withr
      - name: Run tests
        run: Rscript tests/run_tests.R
```

---

## Conventions

- **Naming** — test files mirror source files: `detect_trees.R` → `test_detect_trees.R`.
- **No side-effects** — every test that writes a file uses `withr::local_tempdir()`.
- **No hard-coded paths** — source scripts are loaded with `source(here::here(...))`.
- **Isolation** — each `test_that()` block is self-contained; fixtures are recreated
  per block via helper functions, not shared mutable state.
- **Descriptive labels** — labels follow the pattern `"<function>: <behaviour being tested>"`.
