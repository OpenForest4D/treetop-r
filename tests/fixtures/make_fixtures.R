# tests/fixtures/make_fixtures.R
#
# Factory functions that create deterministic, in-memory test data.
# No external files are required.  Source this file (or rely on the
# auto-source in helpers.R) before calling any helper.

library(raster)
library(sp)

# ---------------------------------------------------------------------------
# make_synthetic_chm()
# ---------------------------------------------------------------------------
# Returns a small RasterLayer (20 x 20 cells, 0.5 m resolution) with three
# artificial tree peaks and a realistic UTM 11N CRS.
# Peak heights: 25 m, 18 m, 12 m at known cell positions.
# ---------------------------------------------------------------------------
make_synthetic_chm <- function(nrow = 20, ncol = 20, res = 0.5,
                                xmin = 500000, ymin = 4000000) {
  r <- raster::raster(
    nrows  = nrow,
    ncols  = ncol,
    xmn    = xmin,
    xmx    = xmin + ncol * res,
    ymn    = ymin,
    ymx    = ymin + nrow * res,
    crs    = "+proj=utm +zone=11 +datum=WGS84 +units=m +no_defs"
  )

  # Fill with low background canopy (2–4 m)
  set.seed(42)
  vals <- runif(raster::ncell(r), min = 2, max = 4)

  # Inject three distinct peaks well apart from each other
  peak_cells <- c(
    raster::cellFromRowCol(r, 4,  4),   # peak 1 → 25 m
    raster::cellFromRowCol(r, 10, 15),  # peak 2 → 18 m
    raster::cellFromRowCol(r, 17, 8)    # peak 3 → 12 m
  )
  vals[peak_cells] <- c(25, 18, 12)

  raster::values(r) <- vals
  r
}

# ---------------------------------------------------------------------------
# make_tree_heights()
# ---------------------------------------------------------------------------
# Returns a numeric vector of tree heights that spans a wide range so that
# Lorenz / Gini tests have meaningful expected values.
# ---------------------------------------------------------------------------
make_tree_heights <- function() {
  c(5, 8, 10, 12, 15, 18, 20, 22, 25, 30, 35, 40, 45, 50)
}

# ---------------------------------------------------------------------------
# make_uniform_tree_heights()
# ---------------------------------------------------------------------------
# All trees the same height → Gini coefficient should be 0.
# ---------------------------------------------------------------------------
make_uniform_tree_heights <- function(n = 10, height = 20) {
  rep(height, n)
}

# ---------------------------------------------------------------------------
# make_tree_locations()
# ---------------------------------------------------------------------------
# Returns a data.frame(x, y) of synthetic UTM coordinates suitable for
# Ripley K/L analysis (spatstat requires > 1 unique point).
# ---------------------------------------------------------------------------
make_tree_locations <- function() {
  set.seed(7)
  data.frame(
    x = c(500001, 500003, 500005, 500002, 500007,
           500009, 500004, 500006, 500008, 500010),
    y = c(4000001, 4000003, 4000002, 4000005, 4000007,
           4000006, 4000009, 4000008, 4000010, 4000004)
  )
}

# ---------------------------------------------------------------------------
# make_single_point_locations()
# ---------------------------------------------------------------------------
# Only one point — useful for testing that functions error / warn gracefully.
# ---------------------------------------------------------------------------
make_single_point_locations <- function() {
  data.frame(x = 500000, y = 4000000)
}
