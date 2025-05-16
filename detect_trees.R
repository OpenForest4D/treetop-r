# Load required libraries
library(raster)
library(lidR)
library(sf)
library(jsonlite)

# Parameters
window_size <- 3.5  # Adjusted for 0.5m resolution
chm_path <- "/path/to/CHM.tif"
output_json <- "/path/to/lidar_output/trees.json"

# Load the CHM raster
chm <- raster::raster(chm_path)
chm <- raster::readAll(chm)

# Detect trees using local maximum filtering
trees <- lidR::find_trees(chm, lidR::lmf(ws = window_size, hmin = 2))
cat("Number of trees detected:", nrow(trees), "\n")

# Get the CRS of the CHM using raster::crs()
chm_crs <- raster::crs(chm)
if (is.na(chm_crs)) {
  warning("CRS not defined in CHM. Assuming UTM Zone 11N (EPSG:32611).")
  chm_crs <- "+proj=utm +zone=11 +datum=WGS84 +units=m +no_defs"
} else {
  chm_crs <- as.character(chm_crs)
}

# Convert trees to an sf object for transformation
coords <- coordinates(trees)
heights <- trees$Z
trees_sf <- st_as_sf(data.frame(x = coords[, 1], y = coords[, 2], Z = heights),
                     coords = c("x", "y"),
                     crs = chm_crs)

# Transform to WGS84 (EPSG:4326)
trees_wgs84 <- st_transform(trees_sf, crs = 4326)

# Extract lat/lon coordinates
latlon <- st_coordinates(trees_wgs84)

# Prepare data for JSON export
tree_list <- list()
for (i in 1:nrow(trees)) {
  tree_info <- list(
    latitude = round(latlon[i, 2], 8),  # Round to 6 decimal places
    longitude = round(latlon[i, 1], 8), # Round to 6 decimal places
    height = round(heights[i], 2),      # Round height to 2 decimal places (meters)
    utm_x = round(coords[i, 1], 2),     # Round UTM to 2 decimal places (meters)
    utm_y = round(coords[i, 2], 2)      # Round UTM to 2 decimal places (meters)
  )
  tree_list[[i]] <- tree_info

  # Print for verification
  cat("Tree", i, "- Lat:", tree_info$latitude, "Lon:", tree_info$longitude, 
      "Height:", tree_info$height, "UTM X:", tree_info$utm_x, "UTM Y:", tree_info$utm_y, "\n")
}

# Export to JSON with specified precision
write_json(tree_list, output_json, pretty = TRUE, auto_unbox = TRUE, digits = 10)
cat("Tree data exported to", output_json, "\n")