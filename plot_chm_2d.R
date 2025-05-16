# The function will save the plot as a PNG file at the specified location (e.g., output/chm_plot.png).
# The plot will include:
#   A color gradient representing tree heights.
#   Red dots marking the locations of detected trees.
#   Blue polygons representing the delineated tree crowns.


plot_chm_2d <- function(chm_path,  # Path to the CHM TIFF file
                        output_file = "chm_plot.png",
                        color_palette = "BlGrRed",
                        window_size = 5,  # The window size for the local maximum filter used in tree detection.
                        height_threshold = 1.37  # The minimum height threshold for tree detection.
) {
  # Load required libraries
  library(raster)
  library(lidR)
  library(sp)
  library(RColorBrewer)

  # Define color ramp based on the selected palette
  myColorRamp <- function(colors, values) {
    v <- (values - min(values)) / diff(range(values))
    x <- colorRamp(colors)(v)
    rgb(x[, 1], x[, 2], x[, 3], maxColorValue = 255)
  }

  if (color_palette == "BlGrRed") {
    col.rev <- myColorRamp(c("blue", "green", "yellow", "red"), 0:255)
  } else if (color_palette == "Viridis") {
    col.rev <- myColorRamp(c("#440154FF", "#482878FF", "#3E4A89FF", "#31688EFF", "#26828EFF", "#1F9E89FF",
                             "#35B779FF", "#6DCD59FF", "#B4DE2CFF", "#FDE725FF"), 0:255)
  } else {
    col <- RColorBrewer::brewer.pal(9, color_palette)
    col.rev <- rev(col)
  }

  # Load the CHM raster from the provided path
  chm <- raster::raster(chm_path)
  chm <- raster::readAll(chm)  # Load the raster into memory

  # Detect trees using local maximum filtering
  trees <- lidR::find_trees(chm, lidR::lmf(ws = window_size))
  print(paste("Number of trees detected:", nrow(trees)))

  # Check if trees were detected
  if (is.null(trees) || nrow(trees) == 0) {
    stop("No trees detected. Adjust the window_size or height_threshold.")
  }

  # Extract coordinates and heights from the trees object
  tree_coords <- sp::coordinates(trees)  # Extract coordinates
  tree_heights <- trees$Height  # Extract heights

  # Filter trees based on height threshold
  valid_trees <- tree_heights >= height_threshold
  tree_coords <- tree_coords[valid_trees, ]
  tree_heights <- tree_heights[valid_trees]
  print(paste("Number of trees after filtering:", sum(valid_trees)))

  # If no trees meet the height threshold, proceed with all detected trees
  if (sum(valid_trees) == 0) {
    warning("No trees meet the height threshold. Proceeding with all detected trees.")
    tree_coords <- sp::coordinates(trees)
    tree_heights <- trees$Height
  }

  # Convert tree coordinates to a data frame
  tree_locations <- data.frame(x = tree_coords[, 1], y = tree_coords[, 2])
  print(head(tree_locations))  # Inspect the first few rows of tree_locations

  # Check if tree_locations is valid
  if (nrow(tree_locations) == 0) {
    stop("No valid tree locations found. Check the tree detection results.")
  }

  # Delineate crowns using Silva's algorithm
  crowns <- lidR::silva2016(chm, trees, max_cr_factor = 0.5, exclusion = 0.1)()
  crown_polygons <- crowns  # Convert to SpatialPolygonsDataFrame if needed

  # Save the plot to a file
  png(output_file, width = 800, height = 600, res = 100)  # Open PNG device
  raster::plot(chm, col = col.rev, axes = TRUE, xlab = "UTM Easting", ylab = "UTM Northing")
  points(tree_locations, pch = 16, cex = 1.5, col = "red")  # Plot tree locations
  raster::plot(crown_polygons, add = TRUE, border = "blue", lwd = 2)  # Plot crown boundaries
  dev.off()  # Close PNG device
}

