
# Define the plot_ripley_kl function
plot_ripley_kl <- function(chm_path, output_file = "/output/ripley_kl_plot.png", width = 1200, height = 400, res = 100) {

  # Load required libraries
  library(raster)       # For handling raster data (CHM)
  library(sp)           # For spatial data handling
  library(spatstat)     # For Ripley's K and L functions
  library(spatstat.geom) # For point pattern analysis
  library(spatstat.explore) # For envelope and Kest/Lest functions
  
  # Load the Canopy Height Model (CHM)
  chm <- raster::raster(chm_path)

  # Extract tree locations from the CHM (e.g., by thresholding)
  # Here, we assume trees are represented by pixels with values above a threshold
  threshold <- 2  # Adjust this threshold based on your CHM
  tree_coords <- raster::xyFromCell(chm, which(chm[] > threshold))

  # Convert tree coordinates to a data frame
  tree_locations <- data.frame(x = tree_coords[, 1], y = tree_coords[, 2])

  # Convert tree_locations to a SpatialPoints object
  coordinates(tree_locations) <- ~x + y  # Convert data frame to SpatialPoints
  S <- tree_locations

  # Get bounding box
  bb <- sp::bbox(S)  # Now S is a SpatialPoints object

  # Define observation window
  W <- spatstat.geom::owin(bb[1, ], bb[2, ])

  # Create point pattern object
  P <- spatstat.geom::ppp(S$x, S$y, window = W)

  # Compute Ripley's K and L functions with envelopes
  K <- spatstat.explore::envelope(P, spatstat.explore::Kest, nsim = 99, verbose = FALSE)
  L <- spatstat.explore::envelope(P, spatstat.explore::Lest, nsim = 99, verbose = FALSE)

  # Open a PNG device with Cairo backend
  png(output_file, width = width, height = height, res = res, type = "cairo")

  # Check if the PNG device was opened successfully
  if (dev.cur() == 1) {
    stop("Failed to open PNG device. Check the output directory and permissions.")
  }

  # Set up the plotting layout
  par(mfrow = c(1, 3), mar = c(8, 5, 4, 3))

  # Plot Ripley's K function
  plot(K, lwd = 2, main = "a) K", xlab = "r (m)", cex.lab = 1.5)

  # Plot Ripley's L function
  plot(L, lwd = 2, main = "b) L", xlab = "r (m)", cex.lab = 1.5)

  # Plot transformed L function (L(r) - r)
  plot(L, . - r ~ r, ylab = expression(hat("L")), xlab = "r (m)", main = "c) L", lwd = 2, cex.lab = 1.5)

  # Close the PNG device
  dev.off()
}
