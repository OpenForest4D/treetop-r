#' Generate interactive 3D CHM visualization for web display
#'
#' @param chm_path Path to Canopy Height Model (CHM) raster file
#' @param output_dir Output directory for saving HTML file
#' @param output_filename Name for output HTML file (default: "chm_3d_viewer.html")
#' @param color_palette Color palette selection ("BlGrRed" or "Viridis")
#' @param window_size Local maximum filter window size for tree detection
#' @param height_threshold Minimum tree height threshold (meters)
#' @param crown_detection Whether to detect and display tree crowns (TRUE/FALSE)
#' @param point_size Size of tree markers in 3D view
#' @param ... Additional parameters passed to `rgl::rglwidget()`

plot_chm_3d_web <- function(chm_path, 
                           output_dir = "output",
                           output_filename = "chm_3d_viewer.html",
                           color_palette = "BlGrRed",
                           window_size = 5,
                           height_threshold = 1.37,
                           crown_detection = TRUE,
                           point_size = 2,
                           ...) {

  options(rgl.useNULL = TRUE, rgl.printRglwidget = TRUE)
  Sys.setenv(DISPLAY = "")  # For headless environments
  suppressWarnings(library(rgl))
    
  # Load required packages
  if (!requireNamespace("raster", quietly = TRUE)) install.packages("raster")
  if (!requireNamespace("rgl", quietly = TRUE)) install.packages("rgl")
  if (!requireNamespace("lidR", quietly = TRUE)) install.packages("lidR")
  if (!requireNamespace("htmlwidgets", quietly = TRUE)) install.packages("htmlwidgets")
  
  library(raster)
  library(rgl)
  library(lidR)
  library(htmlwidgets)
  
  # Define local maxima function
  find_local_maxima <- function(matrix, window_size) {
    rows <- nrow(matrix)
    cols <- ncol(matrix)
    result <- matrix(FALSE, rows, cols)
    
    half_window <- floor(window_size / 2)
    
    for(i in 1:rows) {
      for(j in 1:cols) {
        if(is.na(matrix[i,j])) next
        
        # Define window boundaries
        row_start <- max(1, i - half_window)
        row_end <- min(rows, i + half_window)
        col_start <- max(1, j - half_window)
        col_end <- min(cols, j + half_window)
        
        # Get window values
        window <- matrix[row_start:row_end, col_start:col_end]
        
        # Check if center is maximum
        if(!any(window > matrix[i,j], na.rm = TRUE)) {
          result[i,j] <- TRUE
        }
      }
    }
    return(result)
  }
  
  # Set RGL to use null device for headless environments
  options(rgl.useNULL = TRUE)
  
  # Create output directory if needed
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  # Load and process CHM
  chm <- raster::raster(chm_path)
  chm <- raster::readAll(chm)
  
  # Create color palette
  myColorRamp <- function(colors, values) {
    v <- (values - min(values)) / diff(range(values))
    x <- colorRamp(colors)(v)
    rgb(x[,1], x[,2], x[,3], maxColorValue = 255)
  }
  
  if (color_palette == "BlGrRed") {
    myPal <- myColorRamp(c("blue", "green", "yellow", "red"), 0:255)
  } else if (color_palette == "Viridis") {
    myPal <- myColorRamp(c("#440154FF", "#482878FF", "#3E4A89FF", "#31688EFF", 
                          "#26828EFF", "#1F9E89FF", "#35B779FF", "#6DCD59FF", 
                          "#B4DE2CFF", "#FDE725FF"), 0:255)
  }
  
  # Create scene
  rgl::open3d(useNULL = TRUE)
  par3d(windowRect = c(0, 0, 800, 600))
  
  # Get raster data and coordinates
  z <- t(raster::as.matrix(chm))  # Transpose the matrix
  x <- seq(from = raster::xmin(chm), to = raster::xmax(chm), length.out = ncol(chm))
  y <- seq(from = raster::ymin(chm), to = raster::ymax(chm), length.out = nrow(chm))
  
  # Ensure dimensions match
  if (ncol(z) != length(y) || nrow(z) != length(x)) {
    stop("Dimension mismatch in surface data")
  }
  
  # Create color values
  z_colors <- z
  z_colors[is.na(z_colors)] <- min(z_colors, na.rm = TRUE)
  col_idx <- cut(z_colors, breaks = 255, labels = FALSE)
  
  # Plot surface
  rgl::surface3d(x, y, z,
                 color = myPal[col_idx],
                 front = "lines",
                 back = "lines",
                 alpha = 0.8)
  
  # Try to detect trees with error handling
  tryCatch({
    # Get CHM as matrix for tree detection
    chm_matrix <- as.matrix(chm)
    
    # Find local maxima
    local_max <- find_local_maxima(chm_matrix, window_size)
    
    # Get coordinates of local maxima
    max_indices <- which(local_max, arr.ind = TRUE)
    
    if(nrow(max_indices) > 0) {
      # Convert indices to coordinates
      tree_positions <- data.frame(
        x = xFromCol(chm, max_indices[,2]),
        y = yFromRow(chm, max_indices[,1]),
        z = chm_matrix[max_indices]
      )
      
      # Filter by height threshold
      tree_positions <- tree_positions[tree_positions$z >= height_threshold,]
      
      if(nrow(tree_positions) > 0) {
        # Add tree points
        rgl::points3d(tree_positions$x, 
                     tree_positions$y, 
                     tree_positions$z,
                     color = "red", 
                     size = point_size)
        
        # Add crown visualization if requested
        if (crown_detection) {
          for(i in 1:nrow(tree_positions)) {
            # Create a buffer around each tree point
            radius <- tree_positions$z[i] * 0.3  # Approximate crown radius
            theta <- seq(0, 2*pi, length.out = 20)
            x_crown <- tree_positions$x[i] + radius * cos(theta)
            y_crown <- tree_positions$y[i] + radius * sin(theta)
            z_crown <- rep(tree_positions$z[i], length(theta))
            
            # Draw crown outline
            rgl::lines3d(x_crown, y_crown, z_crown, color = "blue", alpha = 0.5)
          }
        }
        
        message("Detected ", nrow(tree_positions), " trees above ", height_threshold, "m")
      } else {
        message("No trees detected above height threshold of ", height_threshold, "m")
      }
    }
  }, error = function(e) {
    message("Warning: Could not detect trees: ", e$message)
  })
  
  # Add axes and title
  rgl::axes3d(c("x", "y", "z"), col = "black")
  rgl::title3d(main = "3D Canopy Height Model Visualization", 
               xlab = "UTM Easting", 
               ylab = "UTM Northing", 
               zlab = "Height (m)")
  
  # Set initial view
  view3d(theta = 45, phi = 30, zoom = 0.7)
  
  # Create HTML file
  output_path <- file.path(output_dir, output_filename)
  
  # Create the widget with basic settings
  scene <- scene3d()
  rglwidget(
    x = scene,
    width = 800,
    height = 600,
    elementId = "rglplot"
  ) %>%
    saveWidget(
      file = output_path,
      selfcontained = TRUE,
      libdir = "lib",
      background = "white",
      title = "3D Canopy Height Model"
    )
  
  # Close rgl device
  rgl::close3d()
  
  message("3D visualization saved to: ", output_path)
  message("Open the HTML file in a web browser to view the interactive 3D visualization")
}