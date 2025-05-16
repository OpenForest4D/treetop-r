generate_forest_plots <- function(chm_path,
                                  plots = c("chm", "ripley", "lorenz"),
                                  output_dir = "output",
                                  # Common parameters
                                  window_size = 5,
                                  height_threshold = 1.37,
                                  # CHM plot parameters
                                  chm_output = file.path(output_dir, "chm_plot.png"),
                                  color_palette = "BlGrRed",
                                  # Ripley plot parameters
                                  ripley_output = file.path(output_dir, "ripley_kl_plot.png"),
                                  ripley_width = 1200,
                                  ripley_height = 400,
                                  ripley_res = 100,
                                  nsim = 99,
                                  # Lorenz curve parameters
                                  lorenz_output = file.path(output_dir, "lorenz_curve.png"),
                                  lorenz_width = 800,
                                  lorenz_height = 600,
                                  lorenz_res = 100) {
  
  # Load required libraries
  library(raster)
  library(lidR)
  library(sp)
  library(spatstat)
  library(spatstat.geom)
  library(spatstat.explore)
  library(RColorBrewer)
  
  # Load CHM and detect trees (common steps)
  chm <- raster::raster(chm_path)
  chm <- raster::readAll(chm)
  
  trees <- lidR::find_trees(chm, lidR::lmf(ws = window_size))
  if (nrow(trees) == 0) stop("No trees detected. Adjust 'window_size' or 'height_threshold'.")
  
  valid_trees <- trees$Height >= height_threshold
  trees_filtered <- trees[valid_trees, ]
  if (nrow(trees_filtered) == 0) {
    warning("No trees meet height threshold. Using all detected trees.")
    trees_filtered <- trees
  }
  
  tree_coords <- sp::coordinates(trees_filtered)
  tree_heights <- trees_filtered$Height
  
  # Generate selected plots
  if ("chm" %in% plots) {
    .generate_chm_plot(chm, trees_filtered, chm_output, color_palette)
  }
  
  if ("ripley" %in% plots) {
    .generate_ripley_plot(tree_coords, ripley_output, ripley_width, ripley_height, ripley_res, nsim)
  }
  
  if ("lorenz" %in% plots) {
    .generate_lorenz_plot(tree_heights, lorenz_output, lorenz_width, lorenz_height, lorenz_res)
  }
  
  invisible()
}

# Helper function for CHM plot
.generate_chm_plot <- function(chm, trees, output_file, color_palette) {
  # Define color ramp
  myColorRamp <- function(colors, values) {
    v <- (values - min(values)) / diff(range(values))
    x <- colorRamp(colors)(v)
    rgb(x[,1], x[,2], x[,3], maxColorValue = 255)
  }
  
  if (color_palette == "BlGrRed") {
    col.rev <- myColorRamp(c("blue", "green", "yellow", "red"), 0:255)
  } else if (color_palette == "Viridis") {
    viridis_colors <- c("#440154FF", "#482878FF", "#3E4A89FF", "#31688EFF", "#26828EFF",
                        "#1F9E89FF", "#35B779FF", "#6DCD59FF", "#B4DE2CFF", "#FDE725FF")
    col.rev <- myColorRamp(viridis_colors, 0:255)
  } else {
    col <- brewer.pal(9, color_palette)
    col.rev <- rev(col)
  }
  
  # Delineate crowns
  crowns <- lidR::silva2016(chm, trees, max_cr_factor = 0.5, exclusion = 0.1)()
  
  # Plot and save
  png(output_file, width = 800, height = 600, res = 100)
  raster::plot(chm, col = col.rev, axes = TRUE, xlab = "UTM Easting", ylab = "UTM Northing")
  points(trees, pch = 16, cex = 1.5, col = "red")
  plot(crowns, add = TRUE, border = "blue", lwd = 2)
  dev.off()
}

# Helper function for Ripley's plot
.generate_ripley_plot <- function(tree_coords, output_file, width, height, res, nsim) {
  S <- SpatialPoints(tree_coords)
  bb <- bbox(S)
  W <- owin(bb[1,], bb[2,])
  P <- ppp(tree_coords[,1], tree_coords[,2], window = W)
  
  K <- envelope(P, Kest, nsim = nsim, verbose = FALSE)
  L <- envelope(P, Lest, nsim = nsim, verbose = FALSE)
  
  png(output_file, width = width, height = height, res = res, type = "cairo")
  par(mfrow = c(1,3), mar = c(5,5,4,3))
  plot(K, lwd=2, main="a) K", xlab="r (m)", cex.lab=1.5)
  plot(L, lwd=2, main="b) L", xlab="r (m)", cex.lab=1.5)
  plot(L, . - r ~ r, ylab=expression(hat("L")), xlab="r (m)", main="c) L", lwd=2, cex.lab=1.5)
  dev.off()
}

# Helper function for Lorenz curve
.generate_lorenz_plot <- function(tree_heights, output_file, width, height, res) {
  GiniCoeff <- function(x) {
    x <- sort(stats::na.omit(x))  # Fixed extra parenthesis
    n <- length(x)
    G <- 2 * sum(x * seq_along(x)) / sum(x) - (n + 1)  # Fixed extra parenthesis
    GC <- G / (n - 1)
    return(GC)
  }
  
  size <- sort(tree_heights, decreasing = TRUE)
  L.mean <- max(cumsum(size[size >= mean(size)]) / sum(size))  # Added closing parenthesis
  GC <- GiniCoeff(size)  # Fixed function name
  
  png(output_file, width = width, height = height, res = res)
  plot(c(0,1), c(0,1), type="l", col="grey", ylim=c(0,1), xlim=c(0,1),
       xlab="Accumulated proportion of number of trees",
       ylab="Accumulated proportion of tree heights (H ; m)", lwd=3)
  polygon(c(0, seq(0,1, length.out=1000)), 
          c(0, cumsum(seq(100, 2, length.out=1000)/sum(seq(100,2, length.out=1000)))), 
          col="grey", border=NA)
  lines(seq(0,1, length.out=length(size)+1), c(0, cumsum(size)/sum(size)), lwd=2)
  points(sum(size >= mean(size))/length(size), L.mean, pch=10, cex=2.5, lwd=2)
  legend("bottomright", 
         legend = c("Lorenz curve", "Mean H (inflexion point)", "Equality line"),
         col = c("black", "black", "grey"), lty = c(1, NA, 1), lwd = c(2, NA, 3),
         pch = c(NA, 10, NA), pt.cex = c(NA, 2, NA), bg = "transparent")
  text(0.5, 0.4, paste("Gini =", round(GC, 2)), pos=4)
  text(0.5, 0.35, paste("Prop above mean =", round(L.mean, 2)), pos=4)
  dev.off()
}