# Load required libraries (if GiniCoeff is not already defined)
# library(ineq)  # Uncomment if using the 'ineq' library

# Define the GiniCoeff function (if not using an external library)
GiniCoeff <- function(x, finite.sample = TRUE, na.rm = TRUE) {
  if (!na.rm && any(is.na(x)))
    return(NA_real_)
  x <- as.numeric(stats::na.omit(x))
  n <- length(x)
  x <- sort(x)
  G <- 2 * sum(x * 1L:n) / sum(x) - (n + 1L)
  if (finite.sample)
    GC <- G / (n - 1L)
  else
    GC <- G / n
  return(GC)
}

# Define the plot_lorenz_curve function
plot_lorenz_curve <- function(tree_heights, output_file = "lorenz_curve.png", width = 800, height = 600, res = 100) {
  # Sort tree heights in descending order
  size <- sort(tree_heights, decreasing = TRUE)
  
  # Calculate the proportion above the mean
  L.mean <- max(cumsum(size[size >= mean(size)]) / sum(size))
  
  # Calculate the Gini coefficient
  GC <- GiniCoeff(size)

  # Open a PNG device to save the plot
  png(output_file, width = width, height = height, res = res)

  # Plot the Lorenz curve
  plot(c(0, 1), c(0, 1), type = "l", col = "grey", ylim = c(0, 1), xlim = c(0, 1), lty = 1, lwd = 3,
       xlab = "Accumulated proportion of number of trees", ylab = "Accumulated proportion of tree heights (H ; m)")
  
  # Add the grey polygon for reference
  polygon(c(0, seq(0, 1, length.out = 1000)),
          c(0, cumsum(seq(100, 2, length.out = 1000) / sum(seq(100, 2, length.out = 1000)))),
          col = "grey", lty = 0)
  
  # Add the Lorenz curve
  lines(seq(0, 1, length.out = length(size) + 1),
        c(0, cumsum(size / sum(size))),
        lty = 1, lwd = 2)
  
  # Add the point for the mean height
  points(length(size[size >= mean(size)]) / length(size), L.mean, pch = 10, cex = 2.5, lwd = 2)
  
  # Add the legend
  legend("bottomright", c("Lorenz curve", "mean H (inflexion point)", "axis of symmetry", "maximum entropy and absolute equality lines"),
         col = c("black", "black", "grey", NA, NA), pch = c(NA, 10, NA, NA, NA), lty = c("solid", NA, "dotted", NA, NA),
         lwd = c(2, 2, 3, NA, NA), pt.cex = c(NA, 2, NA, NA, NA), fill = c(NA, NA, NA, "gray", NA),
         border = c("white", "white", NA, "gray", NA), x.intersp = c(.4, .4, .4, .001, .001), box.lwd = NA, bg = "transparent")
  
  # Add text for the Gini coefficient and proportion above the mean
  text(0.5, 0.4, "Gini coefficient = ", pos = 4); text(0.75, 0.4, format(GC, digits = 2), pos = 4)
  text(0.5, 0.35, "Proportion above the mean = ", pos = 4); text(0.9, 0.35, format(L.mean, digits = 2), pos = 4)
  
  # Close the PNG device
  dev.off()
}

# # Example tree heights
# tree_heights <- c(10, 15, 20, 25, 30, 35, 40, 45, 50)
#
# # Call the function to save the plot
# plot_lorenz_curve(tree_heights, output_file = "output/lorenz_curve.png", width = 800, height = 600, res = 100)