[![NSF-1948997](https://img.shields.io/badge/NSF-2409885-blue.svg)](https://nsf.gov/awardsearch/showAward?AWD_ID=2409885) [![NSF-2409886](https://img.shields.io/badge/NSF-2409886-blue.svg)](https://nsf.gov/awardsearch/showAward?AWD_ID=2409886) [![NSF-2409887](https://img.shields.io/badge/NSF-2409887-blue.svg)](https://nsf.gov/awardsearch/showAward?AWD_ID=2409887)


## Overview
R functions based on [weblidar-treetop](https://github.com/carlos-alberto-silva/weblidar-treetop/), a shiny-based application for extracting forest information from lidar data. The treetop application enables (1) detection of individual trees from lidar-derived canopy height models, (2) extraction of crown-level attributes, (3) assessment of forest uniformity and spatial distribution of trees, (4) Export of crown-level data products and (5) 2D and 3D visualization of CHM and crown-level outputs.

The scripts have been refactored into standalone, modular functions, each dedicated to specific visualization and analytical tasks on lidar-derived Canopy Height Models (CHMs). This process decouples the original interactive Shiny server components, transforming them into reusable functions that independently generate outputs such as 3D visualizations, 2D plots, Lorenz curves, and spatial statistics (e.g., Ripley's K and L functions). The goal is to encapsulate the core analytical and visualization features of the original [server.R](https://github.com/carlos-alberto-silva/weblidar-treetop/blob/master/inst/app/server.r) code, which was better suited for interactive, real-time use, into standalone functions, removing the dependency on the Shiny framework. As a result, the scripts are now more streamlined, reusable, and better suited for automated or batch processing.

### Treetop Functions

**detect_trees.R**  
R function detects individual trees in a Canopy Height Model (CHM) raster, extracts their locations and heights, converts coordinates to latitude/longitude (WGS84), and exports the tree data (lat, lon, height, UTM coordinates) as a JSON file.

**generate_trees_plot.R**  
R function detects trees from a Canopy Height Model (CHM) raster and generates selected plots (CHM, Ripley's K/L, Lorenz Curve) to visualize tree locations, spatial patterns, and height distribution, saving the results as image files.

**plot_chm_2d.R**  
R function detects and visualizes tree tops and crowns from a Canopy Height Model (CHM) raster, then generates a 2D plot (PNG image).

**plot_chm_3d_web.R**  
R function generates an interactive 3D visualization of a Canopy Height Model (CHM) raster and saves it as an HTML file for web display.

**plot_lorenz_curve_from_treeheights.R**  
R code defines two functions (1) GiniCoeff - calculates the Gini coefficient, a statistical measure of inequality within a numeric vector (tree heights), optionally handling missing values and correcting for finite sample size and (2) plot_lorenz_curve - visualizes the distribution of tree heights by plotting their Lorenz curve, which graphically represents inequality, and annotates the plot with the Gini coefficient and the proportion of total height contributed by trees taller than the mean. Plot also includes reference lines and a marker for the mean height, and saves the resulting image as a PNG file.

**plot_lorenz_curve.R**  
This R function processes a canopy height model (CHM) raster to detect individual trees, extract their heights, and analyze the distribution of tree heights. It calculates the Gini coefficient to quantify height inequality and generates a Lorenz curve to visualize this distribution. The function then saves a PNG plot illustrating the Lorenz curve, the mean height inflection point, and the Gini coefficient, providing a clear summary of tree height variability within the area.

**plot_ripley_kl_from_tree_locations.R**  
The R function analyzes the spatial distribution of tree locations by calculating Ripley's K and L functions, which are used to detect clustering or dispersion in spatial point patterns. It creates and saves a PNG file with three plots: Ripley's K, Ripley's L, and a transformed L function ($L(r) - r$), each with simulation envelopes for comparison to spatial randomness. This helps users visually assess whether the trees are clustered, random, or evenly spaced.

**plot_ripley_kl.R**  
The R function analyzes the spatial distribution of trees detected from a canopy height model (CHM) raster by extracting tree locations, computing Ripley's K and L functions with simulation envelopes to assess spatial clustering or dispersion, and then saving plots of these functions (including the transformed L(r) - r) to a PNG file.

**treetop_install_libraries.R**  
Required suite of packages for spatial data analysis and visualization.

### Acknowledgement

Treetop: A Shiny-based Application for Extracting Forest Information from LiDAR data.  
Authors: Carlos Alberto Silva, Andrew T. Hudak, Lee A. Vierling, Ruben Valbuena, Adrian Cardil, Midhun Mohan, Danilo Roberti Alves de Almeida, Eben N. Broadbent, Angelica M. Almeyda Zambrano, Ben Wilkinson, Ajay Sharma, Jason B. Drake, Paul B. Medley, Jason G. Vogel, Gabriel Atticciati Prata, Jeff Atkins, Caio Hamamura, Carine Klauberg.

github.com/OpenForest4D/treetop-r  
Author: Kai Lin