# Day 4 
# Elevation map
# 4 February 2020
# Ayesha Jones

# Load packages
library(tidyverse)
library(SDMTools)
library(ggplot2)
library(tidyr) # tidies data
library(scales)

# Load data
gebco_sa <- load("data/gebco_sa.Rdata")

# tidy data
gebco_sa_tidy <- gather(bathy_wide, lon, elevation, -lat)

# convert to numeric 
bathy_new <- as.data.frame(apply(gebco_sa_tidy, 2, as.numeric))

# Final map
final_map <- ggplot(data = bathy_new, aes(x = lon, y = lat)) +
  geom_raster(aes(fill = elevation)) + # create map using geom_raster 
  scale_fill_gradientn("Elevation/ \nDepth (m)", values = scales::rescale(c(-6129, 0, 1, 3374)), colours = c("black", "darkblue", "slateblue", "darkturquoise", "gray87", "grey78", "chartreuse4", "seagreen", "darkgreen",  "lightgoldenrod", "darkorange3")) + 
  labs(x = "", y = "") +
  scale_x_continuous(breaks = seq(10, 35, 5), 
                     labels = c("10", "15", "20", "25", "30", "35"),
                     position = "bottom", expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank())
final_map 

# in rescale9by the scale_fill_gradientn function: "(c(-x, 0, 1, y))" allows you to have one colour spectrum between -x & 0 and then another colour between 1 and y
# can find the min and max in bathy_useable dataset by clicking on "elevation" cell  
  
  
  
  
  
  