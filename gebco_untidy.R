# Load packages
library(tidyverse)
library(SDMTools)
library(ggplot2)
library(tidyr) # tidies data
library(scales)
library(raster)

#Untidy data

untidy_data <- read.asc("data/gebco_sa.asc")

untidy_2 <- raster(untidy_data) # converts to raster
try1 <- as(untidy_2, "SpatialPixelsDataFrame") # force into spatial etc format using "as"
tidied <- as.data.frame(try1) # save as df


# Final adventure
tidy_map <- ggplot(data = tidied, aes(x = x, y = y)) +
  geom_raster(aes(fill = layer)) +
  scale_fill_gradientn("Elevation/ \nDepth (m)", values = scales::rescale(c(-6129, 0, 1, 3374)), colors = c("black", "darkblue", "slateblue", "darkturquoise", "gray87", "grey78", "chartreuse4", "seagreen", "darkgreen",  "lightgoldenrod", "darkorange3")) +
  labs(x = "East", y = "South") +
  scale_x_continuous(breaks = seq(10, 35, 5),
                     labels = c("10", "15", "20", "25", "30", "35"),
                     position = "bottom", expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0), name = "South") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank())
tidy_map 
