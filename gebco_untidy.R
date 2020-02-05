# Day 4 exercise
# recreate SA's elevation map
# 5 February 2020
# Ayesha Jones

# Load packages
library(tidyverse)
library(SDMTools)
library(ggplot2)
library(tidyr) # tidies data
library(scales)
library(raster)

#Untidy data

adven_data <- read.asc("data/gebco_sa.asc")

untidy_adven <- raster(adven_data) # converts to raster & rename
tidy_adven <- as(untidy_adven, "SpatialPixelsDataFrame") # force into SpatialPixelsDataFrame format using "as" i.e. tidies data
tidied_final <- as.data.frame(tidy_adven) # save as df

head(tidied_final) # view first 6 rows
tail(tidied_final) # view last 6 rows

# Final adventure
tidy_map <- ggplot(data = tidied_final, aes(x = x, y = y)) +
  geom_raster(aes(fill = layer)) +
  scale_fill_gradientn("Elevation/ \nDepth (m)", values = scales::rescale(c(-6129, 0, 1, 3374)), colors = c("black", "darkblue", "slateblue", "darkturquoise", "gray87", "grey78", "chartreuse4", "seagreen", "darkgreen",  "lightgoldenrod", "darkorange3")) +
  labs(x = "", y = "") +
  scale_x_continuous(breaks = seq(10, 35, 5),
                     labels = c("10", "15", "20", "25", "30", "35"),
                     position = "bottom", expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank())
tidy_map 
