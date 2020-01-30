# DAy 3
# Chapter 8, 9: ggplot, mapping
# 30 January 2020
# Author: Ayesha Jones

# Load packages
library(tidyverse)
library(boot) # need urine dataset

# Plotting with boot
urine <- boot::urine

# Create scatterplot i.e. point graph
ggplot(data = urine, aes(x = osmo, y = ph)) +
  geom_point(aes(colour = cond)) + # cond = conduct
  labs(x = "Osmoregulation", y = "pH")


### Mapping in R

# Load packages
library(tidyverse)
library(ggpubr) # to use ggplot


# Load data
# Note: not csv format
# highight all and then run
load("data/south_africa_coast.RData")
load("data/sa_provinces.RData")
load("data/rast_annual.RData")
# load("data/MUR.RData") # removed by adding # cos its high resoluion i.e.takes long
load("data/MUR_low_res.RData")

# Custome palette already made
# The colour pallette we will use for ocean temperature
cols11 <- c("#004dcd", "#0068db", "#007ddb", "#008dcf", "#009bbc",
            "#00a7a9", "#1bb298", "#6cba8f", "#9ac290", "#bec99a")
# appears in enviro under " Values" cos not dataset but stil in R memory

# Create map
ggplot(data = south_africa_coast, aes(x = lon, y = lat)) + # need lat & longi to plot points as map
  geom_point()

# Creating land mask
ggplot(data = south_africa_coast, aes(x = lon, y = lat)) +
  geom_polygon(colour = "black", fill = "grey70", aes(group = group)) # The land mask
# polygon to create shape based on plots
# cant be geom_line cos it'll only join lines & not the top part
# clarify: when to use aes & when you can?
# aes(group = group) creates polygons within the major polygon

# Adding SA province borders 
ggplot(data = south_africa_coast, aes(x = lon, y = lat)) +
  geom_polygon(colour = "black", fill = "grey70", aes(group = group)) +
  geom_path(data = sa_provinces, aes(group = group)) # The province borders

# limiting coords with coord_equal
ggplot(data = south_africa_coast, aes(x = lon, y = lat)) +
  geom_polygon(colour = "black", fill = "grey70", aes(group = group)) +
  geom_path(data = sa_provinces, aes(group = group)) + 
  coord_equal(xlim = c(15, 34), ylim = c(-36, -26), expand = 0) # Force lon/lat extent
# expand cuts off above polygon

# 

sst <- MUR_low_res # rename MUR data to sst by assigning new name to it
# why rename?

ggplot(data = south_africa_coast, aes(x = lon, y = lat)) +
  geom_raster(data = sst, aes(fill = bins)) + # The ocean temperatures
  geom_polygon(colour = "black", fill = "grey70", aes(group = group)) + #black outline
  geom_path(data = sa_provinces, aes(group = group)) +
  coord_equal(xlim = c(15, 34), ylim = c(-36, -26), expand = 0)
# run each line (w.o. +) seperately to see where error lies
# raster contains...?
# each bin is a square, filling each box w diff cols

# Another example
ggplot(data = south_africa_coast, aes(x = lon, y = lat)) +
  geom_raster(data = sst, aes(fill = bins)) + # fill in bin boxes
  geom_polygon(colour = "black", fill = "grey70", aes(group = group)) +
  geom_path(data = sa_provinces, aes(group = group)) + # adding prov bords
  scale_fill_manual("Temp. (°C)", values = oc_pal) + # add own palette oc_pal
  coord_equal(xlim = c(15, 34), ylim = c(-36, -26), expand = 0) +
  labs(x = "Longitude", y = "Latittude") # change axes names

# Create own palette
oc_pal <- c("#3D9981", "#308477", "#28706C", "#225D5E", "#1D4A4F", "#19383F", "#14272E")

# Final map
final_map <- ggplot(data = south_africa_coast, aes(x = lon, y = lat)) +
  geom_raster(data = sst, aes(fill = bins)) +
  geom_polygon(colour = "black", fill = "grey70", aes(group = group)) +
  geom_path(data = sa_provinces, aes(group = group)) +
  geom_tile(data = rast_annual, aes(x = lon, y = lat, fill = bins), 
            colour = "white", size = 0.1) +
  scale_fill_manual("Temp. (°C)", values = cols11) +
  coord_equal(xlim = c(15, 34), ylim = c(-36, -26), expand = 0) +
  scale_x_continuous(position = "top") + # Put x axis labels on top of figure
  theme(axis.title = element_blank(), # Remove the axis labels
        legend.text = element_text(size = 7), # Change text size in legend
        legend.title = element_text(size = 7), # Change legend title text size
        legend.key.height = unit(0.3, "cm"), # Change size of legend
        legend.background = element_rect(colour = "white"), # Add legend background
        legend.justification = c(1, 0), # Change position of legend
        legend.position = c(0.55, 0.4) # Fine tune position of legend
  )
final_map
# 92: map assigned name, appears in enviro, but need to run for map to appear as plot


### Chapter 10: Mapping wih style
# Load packages
library(tidyverse)
library(scales)
library(ggsn) # to add North arrow to map
library(maps)

# Load data
load("data/africa_map.RData") # load func cos data is in R.data format N.B. to look at file format

# Default maps
ggplot() +
  borders() + # The global shape file
  coord_equal() # Equal sizing for lon/lat 
# gives world map
# can use map to cut out area of interest with coord_equal func

# create new plot as map
sa_1 <- ggplot() +
  borders(fill = "grey70", colour = "black") +
  coord_equal(xlim = c(12, 36), ylim = c(-38, -22), expand = 0) # Force lon/lat extent
sa_1


# adding on to sa_1
sa_2 <- sa_1 + # edit sa_1
  annotate("text", label = "Atlantic\nOcean", #\n so that two words appear in 2 diff line
           x = 15.1, y = -32.0, 
           size = 5.0, # text size
           angle = 45, # angle of text
           colour = "navy") +
  annotate("text", label = "Indian\nOcean", 
           x = 33.2, y = -34.2, 
           size = 5.0, 
           angle = 330, 
           colour = "springgreen")
sa_2
# annotate edits of map  text

# 
sa_3 <- sa_2 +
  scalebar(x.min = 22, x.max = 26, y.min = -36, y.max = -35, # Set location of bar
            dist = 200, height = 1, st.dist = 0.8, st.size = 4, # Set particulars
            transform = TRUE, model = "WGS84") + # Set appearance
  north(x.min = 22.5, x.max = 25.5, y.min = -33, y.max = -31, # Set location of symbol
        scale = 1.2, symbol = 16)
sa_3
# north adds arrow, x.min & max shifts arrow position











