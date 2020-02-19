# Day 4 
# Exercise
# 4 Feb 2020
# Author: Ayesha Jones

# Load libraries
library(tidyverse)
library(ggplot2)
library(ggpubr)
library(SDMTools)

# Load data
gebco_sa_asc <- read.table("data/gebco_sa.asc") # load adventurous data
gebco_sa_R <- load("data/gebco_sa.Rdata")

# Tidy data
gebco_sa_tidy <- gather(bathy_wide, longi, elevation, -lat)
# create new dataframe called gebco_sa_tidy
# gather columns into key-value pairs
# select bathy_wide dataframe 
# tranpose 1st row into a col called longi
# create new elevations for the lat and longi 
# "-lat" is selecting the 1st row to create the new col but excluding the first row name called "lat"

# convert to numeric to fix scale issues with continuous/discrete data i.e. elev s continuous
bathy_new <- as.data.frame(apply(gebco_sa_tidy, 2, as.numeric)) # save as new dataframe




# create palette








