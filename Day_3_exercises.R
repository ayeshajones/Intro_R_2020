# Day 3
# Exercise 11.4; ecklonia, laminaria dataset, cut world map
# 30 January 2020
# Author: Ayesha Jones

# Load packages
library(tidyverse)
library(ggplot2)

# load datasets
laminaria <- read_csv("data/laminaria.csv")
ecklonia <- read_csv("data/ecklonia.csv")


# Create 2 graphs per dataset & facet
# ggplot func used to plot graphs

# Laminaria
# Scatter plot
Lam_scatter <- ggplot(data = laminaria, aes(x = stipe_mass, y = stipe_length, colour = region)) + # colour is part of aes, if not, wont seperate species by colour
  geom_point() +
  geom_smooth(method = "lm", colour = "black") + # line of best fit wont appear?
  labs(x = "Stipe mass (kg)", y = "Stipe length") +
  theme_classic()
Lam_scatter  

# filter digits i.e. laminaria with 12 fronds
Lam_digits <- laminaria %>% # new dataset in enviro from chickweight
  filter(digits == 12)

# create histogram
lam_dig_histo <- ggplot(data = Lam_digits, aes(x = digits)) +
  geom_histogram(aes(fill = region), position = "dodge", binwidth = 10) + # dodge means the bars are seperate (always use)
  labs(x = "Number of fronds", y = "Count") # line 96-98 is just creating object in enviro
lam_dig_histo # view graph

# Ecklonia
# create scatter plot 
Ecklo_scatter <- ggplot(data = ecklonia, aes(x = stipe_diameter, y = stipe_length, colour = site)) + # colour is part of aes, if not, wont seperate species by colour
  geom_point() +
  geom_line() + 
  geom_smooth(method = "lm") +
  labs(x = "Stipe diameter (cm)", y = "Stipe length (cm)") +
  theme_classic2()
Ecklo_scatter  

# create boxplot
Ecklo_box <- ggplot(data = ecklonia, aes(x = stipe_length, y = stipe_diameter)) +
  geom_boxplot(aes(fill = site)) +
  labs(x = "Stipe length (cm)", y = "Stipe diameter (cm)")
Ecklo_box

# Faceting ecklonia scatter plot
facet_lam <- ggplot(data = laminaria, aes(x = stipe_mass, y = stipe_length, colour = region)) +
  geom_point() +
  geom_smooth(method = "lm") + # Note the `+` sign here
  facet_wrap(~region, ncol = 2) + # This is the line that creates the facets. ncol means that only 2 graphs on top
  labs(x = "Stipe mass(kg)", y = "Stipe length (m)")
facet_lam
# facet_wrap shows the stipe length and mass sperately for each region as one graph

facet_lam + scale_x_continuous(limits = c(0, 6)) # tried to fix scale

### Mapping
# Load packages
library(tidyverse) 
library(scales) # to modify scales
library(ggsn) # to add North arrow to map
library(maps) # to access global map.shp data

# Load world map data
load("data/africa_map.RData")

#create world map
ggplot() +
  borders() + # The global shape file
  coord_equal()

# create map of india
india_1 <- ggplot() +
  borders(fill = "maroon", colour = "black") +
  coord_equal(xlim = c(65, 95), ylim = c(5, 40), expand = 0) # Force lon/lat extent
india_1

india_2 <- india_1 + # editing india_1
  annotate("text", label = "Indian\nOcean", #\n so that two words appear in 2 seperate line
           x = 70, y = 10, 
           size = 5.0, # text size
           angle = 360, # angle of text
           colour = "black") +
  annotate("text", label = "Indian\nOcean", 
           x = 87, y = 10, 
           size = 5.0, 
           angle = 360, 
           colour = "black")
india_2 <- 

# how do you cut out country/province shape in order to inset on a larger map?









