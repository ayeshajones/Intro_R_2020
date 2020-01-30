# Day 2
# Practise/Recap own dataset & create graphs
# Author: Ayesha Jones

# Load packages
library(tidyverse)

# select in-built dataframe (should have chosen dataset similar to ChickWeight example i.e. not continuous data)
Cars <- datasets::cars

# Plot basic graph
ggplot(data = Cars, aes(x = speed, y = dist)) +
  geom_point() +
  geom_smooth(method = "lm", size = 1) + # how to remove grey band around lm?
  theme_light() +
  labs(x = "Speed (m/s)", y = "Distance (m)")

# Select dataset
Iris <- datasets::iris

# calc mean of each species petal length
iris_ave <- # save new dataframe
Iris %>% # from originl 'Iris" dataframe 
  group_by(Species)  %>% 
  summarise(ave_petal_ln = mean(Petal.Length))


# create basic plot named iris_scatter
iris_scatter <- ggplot(data = Iris, aes(x = Petal.Length, y = Sepal.Length, colour = Species)) + # colour is part of aes, if not, wont seperate species by colour
  geom_point() +
  geom_smooth(method = "lm", colour = "black") +
  labs(x = "Petal length (mm)", y = "Sepal length (mm)") +
  theme_light()
iris_scatter

### UNNECESSARY
# create new subset dataset from original iris data using filter func  
iris_spe <- Iris %>% 
  filter(Petal.Length == 1.4) # looks at ave petal length for setosa sp
# to filter words you need to use quotation marks but not for numbers
# we filter so that we only look at the data with a val of 1.4

# tried to create a histogram for the ave length of a species
# but dont need to filter to create histogram


# histogram 
# histogram of all 3 specie's a specific petal/sepal length

iris_histo <- ggplot(data = Iris, aes(x = Petal.Length)) +
                       geom_histogram(aes(fill = Species),  position = "dodge", binwidth = 1) +
                       labs(x = "Petal length (mm)", y = "Count") +
                       theme_light()
iris_histo
# what does position and dodge mean?

#boxplot
iris_box <- ggplot(data = Iris, aes(x = Species, y = Sepal.Length)) +
                     geom_boxplot(fill = Species) +
  labs(x = "Petal length (mm)",  y = "Sepal/Length (mm)")
iris_box


# Note that we are using 'ChickLast', not 'ChickWeight'
box_1 <- ggplot(data = ChickLast, aes(x = Diet, y = weight)) +
  geom_boxplot(aes(fill = Diet)) +
  labs(x = "Diet", y = "Final Mass (g)")
box_1




