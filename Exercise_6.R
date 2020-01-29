# Day 2
# Activities: make graphs using own built-in dataset & exercise 6
# Author: Ayesha Jones

# Load packages
library(tidyverse)
library(lubridate)
library(ggpubr)

# Select in-built dataset
Airqual <- datasets::airquality # load in-built airquality dataset assigned name Airqual 

# Create a basic figure
ggplot(data = Airqual, aes(x = Day, y = Temp, colour = Month)) + # inputting chickweight data
  geom_point() + # creating a line graph displaying daily temperature
  geom_line(aes(group = Month)) + # groups temperatures across months
  theme_light() + # changes theme
  labs(x = "Time (days)", y = "Temperature (°C)") # change axes names
  theme(legend.text = ) # to change month number to name
  
# changing point size
line_1 <- ggplot(data = Airqual, aes(x = Day, y = Temp, colour = Month)) + # inputting chickweight data
    geom_point(aes(size = 3)) + # creating a line graph displaying daily temperature
    geom_line(aes(group = Month)) + # groups temperatures across months
    theme_light() + # changes theme
    labs(x = "Time (days)", y = "Temperature (°C)")
line_1 


  
  
# change month from numbers to names
# change colour for each month
# modify legend so that its not continuous


### EXercise 6


ggplot(data = Airqual, aes(x = Day, y = Temp, colour = Month)) + 
    geom_point(shape = 17, size = 3, colour = "black", fill = "red") + # change point size to 3 and triangle shapes 
    geom_line(aes(group = Month), size = 1) + # groups temperatures by months with line size of 1
    theme_classic() + # adds lighter border and background
    labs(x = "Time (days)", y = "Temperature (°C)")

# bargraph
bargraph_1 <- ggplot(data = Airqual, aes(x = Day, y = Temp)) +
    geom_bar(stat = "identity", aes(fill = Temp)) +
    labs(x = "Time (days)", y = "Temperature (°C)")  # line 96-98 is just creating object in enviro
bargraph_1 # view graph


# boxplot
# Note that we are using 'ChickLast', not 'ChickWeight'
box_1 <- ggplot(data = Airqual, aes(x = Month, y = Temp)) +
  geom_boxplot(aes(fill = Month)) +
  labs(x = "Month", y = "Temperature (°C)")
box_1
# tried plotting temperature for months as a boxplot



# change point fill
# change legend name
# change colour palette


