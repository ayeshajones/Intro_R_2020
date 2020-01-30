# Day 2
# Summary stats
# 29 January 2020
# Author: Ayesha Jones

# Load packages
library(tidyverse)

# Load data
laminaria <- read_csv("data/laminaria.csv") # need to change commas to points in excel (csv)

laminaria <- read_delim("data/laminaria.csv", 
                            ";", escape_double = FALSE, trim_ws = TRUE) # copied from console,removed plus sign

laminaria %>% # Chose the dataframe
  summarise(avg_bld_wdt = mean(blade_length)) # Calculate mean blade length

laminaria %>% 
  summarise(ave_bld_len = mean(blade_length)) # same as above just changed name

laminaria %>% # Tell R that we want to use the 'laminaria' dataframe
  group_by(site) %>% 
  summarise(avg_stp_ln = mean(total_length), # Create a summary of the mean of the total lengths
            sd_stp_ln = sd(total_length),   # Create a summary of the sd of the total lengths
            med_stp_ln = median(total_length), # median
            var_stp_ln = var(total_length)) # variance
            # stipe lenghth and total length is the same (in the code?)
# to calculate things from specific columns in a dataframe 
# we use group_by %>%  summarise (commas for multi calcs)
# i.e. creates summary (calcs) within those groups (columns)


# Plotting 
# ggplot func used to plot graphs
ggplot(data = laminaria, aes(x = stipe_mass, y = stipe_length)) + # "data" refs to dataset. "aes" to modi. apprnce i.e. select axes to plot
  geom_point(shape = 21, colour = "blue", fill = "white") + # mod point appr with shape is code 21
  labs(x = "Stipe mass (kg)", y = "Stipe length (cm)") # label axes

# NOTE: cant use pipe func in ggplot, have to use "+" 

# Chapter 5: ggplot
# Plotting

ChickWeight <- datasets::ChickWeight # loads R's in-built dataset, 
# have to assign name for it ChickWeight to appear in enviro and not just console
# to select a dataframe press tab for options after double colon

# Create a basic figure
ggplot(data = ChickWeight, aes(x = Time, y = weight)) + # inputting chickweight data
  geom_point() + # creating a pt graph (scatter)
  geom_line(aes(group = Chick)) # linking mass points with a line for each chick. 
                                # same concept as grouping by site

ggplot(data = ChickWeight, aes(x = Time, y = weight, colour = Diet)) + # same graph but colour of diet column differs
  geom_point() +
  geom_line(aes(group = Chick))

ggplot(data = ChickWeight, aes(x = Time, y = weight, colour = Diet)) +
  geom_point() +
  geom_smooth(method = "lm") + # lm = linear model i.e. line of best fit
  theme_bw() # adds black & white border around graph
# now easier to see e.g. which diet is the best from the graph

ggplot(data = ChickWeight, aes(x = Time, y = weight, colour = Diet)) +
  geom_point(aes(size = weight)) + # add aes so that size becomes a function, error can be cos you need to remove it
  geom_smooth(method = "lm", size = 1.2) # changes size of line
# aes(size = weight) makes the size of the line equal the weight so that heaavier chicks will have a point
# that appears bigger & vice versa


ggplot(data = ChickWeight, aes(x = Time, y = weight, colour = Diet)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "Days", y = "Mass (g)", colour = "diet type") + # Change the labels
  theme(legend.position = "bottom") # Change the legend position


# Faceting graphs
library(tidyverse)
library(ggpubr) # first run to see if its installed, if not, install & then run again

# Create faceted figure i.e. 4 graphs in 1 panel, counted as same graph
ggplot(data = ChickWeight, aes(x = Time, y = weight, colour = Diet)) +
  geom_point() +
  geom_smooth(method = "lm") + # Note the `+` sign here
  facet_wrap(~Diet, ncol = 2) + # This is the line that creates the facets. ncol means that only 2 graphs on top
  labs(x = "Days", y = "Mass (g)")
  # facet_wrap makes all the diff diets on its own graph in 1 panel

ChickLast <- ChickWeight %>% # new dataset in enviro from chickweight
  filter(Time == 21) # specifying one value in the time column

# line graph
line_1 <- ggplot(data = ChickWeight, aes(x = Time, y = weight, colour = Diet)) + # assigning name to plot to save to enviro: line_1
  geom_point() +
  geom_line(aes(group = Chick)) +
  labs(x = "Days", y = "Mass (g)") # run code and R saves it as "image" as numbers i.e. details
line_1 # run "line_1" seperately to view graph (copy paste name) as a graph image. cant view if not in enviro

lm_1 <- ggplot(data = ChickWeight, aes(x = Time, y = weight, colour = Diet)) +
  geom_point() +
  geom_smooth(method = "gam") + # gam?
  labs(x = "Days", y = "Mass (g)")
lm_1

# histogram
# Note that we are using 'ChickLast', not 'ChickWeight'
histogram_1 <- ggplot(data = ChickLast, aes(x = weight)) +
  geom_histogram(aes(fill = Diet), position = "dodge", binwidth = 100) +
  labs(x = "Final Mass (g)", y = "Count") # line 96-98 is just creating object in enviro
histogram_1 # view graph

# boxplot
# Note that we are using 'ChickLast', not 'ChickWeight'
box_1 <- ggplot(data = ChickLast, aes(x = Diet, y = weight)) +
  geom_boxplot(aes(fill = Diet)) +
  labs(x = "Diet", y = "Final Mass (g)")
box_1

# Combining graphs into one panel
ggarrange(line_1, lm_1, histogram_1, box_1, 
          ncol = 2, nrow = 2, # Set number of rows and columns
          labels = c("A", "B", "C", "D"), # Label each figure
          common.legend = TRUE) # Create common legend








            


         













