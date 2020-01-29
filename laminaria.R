# Day 1
# Exercise 4.5, statistical analysis, laminaria
# 28 January 2020
# Author: Ayesha Jones

# Load package
library(tidyverse)

# read laminaria dataset
laminaria <- read_csv("data/laminaria.csv")

lam_sub <- laminaria %>% # Tell R which dataframe we are using. saved as dataframe lam_sub 
  select(site, total_length) %>% # Select only specific columns
  mutate(total_length_half = (total_length)/2) %>%
  group_by(site, total_length_half) %>%
  summarise(mean_length = mean(total_length, na.rm = TRUE), #mean blade length
            min_length = min(total_length, na.rm = TRUE), # min blade length
            max_length = max(total_length, na.rm = TRUE), # max blade length
            no_of_obs = n()) # number of observations shown in table
# lam_sub has no NA and the total_length_half can be filtered in lam_sub dataset that using the filter icon 
# to see the all values between 0 and 100

# Calculating heaviest stipe
heav_stipe <- laminaria %>%
  select(site, region, stipe_length,stipe_mass) %>%
  group_by(site) %>%
  summarise(heav_stipe = max(stipe_mass, na.rm = TRUE))


## Neat but it is always good to try something on your own and something different. This improves your understanding and allows for exploring.

# Overal Mark for day 1 : 8/10 

  
             
