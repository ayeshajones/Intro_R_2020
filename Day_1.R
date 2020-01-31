# Day 1
# Data, laminaria, statistical analysis
# 28 January 2020

# Loading packages
library(tidyverse)

laminaria <- read_csv("data/laminaria.csv") # can also import

# viewing data

head(laminaria) # first 6 rows
tail(laminaria) # last 6 rows
glimpse(laminaria) # overview
view(laminaria) # opens the data 
names(laminaria) # column names in console

# Tidyverse

lam_sub <- laminaria %>% # Tell R which dataframe we are using
  select(site, total_length) # Select only specific columns called lam_sub from laminaria dataset

lam_slice <- laminaria %>% 
  select(site, total_length) %>% # Select specific columns first
  slice(56:78) # selects from row 56 to 78 from site and total_length columns and saved as lam_slice
lam_kom <- laminaria %>%
  filter(site == "Kommetjie")

# arrange largest to smallest and slice? to only get values <100

laminaria %>% # Tell R which dataset to use
  filter(site == "Kommetjie") %>% # Filter out only records from Kommetjie
  nrow() # Count the number of remaining rows


## This script is very neat. Nice going for the first day, keep it up.




