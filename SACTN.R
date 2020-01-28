# Day 1
# Purpose: demonstrate some principles of data analysis
# Date: 28 January 2020
# Author: AJ

# load packages
library(tidyverse)
library (lubridate)

# read in the data

temps <- read_csv("data/SACTN_data.csv")

# look at data
head(temps) #looks at first 6 rows
tail(temps, 12) #looks at last 12 rows
glimpse(temps)

# summarise data
mean_temps <- temps %>% # saved new dataset as mean_temps (was added afterwards at beginning of code)
  mutate(mon = month(date)) %>% #creates new column that only shows month in the date
  group_by(site, mon) %>%
  summarise(mean_temp = mean(temp, na.rm = TRUE),
  sd_temp = sd(temp, na.rm = TRUE)) 

# make a graph
ggplot(data = mean_temps, aes(x = mon, y = mean_temp)) +
  geom_point(aes(col = site)) # different coloured points indicates different sites












