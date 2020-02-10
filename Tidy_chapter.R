# Tidy data
# 10/02/20
# Ayesha Jones

# load packages
library(tidyverse)
library(lubridate)

# load data
load("data/SACTN_mangled.RData") # temp data from 7 diff srcs

# gather 3 cols into 1
SACTN2_tidy <- SACTN2 %>%
  gather(DEA, KZNSB, SAWS, key = "src", value = "temp")
# key to give new name, value is all values from your 3 cols 

# spreading var col into 2 cols
SACTN3_tidy1 <- SACTN3 %>% 
  spread(key = var, value = val)
# key is name of col, value is value of col you wanna spread i.e. contains depth & temp vals

# seperate index which is sep'd by "/" into 2 new cols
SACTN4a_tidy <- SACTN4a %>% 
  separate(col = index, into = c("site", "src"), sep = "/ ")
# separate & unite is opposites


my_date <- SACTN4a %>% 
  separate(col = date, into = c("day", "month", "year"), sep = "-") 

SACTN_tidy2 <- SACTN4a %>% 
  separate(col = index, into = c("site", "src"), sep = "/ ") %>% 
  mutate(day = lubridate::day(date),
         month = lubridate::month(date),
         year = lubridate::year(date))
# "c" is concatenate func
# how to change month number to name?
# lubridate works with dates - people save it diff'ly


###
# chapter 12: Tidier data

# Load the data from a .RData file
load("data/SACTNmonthly_v4.0.RData")

# Copy the data as a dataframe with a shorter name
SACTN <- SACTNmonthly_v4.0

# Remove the original
rm(SACTNmonthly_v4.0) # rm is remove the dataset with the long name i.e. remove original after naming it

# filter
SACTN %>% 
  filter(site == "Amanzimtoti")

dash <- SACTN %>% 
  filter(site == "Pollock Beach", month(date) == 12 | month(date) == 1)
# spec to certain months, vert "straight line "|" means "or" 

# filtering per year
SACTN %>% 
  filter(site == "Humewood", year(date) == 1990)

# filter a seq of years
humewood_90s <- SACTN %>% 
  filter(site == "Humewood", year(date) %in% seq(1990, 1999, 1))

# Select columns individually by name
SACTN %>% 
  select(site, src, date, temp)

SACTN %>% 
  select(date, site, src, temp) # can order the cols as you please

SACTN %>% 
  select(-date, site, src, temp) # remove date col

# Select all columns between site and temp like a sequence
SACTN %>% 
  select(site:temp)

# Select all columns except those stated individually
SACTN %>% 
  select(-date, -depth)
# Select all columns except those within a given sequence
# Note that the '-' goes outside of a new set of brackets
# that are wrapped around the sequence of columns to remove)

# Change up order by specifying individual columns
SACTN %>% 
  select(temp, src, date, site)

# Use the everything function to grab all columns 
# not already specified
SACTN %>% 
  select(type, src, everything())

# Or go bananas and use all of the rules at once
# Remember, when dealing with tidy data,
# everything may be interchanged
SACTN %>% 
  select(temp:type, everything(), -src)

# Temp in K from C
SACTN %>% 
  mutate(kelvin = temp + 273.15) # from K to C use minus

# summarise has built-in funcs
SACTN %>% 
  summarise(mean_temp = mean(temp, na.rm = TRUE)) # remove na val's 

###
# Chapter 13
# Create groupings based on temperatures and depth
SACTN_temp_group <- SACTN %>% 
  group_by(round(temp), depth) # rounds off vals for temp

SACTN_anom <- SACTN %>%
  group_by(site, src) %>% 
  mutate(anom = temp - mean(temp, na.rm = T)) %>% 
  select(site:date, anom, depth, type) %>% 
  ungroup()
# calc anomaly in temp...? AJ always asks in exam

SACTN %>% # Choose starting dataframe
  filter(site %in% c("Bordjies", "Tsitsikamma", "Humewood", "Durban")) %>% # Select sites
  select(-depth, -type) %>% # Remove depth and type columns
  mutate(month = month(date), # Create month column
         index = paste(site, src, sep = "/ ")) %>% # Create individual site column
  group_by(index, month) %>% # Group by individual sites and months
  summarise(mean_temp = mean(temp, na.rm = TRUE), # Calculate mean temperature
            sd_temp = sd(temp, na.rm = TRUE)) %>% # Calculate standard deviation
  ggplot(aes(x = month, y = mean_temp)) + # Begin with ggplot, switch from '%>%' to '+'
  geom_ribbon(aes(ymin = mean_temp - sd_temp, ymax = mean_temp + sd_temp), 
              fill = "black", alpha = 0.4) + # Create a ribbon
  geom_line(col = "red", size = 0.3) + # Create lines within ribbon
  facet_wrap(~index) + # Facet by individual sites
  scale_x_continuous(breaks = seq(2, 12, 4)) + # Control x axis ticks
  labs(x = "Month", y = "Temperature (°C)") + # Change labels
  theme_dark() # Set theme

# rename cols in dataset for when you forget what you named things
SACTN %>% 
  rename(source = src)

# once you group something, can un_group it. not really used in course

SACTN %>% 
  group_by(site, src) %>% 
  summarise(mean_temp = round(mean(temp, na.rm = T))
  ) %>% 
  ungroup() %>% 
  ggplot(aes(x = mean_temp)) +
  geom_density(fill = "seagreen", alpha = 0.6) + # if you remove alpha, colour becomes solid
  labs(x = "Temperature (°C)")

