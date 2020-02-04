# Day 4
# Chapter 11: Tidy data
# 31 January 2020
# Ayesha Jones

# Load libraries
library(tidyverse)
library(ggplot2)

# Load dataset
load("data/SACTN_mangled.RData")

# create graph
ggplot(data = SACTN1, aes(x = date, y = temp)) +
  geom_line(aes(colour = site, group = paste0(site, src))) + # paste0 groups srcs together from one site e.g.info from DEA & SAWS from P.Nolloth
  labs(x = "", y = "Temperature (°C)", colour = "Site") + # didnt specify x axis name
  theme_bw()

# paste0("Port Nolloth", "DEA") what does this do?
# paste0("Port Nolloth", "DEA", sep = " / ") # seps two words with a /

SACTN2_tidy <-  SACTN2 %>% 
  gather(DEA, KZNSB, SAWS, key = "src", value = "temp") # gather groups 3 srcs that was in 3 cols into 1 col

# SACTN3 has depth and type in 1 column called var - should sep into 2 sep cols cos vars must be in cols
SACTN3_tidy1 <- SACTN3 %>% 
  spread(key = var, value = val) # spread seps 1 col into 2

# SACTN4 src and site is in same col caled index i.e. 2 vars in 1 col. need to sep
SACTN4a_tidy <- SACTN4a %>% 
  separate(col = index, into = c("site", "src"), sep = "/ ") 
# seps index col into c(c & col = column) 2 where it is sepd in the index col by "\ " 
# (NOTE the space after "\ ")

# SACTN4a can also sep date col into day, month and year with mutate func
SACTN_tidy2 <- SACTN4a %>% 
  separate(col = index, into = c("site", "src"), sep = "/ ") %>% 
  mutate(day = lubridate::day(date),
         month = lubridate::month(date), # how to change month number to month name?
         year = lubridate::year(date))

# SACTN4b is the oppo where d, m, y is in sep cols so we want to combine with unite func
SACTN4b_tidy <- SACTN4b %>% 
  unite(year, month, day, col = "date", sep = "-")






