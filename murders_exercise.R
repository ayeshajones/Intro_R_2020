# Tidy dataset
# 07.02.2020
# Ayesha Jones

# Section 1:
# Load packages
library(tidyverse)

# load built-in datasets
BOD <- datasets::BOD
BJsales <- datasets::BJsales
EuStockMarkets <-  datasets::EuStockMarkets
DNase <- datasets::DNase
Formaldehyde <- datasets::Formaldehyde
UC_BAdmissions <- datasets::UCBAdmissions
Orange <-  datasets::Orange

# save to enviro
BJsales <- as.data.frame(BJsales)
EuStockMarkets <- as.data.frame(EuStockMarkets)
Orange <- as.data.frame(Orange)
UC_BAdmissions <- as.data.frame(UC_BAdmissions)

# Section 1 Answers: 
# 1. c
# 2. tidy datasets: all are tidy


# Section 2
# Load package
library(tidyverse)
library(dplyr) # You can add columns using the dplyr mutate func
library(dslabs)
library(ggplot2)

#load dataset
data(murders) 

# save as dataset
murders <- as.data.frame(murders)

# Explore murder
glimpse(murders)
head(murders)
tail(murders)
view(murders)
summarise(murders)

# write paragraph describing the murders dataset
# The murders dataset has five columns representing five different variables, namely: "state",
# "abb"(state abbreviation), "region", "population" (size), as well as "total", which refers to the total
# number of murders. Each row represents the observation for each variable, and each cell represents the
# value/information. For example, we can deduce from the first row alone that the state Alabama, with the 
# abbreviation AL, is located in the southern region, and has a population size of 4 779 736, and has had 
# a total of 135 murders in that particular state. We can see that there are 51 states in total, and that
# states can belong to the same region e.g. both Alaska and Arizona are from the west region.

mutate_murd <- mutate(murders, population_in_millions = population / 10^6)
# created new col cal'd N in millions

# create dataset showing only state and N size
select_murd <- select(murders, state, population) 

# Removing rows with Florida
wo_florida <- murders %>%  
  filter(state != c("Florida")) # ! removes cells with "florida" from rows

# Removing rows from Southern region
no_south <- murders %>%  
  filter(region != c("South")) 

# how many states in this category? 
states <- murders %>% 
  nrow() - nrow(no_south) %>% 
  as.data.frame(states_in_cats)
# i.e. 51 - 34 = 17 states 
# (subtract state no. of rows in no_south dataset from original murders dataset)

# Filtering NY and Texas
filter_NY_Texas <- murders %>% 
  filter(state %in% c("New York", "Texas"))

# N size of south & west regions
S_W_regions <- murders %>% 
  filter(region == c("South", "West"))

s_reg_pop <- sum(murders$population) %>% 
  as.data.frame(s_reg_pop)

# Dataframe showing N size of NE region
NE_pop <- murders %>% 
  filter(region == c("Northeast")) %>% 
  select("population") 

# Create 2 plots & explain visible trends
# Boxplot showing total murders for each region
# trend: The total number of murders in the Northeast region is right-skewed i.e. positively skewed,
# which means that the data is not normally distributed i.e. it is assymetrical. the Northeast region also 
# shows the greated range ( i.e. max) than the other regions. Positive;y skewed data has a mean that 
# is greather than the median, which means that there is a greater frequency of high murders.
# The total number of murders from the southern region is symmetrical, which means that the median murders is
# roughly in the middle. The total murders from the NOrth Central region is also right-skewed i.e. positive.
# The maximum total murders is approximately over 400. The NOrth Central region also has the largest boxplot,
# meaning that it has greater variability (in terms of total murders). The west region is also right- skewed
# i.e. positive. the West region boxplot also has the smallest box, meaning that the values of the total 
# murders has low variability. Ovrall, the area 
ggplot(data = murders, aes(x = region, y = total)) +
  geom_boxplot(aes(fill = region)) +
  theme_minimal() + 
  labs(x = "Region", y = "Total murders")

# median closer to 0 is right skewed i.positive & vice versa

# bargraph showing total murders per region
# trend: from this graph we can see that the region with the highest total murders is the South region, 
# with a value of over 4000 murders, whereas the region with the lowest total murders would be the Northeast region, 
# with a value of approximately 1 500 total murders. The number of total murders in the North Central and West regions 
# is only slightly higher than the Northeast region.  
ggplot(data = murders, aes(x = region, y = total)) +
  geom_bar(stat = "identity") +
  theme_classic() +
  labs (x = "Region",y = "Total murders")
  
# Calculate S & W pop size
S_pop_size <- S_W_regions %>% 
  filter(region == "South") %>% 
  summarise(S_pop = sum(population))

W_pop_size <- S_W_regions %>% 
  filter(region == "West") %>% 
  summarise(W_pop = sum(population))

# Create a new data frame where the total>20 but <100 and to exclude the value 120
above_20 <- murders %>% 
  filter(total >20)
between_20_and_100 <- above_20 %>% 
  filter(total <100)

# create dataset from 10th to 24th row and 26 th rowiusing slice() function.
btwn_10_and_24 <- murders %>% 
  slice(10:24, 26) # takes out selective rows

# murders_tibble
murders_tibble <- as.tibble(murders) %>% # for dplyr package
  group_by(region)

# same code
murders.tibble <- as.tibble(murders)  # for tidyverse package


# Section 3
# load packages
library(tidyverse)
library(dplyr)
library(dslabs)

# load in-built dataset
data(heights) 
heights <- as.data.frame(heights)

# Paragraph describing heights dataset
# Displays the heights of males and females.When looking at th raw data, we can see that males 
# are typically taller than females, with most females having a height that falls within the 60cm +,
# and males having heights that fall within the high 60s and 70cm+ side. 

glimpse(heights)
head(heights)
tail(heights)
view(heights)
summarise(heights)


# Calc mean, st dev, min, max & median for males and females
female_height <- heights %>% 
  filter(sex == c("Female"))
    
male_height <- heights %>% 
  filter(sex == c("Male"))

male_summ <- male_height %>% 
  select(sex, height) %>% 
  summarise(mean_height = mean(height),
            st_dev = sd(height),
            min_height = min(height),
            max_height = max(height),
            med_height = median(height))
  
female_summ <- female_height %>% 
  select(sex, height) %>% 
  summarise(mean_height = mean(height),
            st_dev = sd(height),
            min_height = min(height),
            max_height = max(height),
            med_height = median(height))

# Section 4
# create vectors
Vector <- data.frame(x = c( 1, 6, 21, 19 , NA, 73, NA), y = c(NA, NA, 3, NA, 13, 24, NA))

# a. remove NAs
wo_NA <- Vector %>% 
  select(everything()) %>% # selects all columns 
  summarise_all(funs(sum(is.na))) # selects cells with NAs in it

# b. Transform the code, used above (a), into a function
new_func <- function(dataset){dataset %>% select(everything()) %>% # the "function" command allows you to create a function from your own code
    summarise_all(funs(sum(is.na(.))))}

# c. create 3 new vectors & test-run your new function
new_vects <- data.frame(x = c( 2, 7, 12, 19 , 46, 73, NA),
                        y = c(1, NA, 3, NA, 13, 24, 37), 
                        z = c(NA, 8, 17, NA, 22, 38, 40))

no_NAs <- new_func(new_vects)


# Section 5

Seasonal_data <- data.frame(year = c(2015, 2016, 2017, 2018),
             winter = c(41, 39, 47, 40),
             spring = c(41, 46, 57, 45),
             summer = c(75, 52, 85, 66),
             Autumn = c(57, 66, 52, 56))

# Design an hypothesis, then create two plots and write a paragraph discussing your findings
# Hypothesis: Average annual temperatures is predicted to have increased rapidly in recent
# years (2015-2018) in summer and decreased in winter in France due to increased carbon emissions 
# contributing to climate change.

# bargraph showing average summer temperature over time
ggplot(data = Seasonal_data, aes(x = year, y = summer)) +
  geom_bar(stat = "identity") +
  theme_classic() +
  labs (x = "Time (yr)",y = "Temperature (°F)")

# bargraph showing average winter temperature over time 
ggplot(data = Seasonal_data, aes(x = year, y = winter)) +
  geom_bar(stat = "identity") +
  theme_classic() +
  labs (x = "Time (yr)",y = "Temperature (°F)")

# line graph displaying annual temps for each season
ggplot(data = Seasonal_data) +
  geom_line(data = Seasonal_data, aes(x = year, y = summer, colour = "Summer")) +
  geom_line(data = Seasonal_data, aes(x = year, y = Autumn, colour = "Autumn")) +
  geom_line(data = Seasonal_data, aes(x = year, y = winter, colour = "Winter")) +
  geom_line(data = Seasonal_data, aes(x = year, y = spring, colour = "Spring")) +
  theme_classic() +
  labs (x = "Time (yr)",y = "Temperature (°F)")
  
# modify dataset so that seasons are under one col
seas_new <- Seasonal_data %>% 
  gather(summer, Autumn, winter, spring, key = "Season", value = "Temperature")

# boxplot of diff seasons' temps over time 
rain_box <- ggplot(data = seas_new, aes(x = year, y = Temperature )) +
  geom_boxplot(aes(fill = Season)) +
  labs(x = "Time (yr)", y = "Temperature (°F)")
rain_box
# how to add all year intervals on x axis?


# Findings: Average annual summer temperatures between 2015-2018 does not increase rapidly as
# predicted, but rather fluctuates rapidly. Additionally, average winter temperatures in France did 
# not decrease rapidly in recent years, rather it experienced a rather constant temperature between
# 2015-2018, with a slight temperature spike in 2017. Additionally, the seaon that flucutated the most
# was summer, followed by Autumn, and then Spring and winter respectively.

cats_data <- tibble(cats = c("A", "B", "C"),
                    position = c("1 2 3", "3 1 2", "2 3 1"), 
                    minutes = c(3, 3, 3),
                    seconds = c(12, 44, 15))
cats_data

# sep col into 1st, 2nd, 3rd
cats_sep <- cats_data%>% 
  separate(col = position, into = c("First place", "Second place", "Third place"), sep = " ") 

cats_time <- cats_data %>% 
  unite(minutes, seconds, col = "total_time", sep = "/ ")

cats_new <- cats_sep %>% 
  unite(minutes, seconds, col = "total_time", sep = "/ ")

# Section 6
# load downloaded dataset
cloudseed <- read_csv("data/CloudSeeding2.csv")

# Treament: TE, TW
# Control: NC, SC, NWC

cloudseed_gather <- cloudseed %>% 
  gather(TE, TW, NC, SC, NWC, key = "region", value = "rainfall")
# gathers rainfall from different regions into 1 col called region

cloud_spread <- cloudseed_gather %>% 
  spread(key = region, value = rainfall)
# seperates rainfall col into diff region columns

# calc ave rainfall for treatment regions using select, group_by & mutate
cloud_summ <- cloudseed %>%  
  select(TE, TW) %>% # Select only specific columns
  group_by(TE, TW) %>% # grouping 2 columns and then summarising to show the following calcs
  summarise(mean_TE_rainfall = mean(TE, na.rm = TRUE),
            mean_TW_rainfall = mean(TW, na.rm = TRUE)) %>% 
  mutate(mean_rainfall = (sum(mean_TE_rainfall, mean_TW_rainfall)))

# joining cloudseed & cloud_sum which have TE & TW cols in common
cloud_new <- left_join(cloudseed, cloud_summ) 


# unite seas & seeded cols into 1
cloud_unite <- cloudseed %>% 
  unite(Seeded, Season, col = "seedseas", sep = "/ ")

# sep Seeded/Season into 2
cloud_sep <- cloud_unite %>% 
  separate(col = seedseas, into = c("Seeded", "Season"), sep = "/ ")

# Arrange vals of selected cols from lowest to highest
cloud_arrange <- cloudseed %>% 
  arrange(TE, TW, NC, SC, NWC)

# Arrange vals of selected cols in descending order
cloud_desc <- cloudseed %>% 
  arrange(desc(TE, TW, NC, SC, NWC))


