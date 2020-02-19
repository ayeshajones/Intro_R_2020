# BioStats chapter 2.2.1
# Day 1
# 19/02/20
# Ayesha Jones

# load packages
library(tidyverse)

# load dataset
chicks <- as_tibble(ChickWeight) # tibble means table in r tidyverse

# view data
head(chicks) # view first 6 rows
tail(chicks) # view last 6 rows
tail(chicks, n = 2) # only looks at last 2 rows
colnames(chicks) # gives col i.e. displays var names
summary(chicks) # gives 1st & 3rd quantile, min, mean, etc of each var i.e. cols

### CHAPTER 3
chicks %>% 
  summarise(length = n()) # finds legnth of dataset i.e. no of samples/rows

length(chicks$weight) # does same thing, better to se pipe

# measures of central tendency

dat1 <- c(23, 45, 23, 66, 13) # creat string of numbers that apprs in enviro
mean(dat1) # mean of that string

chicks %>% # can write df in script to known where to insert dataset name
  summarise(mean_wt = mean(weight)) # create new col and cal mean?

chicks %>% 
  summarise(mean_wt = round(mean(weight), 1)) # round off mean weigh to 1

chicks %>% 
  summarise(mean_wt = sum(weight) / n()) # same as cal'ing mean

# Task:
library(e1071)
skewness(faithful$eruptions)
R> [1] -0.4135498

chicks %>% 
  summarise(sd_wt = sd(weight)) # stand dev

quantile(chicks$weight) # gives you quantiles

# Ommitting the missing data
mean(dat1, na.rm = TRUE)

grp_stat <- chicks %>%
  filter(Time == 21) %>% 
  group_by(Diet, Time) %>% 
  summarise(mean_wt = round(mean(weight, na.rm = TRUE), 2),
            med_wt = median(weight, na.rm = TRUE),
            sd_wt = round(sd(weight, na.rm = TRUE), 2),
            sum_wt = sum(weight),
            min_wt = min(weight),
            qrt1_wt = quantile(weight, p = 0.25),
            med_wt = median(weight),
            qrt3_wt = median(weight, p = 0.75),
            max_wt = max(weight),
            n_wt = n())
grp_stat
# doesnt run

library(ggpubr) # needed for arranging multi-panel plots
plt1 <- chicks %>%
  filter(Time == 21) %>% 
  ggplot(aes(x = Diet, y = weight)) +
  geom_point(data = grp_stat, aes(x = Diet, y = mean_wt), 
             col = "black", fill = "red", shape = 23, size = 3) +
  geom_jitter(width = 0.05) + # geom_point() if jitter not required 
  labs(y = "Chicken mass (g)") + 
  theme_pubr()

plt2 <- ggplot(data = grp_stat, aes(x = Diet, y = mean_wt)) +
  geom_bar(position = position_dodge(), stat = "identity", 
           col = NA, fill = "salmon") +
  geom_errorbar(aes(ymin = mean_wt - sd_wt, ymax = mean_wt + sd_wt),
                width = .2) +
  labs(y = "Chicken mass (g)") + 
  theme_pubr()
# position_dodge() places bars side-by-side
# stat = "identity" prevents the default count from being plotted

# a description of the components of a boxplot is provided in the help file
# geom_boxplot()
plt3 <- chicks %>%
  filter(Time == 21) %>% 
  ggplot(aes(x = Diet, y = weight)) +
  geom_boxplot(fill = "salmon") +
  geom_jitter(width = 0.05, fill = "white", col = "blue", shape = 21) +
  labs(y = "Chicken mass (g)") + 
  theme_pubr()
plt3

plt4 <- chicks %>%
  filter(Time %in% c(10, 21)) %>% 
  ggplot(aes(x = Diet, y = weight, fill = as.factor(Time))) +
  geom_boxplot() +
  geom_jitter(shape = 21, width = 0.1) +
  labs(y = "Chicken mass (g)", fill = "Time") +
  theme_pubr()

ggarrange(plt1, plt2, plt3, plt4, ncol = 2, nrow = 2, labels = "AUTO")

# Can attempt: 
# Notice how the data summary for chicken weights contained within wt_summary is very similar 
# to the summary returned for weight when we apply summary(chicks).
# Please use the summarise() approach and construct a data summary with exactly the same 
# summary statistics for weight as that which summary() returns.

