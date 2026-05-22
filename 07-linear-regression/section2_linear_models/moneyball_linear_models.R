
library(Lahman)
library(tidyverse)
#Confounding: Are BBs More Predictive?

#Association is not causation – BB and runs are associated, 
#but HR is the confounding variable that actually causes the runs.

# Formula for the slope of the regression line = get_slope (DRY)
get_slope <- function(x, y) cor(x, y) * sd(y) / sd(x)

# Calculate the slope of the regression line for BB and R
bb_slope <- Teams %>% 
  filter(yearID %in% 1961:2001) %>% 
  mutate(BB_per_game = BB/G, R_per_game = R/G) %>% 
  summarize(slope = get_slope(BB_per_game, R_per_game))

# Calculate the slope of the regression line for Singles and R
singles_slope <- Teams %>% 
  filter(yearID %in% 1961:2001) %>% 
  mutate(Singles_per_game = (H - HR - X2B - X3B) / G, 
         R_per_game = R / G) %>% 
  summarize(slope = get_slope(Singles_per_game, R_per_game))

# What is the actual correlation between BB, HR and Singles
Teams %>% 
  filter(yearID %in% 1961:2001) %>% 
  mutate(Singles = (H-HR-X2B-X3B)/G, BB = BB/G, HR = HR/G) %>%  
  summarize(cor(BB, HR), cor(Singles, HR), cor(BB, Singles))