
library(Lahman)
library(tidyverse)
#Confounding: Are BBs More Predictive?

#Association is not causation – BB and runs are associated, 
#but HR is the confounding variable that actually causes the runs.

# Formula for the slope of the regression line = get_slope (DRY)----------
get_slope <- function(x, y) cor(x, y) * sd(y) / sd(x)

# Calculate the slope of the regression line for BB and R---------
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

# What is the actual correlation between BB, HR and Singles--------
Teams %>% 
  filter(yearID %in% 1961:2001) %>% 
  mutate(Singles = (H-HR-X2B-X3B)/G, BB = BB/G, HR = HR/G) %>%  
  summarize(cor(BB, HR), cor(Singles, HR), cor(BB, Singles))

# stratify HR per game to nearest 10, filter out strata with few points--------
dat <- Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(HR_strata = round(HR/G, 1), 
         BB_per_game = BB / G,
         R_per_game = R / G) %>%
  filter(HR_strata >= 0.4 & HR_strata <=1.2)

#scaterplot for each HR Stratum-----------
dat %>%
  ggplot(aes(BB_per_game, R_per_game))+
  geom_point(alpha=0.5)+
  geom_smooth(method=lm)+
  facet_wrap(~HR_strata)

#calculate slope of regressionline after stratifying by HR----------
dat%>%
  group_by(HR_strata)%>%
             summarise(slope=cor(BB_per_game, R_per_game)*sd(R_per_game)/sd(BB_per_game))
# The slopes vary considerably across HR strata.
# On average they are closer to 0.45 (singles slope) than 0.74 (BB without controlling for HR).
# This confirms that BB are useful but less so than originally thought - HR was confounding the relationship.
# However, the variation across strata is large, making stratification alone an imperfect approach.
# -> Solution: multivariate regression (next section)

#stratify by BB----
dat<- Teams %>%filter(yearID %in%1961:2001) %>%
  mutate(BB_strata = round(BB/G, 1),
         HR_per_game = HR/G,
         R_per_game = R / G) %>%
  filter(BB_strata >=2.8 & BB_strata <=3.9)

# scatterplot for each BB stratum
dat %>% ggplot(aes(HR_per_game, R_per_game)) +  
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm") +
  facet_wrap( ~ BB_strata)

#slope of regression line after stratifying by BB
dat %>%
  group_by(BB_strata) %>%
  summarise(slope = cor(HR_per_game, R_per_game)*sd(R_per_game)/sd(HR_per_game))

# HR slopes remain stable (~1.5-1.7) across all BB strata.
# This confirms that HR has a real, consistent effect on runs independent of BB.
# BB effect was largely due to confounding with HR.