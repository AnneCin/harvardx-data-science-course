
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

#Assesment: Least Squares Estimates, part 1----------

# Q3: Multivariate regression - predicting runs from BB AND HR
# When controlling for HR, the BB coefficient drops from 0.74 to 0.39
# This confirms that HR was confounding the BB-runs relationship
Teams %>%
  filter(yearID %in% 1961:2001) %>%
  mutate(BB_per_game = BB/G, 
         R_per_game = R/G,
         HR_per_game = HR/G) %>%
  lm(R_per_game ~ BB_per_game + HR_per_game, data = .) %>%
  .$coef

#Assessment: Least Squares Estimates, part 2--------------

#Before we get started, we want to generate two tables: 
#one for 2002 and another for the average of 1999-2001 seasons. 
#We want to define per plate appearance statistics, 
#keeping only players with more than 100 plate appearances. 
#Here is how we create the 2002 table:

bat_02 <- Batting %>% filter(yearID == 2002) %>%
  mutate(pa = AB + BB, singles = (H - X2B - X3B - HR)/pa, bb = BB/pa) %>%
  filter(pa >= 100) %>%
  select(playerID, singles, bb)


# Q9: Stability of singles and BB rates 1999-2001
bat_99_01 <- Batting %>%
  filter(yearID %in% 1999:2001) %>%
  mutate(pa = AB + BB,
         singles = (H - X2B - X3B - HR) / pa,
         bb = BB / pa) %>%
  filter(pa >= 100) %>%
  group_by(playerID) %>%
  summarize(mean_singles = mean(singles),
            mean_bb = mean(bb))

# Q10: Join 2002 data with 1999-2001 averages and calculate correlations
bat_joined <- inner_join(bat_02, bat_99_01, by = "playerID")

bat_joined %>%
  summarize(cor(singles, mean_singles),
            cor(bb, mean_bb))

#Q11 Make scatterplots of mean_singles versus singles and mean_bb versus bb.

bat_joined %>%
  ggplot(aes(mean_bb, bb)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm")

bat_joined %>%
  ggplot(aes(mean_singles, singles)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm")

#Q12 Fit a linear model to predict 2002 singles given 1999-2001 mean_singles.
#What is the coefficient of mean_singles, the slope of the fit?

# 1. Fit linear model for singles and print the coefficients
fit_singles <- lm(singles ~ mean_singles, data = bat_joined)
coef(fit_singles) # The second value (mean_singles) is the slope coefficient

# 2. Fit linear model for bases on balls (bb) and print the coefficients
fit_bb <- lm(bb ~ mean_bb, data = bat_joined)
coef(fit_bb)      # The second value (mean_bb) is the slope coefficient


# ==============================================================================
# SECTION 3: ADVANCED DPLYR - SUMMARIZE WITH FUNCTIONS AND BROOM
# ==============================================================================

# stratify by HR
dat <- Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(HR = round(HR/G, 1), 
         BB = BB/G,
         R = R/G) %>%
  select(HR, BB, R) %>%
  filter(HR >= 0.4 & HR<=1.2)

# calculate slope of regression lines to predict runs by BB in different HR strata
dat %>%  
  group_by(HR) %>%
  summarize(slope = cor(BB,R)*sd(R)/sd(BB))

# use lm to get estimated slopes - lm does not work with grouped tibbles
dat %>%  
  group_by(HR) %>%
  lm(R ~ BB, data = .) %>%
  .$coef

# include the lm inside a summarize and it will work
dat %>%  
  group_by(HR) %>%
  summarize(slope = lm(R ~ BB)$coef[2])

# tidy function from broom returns estimates in and information in a data frame
library(broom)
fit <- lm(R ~ BB, data = dat)
tidy(fit)

# add confidence intervals
tidy(fit, conf.int = TRUE)

# combine with group_by and summarize to get the table we want
dat %>%  
  group_by(HR) %>%
  summarize(tidy(lm(R ~ BB), conf.int = TRUE))

# it's a data frame so we can filter and select the rows and columns we want
dat %>%  
  group_by(HR) %>%
  summarize(tidy(lm(R ~ BB), conf.int = TRUE)) %>%
  filter(term == "BB") %>%
  select(HR, estimate, conf.low, conf.high)

# visualize the table with ggplot
dat %>%  
  group_by(HR) %>%
  summarize(tidy(lm(R ~ BB), conf.int = TRUE)) %>%
  filter(term == "BB") %>%
  select(HR, estimate, conf.low, conf.high) %>%
  ggplot(aes(HR, y = estimate, ymin = conf.low, ymax = conf.high)) +
  geom_errorbar() +
  geom_point()

#Assessment: Advanced dplyr, part 1

#Q5 You want to take the tibble dat, which we used in the video on the advanced dplyr, 
#and run the linear model R ~ BB for each strata of HR. 
#Then you want to add three new columns to your grouped tibble: 
#the coefficient, standard error, and p-value for the BB term in the model.

get_slope <- function(data) {
  fit <- lm(R ~ BB, data = data)
  sum.fit <- summary(fit)
  
  data.frame(slope = sum.fit$coefficients[2, "Estimate"], 
             se = sum.fit$coefficients[2, "Std. Error"],
             pvalue = sum.fit$coefficients[2, "Pr(>|t|)"])
}

dat <- Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(HR = round(HR/G, 1), 
         BB = BB/G,
         R = R/G) %>%
  select(HR, BB, R) %>%
  filter(HR >= 0.4 & HR<=1.2)

#This will create a tibble with four columns: HR, slope, se, and pvalue for each level of HR.
dat %>% 
  group_by(HR) %>% 
  summarize(get_slope(across()))

#Q7You want to know whether the relationship between home runs and runs per game varies by baseball league.

dat <- Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(HR = HR/G,
         R = R/G) %>%
  select(lgID, HR, BB, R)

dat %>% 
  group_by(lgID) %>% 
  summarize(broom::tidy(lm(R ~ HR, data = across()), conf.int = TRUE)) %>% 
  filter(term == "HR") 
#Die Stärke des Effekts: In der American League bringt jeder Home Run pro Spiel dem Team im Schnitt 1,90 zusätzliche Runs. 
#In der National League sind es 1,76 zusätzliche Runs.