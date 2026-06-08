library(Lahman)
library(tidyverse)

# ================================================================
# COURSE CONTENT
# ================================================================

# 1. CONFOUNDING: ARE BBs MORE PREDICTIVE? ----------------------
# Association is not causation – BB and runs are associated,
# but HR is the confounding variable that actually causes the runs.

# Formula for the slope of the regression line (DRY)
get_slope <- function(x, y) cor(x, y) * sd(y) / sd(x)

# Slope of regression line for BB and R
bb_slope <- Teams %>% 
  filter(yearID %in% 1961:2001) %>% 
  mutate(BB_per_game = BB/G, R_per_game = R/G) %>% 
  summarize(slope = get_slope(BB_per_game, R_per_game))

# Slope of regression line for Singles and R
singles_slope <- Teams %>% 
  filter(yearID %in% 1961:2001) %>% 
  mutate(Singles_per_game = (H - HR - X2B - X3B) / G, 
         R_per_game = R / G) %>% 
  summarize(slope = get_slope(Singles_per_game, R_per_game))

# Correlation between BB, HR and Singles
Teams %>% 
  filter(yearID %in% 1961:2001) %>% 
  mutate(Singles = (H-HR-X2B-X3B)/G, BB = BB/G, HR = HR/G) %>%  
  summarize(cor(BB, HR), cor(Singles, HR), cor(BB, Singles))

# Stratify HR per game to nearest 10, filter out strata with few points
dat <- Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(HR_strata = round(HR/G, 1), 
         BB_per_game = BB / G,
         R_per_game = R / G) %>%
  filter(HR_strata >= 0.4 & HR_strata <= 1.2)

# Scatterplot for each HR stratum
dat %>%
  ggplot(aes(BB_per_game, R_per_game)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm") +
  facet_wrap(~HR_strata)

# Slope of regression line after stratifying by HR
dat %>%
  group_by(HR_strata) %>%
  summarise(slope = cor(BB_per_game, R_per_game)*sd(R_per_game)/sd(BB_per_game))
# The slopes vary considerably across HR strata.
# On average they are closer to 0.45 (singles slope) than 0.74 (BB without controlling for HR).
# This confirms that BB are useful but less so than originally thought.
# -> Solution: multivariate regression (next section)

# Stratify by BB
dat <- Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(BB_strata = round(BB/G, 1),
         HR_per_game = HR/G,
         R_per_game = R / G) %>%
  filter(BB_strata >= 2.8 & BB_strata <= 3.9)

# Scatterplot for each BB stratum
dat %>% ggplot(aes(HR_per_game, R_per_game)) +  
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm") +
  facet_wrap(~BB_strata)

# Slope of regression line after stratifying by BB
dat %>%
  group_by(BB_strata) %>%
  summarise(slope = cor(HR_per_game, R_per_game)*sd(R_per_game)/sd(HR_per_game))
# HR slopes remain stable (~1.5-1.7) across all BB strata.
# This confirms that HR has a real, consistent effect on runs independent of BB.

# 2. ADVANCED DPLYR: SUMMARIZE WITH FUNCTIONS AND BROOM ---------

# Stratify by HR
dat <- Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(HR = round(HR/G, 1), 
         BB = BB/G,
         R = R/G) %>%
  select(HR, BB, R) %>%
  filter(HR >= 0.4 & HR <= 1.2)

# Calculate slope of regression lines to predict runs by BB in different HR strata
dat %>%  
  group_by(HR) %>%
  summarize(slope = cor(BB, R)*sd(R)/sd(BB))

# lm does not work with grouped tibbles directly
dat %>%  
  group_by(HR) %>%
  lm(R ~ BB, data = .) %>%
  .$coef

# Include lm inside summarize
dat %>%  
  group_by(HR) %>%
  summarize(slope = lm(R ~ BB)$coef[2])

# tidy() from broom returns estimates in a data frame
library(broom)
fit <- lm(R ~ BB, data = dat)
tidy(fit)
tidy(fit, conf.int = TRUE)

# Combine with group_by and summarize
dat %>%  
  group_by(HR) %>%
  summarize(tidy(lm(R ~ BB), conf.int = TRUE))

# Filter and select relevant rows
dat %>%  
  group_by(HR) %>%
  summarize(tidy(lm(R ~ BB), conf.int = TRUE)) %>%
  filter(term == "BB") %>%
  select(HR, estimate, conf.low, conf.high)

# Visualize with ggplot
dat %>%  
  group_by(HR) %>%
  summarize(tidy(lm(R ~ BB), conf.int = TRUE)) %>%
  filter(term == "BB") %>%
  select(HR, estimate, conf.low, conf.high) %>%
  ggplot(aes(HR, y = estimate, ymin = conf.low, ymax = conf.high)) +
  geom_errorbar() +
  geom_point()

# ================================================================
# EXERCISES & PRACTICE
# ================================================================

# Multivariate regression: predicting runs from BB AND HR
# When controlling for HR, the BB coefficient drops from 0.74 to 0.39
# This confirms that HR was confounding the BB-runs relationship
Teams %>%
  filter(yearID %in% 1961:2001) %>%
  mutate(BB_per_game = BB/G, 
         R_per_game = R/G,
         HR_per_game = HR/G) %>%
  lm(R_per_game ~ BB_per_game + HR_per_game, data = .) %>%
  .$coef

# Stability of singles and BB rates 1999-2001
bat_02 <- Batting %>% filter(yearID == 2002) %>%
  mutate(pa = AB + BB, singles = (H - X2B - X3B - HR)/pa, bb = BB/pa) %>%
  filter(pa >= 100) %>%
  select(playerID, singles, bb)

bat_99_01 <- Batting %>%
  filter(yearID %in% 1999:2001) %>%
  mutate(pa = AB + BB,
         singles = (H - X2B - X3B - HR) / pa,
         bb = BB / pa) %>%
  filter(pa >= 100) %>%
  group_by(playerID) %>%
  summarize(mean_singles = mean(singles),
            mean_bb = mean(bb))

# Join 2002 data with 1999-2001 averages and calculate correlations
# BB is more stable over time than singles (0.718 vs 0.551)
bat_joined <- inner_join(bat_02, bat_99_01, by = "playerID")
bat_joined %>%
  summarize(cor(singles, mean_singles),
            cor(bb, mean_bb))

# Scatterplots: are both distributions bivariate normal?
bat_joined %>%
  ggplot(aes(mean_bb, bb)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm")

bat_joined %>%
  ggplot(aes(mean_singles, singles)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm")

# Linear models: slope of singles and BB
fit_singles <- lm(singles ~ mean_singles, data = bat_joined)
coef(fit_singles)

fit_bb <- lm(bb ~ mean_bb, data = bat_joined)
coef(fit_bb)

# Custom function to extract slope, SE and p-value per HR stratum
get_slope <- function(data) {
  fit <- lm(R ~ BB, data = data)
  sum.fit <- summary(fit)
  data.frame(slope = sum.fit$coefficients[2, "Estimate"], 
             se = sum.fit$coefficients[2, "Std. Error"],
             pvalue = sum.fit$coefficients[2, "Pr(>|t|)"])
}

dat %>% 
  group_by(HR) %>% 
  summarize(get_slope(across()))

# HR effect on runs per game by baseball league
dat <- Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(HR = HR/G, R = R/G) %>%
  select(lgID, HR, BB, R)

# American League: +1.90 runs per HR | National League: +1.76 runs per HR
dat %>% 
  group_by(lgID) %>% 
  summarize(broom::tidy(lm(R ~ HR, data = across()), conf.int = TRUE)) %>% 
  filter(term == "HR")