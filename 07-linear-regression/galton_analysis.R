#################################################################
# CASE STUDY: IS HEIGHT HEREDITARY? (GALTON FAMILIES)           #
#################################################################

library(tidyverse)
library(HistData)

# 1. DATA PREPARATION ------------------------------------------

data("GaltonFamilies")

# Select one son per family to ensure independence
set.seed(1983) 
# Fixes the random generator so that the sample is always identical.
# Important for reproducibility!

galton_heights <- GaltonFamilies %>%
  filter(gender == "male") %>%
  group_by(family) %>%
  sample_n(1) %>%
  ungroup() %>%
  select(father, childHeight) %>%
  rename(son = childHeight)


# 2. EXPLORATORY ANALYSIS --------------------------------------

# Mean and standard deviation (SD)
# SD measures the spread around the mean (unit: inches)
galton_heights %>%
  summarize(mean(father), sd(father), mean(son), sd(son))


# 3. CORRELATION (r) -------------------------------------------

# r describes direction and strength of the linear relationship (-1 to 1)
galton_heights %>% 
  summarize(r = cor(father, son))
# KEY RULE:
# r > 0 (positive): 'the more x, the more y' (example: father-son height)
# r < 0 (negative): 'the more x, the less y' (example: errors vs. wins)
# r = 0 (zero): no linear trend detectable

# Visualization with regression line (blue) and identity line (red)
galton_heights %>%
  ggplot(aes(father, son)) +
  geom_point(alpha = 0.5) +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") + 
  geom_smooth(method = "lm", color = "blue") +
  labs(title = "Regression to the mean",
       subtitle = "Blue: regression line | Red: 1-to-1 inheritance line")


# 4. CORRELATION AS A RANDOM VARIABLE (Monte Carlo) ------------
# The correlation of a sample is an estimate for the population

B <- 1000  # number of simulations
N <- 25    # small sample size
set.seed(1983)  # reproducibility of the Monte Carlo simulation

R <- replicate(B, {
  slice_sample(galton_heights, n = N, replace = TRUE) %>% 
    summarize(r = cor(father, son)) %>% 
    pull(r)
})

# Histogram of results
# Shows that r can vary a lot with small samples (high standard error)
data.frame(R) %>% 
  ggplot(aes(R)) + 
  geom_histogram(binwidth = 0.05, color = "black", fill = "steelblue") +
  labs(title = "Distribution of sample correlation (N=25)",
       x = "Calculated correlation (r)")

# Statistical check of the simulation
mean(R)  # should be close to the true correlation (~0.5)
sd(R)    # standard error of the correlation (relatively high for small N)

# QQ-plot to check for normality (central limit theorem)
# At N=25 the distribution is often not yet perfectly normal
# intercept = mean(R): the line passes through the center of the theoretical normal distribution
# slope: theoretical standard deviation of R (steepness if R were perfectly normally distributed)
data.frame(R) %>%
  ggplot(aes(sample = R)) +
  stat_qq() +
  geom_abline(intercept = mean(R), slope = sqrt((1-mean(R)^2)/(N-2))) +
  labs(title = "QQ-plot: normality check of sample correlation", 
       x = "Theoretical normal quantiles", 
       y = "Sample correlation R")


# 5. STRATIFICATION --------------------------------------------

# Number of fathers with height exactly 72 or 72.5 inches
sum(galton_heights$father == 72)
sum(galton_heights$father == 72.5)
# Too few fathers are exactly 72 or 72.5 inches tall
# -> standard error too large, not useful for prediction

# Predicted height of a son with a 72 inch tall father
conditional_avg <- galton_heights %>%
  filter(round(father) == 72) %>%
  summarize(avg = mean(son)) %>%
  pull(avg)
conditional_avg

# Stratify fathers' heights to make a boxplot of son heights
# Rounding creates groups with enough observations per stratum
galton_heights %>% 
  mutate(father_strata = factor(round(father))) %>%
  ggplot(aes(father_strata, son)) +
  geom_boxplot() +
  geom_point()

# Center of each boxplot (conditional average per stratum)
galton_heights %>%
  mutate(father = round(father)) %>%
  group_by(father) %>%
  summarize(son_conditional_avg = mean(son)) %>%
  ggplot(aes(father, son_conditional_avg)) +
  geom_point()

# Regression line on standardized data
# In standard units: intercept = 0, slope = r (rho)
r <- galton_heights %>% summarize(r = cor(father, son)) %>% pull(r)
r

galton_heights %>% 
  mutate(father = scale(father), son = scale(son)) %>%
  mutate(father = round(father)) %>%
  group_by(father) %>%
  summarize(son = mean(son)) %>%
  ggplot(aes(father, son)) + 
  geom_point() +
  geom_abline(intercept = 0, slope = r)

# Regression line on original data (in inches)
# mu = mean, s = standard deviation
mu_x <- mean(galton_heights$father)  # mean father height
mu_y <- mean(galton_heights$son)     # mean son height
s_x <- sd(galton_heights$father)     # SD father height
s_y <- sd(galton_heights$son)        # SD son height

# Correlation coefficient (rho) between fathers and sons
r <- cor(galton_heights$father, galton_heights$son)
# Slope of regression line in original units (accounts for regression to the mean)
m <- r * s_y / s_x
# Intercept of the regression line
b <- mu_y - m * mu_x

galton_heights %>% 
  ggplot(aes(father, son)) + 
  geom_point(alpha = 0.5) +
  geom_abline(intercept = b, slope = m) +
  labs(title = "Father-son height: regression line in original units", 
       x = "Father height (inches)", 
       y = "Son height (inches)")

# Regression line in standard units: intercept = 0, slope = rho
galton_heights %>% 
  ggplot(aes(scale(father), scale(son))) + 
  geom_point(alpha = 0.5) +
  geom_abline(intercept = 0, slope = r) +
  labs(title = "Father-son height: regression line in standard units", 
       x = "Father height (standard units)", 
       y = "Son height (standard units)")
