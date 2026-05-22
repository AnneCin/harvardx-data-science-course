#################################################################
# CASE STUDY: IS HEIGHT HEREDITARY? (GALTON FAMILIES)           #
#################################################################

library(tidyverse)
library(HistData)
library(ggplot2)

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

# 6. BIVARIATE NORMAL DISTRIBUTION ----------------------------

galton_heights %>%
  mutate(z_father = round((father - mean(father))/sd(father))) %>%
  filter(z_father %in% -2:2) %>%
  ggplot() +  
  stat_qq(aes(sample=son)) +
  facet_wrap(~z_father)+
  labs(title = "Distribution in every Father Group", 
                             x = "Theoretical quantiles", 
                             y = "Son height")
# The son heights are normally distributed in every father group.
# This confirms the bivariate normal distribution assumption for our Galton data.

# 7. Section 8: Warning - there are two regression lines ----------------------------

#
# 1. Fit both linear models
# Model A: Predicting son's height from father's height (Vertical errors)
fit_son_from_father <- lm(son ~ father, data = galton_heights)

# Model B: Predicting father's height from son's height (Horizontal errors)
fit_father_from_son <- lm(father ~ son, data = galton_heights)

# 2. Extract coefficients for plotting Model B correctly on the same axes
# Model B is: father = intercept + slope * son
# Rearranged for the plot axes: son = (-intercept / slope) + (1 / slope) * father
intercept_b <- coef(fit_father_from_son)[1]
slope_b     <- coef(fit_father_from_son)[2]

# 2b. Course approach: Calculate the second line manually using formula
# Formula from course: predicting father's height (X) from son's height (Y)
m_father_from_son <- r * s_x / s_y
b_father_from_son <- mu_x - m_father_from_son * mu_y


# 3. Visualize the two distinct regression lines

# KEY LESSON:
# Why are there two regression lines?
# 1. 'lm(son ~ father)' minimizes VERTICAL errors (deviations in son's height).
# 2. 'lm(father ~ son)' minimizes HORIZONTAL errors (deviations in father's height).
# Because correlation (r) is imperfect (r < 1), these two optimization goals 
# result in different slopes. They only intersect at the mean coordinates (X_bar, Y_bar).
# This demonstrates 'Regression to the Mean': extraordinary fathers tend to 
# have more average sons, and vice versa.

ggplot(galton_heights, aes(x = father, y = son)) +
  geom_point(alpha = 0.3, color = "darkgray") +
  # Line 1: lm(son ~ father) - standard vertical minimization
  geom_smooth(method = "lm", se = FALSE, color = "blue", linetype = "solid") +
  # Line 2: lm(father ~ son) - rearranged for horizontal minimization
  geom_abline(intercept = -intercept_b / slope_b, 
              slope = 1 / slope_b, 
              color = "red", linetype = "dashed") +
  # Reference point: The intersection at the means (X_bar, Y_bar)
  geom_point(x = mean(galton_heights$father), y = mean(galton_heights$son), color = "black", size = 4, shape = 18)+
  labs(
    title = "The Two Regression Lines in Galton's Data",
    subtitle = "Blue: son ~ father (vertical) | Red: father ~ son (horizontal)",
    x = "Father's Height",
    y = "Son's Height"
  ) +
  theme_minimal()

# 8. CASE STUDY: MOTHER & DAUGHTER HEIGHTS (FEMALES) ----------------

data("GaltonFamilies")
# Set the seed with the "Rounding" sampler to match the HarvardX environment
# Note: This must be executed right before sampling to ensure exact reproducibility!
set.seed(1989, sample.kind = "Rounding")

# # Create the female heights dataset by filtering and selecting one daughter per family
female_heights <- GaltonFamilies %>%     
  filter(gender == "female") %>%     
  group_by(family) %>%     
  sample_n(1) %>%     
  ungroup() %>%     
  select(mother, childHeight) %>%     
  rename(daughter = childHeight)

# Calculate and print means
mean_mother   <- mean(female_heights$mother)
mean_daughter <- mean(female_heights$daughter)

# Calculate and print standard deviations
sd_mother     <- sd(female_heights$mother)
sd_daughter   <- sd(female_heights$daughter)

# Calculate and print correlation coefficient
r_female      <- cor(female_heights$mother, female_heights$daughter)

# Display all values in the console
mean_mother
mean_daughter
sd_mother
sd_daughter
r_female

#calculate the slope of regression predicting daughters hights from mothers hights
# Formula: m = r * (sd_y / sd_x)
m_female <- female_heights %>% 
  summarize(m = cor(mother, daughter) * (sd(daughter) / sd(mother))) %>% 
  pull(m)
m_female
#calculate intercept of regression line predicting daughters hights from mothers heights
# Formula: b = mean_y - m * mean_x
b_female <- female_heights %>%
  summarise(b=mean(daughter)  - m_female*mean(mother)) %>%
  pull(b)
b_female

#calculate change in daughters height in inches given a 1inch increase in the mothers heigh
#Note: This is exactly the definition of the slope (m_female)!
change_per_inch <- m_female
change_per_inch

#calculate the percentage of the variability in daughters heights - explained by the mothers heights
# Note: The explained variance (R-squared) in a simple linear regression is exactly r^2.
# We multiply by 100 to get the percentage.
r_squared_percent <- (r_female^2) * 100
r_squared_percent

#A mother has a height of 60 inches.
#Using the regression formula, what is the conditional expected value of her daughter's height given the mother's height?
# Formula: expected_y = b + m * x
mother_height_input <- 60
expected_daughter_height <- b_female + m_female * mother_height_input
expected_daughter_height
