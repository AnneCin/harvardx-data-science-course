library(tidyverse)
library(HistData)
library(ggplot2)
# ================================================================
# COURSE CONTENT
# ================================================================
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

# 2. LEAST SQUARES ESTIMATES -----------------------------------
# RSS function: calculates the sum of squared residuals
rss <- function(beta0, beta1, x, y){
  resid <- y - (beta0 + beta1*x)
  return(sum(resid^2))
}

# Plot RSS as a function of beta1 when beta0 = 25
beta1 <- seq(0, 1, len = nrow(galton_heights))
results <- data.frame(
  beta1 = beta1,
  rss = sapply(beta1, rss, beta0 = 25, 
               x = galton_heights$father, 
               y = galton_heights$son)
)
results %>% 
  ggplot(aes(beta1, rss)) + 
  geom_line() +
  labs(title = "RSS as a function of beta1 (beta0 fixed at 25)",
       x = "beta1", y = "RSS")
# but this is not necessarily true for all beta0.

# Solution: lm function
# The variable to predict goes left of ~, predictors go right.
# The intercept is added automatically.
fit <- lm(son ~ father, data = galton_heights)
fit
summary(fit)

# 3. MONTE CARLO SIMULATION: LSE ARE RANDOM VARIABLES ----------
B <- 1000
N <- 50

# .$coef extracts only the coefficients table from the summary output
lse <- replicate(B, {
  sample_n(galton_heights, N, replace = TRUE) %>% 
    lm(son ~ father, data = .) %>% 
    .$coef
})

# Convert matrix output to a tidy data frame with named columns for beta_0 and beta_1
lse <- data.frame(beta_0 = lse[1,], beta_1 = lse[2,]) 

# Plot the distribution of beta_0 and beta_1
library(gridExtra)
p1 <- lse %>% ggplot(aes(beta_0)) + geom_histogram(binwidth = 5, color = "black") 
p2 <- lse %>% ggplot(aes(beta_1)) + geom_histogram(binwidth = 0.1, color = "black") 
grid.arrange(p1, p2, ncol = 2)

# Compare: standard error from a single sample vs. 1000 Monte Carlo simulations
# Both should be similar - confirms that lm() accurately estimates the standard error
sample_n(galton_heights, N, replace = TRUE) %>% 
  lm(son ~ father, data = .) %>% 
  summary %>%
  .$coef

lse %>% summarize(se_0 = sd(beta_0), se_1 = sd(beta_1))

# 4. ADVANCED NOTE: CORRELATION BETWEEN LSE --------------------
# beta_0 and beta_1 are strongly negatively correlated (~-1)
# because if slope increases, intercept must decrease to fit the data
lse %>% summarize(cor(beta_0, beta_1))

# Centering the predictor reduces this correlation
# When father is centered (father - mean(father)), beta_0 becomes
# the predicted son height for an average father -> more interpretable
lse_centered <- replicate(B, {
  sample_n(galton_heights, N, replace = TRUE) %>%
    mutate(father = father - mean(father)) %>%
    lm(son ~ father, data = .) %>% 
    .$coef 
})
cor(lse_centered[1,], lse_centered[2,])

# 5. PREDICTED VALUES AS RANDOM VARIABLES ---------------------------

# 1. Fit the linear model using the full dataset
# Predicting son's height based on father's height
fit_son <- lm(son ~ father, data = galton_heights)

# 2. Extract predictions (y-hat) and standard errors
# The predict() function returns these statistical values
predictions <- predict(fit_son, se.fit = TRUE)

# Inspect the structure of the prediction object
# Useful for assessments to know what components are available (e.g., "fit", "se.fit")
names(predictions)
head(predictions$fit)     # The predicted values (y-hat)
head(predictions$se.fit)  # The standard errors of the predictions

# 3. Manual prediction approach vs. automated plotting
# Adding manually calculated Y_hat to the dataframe to plot the best fit line directly
galton_heights_with_pred <- galton_heights %>%
  mutate(Y_hat = predict(lm(son ~ father, data = .)))

# 4. Visualization of predictions with their confidence intervals
# Note: geom_smooth(method = "lm") automatically calculates Y_hat and standard errors
galton_heights_with_pred %>%
  ggplot(aes(father, son)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "blue", fill = "lightgray") +
  # This manual line matches the geom_smooth center line exactly:
  geom_line(aes(father, Y_hat), color = "darkred", linetype = "dashed", alpha = 0.7) +
  labs(
    title = "Predictions and Confidence Intervals",
    subtitle = "The shaded band represents the uncertainty (Standard Errors) of Y-hat",
    x = "Father's Height (inches)",
    y = "Son's Height (inches)"
  ) +
  theme_minimal()

# ================================================================
# EXERCISES & PRACTICE
# ================================================================
#Assessment: Least Squares Estimates, part 1----------'

#Find beta1 that minimizes RSS when beta0 = 36
beta1_test <- seq(0, 1, len = nrow(galton_heights))

results_test <- data.frame(
  beta1 = beta1_test,
  rss = sapply(beta1_test, rss, beta0 = 36, 
               x = galton_heights$father, 
               y = galton_heights$son)
)

# Find the beta1 value where RSS is at its absolute minimum
best_beta1 <- results_test$beta1[which.min(results_test$rss)]
best_beta1


# Plot predictions and confidence intervals
galton_heights %>% ggplot(aes(father, son)) +
  geom_point() +
  geom_smooth(method = "lm")

#Assessment: Least Squares Estimates, part 2----------
#Define female_heights, a set of mother and daughter heights sampled from GaltonFamilies, as follows:

set.seed(1989) 
set.seed(1989, sample.kind="Rounding") 
library(HistData)
data("GaltonFamilies")
options(digits = 3)    # report 3 significant digits

female_heights <- GaltonFamilies %>%     
  filter(gender == "female") %>%     
  group_by(family) %>%     
  sample_n(1) %>%     
  ungroup() %>%     
  select(mother, childHeight) %>%     
  rename(daughter = childHeight)

# Fit a linear regression model predicting the mothers' heights using daughters' heights.
# What is the slope of the model? What the intercept of the model?
fit_female <- lm(mother ~ daughter, data = female_heights)
summary(fit_female)

# Predict mothers' heights and the predict() function.
# What is the predicted height of the first mother in the dataset? What is the actual height of the first mother in the dataset?

# This calculates Y-hat (predicted values) for every daughter in the dataset
predicted_mother_heights <- predict(fit_female)
# 2. Extract the predicted value for the first mother in the dataset
print(predicted_mother_heights[1])
# 3. Extract the actual (true) height of the first mother in the dataset
print(female_heights$mother[1])

# Assessment: Advanced dplyr, part 2----------
# We have investigated the relationship between fathers' heights and sons' heights. 
# But what about other parent-child relationships? 
# Does one parent's height have a stronger association with child height? 
# How does the child's gender affect this relationship in heights? 
# Are any differences that we observe statistically significant?
  
# The galton dataset is a sample of one male and one female child from each family in the GaltonFamilies dataset. 
# The pair column denotes whether the pair is father and daughter, father and son, mother and daughter, or mother and son.

# set.seed(1) 
set.seed(1, sample.kind = "Rounding") # if you are using R 3.6 or later
galton <- GaltonFamilies %>%
  group_by(family, gender) %>%
  sample_n(1) %>%
  ungroup() %>% 
  gather(parent, parentHeight, father:mother) %>%
  mutate(child = ifelse(gender == "female", "daughter", "son")) %>%
  unite(pair, c("parent", "child"))

galton

# Group by pair and summarize the number of observations in each group.
# How many father-daughter pairs are in the dataset?
# How many mother-son pairs are in the dataset?
pair_counts <- galton %>%
  group_by(pair) %>%
  summarize(count = n())

# Calculate the correlation coefficients for fathers and daughters, fathers and sons, mothers and daughters and mothers and sons.
# Which pair has the strongest correlation in heights?
# Which pair has the weakest correlation in heights?

# Group by the combined pair column and calculate the correlation 
# between parentHeight and childHeight
pair_correlations <- galton %>%
  group_by(pair) %>%
  summarize(correlation = cor(parentHeight, childHeight))

# Print the sorted results to easily identify the strongest and weakest correlation
pair_correlations %>% arrange(desc(correlation))

# What is the estimate of the father-daughter coefficient?
# For every 1-inch increase in mother's height, how many inches does the typical son's height increase?

# Linear regression coefficients for each parent-child pair ---
# Group by pair and use broom::tidy to run a regression model for each group
  pair_regressions <- galton %>%
  group_by(pair) %>%
  summarize(broom::tidy(lm(childHeight ~ parentHeight, data = across()), conf.int = TRUE)) %>%
  filter(term == "parentHeight") %>%
  select(pair, estimate, std.error, conf.low, conf.high)

# Print the table to the console
print(pair_regressions)