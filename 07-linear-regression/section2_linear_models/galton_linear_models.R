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

#Assesment: Least Squares Estimates, part 1----------'

#Q1: Find beta1 that minimizes RSS when beta0 = 36
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

#Q3 Load the Lahman library and filter the Teams data frame to the years 1961-2001. 
#Mutate the dataset to create variables for 
#bases on balls per game, runs per game, and home runs per game, 
#then run a linear model in R predicting the number of runs per game 
#based on both the number of bases on balls per game and the number of home runs per game.

teams_filtered <- Teams %>% 
  filter(yearID %in% 1961:2001) %>%
  mutate(BB_per_game = BB / G,
         R_per_game  = R / G,
         HR_per_game = HR / G)

fit_baseball_q3 <- lm(R_per_game ~ BB_per_game + HR_per_game, data = teams_filtered)
summary(fit_baseball_q3)

# Assessment Q5: Plot predictions and confidence intervals
# Option 2: geom_smooth(method = "lm") automatically plots confidence intervals
galton_heights %>% ggplot(aes(father, son)) +
  geom_point() +
  geom_smooth(method = "lm")

#Assesment: Least Squares Estimates, part 2----------
#In Questions 7 and 8, I'll look again at female heights from GaltonFamilies.
#Define female_heights, a set of mother and daughter heights sampled from GaltonFamilies, as follows:

set.seed(1989) #if you are using R 3.5 or earlier
set.seed(1989, sample.kind="Rounding") #if you are using R 3.6 or later
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

#Q7Fit a linear regression model predicting the mothers' heights using daughters' heights.
#What is the slope of the model? What the intercept of the model?
fit_female <- lm(mother ~ daughter, data = female_heights)
summary(fit_female)

#Q8Predict mothers' heights using the model from Question 7 and the predict() function.
# What is the predicted height of the first mother in the dataset? What is the actual height of the first mother in the dataset?

# This calculates Y-hat (predicted values) for every daughter in the dataset
predicted_mother_heights <- predict(fit_female)
# 2. Extract the predicted value for the first mother in the dataset
print(predicted_mother_heights[1])
# 3. Extract the actual (true) height of the first mother in the dataset
print(female_heights$mother[1])