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