
library(tidyverse)
library(HistData)
library(ggplot2)

# 1. DATA PREPARATION ------------------------------------------

data("GaltonFamilies")

# Select one son per family to ensure independence
set.seed(1983) 
# Fixes the random generator so that the sample is always identical.
# Important for reproducibility!

# compute RSS for any pair of beta0 and beta1 in Galton's data
galton_heights <- GaltonFamilies %>%
  filter(gender == "male") %>%
  group_by(family) %>%
  sample_n(1) %>%
  ungroup() %>%
  select(father, childHeight) %>%
  rename(son = childHeight)

# 1. LEAST SQUARES ESTIMATES -----------------------------------
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

# but this is not necesary true for all beta0.
#Solution? lm function -->

# fit regression line to predict son's height from father's height

#When calling the lm() function, the variable that we want to predict is put to the left of the ~ symbol, 
#and the variables that we use to predict is put to the right of the ~ symbol. 
#The intercept is added automatically.'

fit <- lm(son ~ father, data = galton_heights)
fit

# summary statistics
summary(fit)

#Add Monte Carlo simulation for LSE as random variables------

B<-1000
N<-50
# .$coef extracts only the coefficients table from the summary output
# (Estimate, Std. Error, t value, p-value)
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

# summary statistics
sample_n(galton_heights, N, replace = TRUE) %>% 
  lm(son ~ father, data = .) %>% 
  summary %>%
  .$coef

# Compare: standard error from a single sample (lm summary) vs. 
# standard error estimated from 1000 Monte Carlo simulations (sd of beta_0 and beta_1)
# Both should be similar - confirms that lm() accurately estimates the standard error

lse %>% summarize(se_0 = sd(beta_0), se_1 = sd(beta_1))

