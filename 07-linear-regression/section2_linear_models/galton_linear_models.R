
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
fit <- lm(son ~ father, data = galton_heights)
fit

# summary statistics
summary(fit)

