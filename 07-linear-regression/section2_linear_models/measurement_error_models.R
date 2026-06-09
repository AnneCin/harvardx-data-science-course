# ==============================================================================
# SECTION 3.1: MEASUREMENT ERROR MODELS (GALILEO'S FALLING OBJECT)
# ==============================================================================

# THEORETICAL BACKGROUND:
# Unlike previous examples with natural biological variability (e.g., heights), 
# this model estimates a deterministic physical law using data with measurement errors.
# The physics formula is: d = h_0 + v_0*t - 0.5*g*t^2
# Where t (time) is a fixed non-random covariate, and randomness comes solely 
# from the camera's or device's measurement error.

library(dslabs)
library(tidyverse)
library(broom) # provides tidy() and augment() to clean up model outputs

# 1. Generate simulated data for a dropping ball experiment
falling_object <- rfalling_object()

# 2. Visualize the trajectory of the falling ball
falling_object %>%
  ggplot(aes(time, observed_distance)) +
  geom_point() +
  ylab("Distance in meters") +
  xlab("Time in seconds")

# 3. Fit a quadratic regression model using lm()
# We add a time-squared column because the physical relationship is parabolic.
fit <- falling_object %>%
  mutate(time_sq = time^2) %>%
  lm(observed_distance ~ time + time_sq, data = .)

# 4. Clean up the regression summary into a tidy data frame
# Look at the p-values: 
# - time_sq (gravity factor) is highly significant (p-value close to 0).
# - time (initial velocity) is not significant (high p-value), meaning v_0 was likely 0.
tidy(fit, conf.int = TRUE)

# 5. Overlay the estimated regression parabola onto the actual noisy data points
augment(fit) %>%
  ggplot() +
  geom_point(aes(time, observed_distance)) +
  geom_line(aes(time, .fitted), col = "blue") +
  ylab("Distance in meters") +
  xlab("Time in seconds")