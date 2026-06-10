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

# Include lm inside summarize and it will work
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

# 3. Regression and BAseball----
# BUilding a Better Offensive Metric for Baseball
# linear regression with two variables
fit <- Teams %>% 
  filter(yearID %in% 1961:2001) %>% 
  mutate(BB = BB/G, HR = HR/G,  R = R/G) %>%  
  lm(R ~ BB + HR, data = .)
tidy(fit, conf.int = TRUE)

# regression with BB, singles, doubles, triples, HR
fit <- Teams %>% 
  filter(yearID %in% 1961:2001) %>% 
  mutate(BB = BB / G, 
         singles = (H - X2B - X3B - HR) / G, 
         doubles = X2B / G, 
         triples = X3B / G, 
         HR = HR / G,
         R = R / G) %>%  
  lm(R ~ BB + singles + doubles + triples + HR, data = .)
coefs <- tidy(fit, conf.int = TRUE)
coefs

# predict number of runs for each team in 2002 and plot
Teams %>% 
  filter(yearID %in% 2002) %>% 
  mutate(BB = BB/G, 
         singles = (H-X2B-X3B-HR)/G, 
         doubles = X2B/G, 
         triples =X3B/G, 
         HR=HR/G,
         R=R/G)  %>% 
  mutate(R_hat = predict(fit, newdata = .)) %>%
  ggplot(aes(R_hat, R, label = teamID)) + 
  geom_point() +
  geom_text(nudge_x=0.1, cex = 2) + 
  geom_abline()

# average number of team plate appearances per game
pa_per_game <- Batting %>% filter(yearID == 2002) %>% 
  group_by(teamID) %>%
  summarize(pa_per_game = sum(AB+BB)/max(G)) %>% 
  pull(pa_per_game) %>% 
  mean

# compute per-plate-appearance rates for players available in 2002 using previous data
players <- Batting %>% filter(yearID %in% 1999:2001) %>% 
  group_by(playerID) %>%
  mutate(PA = BB + AB) %>%
  summarize(G = sum(PA)/pa_per_game,
            BB = sum(BB)/G,
            singles = sum(H-X2B-X3B-HR)/G,
            doubles = sum(X2B)/G, 
            triples = sum(X3B)/G, 
            HR = sum(HR)/G,
            AVG = sum(H)/sum(AB),
            PA = sum(PA)) %>%
  filter(PA >= 300) %>%
  select(-G) %>%
  mutate(R_hat = predict(fit, newdata = .))

# plot player-specific predicted runs
qplot(R_hat, data = players, geom = "histogram", binwidth = 0.5, color = I("black"))

# add 2002 salary of each player
players <- Salaries %>% 
  filter(yearID == 2002) %>%
  select(playerID, salary) %>%
  right_join(players, by="playerID")

# add defensive position
position_names <- c("G_p","G_c","G_1b","G_2b","G_3b","G_ss","G_lf","G_cf","G_rf")
tmp_tab <- Appearances %>% 
  filter(yearID == 2002) %>% 
  group_by(playerID) %>%
  summarize_at(position_names, sum) %>%
  ungroup()  
pos <- tmp_tab %>%
  select(position_names) %>%
  apply(., 1, which.max) 
players <- data_frame(playerID = tmp_tab$playerID, POS = position_names[pos]) %>%
  mutate(POS = str_to_upper(str_remove(POS, "G_"))) %>%
  filter(POS != "P") %>%
  right_join(players, by="playerID") %>%
  filter(!is.na(POS)  & !is.na(salary))

# add players' first and last names
# NOTE: In old versions of the Lahman library, the "People" dataset was called "Master"
# The following code may need to be modified if you have not recently updated the Lahman library.
players <- People %>%
  select(playerID, nameFirst, nameLast, debut) %>%
  mutate(debut = as.Date(debut)) %>%
  right_join(players, by="playerID")

# top 10 players
players %>% select(nameFirst, nameLast, POS, salary, R_hat) %>% 
  arrange(desc(R_hat)) %>% 
  top_n(10) 

# players with a higher metric have higher salaries
players %>% ggplot(aes(salary, R_hat, color = POS)) + 
  geom_point() +
  scale_x_log10()

# remake plot without players that debuted after 1998
library(lubridate)
players %>% filter(year(debut) < 1998) %>%
  ggplot(aes(salary, R_hat, color = POS)) + 
  geom_point() +
  scale_x_log10()


# ==============================================================================
# OPTIMIZATION: LINEAR PROGRAMMING FOR PLAYER SELECTION
# ==============================================================================

library(reshape2)
library(lpSolve)

# Filter for a specific generation of players
players <- players %>% filter(debut <= "1997-01-01" & debut > "1988-01-01")

# Create the constraint matrix for positions (Rows = Positions, Columns = Players)
constraint_matrix <- acast(players, POS ~ playerID, fun.aggregate = length)
npos <- nrow(constraint_matrix)

# Add the salary row to the bottom of the matrix
constraint_matrix <- rbind(constraint_matrix, salary = players$salary)

# Define the direction of the constraints (== 1 for positions, <= 50M for salary)
constraint_dir <- c(rep("==", npos), "<=")
constraint_limit <- c(rep(1, npos), 50*10^6)

# Run the linear programming algorithm to maximize R_hat
lp_solution <- lp("max", players$R_hat,
                  constraint_matrix, constraint_dir, constraint_limit,
                  all.int = TRUE) 

# Extract the chosen 9 players
our_team <- players %>%
  filter(lp_solution$solution == 1) %>%
  arrange(desc(R_hat))

# Display the optimized team
our_team %>% select(nameFirst, nameLast, POS, salary, R_hat)

# Scale the metrics to see WHY the algorithm chose them (using Median and MAD)
my_scale <- function(x) (x - median(x))/mad(x)

players %>% mutate(BB = my_scale(BB), 
                   singles = my_scale(singles),
                   doubles = my_scale(doubles),
                   triples = my_scale(triples),
                   HR = my_scale(HR),
                   AVG = my_scale(AVG),
                   R_hat = my_scale(R_hat)) %>%
  filter(playerID %in% our_team$playerID) %>%
  select(nameFirst, nameLast, BB, singles, doubles, triples, HR, AVG, R_hat) %>%
  arrange(desc(R_hat))

# ==============================================================================
# SECTION 2.3: REGRESSION FALLACY & THE SOPHOMORE SLUMP
# ==============================================================================

# THEORETICAL BACKGROUND:
# The "Sophomore Slump" describes the phenomenon where "Rookies of the Year" (ROY) 
# often perform worse in their second (sophomore) season.
# This is not a psychological issue, but pure mathematics: 
# "Regression to the Mean". Players who start extremely high are statistically 
# bound to move back closer to the average in their next season.

# 1. Determine the primary position per player (the position with the most games played)
playerInfo <- Fielding %>%
  group_by(playerID) %>%
  arrange(desc(G)) %>% # Sort games descending (highest number of games first)
  slice(1) %>%         # Keep only the top row (primary position)
  ungroup() %>%
  left_join(People, by = "playerID") %>%
  select(playerID, nameFirst, nameLast, POS)

# 2. Isolate "Rookie of the Year" winners & attach all career batting statistics
ROY <- AwardsPlayers %>%
  filter(awardID == "Rookie of the Year") %>%
  left_join(playerInfo, by = "playerID") %>%
  rename(rookie_year = yearID) %>% # Save the award year as a fixed reference point
  right_join(Batting, by = "playerID") %>% # Fetch complete career statistics
  mutate(AVG = H/AB) %>%
  filter(POS != "P") # Exclude pitchers to avoid skewing the batting statistics

# 3. Keep only the rookie and sophomore seasons
ROY <- ROY %>%
  filter(yearID == rookie_year | yearID == rookie_year + 1) %>%
  group_by(playerID) %>%
  # Label the earlier year as "rookie" and the later year as "sophomore"
  mutate(rookie = ifelse(yearID == min(yearID), "rookie", "sophomore")) %>%
  filter(n() == 2) %>% # Keep only players who actually played both seasons
  ungroup() %>%
  select(playerID, rookie_year, rookie, nameFirst, nameLast, AVG)

# 4. Reshape from long to wide format for mathematical comparison
ROY_wide <- ROY %>% 
  spread(rookie, AVG) %>% 
  arrange(desc(rookie))

# Calculation: What proportion of players performed worse in their sophomore year?
# (Result: ~ 67.7% decline - a purely mathematical consequence!)
mean(ROY_wide$sophomore - ROY_wide$rookie <= 0)


# ------------------------------------------------------------------------------
# CROSS-CHECK: What happens to the WORST performers?
# ------------------------------------------------------------------------------
# If the theory holds true, extremely poor performers should improve the following year.

# Filter all players active in both 2013 & 2014 (minimum 130 At Bats)
two_years <- Batting %>%
  filter(yearID %in% 2013:2014) %>%
  group_by(playerID, yearID) %>%
  filter(sum(AB) >= 130) %>%
  summarize(AVG = sum(H)/sum(AB)) %>%
  ungroup() %>%
  spread(yearID, AVG) %>%
  filter(!is.na(`2013`) & !is.na(`2014`)) %>%
  left_join(playerInfo, by = "playerID") %>%
  filter(POS != "P") %>%
  select(-POS)

# Take a look at the worst performers of 2013:
# You will notice: Almost all of them improve in 2014, moving back up toward the league average!
arrange(two_years, `2013`)

# Calculate the correlation between the two years (approx. 0.46)
summarize(two_years, cor(`2013`, `2014`))


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
  summarize(get_slope(dplyr::pick(everything())))

# HR effect on runs per game by baseball league
dat <- Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(HR = HR/G, R = R/G) %>%
  select(lgID, HR, BB, R)

# American League: +1.90 runs per HR | National League: +1.76 runs per HR
dat %>% 
  group_by(lgID) %>% 
  summarize(broom::tidy(lm(R ~ HR, data = dplyr::pick(everything())), conf.int = TRUE)) %>% 
  filter(term == "HR")




# MULTIVARIATE REGRESSION FOR A SPECIFIC YEAR (1971)
# Task: Estimate the specific effects of BB and HR on runs scored (R) for the 1971 season only.
teams_1971 <- Teams %>% 
  filter(yearID == 1971)

fit_1971 <- lm(R ~ BB + HR, data = teams_1971)
# Display the estimates for BB and HR 
broom::tidy(fit_1971)




#MULTI-YEAR REGRESSION TRENDS (1961 TO 2018)
#Repeat the above exercise to find the effects of BB and HR on runs (R) 
#for every year from 1961 to 2018 using do() and the broom package.
#Make a scatterplot of the estimate for the effect of BB on runs over time 
#and add a trend line with confidence intervals.

# APPROACH A: The Official Course Version (Using Absolute Season Totals)

# Note: The edX platform uses absolute totals (R ~ BB + HR). Over the decades, 
# the number of games per season increased and the league expanded, which 
# inflates the absolute counts and causes the BB estimate to slightly increase.
res_absolute <- Teams %>%
  filter(yearID %in% 1961:2018) %>%
  group_by(yearID) %>%
  summarize(tidy(lm(R ~ BB + HR, data = dplyr::pick(everything())))) %>%
  ungroup() 

# Plotting the absolute estimates shows a slight upward slope -> "increased over time"
res_absolute %>%
  filter(term == "BB") %>%
  ggplot(aes(yearID, estimate)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(
    title = "Absolute BB Coefficient Trend (Course Version: Increased Over Time)",
    x = "Year",
    y = "Absolute Estimate for BB"
  )


# APPROACH B: The Statistically Robust Version (Using Per-Game Rates)

# Note: To accurately compare seasons regardless of scheduled game changes 
# or strike-shortened years (e.g., 1981, 1994), we must normalize variables 
# to a "per game" baseline.
res_rates <- Teams %>%
  filter(yearID %in% 1961:2018) %>%
  mutate(
    R_per_game  = R / G,
    BB_per_game = BB / G,
    HR_per_game = HR / G
  ) %>%
  group_by(yearID) %>%
  summarize(tidy(lm(R_per_game ~ BB_per_game + HR_per_game, data = dplyr::pick(everything())))) %>%
  ungroup() 

# Plotting the rate-based estimates shows a flat trendline -> "remained the same"
res_rates %>%
  filter(term == "BB_per_game") %>%
  ggplot(aes(yearID, estimate)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(
    title = "Normalized BB Coefficient Trend (Rate Version: Remained the Same)",
    x = "Year",
    y = "Per-Game Estimate for BB"
  )




# LINEAR MODEL OF BB COEFFICIENTS OVER TIME

# Step 1: Calculate the absolute effects of BB and HR on runs for each year
res_absolute <- Teams %>%
  filter(yearID %in% 1961:2018) %>%
  group_by(yearID) %>%
  summarize(tidy(lm(R ~ BB + HR, data = dplyr::pick(everything())))) %>%
  ungroup() 

# Step 2: Isolate the BB effect and fit a second linear model over time (yearID)
# This determines how the estimated impact of BB on runs changes per additional year
bb_trend_model <- res_absolute %>% 
  filter(term == "BB") %>% 
  lm(estimate ~ yearID, data = .)

# Step 3: Extract the exact slope (estimate for yearID) and p-value
broom::tidy(bb_trend_model)


#Game attendance in baseball varies partly as a function of how well a team is playing.
#The Teams data frame contains an attendance column. This is the total attendance for the season. 
#To calculate average attendance, divide by the number of games played, as follows:

# Prepare the data frame as specified in the assessment setup
Teams_small <- Teams %>% 
  filter(yearID %in% 1961:2001) %>% 
  mutate(avg_attendance = attendance/G,
         R_per_game = R/G,
         HR_per_game = HR/G)

# 1. Model: Predict average attendance based on runs per game
fit_runs <- lm(avg_attendance ~ R_per_game, data = Teams_small)
tidy(fit_runs)

# 2. Model: Predict average attendance based on home runs per game
fit_hr <- lm(avg_attendance ~ HR_per_game, data = Teams_small)
tidy(fit_hr)
tidy(fit_runs)

#Use number of wins to predict average attendance; do not normalize for number of games.
#For every game won in a season, how much does average attendance increase?
#Suppose a team won zero games in a season.
#Predict the average attendance.

fit_wins <-lm(avg_attendance ~ W, data=Teams_small)
tidy(fit_wins)

#Use year to predict average attendance.
#How much does average attendance increase each year?

fit_year <- lm(avg_attendance ~ yearID, data=Teams_small)
tidy(fit_year)

#What is the correlation coefficient for runs per game and wins?
Teams_small %>% summarize(cor(R/G, W))

#What is the correlation coefficient for home runs per game and wins?
Teams_small %>% summarize(cor(HR/G, W))

#Stratify Teams_small by wins: divide number of wins by 10 and then round to the nearest integer. 
#Filter to keep only strata 5 through 10. (The other strata have fewer than 20 data points, too few for our analyses).
#Use the stratified dataset to answer this three-part question.


strata_data <- Teams_small%>%
  mutate(W_strata = round(W/10))%>%
           filter(W_strata >=5 & W_strata<=10)

#How many observations are in the 8 win strata?
count_8_strata <- strata_data %>%
  filter(W_strata == 8) %>%
  nrow()

print(count_8_strata)


#Calculate the slope of the regression line predicting average attendance given runs per game for each of the win strata.
slope_results <- strata_data %>%
  group_by(W_strata) %>%
  do(tidy(lm(avg_attendance ~ I(R/G), data = .))) %>%
  filter(term == "I(R/G)") %>%
  select(W_strata, estimate)

print(slope_results)

#Calculate the slope of the regression line predicting average attendance given HR per game for each of the win strata.
## calculate slope of regression line after stratifying by HR per game

slope_results <- strata_data %>%
  group_by(W_strata) %>%
  do(tidy(lm(avg_attendance ~ I(HR/G), data = .))) %>%
  filter(term == "I(HR/G)") %>%
  select(W_strata, estimate)

print(slope_results)

# Model: Attendance as a function of wins and home runs per game.
multi_model <- lm(avg_attendance ~ W + I(HR/G), data = Teams_small)
summary(multi_model)

#Fit a multivariate regression determining the effects of runs per game, 
#home runs per game, wins, and year on average attendance. 
#Use the original Teams_small wins column, not the win strata from question 3.
fit_Multi <- lm(avg_attendance ~ I(R/G) + I(HR/G) + W +yearID, data = Teams_small) 
summary(fit_Multi)

#Suppose a team averaged 5 runs per game, 1.2 home runs per game, and won 80 games in a season
# Create a new data frame for the prediction
# Note: Since the model uses R/G and HR/G, we ensure the input data 
# matches the structure required by the model.
new_team_2002 <- data.frame(
  R = 5 * 162,      # 5 runs per game * 162 games
  G = 162,          # Standard MLB season length
  HR = 1.2 * 162,   # 1.2 home runs per game * 162 games
  W = 80,           # 80 wins
  yearID = 2002     # The year for prediction
)

new_team_1960 <- data.frame(
  R = 5 * 162,      # 5 runs per game * 162 games
  G = 162,          # Standard MLB season length
  HR = 1.2 * 162,   # 1.2 home runs per game * 162 games
  W = 80,           # 80 wins
  yearID = 1960     # The year for prediction
)

# Use the predict() function to generate the average attendance
# The model 'fit_Multi' uses the coefficients calculated previously
prediction_2002 <- predict(fit_Multi, newdata = new_team_2002)
prediction_1960 <- predict(fit_Multi, newdata = new_team_1960)

# Display the result
print(prediction_2002)
print(prediction_1960)

#predict average attendance for teams in 2002 in the original Teams data frame.
#What is the correlation between the predicted attendance and actual attendance?


# 1. Filter the original 'Teams' data for the year 2002
teams_2002 <- Teams %>% 
  filter(yearID == 2002) %>% 
  mutate(avg_attendance = attendance / G)

# 2. Use existing 'fit_Multi' model to predict attendance for 2002
# Note: Ensure the variable names in teams_2002 match what the model expects
teams_2002 <- teams_2002 %>% 
  mutate(predicted_attendance = predict(fit_Multi, newdata = .))

# 3. Calculate the correlation coefficient between actual and predicted
correlation_result <- cor(teams_2002$avg_attendance, teams_2002$predicted_attendance)

# Display the result
print(correlation_result)