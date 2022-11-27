library(tidyverse)
library(moments)
library(EnvStats)


filepath <- '../data/data_reduced.csv'
data <- read.csv(filepath)


#### Test 1 ####

# Paired one-tailed t-test, participant level
# Betting rate in low/high reward
data_1 <- data %>%
  mutate(reward_value = factor(reward_value, levels = c("Low", "High"),
                               labels = c("Low", "High"))) %>%
  filter(block_type == "C") %>%
  group_by(id, reward_value) %>%
  summarize(betting_rate = mean(choice)) %>%
  ungroup


# Check skew and normality assumption
skewness(sqrt(data_1$betting_rate))
shapiro.test(data_1$betting_rate)


# Box-Cox transform
out <- boxcox(data_1$betting_rate + 1, lambda = seq(-25, 25, by = 0.25))
out$lambda[which.max(out$objective)]

data_1$betting_rate_bc <- boxcoxTransform(data_1$betting_rate + 1, lambda = -22)

skewness(sqrt(data_1$betting_rate_bc))
shapiro.test(data_1$betting_rate_bc)


# Run test
t.test(data_1$betting_rate_bc ~ reward_value, data = data_1)



#### Test 2 ####

# Logistic mixed effects regression, trial level
# Predicting betting



#### Test 3 ####

# Logistic mixed effects regression, trial level
# Predicting betting, adding reward exposure



#### Test 4 ####

# Linear regression, participant level
# Predicting betting rate in yellow



#### Test 5 ####

# Logistic mixed effect model, trial level
# Predicting betting in yellow (craving)



#### Test 6 ####

# Bayesian t-test, participant level
# H1: Betting rate in yellow, test is > 0
# H0: Betting rate in yellow, test is 0



#### Test 7 ####

# Paired, one-tailed t-test, participant level
# Betting rate in low/high uncertainty







