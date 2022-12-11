library(tidyverse)
library(moments)
library(EnvStats)
library(blme)
library(BayesianFirstAid)


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

t.test(data_1$betting_rate ~ reward_value, data = data_1)


#### Test 2 ####

# Logistic mixed effects regression, trial level
# Predicting betting
data_2 <- data %>%
  mutate(reward_value = factor(reward_value, levels = c("Low", "High"),
                               labels = c("Low", "High")),
         uncertainty = factor(aaron_mood, levels = c("Low", "High"),
                              labels = c("Low", "High")),
         treatment = factor(treatment, levels = c("control", "test"),
                            labels = c("Control", "Test")),
         color = factor(block_type, levels = c("C", "S"),
                        labels = c("Yellow", "Blue")))

log_mod_2 <- bglmer(choice ~ reward_value + uncertainty + treatment * color +
                      scale(age) + gender + major + (1 | id),
                    fixef.prior = t, data = data_2,
                    family = binomial(link = "logit"))

summary(log_mod_2)

#### Test 3 ####

# Logistic mixed effects regression, trial level
# Predicting betting, adding reward exposure
log_mod_3 <- bglmer(choice ~ reward_value + uncertainty + treatment * color +
                      scale(age) + gender + major + scale(exposure_time) +
                      (1 | id), data = data_2, fixef.prior = t,
                    family = binomial(link = "logit"))

summary(log_mod_3)

#### Test 4 ####

# Linear regression, participant level
# Predicting betting rate in yellow



#### Test 5 ####

# Logistic mixed effect model, trial level
# Predicting betting in yellow (craving)
data_5 <- data %>%
  filter(block_type == "C" & craver_2 == 1) %>%
  mutate(reward_value = factor(reward_value, levels = c("Low", "High"),
                               labels = c("Low", "High")),
         uncertainty = factor(aaron_mood, levels = c("Low", "High"),
                              labels = c("Low", "High")),
         treatment = factor(treatment, levels = c("control", "test"),
                            labels = c("Control", "Test")))

log_mod_5 <- bglmer(choice ~ reward_value + uncertainty + treatment +
                      previous_choice + scale(age) + gender + major + (1 | id),
                    data = data_5, fixef.prior = t,
                    family = binomial(link = "logit"))

summary(log_mod_5)

#### Test 6 ####

# Bayesian t-test, participant level
# H1: Betting rate in yellow, test is > 0
# H0: Betting rate in yellow, test is 0
data_6 <- data %>%
  filter(block_type == "C" & treatment == "test") %>%
  group_by(id) %>%
  summarize(betting_rate = mean(choice)) %>%
  ungroup

# Check skew and normality assumption
skewness(sqrt(data_6$betting_rate))
shapiro.test(data_6$betting_rate)


# Box-Cox transform
out <- boxcox(data_6$betting_rate + 1, lambda = seq(-25, 25, by = 0.25))
out$lambda[which.max(out$objective)]

data_6$betting_rate_bc <- boxcoxTransform(data_6$betting_rate + 1, lambda = -4.75)

skewness(sqrt(data_6$betting_rate_bc))
shapiro.test(data_6$betting_rate_bc)

# Run test
bayes.t.test(x = data_6$betting_rate_bc, mu = 0)
print(paste('Bayes factor:', .682/.318))

bayes.t.test(x = data_6$betting_rate, mu = 0)
print(paste('Bayes factor:', .621/.379))


#### Test 7 ####

# Paired, one-tailed t-test, participant level
# Betting rate in low/high uncertainty
data_7 <- data %>%
  mutate(uncertainty = factor(aaron_mood, levels = c("Low", "High"),
                               labels = c("Low", "High"))) %>%
  filter(block_type == "C") %>%
  group_by(id, uncertainty) %>%
  summarize(betting_rate = mean(choice)) %>%
  ungroup


# Check skew and normality assumption
skewness(sqrt(data_7$betting_rate))
shapiro.test(data_7$betting_rate)


# Box-Cox transform
out <- boxcox(data_7$betting_rate + 1, lambda = seq(-25, 25, by = 0.25))
out$lambda[which.max(out$objective)]

data_7$betting_rate_bc <- boxcoxTransform(data_7$betting_rate + 1, lambda = -9)

skewness(sqrt(data_7$betting_rate_bc))
shapiro.test(data_7$betting_rate_bc)


# Run test
t.test(data_7$betting_rate_bc ~ uncertainty, data = data_7)

t.test(data_7$betting_rate ~ uncertainty, data = data_7)



