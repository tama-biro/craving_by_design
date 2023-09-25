
library(tidyverse)
library(moments)
library(EnvStats)
library(blme)
library(DescTools)
library(knitr)
library(MuMIn)
library(performance)
library(ggsignif)

setwd('C:/Users/samue/Documents/GitHub/craving_by_design/main_analysis')

# SE function
se <- function (x) {
  se = sd(x, na.rm = TRUE)/sqrt(length(x))
  return(se)
}

filepath <- '../data/data_full.csv'
data <- read.csv(filepath)

theta_fun <- function() {
  return(1)
}

source("./feature_engineering.R", local = knit_global())

# Save engineered data
write.csv(data, file = '../data/data_reduced.csv', row.names = FALSE)

set.seed(41)

filepath <- '../data/data_reduced.csv'
data <- read.csv(filepath)


#### Table 1B in 2nd yellow session ####



#### Table 1B with those who bet only once ####
data_5_bet1 <- data %>%
  filter(block_type == "C" & craver == 1) %>%
  mutate(reward_value = factor(reward_value, levels = c("Low", "High"),
                               labels = c("Low", "High")),
         uncertainty = factor(aaron_mood, levels = c("Low", "High"),
                              labels = c("Low", "High")),
         treatment = factor(treatment, levels = c("control", "test"),
                            labels = c("Control", "Test")),
         age = scale(age),
         gender = factor(gender),
         major = factor(major),
         reward_exposure = scale(exposure_time),
         reaction_time = scale(reaction_time),
         sequence_number = scale(sequence_number))

log_mod_5_bet1 <- bglmer(choice ~ reward_value + uncertainty * treatment +
                           age + gender + major + sequence_number +
                           previous_choice + reward_exposure +
                           reaction_time + (1 | id),
                         data = data_5_bet1, fixef.prior = t,
                         family = binomial(link = "logit"))

summary(log_mod_5_bet1)

# Nakagawa R^2
r2_nakagawa(log_mod_5_bet1)$R2_conditional

# Number of obs
summary(log_mod_5_bet1)$devcomp$dims[1]


#### T-test T vs C when the average for C is computed for the subsample ####



#### Regressions with RT as DV ####
data_3 <- data %>%
  mutate(reward_value = factor(reward_value, levels = c("Low", "High"),
                               labels = c("Low", "High")),
         uncertainty = factor(aaron_mood, levels = c("Low", "High"),
                              labels = c("Low", "High")),
         treatment = factor(treatment, levels = c("control", "test"),
                            labels = c("Control", "Test")),
         color = factor(block_type, levels = c("S", "C"),
                        labels = c("Blue", "Yellow")),
         age = scale(age),
         gender = factor(gender),
         major = factor(major),
         reaction_time = scale(reaction_time),
         sequence_number = scale(sequence_number),
         reward_exposure = scale(exposure_time))

log_mod_3_RT <- blmer(reaction_time ~ reward_value + uncertainty +
                      treatment * color + previous_choice + reward_exposure +
                      age + gender + major + sequence_number + choice +
                      (1 | id), fixef.prior = t, data = data_3,
                      REML = FALSE)

summary(log_mod_3_RT)

2*pt(abs(summary(log_mod_3_RT)$coefficients[,3]), df = 199, lower.tail = FALSE)

# Nakagawa R^2
r2_nakagawa(log_mod_3_RT)$R2_conditional

# Number of obs
summary(log_mod_3_RT)$devcomp$dims[1]

#### Test 5 ####

data_5 <- data %>%
  filter(block_type == "C" & craver_2 == 1) %>%
  mutate(reward_value = factor(reward_value, levels = c("Low", "High"),
                               labels = c("Low", "High")),
         uncertainty = factor(aaron_mood, levels = c("Low", "High"),
                              labels = c("Low", "High")),
         treatment = factor(treatment, levels = c("control", "test"),
                            labels = c("Control", "Test")),
         age = scale(age),
         gender = factor(gender),
         major = factor(major),
         reward_exposure = scale(exposure_time),
         reaction_time = scale(reaction_time),
         sequence_number = scale(sequence_number))

log_mod_5_RT <- blmer(reaction_time ~ reward_value + uncertainty * treatment +
                      age + gender + major + sequence_number +
                      previous_choice + reward_exposure + choice +
                        (1 | id), data = data_5, fixef.prior = t,
                      REML = FALSE)

summary(log_mod_5_RT)

2*pt(abs(summary(log_mod_5_RT)$coefficients[,3]), df = 77, lower.tail = FALSE)

# Nakagawa R^2
r2_nakagawa(log_mod_5_RT)$R2_conditional

# Number of obs
summary(log_mod_5_RT)$devcomp$dims[1]


#### Net accumulated outcomes ####
data_3 <- data %>%
  group_by(id) %>%
  mutate(wealth = cumsum(outcome)) %>%
  ungroup %>%
  mutate(reward_value = factor(reward_value, levels = c("Low", "High"),
                               labels = c("Low", "High")),
         uncertainty = factor(aaron_mood, levels = c("Low", "High"),
                              labels = c("Low", "High")),
         treatment = factor(treatment, levels = c("control", "test"),
                            labels = c("Control", "Test")),
         color = factor(block_type, levels = c("S", "C"),
                        labels = c("Blue", "Yellow")),
         age = scale(age),
         gender = factor(gender),
         major = factor(major),
         reaction_time = scale(reaction_time),
         sequence_number = scale(sequence_number),
         reward_exposure = scale(exposure_time),
         wealth = scale(wealth))

log_mod_3_wealth <- bglmer(choice ~ reward_value + uncertainty * wealth +
                      treatment * color + previous_choice + reward_exposure +
                      age + gender + major + sequence_number + reaction_time +
                      (1 | id), fixef.prior = t, data = data_3,
                    family = binomial(link = "logit"))

summary(log_mod_3_wealth)

# Nakagawa R^2
r2_nakagawa(log_mod_3_wealth)$R2_conditional

# Number of obs
summary(log_mod_3_wealth)$devcomp$dims[1]


#### Test 5 ####

data_5 <- data %>%
  filter(block_type == "C" & craver_2 == 1) %>%
  group_by(id) %>%
  mutate(wealth = cumsum(outcome)) %>%
  ungroup %>%
  mutate(reward_value = factor(reward_value, levels = c("Low", "High"),
                               labels = c("Low", "High")),
         uncertainty = factor(aaron_mood, levels = c("Low", "High"),
                              labels = c("Low", "High")),
         treatment = factor(treatment, levels = c("control", "test"),
                            labels = c("Control", "Test")),
         age = scale(age),
         gender = factor(gender),
         major = factor(major),
         reward_exposure = scale(exposure_time),
         reaction_time = scale(reaction_time),
         sequence_number = scale(sequence_number),
         wealth = scale(wealth))

log_mod_5_wealth <- bglmer(choice ~ reward_value + age + gender +
                           major + sequence_number +
                           uncertainty * (treatment + wealth) +
                           previous_choice + reward_exposure +
                           reaction_time + (1 | id),
                         data = data_5, fixef.prior = t,
                         family = binomial(link = "logit"))

summary(log_mod_5_wealth)

# Nakagawa R^2
r2_nakagawa(log_mod_5_wealth)$R2_conditional

# Number of obs
summary(log_mod_5_wealth)$devcomp$dims[1]


