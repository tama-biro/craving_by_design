
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

# T-test report
t_test_report <- function (t_test) {
  df <- round(t_test$parameter, 3)
  t <- round(t_test$statistic, 3)
  p <- round(t_test$p.value, 3)
  if (t_test$p.value < .001) {
    p <- 'p < .001'
  } else {
    p <- paste('p =', p)
  }
  out <- paste0('t(', df, ') = ', t, ', ', p)
  return(out)
}

# Wilcox report
wilcox_report <- function (wilcox) {
  v <- round(wilcox$statistic, 3)
  p <- round(wilcox$p.value, 3)
  if (wilcox$p.value < .001) {
    p <- 'p < .001'
  } else {
    p <- paste('p =', p)
  }
  out <- paste('V =', v, p)
  return(out)
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


# Add yellow order ctrl
data$yellow_order_ctrl <- 0
for(id in unique(data$id[data$treatment == 'control'])) {
  for(seq in unique(data$sequence_number[data$id == id])) {
    indicator <- data$blue_interspersed[
      data$sequence_number == seq & data$id == id
    ][1]
    
    if(indicator == 2) {
      data$yellow_order_ctrl[
        data$sequence_number == seq &
          data$id == id &
          data$block_type == 'C' &
          data$block_number %in% c(1, 3)
      ] <- 1
      data$yellow_order_ctrl[
        data$sequence_number == seq &
          data$id == id &
          data$block_type == 'C' &
          data$block_number %in% c(4, 5)
      ] <- 2
    } else if(indicator == 3) {
      data$yellow_order_ctrl[
        data$sequence_number == seq &
          data$id == id &
          data$block_type == 'C' &
          data$block_number %in% c(1, 4)
      ] <- 1
      data$yellow_order_ctrl[
        data$sequence_number == seq &
          data$id == id &
          data$block_type == 'C' &
          data$block_number %in% c(2, 5)
      ] <- 2
    } else if(indicator == 4) {
      data$yellow_order_ctrl[
        data$sequence_number == seq &
          data$id == id &
          data$block_type == 'C' &
          data$block_number %in% c(1, 5)
      ] <- 1
      data$yellow_order_ctrl[
        data$sequence_number == seq &
          data$id == id &
          data$block_type == 'C' &
          data$block_number %in% c(2, 3)
      ] <- 2
    }
    
  }
}


#### Table 1B in first yellow session in order ####
data_5_bet1 <- data %>%
  filter(block_type == "C" & craver == 1 & yellow_order_ctrl %in% 0:1) %>%
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
data_7_2 <- data %>%
  filter(block_type == "C" & yellow_order_ctrl %in% 0:1) %>%
  group_by(id, treatment) %>%
  summarize(betting_rate = mean(choice)) %>%
  ungroup

out <- boxcox(data_7_2$betting_rate + 1, lambda = seq(-25, 25, by = 0.25))
bc_72_lambda <- out$lambda[which.max(out$objective)]

data_7_2$betting_rate_bc <- boxcoxTransform(data_7_2$betting_rate + 1,
                                            lambda = bc_72_lambda)

t_test_7_2 <- t.test(betting_rate_bc ~ treatment, data = data_7_2)
t_test_7_2_out <- t_test_report(t_test_7_2)
t_test_7_2_out


effect_size_t_7_2 <- (
  (mean(data_7_2$betting_rate_bc[data_7_2$treatment == 'test']) - mean(data_7_2$betting_rate_bc[data_7_2$treatment == 'control']))/
    sqrt(
      (sd(data_7_2$betting_rate_bc[data_7_2$treatment == 'test'])^2 - sd(data_7_2$betting_rate_bc[data_7_2$treatment == 'control'])^2)/2
    )
)
effect_size_t_7_2

# Non-parametric
wilcox_7_2 <- wilcox.test(betting_rate ~ treatment, data = data_7_2)
wilcox_7_2 <- wilcox_report(wilcox_7_2)
wilcox_7_2


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


