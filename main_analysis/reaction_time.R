library(tidyverse)


filepath <- '../data/data_reduced.csv'
data <- read.csv(filepath)


# Include in log mod 2
data_rt_2 <- data %>%
  mutate(reward_value = factor(reward_value, levels = c("Low", "High"),
                               labels = c("Low", "High")),
         uncertainty = factor(aaron_mood, levels = c("Low", "High"),
                              labels = c("Low", "High")),
         treatment = factor(treatment, levels = c("control", "test"),
                            labels = c("Control", "Test")),
         color = factor(block_type, levels = c("C", "S"),
                        labels = c("Yellow", "Blue")))

log_mod_rt <- bglmer(choice ~ reward_value + uncertainty + treatment * color +
                      scale(age) + gender + major + scale(reaction_time) + (1 | id),
                    fixef.prior = t, data = data_rt_2,
                    family = binomial(link = "logit"))

summary(log_mod_rt_2)


# Include in log mod 5
data_rt_5 <- data %>%
  filter(block_type == "C" & craver_2 == 1) %>%
  mutate(reward_value = factor(reward_value, levels = c("Low", "High"),
                               labels = c("Low", "High")),
         uncertainty = factor(aaron_mood, levels = c("Low", "High"),
                              labels = c("Low", "High")),
         treatment = factor(treatment, levels = c("control", "test"),
                            labels = c("Control", "Test")))

log_mod_rt_5 <- bglmer(choice ~ reward_value + uncertainty + treatment +
                      previous_choice + scale(age) + gender + major +
                        scale(reaction_time) + (1 | id),
                    data = data_rt_5, fixef.prior = t,
                    family = binomial(link = "logit"))

summary(log_mod_rt_5)
