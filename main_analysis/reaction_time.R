library(tidyverse)


# SE function
se <- function (x) {
  se = sd(x, na.rm = TRUE)/sqrt(length(x))
  return(se)
}

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
                      scale(age) + gender + major + scale(reaction_time) +
                       (1 | id), fixef.prior = t, data = data_rt_2,
                    family = binomial(link = "logit"))

summary(log_mod_rt)


steam# Include in log mod 5
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


# Reaction time by color and treatment
rt_plot <- data %>%
  mutate(treatment = factor(treatment, levels = c("control", "test"),
                            labels = c("Control", "Test")),
         color = factor(block_type, levels = c('C', 'S'),
                        labels = c('Yellow', 'Blue'))) %>%
  group_by(id, treatment, color) %>%
  summarize(RT = mean(reaction_time)) %>%
  ungroup %>%
  group_by(treatment, color) %>%
  summarize(mean = mean(RT),
            se = se(RT)) %>%
  ungroup

ggplot(rt_plot, aes(x = treatment, y = mean, fill = color)) +
  geom_bar(stat = "identity", width = 0.6,
           position = position_dodge()) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se),
                width = 0.1,  position = position_dodge(width = 0.6)) +
  labs(x = 'Treatment', y = 'Mean betting rate') +
  theme_minimal()





