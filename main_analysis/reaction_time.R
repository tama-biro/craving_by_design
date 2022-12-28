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
                      scale(age) + factor(gender) + factor(major) +
                       scale(reaction_time) + (1 | id),
                     fixef.prior = t, data = data_rt_2,
                    family = binomial(link = "logit"))

summary(log_mod_rt)


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
                      previous_choice + scale(age) + factor(gender) +
                        factor(major) + scale(reaction_time) + (1 | id),
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
  labs(x = 'Treatment', y = 'Mean RT') +
  scale_fill_manual(name = 'Session color',
                    breaks = c('Yellow', 'Blue'),
                    values = c('#ffd700', '#0057b7')) +
  theme_minimal() +
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 16))

ggsave('../Plots/main_analysis/rt_plot_by_col_treat.png', width = 10, height = 6)


# RT by decision and session color
rt_plot_2 <- data %>%
  filter(craver_2 == 1) %>%
  mutate(decision = factor(choice, levels = 0:1,
                           labels = c("Skip", "Bet")),
         color = factor(block_type, levels = c('C', 'S'),
                        labels = c('Yellow', 'Blue'))) %>%
  group_by(id, decision, color) %>%
  summarize(RT = mean(reaction_time)) %>%
  ungroup %>%
  group_by(decision, color) %>%
  summarize(mean = mean(RT),
            se = se(RT)) %>%
  ungroup

ggplot(rt_plot_2, aes(x = decision, y = mean, fill = color)) +
  geom_bar(stat = "identity", width = 0.6,
           position = position_dodge()) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se),
                width = 0.1,  position = position_dodge(width = 0.6)) +
  labs(x = 'Decision', y = 'Mean RT') +
  scale_fill_manual(name = 'Session color',
                    breaks = c('Yellow', 'Blue'),
                    values = c('#ffd700', '#0057b7')) +
  theme_minimal() +
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 16))

ggsave('../Plots/main_analysis/rt_plot_by_col_bet.png', width = 10, height = 6)


