library(tidyverse)
library(lme4)
library(blme)


set.seed(41)

filepath <- '../data/november_subset/data_reduced.csv'
data <- read.csv(filepath)


# SE function
se <- function (x) {
  se = sd(x, na.rm = TRUE)/sqrt(length(x))
  return(se)
}


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
                      previous_choice + scale(age) + factor(gender) +
                      factor(major) + (1 | id),
                    data = data_5, fixef.prior = t,
                    family = binomial(link = "logit"))

summary(log_mod_5)


# Betting rate by col/treat
data_plot1 <- data %>%
  filter(craver_2 == 1) %>%
  group_by(treatment, block_type, id) %>%
  summarize(betting_rate = mean(choice)) %>%
  ungroup %>%
  group_by(treatment, block_type) %>%
  summarize(se = se(betting_rate),
            betting_rate = mean(betting_rate)) %>%
  ungroup

data_plot2 <- data %>%
  group_by(treatment, block_type, id) %>%
  summarize(betting_rate = mean(choice)) %>%
  ungroup %>%
  group_by(treatment, block_type) %>%
  summarize(se = se(betting_rate),
            betting_rate = mean(betting_rate)) %>%
  ungroup

data_plot <- data_plot1 %>%
  rbind(data_plot2) %>%
  mutate(participant_group = rep(c('Cravers', 'Pooled'), each = 4),
         participant_group = factor(participant_group,
                                    levels = c("Pooled", "Cravers"),
                                    labels = c("(A) Pooled", "(B) Cravers")))


ggplot(data_plot, aes(x = treatment, y = betting_rate, fill = block_type)) +
  geom_bar(stat='identity', position = position_dodge(.9)) +
  geom_errorbar(aes(ymin = betting_rate - se, ymax = betting_rate + se),
                position = position_dodge(.9), width = .2) +
  scale_x_discrete(name = 'Treatment',
                   breaks = c('control', 'test'),
                   labels = c('Control', 'Test')) +
  scale_y_continuous(name = 'Betting rate') +
  scale_fill_manual(name = 'Session color',
                    breaks = c('C', 'S'),
                    labels = c('Yellow', 'Blue'),
                    values = c('#ffd700', '#0057b7')) +
  theme_minimal() +
  facet_wrap('participant_group', nrow = 1) +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        strip.text = element_text(size = 14),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12))


ggsave('../Plots/main_analysis/betting_by_col_and_treat_new.png',
       width = 10, height = 6)

