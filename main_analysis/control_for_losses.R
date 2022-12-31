library(tidyverse)


# SE function
se <- function (x) {
  se = sd(x, na.rm = TRUE)/sqrt(length(x))
  return(se)
}


filepath <- '../data/data_reduced.csv'
data <- read.csv(filepath)


# Plot betting in yellow across sequences
data_plot <- data %>%
  filter(craver_2 == 1 & block_type == 'C') %>%
  group_by(id, treatment, sequence_number) %>%
  summarize(betting_rate = mean(choice)) %>%
  group_by(treatment, sequence_number) %>%
  summarize(mean = mean(betting_rate),
            se = se(betting_rate)) %>%
  ungroup

ggplot(data_plot, aes(x = sequence_number, y = mean, color = treatment)) +
  geom_line() +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se),
                width = 0.2) +
  labs(x = 'Sequence number', y = 'Mean betting rate (yellow)') +
  theme_minimal() +
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 16))


ggsave('../Plots/main_analysis/betting_rate_sequence.png',
       width = 12, height = 6)


# Plot betting in yellow across cumulative losses
data_plot <- data %>%
  mutate(losses = if_else(outcome < 0 & block_type == 'C', outcome, 0)) %>%
  group_by(id) %>%
  mutate(losses = cumsum(-losses)) %>%
  ungroup %>%
  mutate(loss_bins = as.numeric(cut_interval(losses, 5))) %>%
  filter(craver_2 == 1 & block_type == 'C') %>%
  group_by(id, treatment, loss_bins) %>%
  summarize(betting_rate = mean(choice)) %>%
  group_by(treatment, loss_bins) %>%
  summarize(mean = mean(betting_rate),
            se = se(betting_rate)) %>%
  ungroup %>%
  drop_na

ggplot(data_plot, aes(x = loss_bins, y = mean, color = treatment)) +
  geom_line() +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se),
                width = 0.2) +
  labs(x = 'Cumulative losses (5 bins) in yellow',
       y = 'Mean betting rate (yellow)') +
  theme_minimal() +
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 16))


ggsave('../Plots/main_analysis/betting_rate_lossbins.png',
       width = 12, height = 6)


# Plot betting in yellow across sequences
data_plot <- data %>%
  mutate(losses = if_else(outcome < 0 & block_type == 'C', outcome, 0),
         bets_in_y = if_else(choice == 1 & block_type == 'C', 1, 0)) %>%
  group_by(id) %>%
  mutate(losses = cumsum(-losses),
         bets_in_y = cumsum(bets_in_y),
         losses_by_bets = losses/bets_in_y) %>%
  ungroup %>%
  mutate(loss_bins = as.numeric(cut_interval(losses_by_bets, 5))) %>%
  filter(craver_2 == 1 & block_type == 'C') %>%
  group_by(id, treatment, loss_bins) %>%
  summarize(betting_rate = mean(choice)) %>%
  group_by(treatment, loss_bins) %>%
  summarize(mean = mean(betting_rate),
            se = se(betting_rate)) %>%
  ungroup %>%
  drop_na

ggplot(data_plot, aes(x = loss_bins, y = mean, color = treatment)) +
  geom_line() +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se),
                width = 0.2) +
  labs(x = 'Cumulative losses by bets (5 bins)',
       y = 'Mean betting rate (yellow)') +
  theme_minimal() +
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 16))


ggsave('../Plots/main_analysis/betting_rate_lossbybetbins.png',
       width = 12, height = 6)


# Losses in first 2 sequences and betting rate over sequences
data_plot <- data %>%
  mutate(losses = if_else(outcome < 0 &
                            block_type == 'C' &
                            sequence_number %in% c(1),
                          outcome, 0)) %>%
  filter(craver_2 == 1 & block_type == 'C') %>%
  group_by(id, treatment) %>%
  mutate(losses = sum(-losses)) %>%
  ungroup %>%
  mutate(loss_bins = as.numeric(cut_interval(losses, 3))) %>%
  group_by(id, sequence_number, treatment, loss_bins) %>%
  summarize(betting_rate = mean(choice)) %>%
  group_by(sequence_number, treatment, loss_bins) %>%
  summarize(mean = mean(betting_rate),
            se = se(betting_rate)) %>%
  ungroup %>%
  drop_na

ggplot(data_plot, aes(x = sequence_number, y = mean, color = treatment)) +
  geom_line() +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se),
                width = 0.2) +
  labs(x = 'Sequence number',
       y = 'Mean betting rate (yellow)') +
  theme_minimal() +
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 16)) +
  facet_wrap('loss_bins')


ggsave('../Plots/main_analysis/betting_rate_wrapbybins.png',
       width = 12, height = 6)


# Add loss variable to glmm
data_5 <- data %>%
  mutate(losses = if_else(outcome < 0 & block_type == 'C', outcome, 0),
         bets_in_y = if_else(choice == 1 & block_type == 'C', 1, 0)) %>%
  group_by(id) %>%
  mutate(losses = cumsum(-losses),
         bets_in_y = cumsum(bets_in_y),
         losses_by_bets = losses/bets_in_y) %>%
  ungroup %>%
  filter(block_type == "C" & craver_2 == 1) %>%
  mutate(reward_value = factor(reward_value, levels = c("Low", "High"),
                               labels = c("Low", "High")),
         uncertainty = factor(aaron_mood, levels = c("Low", "High"),
                              labels = c("Low", "High")),
         treatment = factor(treatment, levels = c("control", "test"),
                            labels = c("Control", "Test")))

log_mod_5 <- bglmer(choice ~ reward_value + uncertainty + treatment +
                      previous_choice + scale(age) + factor(gender) +
                      factor(major) + scale(losses_by_bets) * treatment +
                      (1 | id),
                    data = data_5, fixef.prior = t,
                    family = binomial(link = "logit"))

summary(log_mod_5)


