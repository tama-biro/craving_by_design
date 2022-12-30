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
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se)) +
  labs(x = 'Sequence number', y = 'Mean betting rate (yellow)') +
  theme_minimal() +
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 16))


ggsave('../Plots/main_analysis/betting_rate_sequence.png',
       width = 10, height = 6)


# Plot betting in yellow across cumulative losses
data_plot <- data %>%
  mutate(losses = if_else(outcome < 0 & block_type == 'C', outcome, 0)) %>%
  group_by(id) %>%
  mutate(losses = cumsum(-losses)) %>%
  filter(craver_2 == 1 & block_type == 'C') %>%
  group_by(id, treatment, losses) %>%
  summarize(betting_rate = mean(choice)) %>%
  group_by(treatment, losses) %>%
  summarize(mean = mean(betting_rate),
            se = se(betting_rate)) %>%
  ungroup %>%
  drop_na

ggplot(data_plot, aes(x = losses, y = mean, color = treatment)) +
  geom_line() +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se)) +
  labs(x = 'Cumulative losses in yellow', y = 'Mean betting rate (yellow)') +
  theme_minimal() +
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 16))


ggsave('../Plots/main_analysis/betting_rate_losses.png',
       width = 12, height = 6)


# Plot betting in yellow across sequences
data_plot <- data %>%
  mutate(losses = if_else(outcome < 0 & block_type == 'C', outcome, 0),
         bets_in_y = if_else(choice == 1 & block_type == 'C', 1, 0)) %>%
  group_by(id) %>%
  mutate(losses = cumsum(-losses),
         bets_in_y = cumsum(bets_in_y),
         losses_by_bets = losses/bets_in_y) %>%
  filter(craver_2 == 1 & block_type == 'C') %>%
  group_by(id, treatment, losses_by_bets) %>%
  summarize(betting_rate = mean(choice)) %>%
  group_by(treatment, losses_by_bets) %>%
  summarize(mean = mean(betting_rate),
            se = se(betting_rate)) %>%
  ungroup %>%
  drop_na

ggplot(data_plot, aes(x = losses_by_bets, y = mean, color = treatment)) +
  geom_line() +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se)) +
  labs(x = 'Sequence number', y = 'Mean betting rate (yellow)') +
  theme_minimal() +
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 16))


ggsave('../Plots/main_analysis/betting_rate_sequence.png',
       width = 10, height = 6)


