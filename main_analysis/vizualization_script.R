library(tidyverse)


filepath <- '../data/data_reduced.csv'
data <- read.csv(filepath)


# SE function
se <- function (x) {
  se = sd(x, na.rm = TRUE)/sqrt(length(x))
  return(se)
}


# Betting rate in blue/yellow for test/control
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


ggsave('../Plots/main_analysis/betting_by_col_and_treat.png',
       width = 10, height = 6)


# Betting rates in yellow or blue (switch C and S) by treat
data_dists1 <- data %>%
  filter(block_type == 'C') %>%
  group_by(id, treatment) %>%
  summarize(betting_rate = mean(choice, na.rm=TRUE)) %>%
  ungroup

data_dists2 <- data %>%
  filter(block_type == 'C' & craver_2 == 1) %>%
  group_by(id, treatment) %>%
  summarize(betting_rate = mean(choice, na.rm=TRUE)) %>%
  ungroup

data_dists <- data_dists1 %>%
  rbind(data_dists2) %>%
  mutate(participant_group = rep(c('Pooled', 'Cravers'),
                                 times = c(nrow(data_dists1),
                                           nrow(data_dists2))),
         participant_group = factor(participant_group,
                                    levels = c("Pooled", "Cravers"),
                                    labels = c("(A) Pooled", "(B) Cravers")))


# Plot
ggplot(data_dists, aes(x = treatment, y = betting_rate)) +
  geom_boxplot() +
  scale_x_discrete(breaks = c('control', 'test'),
                   labels = c('Control', 'Test'),
                   name = 'Treatment') +
  scale_y_continuous(breaks = seq(0, 1, 0.025),
                     name = 'Betting rate') +
  theme_minimal() +
  facet_wrap('participant_group', nrow = 1) +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        strip.text = element_text(size = 14))

ggsave('../Plots/main_analysis/betting_rates_box_yellow_treat.png',
       width = 10, height = 6)


# Exposure time
data_plot_y <- data %>%
  filter(block_type == 'C') %>%
  mutate(exp_bins = as.numeric(cut_interval(exposure_time, 3)),
         exp_num = cut_interval(exposure_time, 3)) %>%
  group_by(exp_bins, craver_2, id) %>%
  summarize(betting_rate = mean(choice)) %>%
  ungroup %>%
  group_by(exp_bins) %>%
  summarize(se = se(betting_rate),
            betting_rate = mean(betting_rate, na.rm = TRUE)) %>%
  ungroup

data_plot_b <- data %>%
  filter(block_type == 'S') %>%
  mutate(exp_bins = as.numeric(cut_interval(exposure_time, 3)),
         exp_num = cut_interval(exposure_time, 3)) %>%
  group_by(exp_bins, craver_2, id) %>%
  summarize(betting_rate = mean(choice)) %>%
  ungroup %>%
  filter(betting_rate > .6) %>%
  group_by(exp_bins) %>%
  summarize(se = se(betting_rate),
            betting_rate = mean(betting_rate, na.rm = TRUE)) %>%
  ungroup

data_plot <- data_plot_b %>%
  rbind(data_plot_y) %>%
  mutate(session_color = rep(c('Blue', 'Yellow'),
                             times = c(nrow(data_plot_b),
                                       nrow(data_plot_y))),
         session_color = factor(session_color,
                                levels = c('Blue', 'Yellow'),
                                labels = c("(A) Blue", "(B) Yellow")))


ggplot(data_plot, aes(x = exp_bins,
                      y = betting_rate,
                      col = session_color)) +
  geom_line() +
  geom_errorbar(aes(ymin = betting_rate - se,
                    ymax = betting_rate + se),
                width = 0.2) +
  labs(x = 'Previous reward exposure', y = 'Betting rate') +
  scale_x_continuous(breaks = 1:3) +
  scale_color_manual(name = 'Session color',
                     breaks = c('(A) Blue', '(B) Yellow'),
                     values = c('#0057b7', '#ffd700')) +
  theme_minimal() +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 14),
        legend.position = 'none') +
  facet_wrap('session_color', scales = 'free_y')

ggsave('../Plots/main_analysis/betting_exposure_pooled_all_bins_3.png',
       width = 10, height = 5)


# Betting rate by uncertainty and reward
data_plot_r <- data %>%
  filter(craver_2 == 1) %>%
  group_by(block_type, reward_value, id) %>%
  summarize(betting_rate = mean(choice)) %>%
  ungroup %>%
  group_by(block_type, reward_value) %>%
  summarize(se = se(betting_rate),
            betting_rate = mean(betting_rate)) %>%
  ungroup %>%
  mutate(x_ax = reward_value) %>%
  select(-reward_value)

data_plot_u <- data %>%
  filter(craver_2 == 1) %>%
  group_by(block_type, aaron_mood, id) %>%
  summarize(betting_rate = mean(choice)) %>%
  ungroup %>%
  group_by(block_type, aaron_mood) %>%
  summarize(se = se(betting_rate),
            betting_rate = mean(betting_rate)) %>%
  ungroup %>%
  mutate(x_ax = aaron_mood) %>%
  select(-aaron_mood)

data_plot <- data_plot_r %>%
  rbind(data_plot_u) %>%
  mutate(type = rep(c('Reward', 'Uncertainty'),
                    times = c(nrow(data_plot_r),
                              nrow(data_plot_u))),
         type = factor(type,
                       levels = c('Reward', 'Uncertainty'),
                       labels = c("(A) Reward", "(B) Uncertainty")),
         x_ax = factor(x_ax, levels = c("Low", "High"), labels = c("Low", "High")))


ggplot(data_plot, aes(x = x_ax, y = betting_rate, fill = block_type)) +
  geom_bar(stat='identity', position = position_dodge(.9)) +
  geom_errorbar(aes(ymin = betting_rate - se, ymax = betting_rate + se),
                position = position_dodge(.9), width = .2) +
  scale_x_discrete(name = '',
                   breaks = c('Low', 'High'),
                   labels = c('Low', 'High')) +
  scale_y_continuous(name = 'Betting rate') +
  scale_fill_manual(name = 'Session color',
                    breaks = c('C', 'S'),
                    labels = c('Yellow', 'Blue'),
                    values = c('#ffd700', '#0057b7')) +
  theme_minimal() +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        strip.text = element_text(size = 14)) +
  facet_wrap("type", nrow = 1)


ggsave('../Plots/main_analysis/betting_rate_by_unc_reward.png',
       width = 10, height = 6)


