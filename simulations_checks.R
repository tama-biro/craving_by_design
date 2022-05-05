
# Simulate dataset
sim_data <- simulate_choice_vk2(sim_data)

# Make exposure time and previous choice
sim_data <- add_exposure_time(sim_data)


# Plot betting rate by test/control for blue/yellow
data_plot <- sim_data %>%
  group_by(ID) %>%
  mutate(craver = sum(choice[win_chance == 0.2]),
         craver = if_else(craver > 1, 1, 0)) %>%
  ungroup %>%
  filter(craver == 1) %>%
  group_by(treat, win_chance) %>%
  summarize(betting_rate = mean(choice),
            se = se(choice)) %>%
  ungroup()


ggplot(data_plot, aes(x = treat, y = betting_rate,
                      fill = factor(win_chance))) +
  geom_bar(stat='identity', position = position_dodge(.9)) +
  geom_errorbar(aes(ymin = betting_rate - se, ymax = betting_rate + se),
                position = position_dodge(.9), width = .2) +
  scale_x_discrete(name = 'Treatment',
                   breaks = c('control', 'test'),
                   labels = c('Control', 'Test')) +
  scale_y_continuous(name = 'Betting rate') +
  scale_fill_manual(name = 'Session color',
                    breaks = c("0.2", "0.8"),
                    labels = c('Yellow', 'Blue'),
                    values = c('#ffd700', '#0057b7')) +
  labs(title = "Overall") +
  theme_minimal()


ggsave('betting_rate_by_col_and_treat_all1.png', width = 10, height = 8)


# Distribtions by treatment 
data_dists <- sim_data %>%
  group_by(ID) %>%
  mutate(craver = sum(choice[win_chance == 0.2]),
         craver = if_else(craver > 1, 1, 0)) %>%
  ungroup %>%
  group_by(ID, win_chance, craver) %>%
  summarize(betting_rate = mean(choice, na.rm=TRUE)) %>%
  ungroup() %>%
  filter(win_chance == 0.2)


data_dists <- sim_data %>%
  group_by(ID) %>%
  mutate(craver = sum(choice[win_chance == 0.2]),
         craver = if_else(craver > 1, 1, 0)) %>%
  ungroup %>%
  filter(win_chance == 0.8 & craver == 1) %>%
  group_by(ID, treat) %>%
  summarize(betting_rate = mean(choice, na.rm=TRUE)) %>%
  ungroup


# Plot
ggplot(data_dists, aes(x = treat, y = betting_rate)) +
  geom_boxplot() +
  scale_x_discrete(breaks = c('control', 'test'),
                   labels = c('Control', 'Test'),
                   name = 'Treatment') +
  labs(title = 'In blue sessions', y = "Betting Rate") +
  theme_minimal()

ggsave('betting_rates_box_blue_treat1.png', width = 10, height = 7)



# Check betting rate for craver/optimal in b/y
sim_data %>%
  group_by(ID) %>%
  mutate(craver = sum(choice[win_chance == 0.2]),
         craver = if_else(craver > 1, 1, 0)) %>%
  ungroup %>%
  group_by(ID, win_chance, craver) %>%
  summarize(betting_rate = mean(choice, na.rm = TRUE)) %>%
  group_by(win_chance, craver) %>%
  summarize(betting_rate = mean(betting_rate, na.rm = TRUE))




