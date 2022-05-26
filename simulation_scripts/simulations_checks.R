
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
  labs(title = "Cravers") +
  theme_minimal()


ggsave('betting_rate_by_col_and_treat_beta10.png', width = 10, height = 8)


# Distributions by treatment 
data_dists <- sim_data %>%
  group_by(ID) %>%
  mutate(craver = sum(choice[win_chance == 0.2]),
         craver = if_else(craver > 1, 1, 0)) %>%
  ungroup %>%
  filter(win_chance == 0.2 & craver == 1) %>%
  group_by(ID, treat) %>%
  summarize(betting_rate = mean(choice, na.rm=TRUE)) %>%
  ungroup


# Plot
ggplot(data_dists, aes(x = treat, y = betting_rate)) +
  geom_boxplot() +
  scale_x_discrete(breaks = c('control', 'test'),
                   labels = c('Control', 'Test'),
                   name = 'Treatment') +
  labs(title = 'In yellow sessions', y = "Betting Rate") +
  theme_minimal()

ggsave('betting_rates_box_yellow_treat2.png', width = 10, height = 7)



# Check betting rate for craver/optimal in b/y
for(i in 1:5) {
  sim_data <- simulate_choice_vk2(sim_data)
  
  sim_data %>%
    group_by(ID) %>%
    mutate(craver = sum(choice[win_chance == 0.2]),
           craver = if_else(craver > 1, 1, 0)) %>%
    ungroup %>%
    group_by(ID, win_chance, craver) %>%
    summarize(betting_rate = mean(choice, na.rm = TRUE)) %>%
    group_by(win_chance, craver) %>%
    summarize(betting_rate = mean(betting_rate, na.rm = TRUE)) %>%
    print
}



# Different values of UNC
win_chance <- c(0.2, 0.8)
reward_value <- 1:2
UNC <- seq(2, 10, by = 0.25)

unc_params <- expand_grid(win_chance, reward_value, UNC)
unc_params$p_bet <- NA

alpha_1 <- alpha_2 <- 0.8
lmbda <- 1.955
beta <- 10

for(i in 1:nrow(unc_params)) {
  win_chance = unc_params$win_chance[i]
  loss_chance = 1 - win_chance
  reward_value = unc_params$reward_value[i]
  unc <- unc_params$UNC[i]
  
  v <- (win_chance*(reward_value-0.7)^alpha_1-lmbda*loss_chance*0.7^alpha_2)
  
  v <- unc*0.5*v
  
  p_bet = 1/(1 + exp(-beta*v))
  
  unc_params$p_bet[i] <- p_bet
}

unc_params$reward_value <- factor(unc_params$reward_value,
                                  levels = 1:2,
                                  labels = c("Low", "High"))
unc_params$win_chance <- factor(unc_params$win_chance,
                                levels = c(0.2, 0.8),
                                labels = c("Yellow", "Blue"))

ggplot(unc_params, aes(x = UNC, y = p_bet,
                       color = factor(reward_value))) +
  geom_line(size = 1.2) +
  labs(color = "Reward value", y = "P(bet)") +
  theme_minimal() +
  facet_wrap("win_chance")

ggsave("unc_pbet.png", width = 10, height = 8)



