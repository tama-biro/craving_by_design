library(tidyverse)

# Function to call
simulate_choice = function(sim_data) {
  for(i in 1:nrow(sim_data)) {
    win_chance = sim_data$win_chance[i]
    loss_chance = 1 - win_chance
    reward_value = sim_data$reward_value[i]
    uncertainty = sim_data$uncertainty[i]
    
    v = uncertainty*(win_chance*(reward_value-0.7)^alpha_2-lmbda*loss_chance*0.7^alpha_2)
    
    p_bet = 1/(1 + exp(-beta*v))
    
    if(sim_data$craver[i] == 1) {
      p_bet = K + (1 - K)*p_bet
    }
    sim_data$choice[i] = rbinom(1, 1, p_bet)
  }
  return(sim_data)
}

# Function to calculate standard error
se = function(x) {
  se = sd(x)/sqrt(length(x))
  return(se)
}

# Setup per repetition
reward_test = rep(c(rep(1:2, each = 5, times = 2), rep(1, 5), rep(1:2, each = 5, times = 2), rep(2, 5)), 2)
reward_control = rep(rep(1:2, each = 5), 10)

uncertainty_test = c(rep(c(rep(c(1, 0.5), each = 10), rep(0.5, 5)), 2), 
                     rep(c(rep(c(1, 0.5), each = 10), rep(1, 5)), 2))
uncertainty_control = c(rep(c(0.5, 1), each = 10), rep(rep(c(1, 0.5), each = 10), 4))

win_test = rep(c(rep(0.8, 20), rep(0.2, 5)), 4)
win_control = c(rep(0.2, 20), rep(0.8, 80))

# Set-up per participant
reward_test_p = rep(reward_test, 6)
reward_control_p = rep(reward_control, 6)

uncertainty_test_p = rep(uncertainty_test, 6)
uncertainty_control_p = rep(uncertainty_control, 6)

win_test_p = rep(win_test, 6)
win_control_p = rep(win_control, 6)

# Make data frame
N = 200
sim_data = data.frame(ID = rep(1:N, each = 600),
                      reward_value = rep(c(reward_test_p, reward_control_p), N/2),
                      uncertainty = rep(c(uncertainty_test_p, uncertainty_control_p), N/2),
                      win_chance = rep(c(win_test_p, win_control_p), N/2),
                      craver = c(rep(0, 600*N*0.4), rep(1, 600*N*0.6)))

# Running simulation - v1.0

# Setting up parameters
alpha = c(0.8, 1)
lmbda = seq(1.8, 2.1, 0.05)
beta = c(0.25, 0.5, 1, 2, 5, 10, 20, 30)
K = c(0.1, 0.25, 0.33, 0.5)
parameters = expand_grid(alpha, lmbda, beta, K)


tm <- proc.time()

data_compare = data.frame()
for(i in 1:nrow(parameters)) {
  alpha_1 = alpha_2 = parameters$alpha[i]
  beta = parameters$beta[i]
  K = parameters$K[i]
  lmbda = parameters$lmbda[i]
  
  sim_data = simulate_choice(sim_data)
  
  data_cc = sim_data %>%
    group_by(win_chance, craver) %>%
    summarize(betting_rate = mean(choice),
              se = se(choice)) %>%
    ungroup() %>%
    mutate(alpha = alpha_1,
           beta = beta,
           K = K,
           lmbda = lmbda)
  
  data_compare = rbind(data_compare, data_cc)
  
  print(i)
}

proc.time() - tm

# Set up labels and plot betting rates
data_plot <- data_compare %>%
  filter(lmbda == 1.95 & craver == 0)

win_labs <- c('Yellow', 'Blue')
names(win_labs) <- c(0.2, 0.8)

ggplot(data_compare, aes(x = beta, y = betting_rate, color = factor(K))) +
  geom_hline(yintercept = 0, color = 'gray') +
  geom_vline(xintercept = 0, color = 'gray') +
  geom_line() +
  geom_errorbar(aes(ymin = betting_rate - se, ymax = betting_rate + se)) +
  facet_wrap(vars(win_chance), labeller = labeller(win_chance = win_labs)) +
  scale_y_continuous(limits = c(0, 1)) +
  labs(x = 'Beta', y = 'Betting Rate', title = 'Lambda = 1.95, alpha 1 and 2 = 0.8') +
  theme_minimal()

ggsave('lambda1-95_alpha0-8.png', width = 8, height = 7)



# Plot lambda across K
data_plot_lam <- data_compare %>%
  group_by(win_chance, craver, beta, lmbda) %>%
  summarize(betting_rate = mean(betting_rate),
            se = mean(se))

crave_labs <- c('Optimal', 'Craver')
names(crave_labs) <- 0:1
win_labs <- c('Yellow', 'Blue')
names(win_labs) <- c(0.2, 0.8)

ggplot(data_plot_lam, aes(x = beta, y = betting_rate, color = factor(lmbda))) +
  geom_hline(yintercept = 0, color = 'gray') +
  geom_vline(xintercept = 0, color = 'gray') +
  geom_line() +
  geom_errorbar(aes(ymin = betting_rate - se, ymax = betting_rate + se)) +
  facet_grid(vars(win_chance), vars(craver),
             labeller = labeller(craver = crave_labs,
                                 win_chance = win_labs)) +
  scale_y_continuous(limits = c(0, 1)) +
  labs(x = 'Beta', y = 'Betting Rate', title = 'Varying lambda') +
  theme_minimal()

ggsave('varying_lambda.png', width = 8, height = 7)



