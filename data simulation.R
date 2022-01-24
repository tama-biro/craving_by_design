
library(tidyverse)

# Setting up parameters
lmbda = 2
alpha_1 = alpha_2 = 0.8
beta = 1
K = 0.5

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
sim_data = data.frame(ID = rep(1:200, each = 600),
                      reward_value = rep(c(reward_test_p, reward_control_p), 100),
                      uncertainty = rep(c(uncertainty_test_p, uncertainty_control_p), 100),
                      win_chance = rep(c(win_test_p, win_control_p), 100),
                      craver = c(rep(0, 600*80), rep(1, 600*120)))

# Running simulation - v1.0
for(i in 1:nrow(sim_data)) {
  win_chance = sim_data$win_chance[i]
  loss_chance = 1 - win_chance
  reward_value = sim_data$reward_value[i]
  uncertainty = sim_data$uncertainty[i]
  
  v = uncertainty*(win_chance*(reward_value-0.7)**alpha_2-lmbda*loss_chance*0.7**alpha_2)
  
  p_bet = 1/(1 + exp(beta*v))
  
  if(sim_data$craver[i] == 1) {
    p_bet = K + (1 - K)*p_bet
  }
  
  sim_data$choice[i] = rbinom(1, 1, p_bet)
  
}


