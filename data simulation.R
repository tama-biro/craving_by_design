library(tidyverse)

# Function to call
simulate_choice = function(sim_data) {
  for(i in 1:nrow(sim_data)) {
    win_chance = sim_data$win_chance[i]
    loss_chance = 1 - win_chance
    reward_value = sim_data$reward_value[i]
    uncertainty = sim_data$uncertainty[i]
    
    v = uncertainty*(win_chance*(reward_value-0.7)^alpha_1-lmbda*loss_chance*0.7^alpha_2)
    
    p_bet = 1/(1 + exp(-beta*v))
    
    if(sim_data$craver[i]) {
      p_bet = K + (1 - K)*p_bet
    }
    sim_data$choice[i] = rbinom(1, 1, p_bet)
  }
  return(sim_data)
}

# Function to call
simulate_choice_n = function(sim_data) {
  
  counter = 0
  for(i in 1:nrow(sim_data)) {
    win_chance = sim_data$win_chance[i]
    loss_chance = 1 - win_chance
    reward_value = sim_data$reward_value[i]
    uncertainty = sim_data$uncertainty[i]
    
    v = uncertainty*(win_chance*(reward_value-0.7)^alpha_1-lmbda*loss_chance*0.7^alpha_2)
    
    p_bet = 1/(1 + exp(-beta*v))
    
    if(sim_data$craver[i]) {
      if(counter >= n) {
        p_bet = K + (1 - K)*p_bet
      }
    }
    sim_data$choice[i] = rbinom(1, 1, p_bet)
    counter = counter + 1
    
    # Restart if we change repetition
    if(i + 1 %% 100 == 0) {
      counter = 0
    }
  }
  return(sim_data)
}

simulate_choice_vk = function(sim_data) {
  
  back_k = 0
  for(i in 1:nrow(sim_data)) {
    win_chance = sim_data$win_chance[i]
    loss_chance = 1 - win_chance
    reward_value = sim_data$reward_value[i]
    uncertainty = sim_data$uncertainty[i]
    
    v = uncertainty*(win_chance*(reward_value-0.7)^alpha_1-lmbda*loss_chance*0.7^alpha_2)
    
    p_bet = 1/(1 + exp(-beta*v))
    
    if(sim_data$craver[i]) {
      p_bet = K + (1 - K)*p_bet
    }
    
    # Make choice
    sim_data$choice[i] = rbinom(1, 1, p_bet)
    
    # Play game
    if(sim_data$choice[i]) {
      if(rbinom(1, 1, uncertainty)) {
        sim_data$outcome[i] = rbinom(1, 1, win_chance)*reward_value - 0.7
      } else {
        sim_data$outcome[i] = 0
      }
    } else {
      sim_data$outcome[i] = 0
    }
    
    
    if(sim_data$craver[i]) {
      K = 0
      if(back_k > 0) {
        
        # implementation of varying K from Payzan-LeNestour & Doran (2022), 1.2.2
        u = sum(if_else(sim_data$outcome[(i-back_k):i] > 0, 1, 0)*theta^(i-(i-back_k):i))
        
        K = max(1/(1+exp(-kappa_1*u))-kappa_2, 0)
        
      }
    }
    
    # Set steps back for K variable
    if(sim_data$outcome[i] < 0) {
      back_k = 0
    } else {
      back_k = back_k + 1
    }
    
    # Restart if we change repetition
    if(i + 1 %% 100 == 0) {
      back_k = 0
      K = 0
    }
  }
  return(sim_data)
}

simulate_choice_vk2 = function(sim_data) {
  
  back_k = 0
  for(i in 1:nrow(sim_data)) {
    win_chance = sim_data$win_chance[i]
    loss_chance = 1 - win_chance
    reward_value = sim_data$reward_value[i]
    uncertainty = sim_data$uncertainty[i]
    
    v = uncertainty*(win_chance*(reward_value-0.7)^alpha_1-lmbda*loss_chance*0.7^alpha_2)
    
    p_bet = 1/(1 + exp(-beta*v))
    
    if(sim_data$craver[i]) {
      p_bet = K + (1 - K)*p_bet
    }
    
    # Make choice
    sim_data$choice[i] = rbinom(1, 1, p_bet)
    
    # Play game
    if(sim_data$choice[i]) {
      if(rbinom(1, 1, uncertainty)) {
        sim_data$outcome[i] = rbinom(1, 1, win_chance)*reward_value - 0.7
      } else {
        sim_data$outcome[i] = 0
      }
    } else {
      sim_data$outcome[i] = 0
    }
    
    if(sim_data$craver[i]) {
      K = 0
      if(back_k > 0) {
        
        # implementation of varying K from Payzan-LeNestour & Doran (2022), 1.2.2
        outcomes = sim_data %>%
          slice((i-back_k):i) %>%
          filter(outcome > 0)
        
        if(nrow(outcomes)) {
          u = sum(theta^(i-(i-nrow(outcomes)):i))
          
          K = max(1/(1+exp(-kappa_1*u))-kappa_2, 0)
        }
      }
      
    }
    
    # Set steps back for K variable
    if(sim_data$outcome[i] < 0) {
      back_k = 0
    } else {
      back_k = back_k + 1
    }
    
    # Restart if we change repetition
    if(i + 1 %% 100 == 0) {
      back_k = 0
      K = 0
    }
  }
  return(sim_data)
}


simulate_choice_vk_neg_loss = function(sim_data) {
  
  back_k = 0
  for(i in 1:nrow(sim_data)) {
    win_chance = sim_data$win_chance[i]
    loss_chance = 1 - win_chance
    reward_value = sim_data$reward_value[i]
    uncertainty = sim_data$uncertainty[i]
    
    v = uncertainty*(win_chance*(reward_value-0.7)^alpha_1-lmbda*loss_chance*0.7^alpha_2)
    
    p_bet = 1/(1 + exp(-beta*v))
    
    if(sim_data$craver[i]) {
      p_bet = K + (1 - K)*p_bet
    }
    
    # Make choice
    sim_data$choice[i] = rbinom(1, 1, p_bet)
    
    # Play game
    if(sim_data$choice[i]) {
      if(rbinom(1, 1, uncertainty)) {
        sim_data$outcome[i] = rbinom(1, 1, win_chance)*reward_value - 0.7
      } else {
        sim_data$outcome[i] = 0
      }
    } else {
      sim_data$outcome[i] = 0
    }
    
    
    if(sim_data$craver[i]) {
      K = 0
      if(back_k > 0) {
        
        # implementation of varying K from Payzan-LeNestour & Doran (2022), 1.2.2
        u = sum(if_else(sim_data$outcome[(i-back_k):i] > 0, 1, 
                        if_else(sim_data$outcome[(i-back_k):i] == 0, 0, -1))*theta^(i-(i-back_k):i))
        
        K = max(1/(1+exp(-kappa_1*u))-kappa_2, 0)
        
      }
    }
    
    
    # Restart if we change repetition
    if(i + 1 %% 100 == 0) {
      back_k = 0
      K = 0
    } else {
      back_k = back_k + 1
    }
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

#### Running simulation - v1.0 ####

# Setting up parameters
alpha = 0.8
lmbda = 1.95
beta = c(0, 10, 20, 30, 40, 50)
K = c(0.05, 0.1, 0.2)
parameters = expand_grid(alpha, lmbda, beta, K)
alpha = lmbda = 1
risk_neutral <- expand_grid(alpha, lmbda, beta, K)
parameters <- rbind(parameters, risk_neutral)

tm <- proc.time()

data_compare = data.frame()
for(i in 1:nrow(parameters)) {
  alpha_1 = alpha_2 = parameters$alpha[i]
  beta = parameters$beta[i]
  K = parameters$K[i]
  lmbda = parameters$lmbda[i]
  n = parameters$n[i]
  
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
  filter(lmbda > 1.94 & 
           lmbda < 1.96 & 
           alpha == 0.8 &
           K == 0.1 &
           craver == 0 &
           win_chance == 0.8)

data_plot <- data_compare %>%
  filter(lmbda == 1 &
           alpha == 1 &
#           K == 0.1 &
           craver == 1 &
           win_chance == 0.8)

ggplot(data_plot, aes(x = beta, y = betting_rate, color = factor(K))) +
  geom_line(size = 0.6) +
  geom_errorbar(aes(ymin = betting_rate - se, ymax = betting_rate + se),
                width = 0.6) +
  scale_y_continuous(expand = c(0, 0), breaks = seq(0, 1, by = 0.05)) +
  labs(x = expression(beta), 
       y = 'Betting Rate', 
       title = expression("Cravers (blue):" ~ lambda ~ "= 1," ~ alpha[1] ~ "=" ~ alpha[2] ~ "= 1")) +
  theme_minimal() +
  guides(color=guide_legend(title='K factor'))

ggsave('risk_neutral_cravers_blue.png', width = 10, height = 7)



# Plot lambda across K
data_plot_lam <- data_compare %>%
  filter(craver == 1 &
           alpha == 0.8) %>%
  group_by(win_chance, beta, lmbda) %>%
  summarize(betting_rate = mean(betting_rate),
            se = mean(se))

win_labs <- c('Yellow', 'Blue')
names(win_labs) <- c(0.2, 0.8)

ggplot(data_plot_lam, aes(x = beta, y = betting_rate, color = factor(lmbda))) +
  geom_hline(yintercept = 0, color = 'gray') +
  geom_vline(xintercept = 0, color = 'gray') +
  geom_line(size = 0.6) +
  geom_errorbar(aes(ymin = betting_rate - se, ymax = betting_rate + se),
                width = 0.6) +
  facet_wrap(vars(win_chance), labeller = labeller(win_chance = win_labs)) +
  labs(x = expression(beta), y = 'Betting Rate', title = expression("Craver: Varying" ~ lambda)) +
  scale_y_continuous(breaks = seq(0, 1, 0.05)) +
  theme_minimal()

ggsave('varying_lambda.png', width = 8, height = 7)


#### K changes after nth exposure ####

# Setting up parameters
alpha = 0.8
lmbda = 1.95
beta = c(0, 10, 20, 30, 40, 50)
K = c(0.05, 0.1, 0.2)
n = c(0, 1, 3, 4)
parameters = expand_grid(alpha, lmbda, beta, K, n)
alpha = lmbda = 1
risk_neutral <- expand_grid(alpha, lmbda, beta, K, n)
parameters <- rbind(parameters, risk_neutral)

tm <- proc.time()

data_compare_n = data.frame()
for(i in 1:nrow(parameters)) {
  alpha_1 = alpha_2 = parameters$alpha[i]
  beta = parameters$beta[i]
  K = parameters$K[i]
  lmbda = parameters$lmbda[i]
  n = parameters$n[i]
  
  sim_data = simulate_choice_n(sim_data)
  
  data_cc = sim_data %>%
    group_by(win_chance, craver) %>%
    summarize(betting_rate = mean(choice),
              se = se(choice)) %>%
    ungroup() %>%
    mutate(alpha = alpha_1,
           beta = beta,
           K = K,
           lmbda = lmbda,
           n = n)
  
  data_compare_n = rbind(data_compare_n, data_cc)
  
  print(i)
}

proc.time() - tm

# Set up labels and plot betting rates
data_plot <- data_compare_n %>%
  filter(lmbda > 1.94 & 
           lmbda < 1.96 & 
           alpha == 0.8 &
           craver == 1 &
           win_chance == 0.8)

data_plot <- data_compare_n %>%
  filter(lmbda == 1 &
           alpha == 1 &
           craver == 1 &
           win_chance == 0.2)

ggplot(data_plot, aes(x = beta, y = betting_rate, color = factor(K))) +
  geom_line(size = 0.6) +
  geom_errorbar(aes(ymin = betting_rate - se, ymax = betting_rate + se),
                width = 0.6) +
  scale_y_continuous(expand = c(0, 0), breaks = seq(0, 1, by = 0.05)) +
  labs(x = expression(beta), 
       y = 'Betting Rate', 
       title = expression("Cravers (blue):" ~ lambda ~ "= 1.95," ~ alpha[1] ~ "=" ~ alpha[2] ~ "= 0.8. By nth exposure")) +
  facet_wrap(vars(n), ncol = 4) +
  theme_minimal() +
  guides(color=guide_legend(title='K factor'))

ggsave('lossaverse_riskaverse_by_n_cravers_blue.png', width = 18, height = 7)


#### Varying K version ####

# Setting up parameters
alpha_1 = alpha_2 = 0.8
lmbda = 1.95
beta = 30
K = 0
theta = 0.9
kappa_1 = seq(0.1, 1, 0.1)
kappa_2 = seq(0.5, 0.7, 0.05)

parameters = expand_grid(kappa_1, kappa_2)

tm <- proc.time()

data_compare2 = data.frame()
for(i in 1:nrow(parameters)) {
  kappa_1 = parameters$kappa_1[i]
  kappa_2 = parameters$kappa_2[i]
  
  sim_data = simulate_choice_vk(sim_data)
  
  data_cc = sim_data %>%
    group_by(win_chance, craver) %>%
    summarize(betting_rate = mean(choice),
              se = se(choice)) %>%
    ungroup() %>%
    mutate(kappa_1 = kappa_1,
           kappa_2 = kappa_2)
  
  data_compare2 = rbind(data_compare2, data_cc)
  
  print(i)
}

alpha_1 = alpha_2 = 0.8
lmbda = 1.95
beta = 30
K = 0
theta = 0.9
kappa_1 = seq(0.1, 1, 0.1)
kappa_2 = seq(0.5, 0.7, 0.05)

parameters = expand_grid(kappa_1, kappa_2)

data_compare3 = data.frame()
for(i in 1:nrow(parameters)) {
  kappa_1 = parameters$kappa_1[i]
  kappa_2 = parameters$kappa_2[i]
  
  sim_data = simulate_choice_vk2(sim_data)
  
  data_cc = sim_data %>%
    group_by(win_chance, craver) %>%
    summarize(betting_rate = mean(choice),
              se = se(choice)) %>%
    ungroup() %>%
    mutate(kappa_1 = kappa_1,
           kappa_2 = kappa_2)
  
  data_compare3 = rbind(data_compare3, data_cc)
  
  print(i)
}

alpha_1 = alpha_2 = 0.8
lmbda = 1.95
beta = 30
K = 0
theta = 0.9
kappa_1 = seq(0.1, 1, 0.1)
kappa_2 = seq(0.5, 0.7, 0.05)

parameters = expand_grid(kappa_1, kappa_2)

data_compare4 = data.frame()
for(i in 1:nrow(parameters)) {
  kappa_1 = parameters$kappa_1[i]
  kappa_2 = parameters$kappa_2[i]
  
  sim_data = simulate_choice_vk_neg_loss(sim_data)
  
  data_cc = sim_data %>%
    group_by(win_chance, craver) %>%
    summarize(betting_rate = mean(choice),
              se = se(choice)) %>%
    ungroup() %>%
    mutate(kappa_1 = kappa_1,
           kappa_2 = kappa_2)
  
  data_compare4 = rbind(data_compare4, data_cc)
  
  print(i)
}

proc.time() - tm


data_plot <- data_compare4 %>%
  filter(craver == 1 &
           !grepl('25', as.character(kappa_2)) &
           !grepl('75', as.character(kappa_2)) &
           win_chance == 0.2)

ggplot(data_plot, aes(x = kappa_1, y = betting_rate, color = factor(kappa_2))) +
  geom_line(size = 0.6) +
  geom_errorbar(aes(ymin = betting_rate - se, ymax = betting_rate + se),
                width = 0.03) +
  labs(x = expression(kappa[1]), 
       y = 'Betting Rate', 
       title = expression("Cravers (yellow):" ~ lambda ~ "= 1.95," ~ alpha[1] ~ "=" ~ alpha[2] ~ "= 0.8. Time varying K - negative loss")) +
  #scale_y_continuous(breaks = seq(0, 1, 0.025)) +
  scale_x_continuous(breaks = seq(-0.1, 1.1, 0.2)) +
  theme_minimal() +
  guides(color=guide_legend(title=expression(kappa[2])))

ggsave('time_dependent_k_negloss_cravers_yellow.png', width = 10, height = 7)


# Misc

alpha_1 = alpha_2 = 0.8
beta = 30
K = 0.1
lmbda = 1.95
beta = 30


sim_data = simulate_choice(sim_data)

by_part_blue <- sim_data %>%
  filter(win_chance == 0.8) %>%
  group_by(ID, craver) %>%
  summarize(betting_rate = mean(choice))


crave_labs <- c('Craver', 'Optimal')
names(crave_labs) <- c(1, 0)

ggplot(by_part_blue, aes(betting_rate)) +
  geom_histogram(bins = 20, alpha = 0.6) +
  facet_wrap(vars(craver), labeller = labeller(craver = crave_labs)) +
  theme_minimal()

ggsave('sim_distribution_in_blue.png')



