
library(tidyverse)

# Functions to set parameters
set_alpha <- function() {
  # E(X) = alpha/(alpha+beta)
  # For E(X) = 0.88, we can use 22/(22+3)
  alpha <- rbeta(1, 22, 3)
  
  return(alpha)
}

set_lmbda <- function() {
  #' Setting lambda from LA meta-analysis paper, p.26
  #' The estimated overall mean 1.955, 95% CI [1.824, 2.104]
  #' half-normal with abs(X)
  
  # 'Brown, Alexander L. and Imai, Taisuke and Vieider, Ferdinand and Camerer, Colin F., 
  #' Meta-Analysis of Empirical Estimates of Loss-Aversion (2021). 
  #' CESifo Working Paper No. 8848, 
  #' Available at SSRN: 
  #" https://ssrn.com/abstract=3772089
  
  lmbda <- abs(rnorm(1, 1.955, 0.14))
  
  return(lmbda)
}

set_beta <- function(varying=TRUE,
                     lower=0,
                     upper=50,
                     constant_b=30) {
  #' If varying = TRUE, set beta from uniform between bounds
  #' Else, beta is constant
  if(varying) {
    beta <- runif(1, lower, upper)
    return(beta)
  }
  return(constant_b)
}

set_kappa1 <- function() {
  #' Set kappa1 from uniform
  kappa1 <- runif(1, 0.1, 1)
  
  return(kappa1)
}

set_kappa2 <- function() {
  #' Set kappa2 from uniform
  kappa2 <- runif(1, 0.4, 1)
  
  return(kappa2)
}

set_theta <- function() {
  #' Set theta from uniform
  theta <- runif(1, 0.5, 1)
  
  return(theta)
}

# Timestop version of data simulation
simulate_choice_vk2 = function(sim_data) {
  
  # Set up parameters for first participant
  back_k = 0
  
  alpha_1 = alpha_2 = set_alpha()
  lmbda = set_lmbda()
  beta = set_beta()
  kappa_1 = set_kappa1()
  kappa_2 = set_kappa2()
  theta = set_theta()
  
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
          filter(outcome != 0) %>%
          select(outcome)
        
        if(nrow(outcomes)) {
          u = sum(if_else(outcomes$outcome > 0, 1, 0)*theta^(i-(i-nrow(outcomes)):i))
          
          K = max(1/(1+exp(-kappa_1*u))-kappa_2, 0)
        }
      }
      
    }
    
    # Restart if we change repetition
    if(i %% 100 == 0) {
      back_k = 0
      K = 0
    } else {
      back_k = back_k + 1
    }
    
    # Set parameters for new participant
    if(i %% 600 == 0) {
      alpha_1 = alpha_2 = set_alpha()
      lmbda = set_lmbda()
      beta = set_beta()
      kappa_1 = set_kappa1()
      kappa_2 = set_kappa2()
      theta = set_theta()
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

# Simulation
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
