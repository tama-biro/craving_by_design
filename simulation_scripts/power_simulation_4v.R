
library(tidyverse)
library(truncnorm)
library(lme4)
library(rjson)

#### Version 1 - Low beta + prospect ####

# Create list for storing results
power_list <- list(
  't1' = list('t' = c(), 'power' = c()),
  't2' = list(
    'b_rew' = c(),
    'b_unc' = c(),
    'b_int' = c(),
    'power_r' = c(),
    'power_u' = c(),
    'power_i' = c()
  ),
  't3' = list('beta_log' = c(), 'power_log' = c(), 
              'beta_lin' = c(), 'power_lin' = c()),
  't4' = list(
    'b_rew' = c(),
    'b_unc' = c(),
    'b_treat' = c(),
    'power_r' = c(),
    'power_u' = c(),
    'power_t' = c()
  ),
  't5' = list('t' = c(), 'power' = c()),
  't6' = list('beta' = c(), 'power' = c()),
  't7' = list('t' = c(), 'power' = c()),
  't8' = list('t' = c(), 'power' = c())
)


for (i in 1:100) {
  
  # Simulate dataset
  sim_data <- simulate_choice_vk2(sim_data)
  
  # Make exposure time and previous choice
  sim_data <- add_exposure_time(sim_data)
  
  # Add noise control variables
  sim_data$noise1 <- rnorm(nrow(sim_data), 0, 1)
  sim_data$noise2 <- rnorm(nrow(sim_data), 2, 3)
  
  # Test 1
  d1 <- sim_data %>%
    filter(win_chance == 0.2) %>%
    group_by(ID, reward_value) %>%
    summarize(betting_rate = mean(choice)) %>%
    ungroup() %>%
    mutate(reward = factor(reward_value,
                           levels = 1:2,
                           labels = c('Low', 'High')))
  
  t1 <- t.test(betting_rate ~ reward_value, data = d1, paired = TRUE)
  
  # Save results
  power_list$t1$t <- append(power_list$t1$t, t1$statistic)
  power_list$t1$power <- append(power_list$t1$power, t1$p.value/2)
  
  
  # Test 2
  t2 <- glmer(choice ~ factor(reward_value) + factor(uncertainty) +
                factor(win_chance) * factor(treat) + factor(previous_choice) +
                scale(noise1) + scale(noise2) + (1 | ID),
              data = sim_data, family = binomial(link = 'logit'))
  
  s <- summary(t2)
  
  # Save results
  power_list$t2$b_rew <- append(power_list$t2$b_rew, s$coefficients[2, 1])
  power_list$t2$power_r <- append(power_list$t2$power_r, s$coefficients[2, 3])
  power_list$t2$b_unc <- append(power_list$t2$b_unc, s$coefficients[3, 1])
  power_list$t2$power_u <- append(power_list$t2$power_u, s$coefficients[3, 3])
  power_list$t2$b_int <- append(power_list$t2$b_int, s$coefficients[9, 1])
  power_list$t2$power_i <- append(power_list$t2$power_i, s$coefficients[9, 3])
  
  
  # Test 3
  d2 <- sim_data %>%
    filter(win_chance == 0.2) %>%
    group_by(ID, treat) %>%
    summarize(betting_rate = mean(choice),
              noise1 = mean(noise1),
              noise2 = mean(noise2),
              craver = sum(choice)) %>%
    ungroup() %>%
    mutate(craver = if_else(craver > 1, 1, 0))
  
  t3_log <- glm(craver ~ treat + noise1 + noise2,
                data = d2, family = binomial(link = 'logit'))
  
  s_log <- summary(t3_log)
  
  t3_lin <- lm(betting_rate ~ treat + noise1 + noise2, data = d2)
  
  s_lin <- summary(t3_lin)
  
  # Save results
  power_list$t3$beta_log <- append(power_list$t3$beta_log, s_log$coefficients[2, 1])
  power_list$t3$power_log <- append(power_list$t3$power_log, s_log$coefficients[2, 3])
  power_list$t3$beta_lin <- append(power_list$t3$beta_lin, s_lin$coefficients[2, 1])
  power_list$t3$power_lin <- append(power_list$t3$power_lin, s_lin$coefficients[2, 3])
  
  
  # Test 4
  t4 <- glmer(choice ~ factor(reward_value) + factor(uncertainty) +
                factor(treat) + factor(previous_choice) +
                scale(noise1) + scale(noise2) + (1 | ID),
              data = sim_data[sim_data$win_chance == 0.2, ], 
              family = binomial(link = 'logit'))
  
  s4 <- summary(t4)
  
  # Save results
  power_list$t4$b_rew <- append(power_list$t4$b_rew, s4$coefficients[2, 1])
  power_list$t4$power_r <- append(power_list$t4$power_r, s4$coefficients[2, 3])
  power_list$t4$b_unc <- append(power_list$t4$b_unc, s4$coefficients[3, 1])
  power_list$t4$power_u <- append(power_list$t4$power_u, s4$coefficients[3, 3])
  power_list$t4$b_treat <- append(power_list$t4$b_treat, s4$coefficients[4, 1])
  power_list$t4$power_t <- append(power_list$t4$power_t, s4$coefficients[4, 3])
  
  
  # Test 5
  d5 <- sim_data %>%
    filter(win_chance == 0.8) %>%
    group_by(ID, treat) %>%
    summarize(betting_rate = mean(choice)) %>%
    ungroup()
  
  t5 <- t.test(betting_rate ~ treat, data = d5)
  
  # Save results
  power_list$t5$t <- append(power_list$t5$t, t5$statistic)
  power_list$t5$power <- append(power_list$t5$power, t5$p.value/2)
  
  
  # Test 6
  t6 <- glmer(choice ~ factor(reward_value) + factor(uncertainty) +
                factor(treat) + factor(previous_choice) +
                scale(noise1) + scale(noise2) + scale(exposure_time) +
                (1 | ID),
              data = sim_data[sim_data$win_chance == 0.8, ], 
              family = binomial(link = 'logit'))
  
  s6 <- summary(t6)
  
  # Save results
  power_list$t6$beta <- append(power_list$t6$beta, s6$coefficients[8, 1])
  power_list$t6$power <- append(power_list$t6$power, s6$coefficients[8, 3])
  
  
  # Test 7
  d7 <- sim_data %>%
    filter(win_chance == 0.2 & treat == 'test') %>%
    group_by(ID) %>%
    summarize(betting_rate = mean(choice)) %>%
    ungroup()
  
  t7 <- t.test(d7$betting_rate, mu = 0)
  
  # Save results
  power_list$t7$t <- append(power_list$t7$t, t7$statistic)
  power_list$t7$power <- append(power_list$t7$power, t7$p.value/2)
  
  
  # Test 8
  d8 <- sim_data %>%
    filter(win_chance == 0.2) %>%
    group_by(ID, uncertainty) %>%
    summarize(betting_rate = mean(choice)) %>%
    ungroup() %>%
    mutate(uncertainty = factor(uncertainty,
                                levels = c(1, 0.5),
                                labels = c('Low', 'High')))
  
  t8 <- t.test(betting_rate ~ uncertainty, data = d8, paired = TRUE)
  
  # Save results
  power_list$t8$t <- append(power_list$t8$t, t8$statistic)
  power_list$t8$power <- append(power_list$t8$power, t8$p.value/2)
  
  # Save every 10 iterations
  if(i %% 10 == 0) {
    power_list %>% 
      toJSON(indent=0, method="C" ) %>%
      write("C:/Users/samue/Documents/simulation_v3_1.json")
  }
}



#### Version 2 - Low beta + neutral####

# Functions to set parameters
set_alpha <- function() {
  # E(X) = alpha/(alpha+beta)
  alpha <- rbeta(1, 50, 1)
  
  return(alpha)
}

set_lmbda <- function() {
  #' Setting lambda from LA meta-analysis paper, p.26
  #' The estimated overall mean 1.955, 95% CI [1.824, 2.104]
  #' truncated normal with truncnorm a = 0
  
  # 'Brown, Alexander L. and Imai, Taisuke and Vieider, Ferdinand and Camerer, Colin F., 
  #' Meta-Analysis of Empirical Estimates of Loss-Aversion (2021). 
  #' CESifo Working Paper No. 8848, 
  #' Available at SSRN: 
  #" https://ssrn.com/abstract=3772089
  
  lmbda <- rtruncnorm(1, a = 0, b = 3, mean = 1, sd = 0.5)
  
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

# Timestop version of data simulation
simulate_choice_vk2 <- function(sim_data) {
  
  # Set up parameters for first participant
  back_k <- 0
  K <- 0
  
  alpha_1 <- alpha_2 <- set_alpha()
  lmbda <- set_lmbda()
  beta <- set_beta()
  kappa_1 <- set_kappa1()
  kappa_2 <- set_kappa2()
  theta <- set_theta()
  unc <- set_unc()
  
  for(i in 1:nrow(sim_data)) {
    win_chance <- sim_data$win_chance[i]
    loss_chance <- 1 - win_chance
    reward_value <- sim_data$reward_value[i]
    uncertainty <- sim_data$uncertainty[i]
    
    v <- (win_chance*(reward_value-0.7)^alpha_1-lmbda*loss_chance*0.7^alpha_2)
    
    if(uncertainty == 0.5) {
      v <- unc*uncertainty*v
    }
    
    p_bet <- 1/(1 + exp(-beta*v))
    
    p_bet <- K + (1 - K)*p_bet
    
    # Make choice
    sim_data$choice[i] <- rbinom(1, 1, p_bet)
    
    # Play game
    if(sim_data$choice[i]) {
      if(rbinom(1, 1, uncertainty)) {
        sim_data$outcome[i] <- rbinom(1, 1, win_chance)*reward_value - 0.7
      } else {
        sim_data$outcome[i] <- 0
      }
    } else {
      sim_data$outcome[i] <- 0
    }
    
    if(back_k > 0) {
      
      # implementation of varying K from Payzan-LeNestour & Doran (2022), 1.2.2
      outcomes <- sim_data %>%
        slice((i-back_k):i) %>%
        filter(outcome != 0) %>%
        select(outcome)
      
      if(nrow(outcomes)) {
        
        k_outcomes <- if_else(outcomes$outcome > 0, outcomes$outcome + 0.7, 0)
        
        u <- sum(k_outcomes*theta^(i-(i-nrow(outcomes)+1):i))
        
        K <- max(1/(1+exp(-kappa_1*u))-kappa_2, 0)
      }
    }
    
    
    # Restart if we change repetition
    if(i %% 100 == 0) {
      back_k <- 0
      K <- 0
    } else {
      back_k <- back_k + 1
    }
    
    # Set parameters for new participant
    if(i %% 600 == 0) {
      alpha_1 <- alpha_2 <- set_alpha()
      lmbda <- set_lmbda()
      beta <- set_beta()
      kappa_1 <- set_kappa1()
      kappa_2 <- set_kappa2()
      theta <- set_theta()
      unc <- set_unc()
    }
    
  }
  return(sim_data)
}


power_list <- list(
  't1' = list('t' = c(), 'power' = c()),
  't2' = list(
    'b_rew' = c(),
    'b_unc' = c(),
    'b_int' = c(),
    'power_r' = c(),
    'power_u' = c(),
    'power_i' = c()
  ),
  't3' = list('beta_log' = c(), 'power_log' = c(), 
              'beta_lin' = c(), 'power_lin' = c()),
  't4' = list(
    'b_rew' = c(),
    'b_unc' = c(),
    'b_treat' = c(),
    'power_r' = c(),
    'power_u' = c(),
    'power_t' = c()
  ),
  't5' = list('t' = c(), 'power' = c()),
  't6' = list('beta' = c(), 'power' = c()),
  't7' = list('t' = c(), 'power' = c()),
  't8' = list('t' = c(), 'power' = c())
)


for (i in 1:100) {
  
  # Simulate dataset
  sim_data <- simulate_choice_vk2(sim_data)
  
  # Make exposure time and previous choice
  sim_data <- add_exposure_time(sim_data)
  
  # Add noise control variables
  sim_data$noise1 <- rnorm(nrow(sim_data), 0, 1)
  sim_data$noise2 <- rnorm(nrow(sim_data), 2, 3)
  
  # Test 1
  d1 <- sim_data %>%
    filter(win_chance == 0.2) %>%
    group_by(ID, reward_value) %>%
    summarize(betting_rate = mean(choice)) %>%
    ungroup() %>%
    mutate(reward = factor(reward_value,
                           levels = 1:2,
                           labels = c('Low', 'High')))
  
  t1 <- t.test(betting_rate ~ reward_value, data = d1, paired = TRUE)
  
  # Save results
  power_list$t1$t <- append(power_list$t1$t, t1$statistic)
  power_list$t1$power <- append(power_list$t1$power, t1$p.value/2)
  
  
  # Test 2
  t2 <- glmer(choice ~ factor(reward_value) + factor(uncertainty) +
                factor(win_chance) * factor(treat) + factor(previous_choice) +
                scale(noise1) + scale(noise2) + (1 | ID),
              data = sim_data, family = binomial(link = 'logit'))
  
  s <- summary(t2)
  
  # Save results
  power_list$t2$b_rew <- append(power_list$t2$b_rew, s$coefficients[2, 1])
  power_list$t2$power_r <- append(power_list$t2$power_r, s$coefficients[2, 3])
  power_list$t2$b_unc <- append(power_list$t2$b_unc, s$coefficients[3, 1])
  power_list$t2$power_u <- append(power_list$t2$power_u, s$coefficients[3, 3])
  power_list$t2$b_int <- append(power_list$t2$b_int, s$coefficients[9, 1])
  power_list$t2$power_i <- append(power_list$t2$power_i, s$coefficients[9, 3])
  
  
  # Test 3
  d2 <- sim_data %>%
    filter(win_chance == 0.2) %>%
    group_by(ID, treat) %>%
    summarize(betting_rate = mean(choice),
              noise1 = mean(noise1),
              noise2 = mean(noise2),
              craver = sum(choice)) %>%
    ungroup() %>%
    mutate(craver = if_else(craver > 1, 1, 0))
  
  t3_log <- glm(craver ~ treat + noise1 + noise2,
                data = d2, family = binomial(link = 'logit'))
  
  s_log <- summary(t3_log)
  
  t3_lin <- lm(betting_rate ~ treat + noise1 + noise2, data = d2)
  
  s_lin <- summary(t3_lin)
  
  # Save results
  power_list$t3$beta_log <- append(power_list$t3$beta_log, s_log$coefficients[2, 1])
  power_list$t3$power_log <- append(power_list$t3$power_log, s_log$coefficients[2, 3])
  power_list$t3$beta_lin <- append(power_list$t3$beta_lin, s_lin$coefficients[2, 1])
  power_list$t3$power_lin <- append(power_list$t3$power_lin, s_lin$coefficients[2, 3])
  
  
  # Test 4
  t4 <- glmer(choice ~ factor(reward_value) + factor(uncertainty) +
                factor(treat) + factor(previous_choice) +
                scale(noise1) + scale(noise2) + (1 | ID),
              data = sim_data[sim_data$win_chance == 0.2, ], 
              family = binomial(link = 'logit'))
  
  s4 <- summary(t4)
  
  # Save results
  power_list$t4$b_rew <- append(power_list$t4$b_rew, s4$coefficients[2, 1])
  power_list$t4$power_r <- append(power_list$t4$power_r, s4$coefficients[2, 3])
  power_list$t4$b_unc <- append(power_list$t4$b_unc, s4$coefficients[3, 1])
  power_list$t4$power_u <- append(power_list$t4$power_u, s4$coefficients[3, 3])
  power_list$t4$b_treat <- append(power_list$t4$b_treat, s4$coefficients[4, 1])
  power_list$t4$power_t <- append(power_list$t4$power_t, s4$coefficients[4, 3])
  
  
  # Test 5
  d5 <- sim_data %>%
    filter(win_chance == 0.8) %>%
    group_by(ID, treat) %>%
    summarize(betting_rate = mean(choice)) %>%
    ungroup()
  
  t5 <- t.test(betting_rate ~ treat, data = d5)
  
  # Save results
  power_list$t5$t <- append(power_list$t5$t, t5$statistic)
  power_list$t5$power <- append(power_list$t5$power, t5$p.value/2)
  
  
  # Test 6
  t6 <- glmer(choice ~ factor(reward_value) + factor(uncertainty) +
                factor(treat) + factor(previous_choice) +
                scale(noise1) + scale(noise2) + scale(exposure_time) +
                (1 | ID),
              data = sim_data[sim_data$win_chance == 0.8, ], 
              family = binomial(link = 'logit'))
  
  s6 <- summary(t6)
  
  # Save results
  power_list$t6$beta <- append(power_list$t6$beta, s6$coefficients[8, 1])
  power_list$t6$power <- append(power_list$t6$power, s6$coefficients[8, 3])
  
  
  # Test 7
  d7 <- sim_data %>%
    filter(win_chance == 0.2 & treat == 'test') %>%
    group_by(ID) %>%
    summarize(betting_rate = mean(choice)) %>%
    ungroup()
  
  t7 <- t.test(d7$betting_rate, mu = 0)
  
  # Save results
  power_list$t7$t <- append(power_list$t7$t, t7$statistic)
  power_list$t7$power <- append(power_list$t7$power, t7$p.value/2)
  
  
  # Test 8
  d8 <- sim_data %>%
    filter(win_chance == 0.2) %>%
    group_by(ID, uncertainty) %>%
    summarize(betting_rate = mean(choice)) %>%
    ungroup() %>%
    mutate(uncertainty = factor(uncertainty,
                                levels = c(1, 0.5),
                                labels = c('Low', 'High')))
  
  t8 <- t.test(betting_rate ~ uncertainty, data = d8, paired = TRUE)
  
  # Save results
  power_list$t8$t <- append(power_list$t8$t, t8$statistic)
  power_list$t8$power <- append(power_list$t8$power, t8$p.value/2)
  
  # Save every 10 iterations
  if(i %% 10 == 0) {
    power_list %>% 
      toJSON(indent=0, method="C" ) %>%
      write("C:/Users/samue/Documents/simulation_v3_2.json")
  }
}


#### Version 3 - High beta + prospect ####

# Functions to set parameters
set_alpha <- function() {
  # E(X) = alpha/(alpha+beta)
  alpha <- rbeta(1, 22, 3)
  
  return(alpha)
}

set_lmbda <- function() {
  #' Setting lambda from LA meta-analysis paper, p.26
  #' The estimated overall mean 1.955, 95% CI [1.824, 2.104]
  #' truncated normal with truncnorm a = 0
  
  # 'Brown, Alexander L. and Imai, Taisuke and Vieider, Ferdinand and Camerer, Colin F., 
  #' Meta-Analysis of Empirical Estimates of Loss-Aversion (2021). 
  #' CESifo Working Paper No. 8848, 
  #' Available at SSRN: 
  #" https://ssrn.com/abstract=3772089
  
  lmbda <- rtruncnorm(1, a = 0, b = 3, mean = 1.955, sd = 0.5)
  
  return(lmbda)
}

set_beta <- function(varying=TRUE,
                     lower=10,
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

# Timestop version of data simulation
simulate_choice_vk2 <- function(sim_data) {
  
  # Set up parameters for first participant
  back_k <- 0
  K <- 0
  
  alpha_1 <- alpha_2 <- set_alpha()
  lmbda <- set_lmbda()
  beta <- set_beta()
  kappa_1 <- set_kappa1()
  kappa_2 <- set_kappa2()
  theta <- set_theta()
  unc <- set_unc()
  
  for(i in 1:nrow(sim_data)) {
    win_chance <- sim_data$win_chance[i]
    loss_chance <- 1 - win_chance
    reward_value <- sim_data$reward_value[i]
    uncertainty <- sim_data$uncertainty[i]
    
    v <- (win_chance*(reward_value-0.7)^alpha_1-lmbda*loss_chance*0.7^alpha_2)
    
    if(uncertainty == 0.5) {
      v <- unc*uncertainty*v
    }
    
    p_bet <- 1/(1 + exp(-beta*v))
    
    p_bet <- K + (1 - K)*p_bet
    
    # Make choice
    sim_data$choice[i] <- rbinom(1, 1, p_bet)
    
    # Play game
    if(sim_data$choice[i]) {
      if(rbinom(1, 1, uncertainty)) {
        sim_data$outcome[i] <- rbinom(1, 1, win_chance)*reward_value - 0.7
      } else {
        sim_data$outcome[i] <- 0
      }
    } else {
      sim_data$outcome[i] <- 0
    }
    
    if(back_k > 0) {
      
      # implementation of varying K from Payzan-LeNestour & Doran (2022), 1.2.2
      outcomes <- sim_data %>%
        slice((i-back_k):i) %>%
        filter(outcome != 0) %>%
        select(outcome)
      
      if(nrow(outcomes)) {
        
        k_outcomes <- if_else(outcomes$outcome > 0, outcomes$outcome + 0.7, 0)
        
        u <- sum(k_outcomes*theta^(i-(i-nrow(outcomes)+1):i))
        
        K <- max(1/(1+exp(-kappa_1*u))-kappa_2, 0)
      }
    }
    
    
    # Restart if we change repetition
    if(i %% 100 == 0) {
      back_k <- 0
      K <- 0
    } else {
      back_k <- back_k + 1
    }
    
    # Set parameters for new participant
    if(i %% 600 == 0) {
      alpha_1 <- alpha_2 <- set_alpha()
      lmbda <- set_lmbda()
      beta <- set_beta()
      kappa_1 <- set_kappa1()
      kappa_2 <- set_kappa2()
      theta <- set_theta()
      unc <- set_unc()
    }
    
  }
  return(sim_data)
}


power_list <- list(
  't1' = list('t' = c(), 'power' = c()),
  't2' = list(
    'b_rew' = c(),
    'b_unc' = c(),
    'b_int' = c(),
    'power_r' = c(),
    'power_u' = c(),
    'power_i' = c()
  ),
  't3' = list('beta_log' = c(), 'power_log' = c(), 
              'beta_lin' = c(), 'power_lin' = c()),
  't4' = list(
    'b_rew' = c(),
    'b_unc' = c(),
    'b_treat' = c(),
    'power_r' = c(),
    'power_u' = c(),
    'power_t' = c()
  ),
  't5' = list('t' = c(), 'power' = c()),
  't6' = list('beta' = c(), 'power' = c()),
  't7' = list('t' = c(), 'power' = c()),
  't8' = list('t' = c(), 'power' = c())
)


for (i in 1:100) {
  
  # Simulate dataset
  sim_data <- simulate_choice_vk2(sim_data)
  
  # Make exposure time and previous choice
  sim_data <- add_exposure_time(sim_data)
  
  # Add noise control variables
  sim_data$noise1 <- rnorm(nrow(sim_data), 0, 1)
  sim_data$noise2 <- rnorm(nrow(sim_data), 2, 3)
  
  # Test 1
  d1 <- sim_data %>%
    filter(win_chance == 0.2) %>%
    group_by(ID, reward_value) %>%
    summarize(betting_rate = mean(choice)) %>%
    ungroup() %>%
    mutate(reward = factor(reward_value,
                           levels = 1:2,
                           labels = c('Low', 'High')))
  
  t1 <- t.test(betting_rate ~ reward_value, data = d1, paired = TRUE)
  
  # Save results
  power_list$t1$t <- append(power_list$t1$t, t1$statistic)
  power_list$t1$power <- append(power_list$t1$power, t1$p.value/2)
  
  
  # Test 2
  t2 <- glmer(choice ~ factor(reward_value) + factor(uncertainty) +
                factor(win_chance) * factor(treat) + factor(previous_choice) +
                scale(noise1) + scale(noise2) + (1 | ID),
              data = sim_data, family = binomial(link = 'logit'))
  
  s <- summary(t2)
  
  # Save results
  power_list$t2$b_rew <- append(power_list$t2$b_rew, s$coefficients[2, 1])
  power_list$t2$power_r <- append(power_list$t2$power_r, s$coefficients[2, 3])
  power_list$t2$b_unc <- append(power_list$t2$b_unc, s$coefficients[3, 1])
  power_list$t2$power_u <- append(power_list$t2$power_u, s$coefficients[3, 3])
  power_list$t2$b_int <- append(power_list$t2$b_int, s$coefficients[9, 1])
  power_list$t2$power_i <- append(power_list$t2$power_i, s$coefficients[9, 3])
  
  
  # Test 3
  d2 <- sim_data %>%
    filter(win_chance == 0.2) %>%
    group_by(ID, treat) %>%
    summarize(betting_rate = mean(choice),
              noise1 = mean(noise1),
              noise2 = mean(noise2),
              craver = sum(choice)) %>%
    ungroup() %>%
    mutate(craver = if_else(craver > 1, 1, 0))
  
  t3_log <- glm(craver ~ treat + noise1 + noise2,
                data = d2, family = binomial(link = 'logit'))
  
  s_log <- summary(t3_log)
  
  t3_lin <- lm(betting_rate ~ treat + noise1 + noise2, data = d2)
  
  s_lin <- summary(t3_lin)
  
  # Save results
  power_list$t3$beta_log <- append(power_list$t3$beta_log, s_log$coefficients[2, 1])
  power_list$t3$power_log <- append(power_list$t3$power_log, s_log$coefficients[2, 3])
  power_list$t3$beta_lin <- append(power_list$t3$beta_lin, s_lin$coefficients[2, 1])
  power_list$t3$power_lin <- append(power_list$t3$power_lin, s_lin$coefficients[2, 3])
  
  
  # Test 4
  t4 <- glmer(choice ~ factor(reward_value) + factor(uncertainty) +
                factor(treat) + factor(previous_choice) +
                scale(noise1) + scale(noise2) + (1 | ID),
              data = sim_data[sim_data$win_chance == 0.2, ], 
              family = binomial(link = 'logit'))
  
  s4 <- summary(t4)
  
  # Save results
  power_list$t4$b_rew <- append(power_list$t4$b_rew, s4$coefficients[2, 1])
  power_list$t4$power_r <- append(power_list$t4$power_r, s4$coefficients[2, 3])
  power_list$t4$b_unc <- append(power_list$t4$b_unc, s4$coefficients[3, 1])
  power_list$t4$power_u <- append(power_list$t4$power_u, s4$coefficients[3, 3])
  power_list$t4$b_treat <- append(power_list$t4$b_treat, s4$coefficients[4, 1])
  power_list$t4$power_t <- append(power_list$t4$power_t, s4$coefficients[4, 3])
  
  
  # Test 5
  d5 <- sim_data %>%
    filter(win_chance == 0.8) %>%
    group_by(ID, treat) %>%
    summarize(betting_rate = mean(choice)) %>%
    ungroup()
  
  t5 <- t.test(betting_rate ~ treat, data = d5)
  
  # Save results
  power_list$t5$t <- append(power_list$t5$t, t5$statistic)
  power_list$t5$power <- append(power_list$t5$power, t5$p.value/2)
  
  
  # Test 6
  t6 <- glmer(choice ~ factor(reward_value) + factor(uncertainty) +
                factor(treat) + factor(previous_choice) +
                scale(noise1) + scale(noise2) + scale(exposure_time) +
                (1 | ID),
              data = sim_data[sim_data$win_chance == 0.8, ], 
              family = binomial(link = 'logit'))
  
  s6 <- summary(t6)
  
  # Save results
  power_list$t6$beta <- append(power_list$t6$beta, s6$coefficients[8, 1])
  power_list$t6$power <- append(power_list$t6$power, s6$coefficients[8, 3])
  
  
  # Test 7
  d7 <- sim_data %>%
    filter(win_chance == 0.2 & treat == 'test') %>%
    group_by(ID) %>%
    summarize(betting_rate = mean(choice)) %>%
    ungroup()
  
  t7 <- t.test(d7$betting_rate, mu = 0)
  
  # Save results
  power_list$t7$t <- append(power_list$t7$t, t7$statistic)
  power_list$t7$power <- append(power_list$t7$power, t7$p.value/2)
  
  
  # Test 8
  d8 <- sim_data %>%
    filter(win_chance == 0.2) %>%
    group_by(ID, uncertainty) %>%
    summarize(betting_rate = mean(choice)) %>%
    ungroup() %>%
    mutate(uncertainty = factor(uncertainty,
                                levels = c(1, 0.5),
                                labels = c('Low', 'High')))
  
  t8 <- t.test(betting_rate ~ uncertainty, data = d8, paired = TRUE)
  
  # Save results
  power_list$t8$t <- append(power_list$t8$t, t8$statistic)
  power_list$t8$power <- append(power_list$t8$power, t8$p.value/2)
  
  # Save every 10 iterations
  if(i %% 10 == 0) {
    power_list %>% 
      toJSON(indent=0, method="C" ) %>%
      write("C:/Users/samue/Documents/simulation_v3_3.json")
  }
}


#### Version 2 - High beta + neutral####

# Functions to set parameters
set_alpha <- function() {
  # E(X) = alpha/(alpha+beta)
  alpha <- rbeta(1, 50, 1)
  
  return(alpha)
}

set_lmbda <- function() {
  #' Setting lambda from LA meta-analysis paper, p.26
  #' The estimated overall mean 1.955, 95% CI [1.824, 2.104]
  #' truncated normal with truncnorm a = 0
  
  # 'Brown, Alexander L. and Imai, Taisuke and Vieider, Ferdinand and Camerer, Colin F., 
  #' Meta-Analysis of Empirical Estimates of Loss-Aversion (2021). 
  #' CESifo Working Paper No. 8848, 
  #' Available at SSRN: 
  #" https://ssrn.com/abstract=3772089
  
  lmbda <- rtruncnorm(1, a = 0, b = 3, mean = 1, sd = 0.5)
  
  return(lmbda)
}

set_beta <- function(varying=TRUE,
                     lower=10,
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

# Timestop version of data simulation
simulate_choice_vk2 <- function(sim_data) {
  
  # Set up parameters for first participant
  back_k <- 0
  K <- 0
  
  alpha_1 <- alpha_2 <- set_alpha()
  lmbda <- set_lmbda()
  beta <- set_beta()
  kappa_1 <- set_kappa1()
  kappa_2 <- set_kappa2()
  theta <- set_theta()
  unc <- set_unc()
  
  for(i in 1:nrow(sim_data)) {
    win_chance <- sim_data$win_chance[i]
    loss_chance <- 1 - win_chance
    reward_value <- sim_data$reward_value[i]
    uncertainty <- sim_data$uncertainty[i]
    
    v <- (win_chance*(reward_value-0.7)^alpha_1-lmbda*loss_chance*0.7^alpha_2)
    
    if(uncertainty == 0.5) {
      v <- unc*uncertainty*v
    }
    
    p_bet <- 1/(1 + exp(-beta*v))
    
    p_bet <- K + (1 - K)*p_bet
    
    # Make choice
    sim_data$choice[i] <- rbinom(1, 1, p_bet)
    
    # Play game
    if(sim_data$choice[i]) {
      if(rbinom(1, 1, uncertainty)) {
        sim_data$outcome[i] <- rbinom(1, 1, win_chance)*reward_value - 0.7
      } else {
        sim_data$outcome[i] <- 0
      }
    } else {
      sim_data$outcome[i] <- 0
    }
    
    if(back_k > 0) {
      
      # implementation of varying K from Payzan-LeNestour & Doran (2022), 1.2.2
      outcomes <- sim_data %>%
        slice((i-back_k):i) %>%
        filter(outcome != 0) %>%
        select(outcome)
      
      if(nrow(outcomes)) {
        
        k_outcomes <- if_else(outcomes$outcome > 0, outcomes$outcome + 0.7, 0)
        
        u <- sum(k_outcomes*theta^(i-(i-nrow(outcomes)+1):i))
        
        K <- max(1/(1+exp(-kappa_1*u))-kappa_2, 0)
      }
    }
    
    
    # Restart if we change repetition
    if(i %% 100 == 0) {
      back_k <- 0
      K <- 0
    } else {
      back_k <- back_k + 1
    }
    
    # Set parameters for new participant
    if(i %% 600 == 0) {
      alpha_1 <- alpha_2 <- set_alpha()
      lmbda <- set_lmbda()
      beta <- set_beta()
      kappa_1 <- set_kappa1()
      kappa_2 <- set_kappa2()
      theta <- set_theta()
      unc <- set_unc()
    }
    
  }
  return(sim_data)
}


power_list <- list(
  't1' = list('t' = c(), 'power' = c()),
  't2' = list(
    'b_rew' = c(),
    'b_unc' = c(),
    'b_int' = c(),
    'power_r' = c(),
    'power_u' = c(),
    'power_i' = c()
  ),
  't3' = list('beta_log' = c(), 'power_log' = c(), 
              'beta_lin' = c(), 'power_lin' = c()),
  't4' = list(
    'b_rew' = c(),
    'b_unc' = c(),
    'b_treat' = c(),
    'power_r' = c(),
    'power_u' = c(),
    'power_t' = c()
  ),
  't5' = list('t' = c(), 'power' = c()),
  't6' = list('beta' = c(), 'power' = c()),
  't7' = list('t' = c(), 'power' = c()),
  't8' = list('t' = c(), 'power' = c())
)


for (i in 1:100) {
  
  # Simulate dataset
  sim_data <- simulate_choice_vk2(sim_data)
  
  # Make exposure time and previous choice
  sim_data <- add_exposure_time(sim_data)
  
  # Add noise control variables
  sim_data$noise1 <- rnorm(nrow(sim_data), 0, 1)
  sim_data$noise2 <- rnorm(nrow(sim_data), 2, 3)
  
  # Test 1
  d1 <- sim_data %>%
    filter(win_chance == 0.2) %>%
    group_by(ID, reward_value) %>%
    summarize(betting_rate = mean(choice)) %>%
    ungroup() %>%
    mutate(reward = factor(reward_value,
                           levels = 1:2,
                           labels = c('Low', 'High')))
  
  t1 <- t.test(betting_rate ~ reward_value, data = d1, paired = TRUE)
  
  # Save results
  power_list$t1$t <- append(power_list$t1$t, t1$statistic)
  power_list$t1$power <- append(power_list$t1$power, t1$p.value/2)
  
  
  # Test 2
  t2 <- glmer(choice ~ factor(reward_value) + factor(uncertainty) +
                factor(win_chance) * factor(treat) + factor(previous_choice) +
                scale(noise1) + scale(noise2) + (1 | ID),
              data = sim_data, family = binomial(link = 'logit'))
  
  s <- summary(t2)
  
  # Save results
  power_list$t2$b_rew <- append(power_list$t2$b_rew, s$coefficients[2, 1])
  power_list$t2$power_r <- append(power_list$t2$power_r, s$coefficients[2, 3])
  power_list$t2$b_unc <- append(power_list$t2$b_unc, s$coefficients[3, 1])
  power_list$t2$power_u <- append(power_list$t2$power_u, s$coefficients[3, 3])
  power_list$t2$b_int <- append(power_list$t2$b_int, s$coefficients[9, 1])
  power_list$t2$power_i <- append(power_list$t2$power_i, s$coefficients[9, 3])
  
  
  # Test 3
  d2 <- sim_data %>%
    filter(win_chance == 0.2) %>%
    group_by(ID, treat) %>%
    summarize(betting_rate = mean(choice),
              noise1 = mean(noise1),
              noise2 = mean(noise2),
              craver = sum(choice)) %>%
    ungroup() %>%
    mutate(craver = if_else(craver > 1, 1, 0))
  
  t3_log <- glm(craver ~ treat + noise1 + noise2,
                data = d2, family = binomial(link = 'logit'))
  
  s_log <- summary(t3_log)
  
  t3_lin <- lm(betting_rate ~ treat + noise1 + noise2, data = d2)
  
  s_lin <- summary(t3_lin)
  
  # Save results
  power_list$t3$beta_log <- append(power_list$t3$beta_log, s_log$coefficients[2, 1])
  power_list$t3$power_log <- append(power_list$t3$power_log, s_log$coefficients[2, 3])
  power_list$t3$beta_lin <- append(power_list$t3$beta_lin, s_lin$coefficients[2, 1])
  power_list$t3$power_lin <- append(power_list$t3$power_lin, s_lin$coefficients[2, 3])
  
  
  # Test 4
  t4 <- glmer(choice ~ factor(reward_value) + factor(uncertainty) +
                factor(treat) + factor(previous_choice) +
                scale(noise1) + scale(noise2) + (1 | ID),
              data = sim_data[sim_data$win_chance == 0.2, ], 
              family = binomial(link = 'logit'))
  
  s4 <- summary(t4)
  
  # Save results
  power_list$t4$b_rew <- append(power_list$t4$b_rew, s4$coefficients[2, 1])
  power_list$t4$power_r <- append(power_list$t4$power_r, s4$coefficients[2, 3])
  power_list$t4$b_unc <- append(power_list$t4$b_unc, s4$coefficients[3, 1])
  power_list$t4$power_u <- append(power_list$t4$power_u, s4$coefficients[3, 3])
  power_list$t4$b_treat <- append(power_list$t4$b_treat, s4$coefficients[4, 1])
  power_list$t4$power_t <- append(power_list$t4$power_t, s4$coefficients[4, 3])
  
  
  # Test 5
  d5 <- sim_data %>%
    filter(win_chance == 0.8) %>%
    group_by(ID, treat) %>%
    summarize(betting_rate = mean(choice)) %>%
    ungroup()
  
  t5 <- t.test(betting_rate ~ treat, data = d5)
  
  # Save results
  power_list$t5$t <- append(power_list$t5$t, t5$statistic)
  power_list$t5$power <- append(power_list$t5$power, t5$p.value/2)
  
  
  # Test 6
  t6 <- glmer(choice ~ factor(reward_value) + factor(uncertainty) +
                factor(treat) + factor(previous_choice) +
                scale(noise1) + scale(noise2) + scale(exposure_time) +
                (1 | ID),
              data = sim_data[sim_data$win_chance == 0.8, ], 
              family = binomial(link = 'logit'))
  
  s6 <- summary(t6)
  
  # Save results
  power_list$t6$beta <- append(power_list$t6$beta, s6$coefficients[8, 1])
  power_list$t6$power <- append(power_list$t6$power, s6$coefficients[8, 3])
  
  
  # Test 7
  d7 <- sim_data %>%
    filter(win_chance == 0.2 & treat == 'test') %>%
    group_by(ID) %>%
    summarize(betting_rate = mean(choice)) %>%
    ungroup()
  
  t7 <- t.test(d7$betting_rate, mu = 0)
  
  # Save results
  power_list$t7$t <- append(power_list$t7$t, t7$statistic)
  power_list$t7$power <- append(power_list$t7$power, t7$p.value/2)
  
  
  # Test 8
  d8 <- sim_data %>%
    filter(win_chance == 0.2) %>%
    group_by(ID, uncertainty) %>%
    summarize(betting_rate = mean(choice)) %>%
    ungroup() %>%
    mutate(uncertainty = factor(uncertainty,
                                levels = c(1, 0.5),
                                labels = c('Low', 'High')))
  
  t8 <- t.test(betting_rate ~ uncertainty, data = d8, paired = TRUE)
  
  # Save results
  power_list$t8$t <- append(power_list$t8$t, t8$statistic)
  power_list$t8$power <- append(power_list$t8$power, t8$p.value/2)
  
  # Save every 10 iterations
  if(i %% 10 == 0) {
    power_list %>% 
      toJSON(indent=0, method="C" ) %>%
      write("C:/Users/samue/Documents/simulation_v3_4.json")
  }
}

# Shutdown
system('shutdown -s -f')
