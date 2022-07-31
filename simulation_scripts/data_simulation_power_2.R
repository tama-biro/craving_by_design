
library(tidyverse)
library(truncnorm)
library(lme4)
library(effectsize)
library(rjson)
library(parallel)

# Functions to set parameters
set_alpha <- function() {
  # E(X) = alpha/(alpha+beta)
  # For E(X) = 0.88, we can use 22/(22+3)
  # alpha <- rbeta(1, 22, 3)
  
  # risk neutral
  alpha <- rbeta(1, 99, 1)
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
  
  # lmbda <- rtruncnorm(1, a = 0, b = 3, mean = 1.955, sd = 0.5)
  
  # loss neutral
  lmbda <- rtruncnorm(1, a = 0, b = 2.5, mean = 1, sd = 0.5)
  
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
  #' Set kappa1 from truncated normal
  # kappa1 <- runif(1, 0.1, 1)
  if(rbinom(1, 1, 0.5)) {
    kappa1 <- rtruncnorm(1, a = 0.05, b = 0.2, mean = 0.1, sd = 0.05)
  } else {
    kappa1 <- .05
  }
  
  return(kappa1)
}

set_kappa2 <- function() {
  #' Set kappa2 from truncated normal

  if(rbinom(1, 1, 0.5)) {
    kappa2 <- rtruncnorm(1, a = 0.6, b = 0.8, mean = 0.7, sd = 0.2)
  } else {
    kappa2 <- .5
  }
  
  return(kappa2)
}

set_theta <- function() {
  #' Set theta from uniform
#  theta <- runif(1, 0.5, 1)
  theta <- rtruncnorm(1, a = 0.9, b = 1, mean = 0.95, sd = 0.2)
  
  return(theta)
}

set_unc <- function(upper = 10) {
  #' Set unc from uniform
  unc <- runif(1, 2, upper)
  
  return(unc)
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
    
    # Implement UNC parameter if high uncertainty
    if(uncertainty == 0.5) {
      # Inverse of xi for yellow session
      if(win_chance == 0.2) {
        v <- (1/unc)*uncertainty*v
      } else {
        v <- unc*uncertainty*v
      }
    }
    
    p_bet <- 1/(1 + exp(-beta*v))
    
    # Apply DA(t) to cravers
    # if(sim_data$craver_x[i] == 1) {
    #   p_bet <- K + (1 - K)*p_bet
    # }
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

# Exposure time function
add_exposure_time <- function(data) {
  
  # Initialize exposure time as 0
  e_time <- 0
  
  # Loop through data and add exposure time and update value
  for (i in 1:nrow(data)) {
    
    # If exposure time is 0, assign zero
    # Else assign craving variable
    if (e_time == 0) {
      data$exposure_time[i] <- 0
    } else {
      out_hist <- if_else(data$outcome[(i-e_time):(i-1)] > 0, 1, 0)
      
      data$exposure_time[i] <- sum(out_hist*0.9^((e_time-1):0))
    }
    
    # Reset if new sequence
    if(((i+1) %% 100) == 0) {
      e_time <- 0
    } else {
      e_time <- e_time + 1
    }

    
    # Add previous choice
    if(i != 1) {
      if(data$ID[i] == data$ID[i-1]) {
        data$previous_choice[i] <- data$choice[i - 1]
      }
    }
  }
  return(data)
}

# Function to calculate standard error
se = function(x) {
  se = sd(x)/sqrt(length(x))
  return(se)
}


#### Simulation ####

## The following tests will be run ##

# Test 1
# Paired, one-tailed t-test
# Difference between low/high reward in yellow
# High = higher betting

# Test 2
# Logistic mixed-model
# Difference between low/high reward overall
# High = higher betting

# Difference low/high uncertainty overall
# High = higher betting

# Interaction treat/session color
# Negative coefficient

# Add noise + previous choice

# Test 3
# logistic/linear regression by-participant
# Test treatment higher betting

# Test 4
# Logistic mixed model for betting in yellow
# Reward
# Uncertainty
# Treatment

# Test 5
# Two-sample one-tailed t-test
# Control vs test in blue
# Test higher

# Test 6
# Logistic mixed model in blue
# Exposure time positive

# Test 7
# One-sample t-test in yellow+test
# Betting different from 0

# Test 8
# Paired, one-tailed t-test
# Difference low/high uncertainty in yellow
# High = higher betting

# Create list for storing results
power_list <- list(
  't1' = list('t' = c(), 'D' = c(), 'power' = c()),
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
  't5' = list('t' = c(), 'D' = c(), 'power' = c()),
  't6' = list('beta' = c(), 'power' = c()),
  't7' = list('t' = c(), 'D' = c(), 'power' = c()),
  't8' = list('t' = c(), 'D' = c(),  'power' = c()),
  't8_b' = list('t' = c(), 'D' = c(),  'power' = c()),
  't8_y' = list('t' = c(), 'D' = c(),  'power' = c())
)

time_1 <- Sys.time()

for (i in 1:2) {
  
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
  d <- cohens_d(betting_rate ~ reward_value, data = d1, paired = TRUE)
  
  # Save results
  power_list$t1$t <- append(power_list$t1$t, t1$statistic)
  power_list$t1$power <- append(power_list$t1$power, t1$p.value/2)
  power_list$t1$D <- append(power_list$t1$D, d$Cohens_d)
  
  
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
  d <- cohens_d(betting_rate ~ treat, data = d5)
  
  # Save results
  power_list$t5$t <- append(power_list$t5$t, t5$statistic)
  power_list$t5$power <- append(power_list$t5$power, t5$p.value/2)
  power_list$t5$D <- append(power_list$t5$D, d$Cohens_d)
  
  
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
  d <- cohens_d(d7$betting_rate, mu = 0)
  
  # Save results
  power_list$t7$t <- append(power_list$t7$t, t7$statistic)
  power_list$t7$power <- append(power_list$t7$power, t7$p.value/2)
  power_list$t7$D <- append(power_list$t7$D, d$Cohens_d)
  
  
  # Test 8
  d8 <- sim_data %>%
    group_by(ID, uncertainty) %>%
    summarize(betting_rate = mean(choice)) %>%
    ungroup() %>%
    mutate(uncertainty = factor(uncertainty,
                                levels = c(1, 0.5),
                                labels = c('Low', 'High')))
  
  t8 <- t.test(betting_rate ~ uncertainty, data = d8, paired = TRUE)
  d <- cohens_d(betting_rate ~ uncertainty, data = d8, paired = TRUE)
  
  # Save results
  power_list$t8$t <- append(power_list$t8$t, t8$statistic)
  power_list$t8$power <- append(power_list$t8$power, t8$p.value/2)
  power_list$t8$D <- append(power_list$t8$D, d$Cohens_d)
  
  # Test 8.2
  d8 <- sim_data %>%
    filter(win_chance == 0.8) %>%
    group_by(ID, uncertainty) %>%
    summarize(betting_rate = mean(choice)) %>%
    ungroup() %>%
    mutate(uncertainty = factor(uncertainty,
                                levels = c(1, 0.5),
                                labels = c('Low', 'High')))
  
  t8 <- t.test(betting_rate ~ uncertainty, data = d8, paired = TRUE)
  d <- cohens_d(betting_rate ~ uncertainty, data = d8, paired = TRUE)
  
  # Save results
  power_list$t8_b$t <- append(power_list$t8_b$t, t8$statistic)
  power_list$t8_b$power <- append(power_list$t8_b$power, t8$p.value/2)
  power_list$t8_b$D <- append(power_list$t8_b$D, d$Cohens_d)
  
  # Test 8.3
  d8 <- sim_data %>%
    filter(win_chance == 0.2) %>%
    group_by(ID, uncertainty) %>%
    summarize(betting_rate = mean(choice)) %>%
    ungroup() %>%
    mutate(uncertainty = factor(uncertainty,
                                levels = c(1, 0.5),
                                labels = c('Low', 'High')))
  
  t8 <- t.test(betting_rate ~ uncertainty, data = d8, paired = TRUE)
  d <- cohens_d(betting_rate ~ uncertainty, data = d8, paired = TRUE)
  
  # Save results
  power_list$t8_y$t <- append(power_list$t8_y$t, t8$statistic)
  power_list$t8_y$power <- append(power_list$t8_y$power, t8$p.value/2)
  power_list$t8_y$D <- append(power_list$t8_y$D, d$Cohens_d)
  
  # Save every 10 iterations
  if(i %% 10 == 0) {
    power_list %>% 
      toJSON(indent=0, method="C" ) %>%
      write("simulation_v7_1.json")
  }
}

Sys.time() - time_1



parallel_simulation <- function(x) {
  # Simulate dataset
  sim_data <- simulate_choice_vk2(sim_data)
  
  # Make exposure time and previous choice
  sim_data <- add_exposure_time(sim_data)
  
  # Add noise control variables
  sim_data$noise1 <- rnorm(nrow(sim_data), 0, 1)
  sim_data$noise2 <- rnorm(nrow(sim_data), 2, 3)
  
  # Set up empty vector for output
  out_vec <- c()
  
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
  d <- cohens_d(betting_rate ~ reward_value, data = d1, paired = TRUE)
  
  # Save results
  out_vec <- c(out_vec, c(t1$statistic, t1$p.value/2, d$Cohens_d))
  
  # Test 2
  t2 <- glmer(choice ~ factor(reward_value) + factor(uncertainty) +
                factor(win_chance) * factor(treat) + factor(previous_choice) +
                scale(noise1) + scale(noise2) + (1 | ID),
              data = sim_data, family = binomial(link = 'logit'))
  
  s <- summary(t2)
  
  # Save results
  out_vec <- c(out_vec, c(s$coefficients[2, 1],
                          s$coefficients[2, 3],
                          s$coefficients[3, 1],
                          s$coefficients[3, 3],
                          s$coefficients[9, 1],
                          s$coefficients[9, 3]))
  
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
  out_vec <- c(out_vec, c(s_log$coefficients[2, 1],
                          s_log$coefficients[2, 3],
                          s_lin$coefficients[2, 1],
                          s_lin$coefficients[2, 3]))
  
  # Test 4
  t4 <- glmer(choice ~ factor(reward_value) + factor(uncertainty) +
                factor(treat) + factor(previous_choice) +
                scale(noise1) + scale(noise2) + (1 | ID),
              data = sim_data[sim_data$win_chance == 0.2, ], 
              family = binomial(link = 'logit'))
  
  s4 <- summary(t4)
  
  # Save results
  out_vec <- c(out_vec, c(s4$coefficients[2, 1],
                          s4$coefficients[2, 3],
                          s4$coefficients[3, 1],
                          s4$coefficients[3, 3],
                          s4$coefficients[4, 1],
                          s4$coefficients[4, 3]))
  
  # Test 5
  d5 <- sim_data %>%
    filter(win_chance == 0.8) %>%
    group_by(ID, treat) %>%
    summarize(betting_rate = mean(choice)) %>%
    ungroup()
  
  t5 <- t.test(betting_rate ~ treat, data = d5)
  d <- cohens_d(betting_rate ~ treat, data = d5)
  
  # Save results
  out_vec <- c(out_vec, c(t5$statistic,
                          t5$p.value/2,
                          d$Cohens_d))
  
  # Test 6
  t6 <- glmer(choice ~ factor(reward_value) + factor(uncertainty) +
                factor(treat) + factor(previous_choice) +
                scale(noise1) + scale(noise2) + scale(exposure_time) +
                (1 | ID),
              data = sim_data[sim_data$win_chance == 0.8, ], 
              family = binomial(link = 'logit'))
  
  s6 <- summary(t6)
  
  # Save results
  out_vec <- c(out_vec, c(s6$coefficients[8, 1],
                          s6$coefficients[8, 3]))
  
  # Test 7
  d7 <- sim_data %>%
    filter(win_chance == 0.2 & treat == 'test') %>%
    group_by(ID) %>%
    summarize(betting_rate = mean(choice)) %>%
    ungroup()
  
  t7 <- t.test(d7$betting_rate, mu = 0)
  d <- cohens_d(d7$betting_rate, mu = 0)
  
  # Save results
  out_vec <- c(out_vec, c(t7$statistic,
                          t7$p.value/2,
                          d$Cohens_d))
  
  # Test 8
  d8 <- sim_data %>%
    group_by(ID, uncertainty) %>%
    summarize(betting_rate = mean(choice)) %>%
    ungroup() %>%
    mutate(uncertainty = factor(uncertainty,
                                levels = c(1, 0.5),
                                labels = c('Low', 'High')))
  
  t8 <- t.test(betting_rate ~ uncertainty, data = d8, paired = TRUE)
  d <- cohens_d(betting_rate ~ uncertainty, data = d8, paired = TRUE)
  
  # Save results
  out_vec <- c(out_vec, c(t8$statistic,
                          t8$p.value/2,
                          d$Cohens_d))
  
  # Test 8.2
  d8 <- sim_data %>%
    filter(win_chance == 0.8) %>%
    group_by(ID, uncertainty) %>%
    summarize(betting_rate = mean(choice)) %>%
    ungroup() %>%
    mutate(uncertainty = factor(uncertainty,
                                levels = c(1, 0.5),
                                labels = c('Low', 'High')))
  
  t8 <- t.test(betting_rate ~ uncertainty, data = d8, paired = TRUE)
  d <- cohens_d(betting_rate ~ uncertainty, data = d8, paired = TRUE)
  
  # Save results
  out_vec <- c(out_vec, c(t8$statistic,
                          t8$p.value/2,
                          d$Cohens_d))
  
  # Test 8.3
  d8 <- sim_data %>%
    filter(win_chance == 0.2) %>%
    group_by(ID, uncertainty) %>%
    summarize(betting_rate = mean(choice)) %>%
    ungroup() %>%
    mutate(uncertainty = factor(uncertainty,
                                levels = c(1, 0.5),
                                labels = c('Low', 'High')))
  
  t8 <- t.test(betting_rate ~ uncertainty, data = d8, paired = TRUE)
  d <- cohens_d(betting_rate ~ uncertainty, data = d8, paired = TRUE)
  
  # Save results
  out_vec <- c(out_vec, c(t8$statistic,
                          t8$p.value/2,
                          d$Cohens_d))
  
  return(out_vec)
}

time_1 <- Sys.time()

test_parallel <- sapply(1:100, FUN = parallel_simulation) %>%
  t %>%
  as.data.frame

Sys.time() - time_1

write.csv(test_parallel, 'test_parallel220725_v1.csv', row.names = FALSE)

# sapply time = 1.006115
# loop time = 1.154574 hours

