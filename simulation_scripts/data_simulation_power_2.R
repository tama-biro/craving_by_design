
library(tidyverse)
library(truncnorm)
library(lme4)
library(effectsize)
library(rjson)
library(parallel)
library(BayesianFirstAid)

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
  # lmbda <- rtruncnorm(1, a = 0, b = 5, mean = 1.97, sd = 0.5)
  
  lmbda <- rsn(n = 1, xi = .8, omega = 1, alpha = 10, tau = 0)
  
  if(lmbda < 0) {
    return(0)
  } else if(lmbda > 6) {
    return(6)
  }
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
  kappa1 <- rtruncnorm(1, a = 0.05, b = 0.2, mean = 0.1, sd = 0.05)
  
  return(kappa1)
}

set_kappa2 <- function() {
  #' Set kappa2 from truncated normal

  kappa2 <- rtruncnorm(1, a = 0.6, b = 0.8, mean = 0.7, sd = 0.2)
  
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
  beta <- set_beta(lower = 30)
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
    if(uncertainty == 0.5 & sim_data$craver_x[i]) {
      # Inverse of xi for yellow session
      if(win_chance == 0.2) {
        v <- (1/unc)*uncertainty*v
      } else {
        v <- unc*uncertainty*v
      }
    }
    
    p_bet <- 1/(1 + exp(-beta*v))
    
    # Apply DA(t) to cravers
    if(sim_data$craver_x[i]) {
      p_bet <- K + (1 - K)*p_bet
    }
    
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
      beta <- set_beta(lower = 30)
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
  t3 <- glmer(choice ~ factor(reward_value) + factor(uncertainty) +
                factor(win_chance) + factor(treat) + factor(previous_choice) +
                scale(noise1) + scale(noise2) + 
                scale(exposure_time) + (1 | ID),
              data = sim_data, family = binomial(link = 'logit'))
  
  s <- summary(t3)
  
  # Save results
  out_vec <- c(out_vec, c(s$coefficients[9, 1],
                          s$coefficients[9, 3]))
  
  # Test 4
  d4 <- sim_data %>%
    filter(win_chance == 0.2) %>%
    group_by(ID, treat) %>%
    summarize(betting_rate = mean(choice),
              noise1 = mean(noise1),
              noise2 = mean(noise2),
              craver = sum(choice)) %>%
    ungroup() %>%
    mutate(craver = if_else(craver > 1, 1, 0))
  
  t4_log <- glm(craver ~ treat + noise1 + noise2,
                data = d4, family = binomial(link = 'logit'))
  
  s_log <- summary(t4_log)
  
  t4_lin <- lm(betting_rate ~ treat + noise1 + noise2, data = d4)
  
  s_lin <- summary(t4_lin)
  
  # Save results
  out_vec <- c(out_vec, c(s_log$coefficients[2, 1],
                          s_log$coefficients[2, 3],
                          s_lin$coefficients[2, 1],
                          s_lin$coefficients[2, 3]))
  
  # Test 5
  t5 <- glmer(choice ~ factor(reward_value) + factor(uncertainty) +
                factor(treat) + factor(previous_choice) +
                scale(noise1) + scale(noise2) + (1 | ID),
              data = sim_data[sim_data$win_chance == 0.2, ], 
              family = binomial(link = 'logit'))
  
  s5 <- summary(t5)
  
  # Save results
  out_vec <- c(out_vec, c(s5$coefficients[2, 1],
                          s5$coefficients[2, 3],
                          s5$coefficients[3, 1],
                          s5$coefficients[3, 3],
                          s5$coefficients[4, 1],
                          s5$coefficients[4, 3]))
  
  # Test 6
  d6 <- sim_data %>%
    filter(win_chance == 0.2 & treat == 'test') %>%
    group_by(ID) %>%
    summarize(betting_rate = mean(choice)) %>%
    ungroup

  t6 <- bayes.t.test(d6$betting_rate, mu = 0)
  bf <- t6$stats[1,7]/t6$stats[1,8]

  # Save results
  out_vec <- c(out_vec, bf)
  
  
  # Test 7
  d7 <- sim_data %>%
    group_by(ID, uncertainty) %>%
    summarize(betting_rate = mean(choice)) %>%
    ungroup() %>%
    mutate(uncertainty = factor(uncertainty,
                                levels = c(1, 0.5),
                                labels = c('Low', 'High')))
  
  t7 <- t.test(betting_rate ~ uncertainty, data = d7, paired = TRUE)
  d <- cohens_d(betting_rate ~ uncertainty, data = d7, paired = TRUE)
  
  # Save results
  out_vec <- c(out_vec, c(t7$statistic,
                          t7$p.value/2,
                          d$Cohens_d))
  
  return(out_vec)
}


for(k in 1:5) {
  test_parallel <- mclapply(1:10, parallel_simulation)
  
  test_parallel <- do.call(rbind, test_parallel) %>%
    as.data.frame
  
  write.csv(test_parallel, paste0('test_parallel220811_', k ,'.csv'), row.names = FALSE)
}
time_1 <- Sys.time()

test_parallel <- sapply(1:50, FUN = parallel_simulation) %>%
  t %>%
  as.data.frame

Sys.time() - time_1

test_parallel_2 <- sapply(1:100, FUN = parallel_simulation2) %>%
  t %>%
  as.data.frame

write.csv(test_parallel, 'test_parallel220802.csv', row.names = FALSE)

# sapply time = 1.006115
# loop time = 1.154574 hours

