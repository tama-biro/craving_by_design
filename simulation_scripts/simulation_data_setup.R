

make_100_trials_test <- function(block_order) {
  
  # Make order for yellow blocks
  y_order <- block_order[sample(1:4, 4),]
  
  # Make win_chance variable to add to df
  win_c <- rep(c(rep(0.8, 4), 0.2), each = 5)
  
  trials <- data.frame()
  for(i in 1:4) {
    # Get order of blue blocks
    b_order <- block_order[sample(1:4, 4),]
    
    # Combine blue and yellow with 5 trials for each block
    rew_c <- c(rep(b_order$rew_opt, each = 5), rep(y_order$rew_opt[i], 5))
    unc_c <- c(rep(b_order$unc_opt, each = 5), rep(y_order$unc_opt[i], 5))
    
    temp <- data.frame(reward_value = rew_c,
                       uncertainty = unc_c,
                       win_chance = win_c)
    
    trials <- rbind(trials, temp)
  }
  
  return(trials)
  
}

make_100_trials_ctrl <- function(block_order) {

  # Make order for yellow blocks
  y_order <- block_order[sample(1:4, 4),]

  # Make initial df with yellow trials
  trials <- data.frame(reward_value = rep(y_order$rew_opt, each = 5),
                       uncertainty = rep(y_order$unc_opt, each = 5))

  for(i in 1:4) {
    # Get order of blue blocks
    b_order <- block_order[sample(1:4, 4),]

    # Make temp df with blue blocks
    temp <- data.frame(reward_value = rep(b_order$rew_opt, each = 5),
                       uncertainty = rep(b_order$unc_opt, each = 5))

    trials <- rbind(trials, temp)
  }

  # Make win_chance variable to add to df
  trials$win_chance <- rep(c(0.2, rep(0.8, 4)), each = 20)

  return(trials)
  
}

# Set up parameters
unc_opt <- c(1,0.5)
rew_opt <- c(1, 2)
block_order <- expand_grid(unc_opt, rew_opt)

N <- 200

# make empty df
sim_data <- data.frame()

# Add data
for(i in 1:N) {
  
  # Run test for first half
  if(i <= N/2) {
    # Make 6 sequences
    for(j in 1:6) {
      sim_data <- rbind(sim_data, make_100_trials_test(block_order))
    }
  } else {
    # Make 6 sequences
    for(j in 1:6) {
      sim_data <- rbind(sim_data, make_100_trials_ctrl(block_order))
    }
  }
  
}

sim_data <- sim_data %>%
  mutate(ID = rep(1:N, each = 600),
         treat = rep(c(rep('test', 600), 
                       rep('control', 600)), 
                     N/2),
         UNC = NA)





