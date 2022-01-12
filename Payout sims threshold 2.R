
library(ggplot2)

# Different craving types
optimal = c(1, 0)
craving_1 = c(1, 1)
craving_2 = c(1, 0.5)
craving_3 = c(1, 0.25)

craving_list = list(optimal, craving_1, craving_2, craving_3)

run_list = list()

# Loop creating data frames for plotting
for(run in c(8, 10)) {
  
  # Make temp data frame for current run and loop through craving types
  payouts = data.frame(optimal = 1, craving_1 = 1, craving_2 = 1, craving_3 = 1)
  for(col in 1:4) {
    craving_type = craving_list[[col]]
    
    # Repeat sampling 'row' times
    for(row in 1:1000) {
      total_sum = 0
      
      # Runs through every bet in the experiment and simulates the outcome
      # There are 6 sequences with 5 trials in each block
      # 4 runs of all good blocks, with 1/2 dollar win and half cheeky
      # After that, the bad blocks are simulated
      for(seq in 1:6) {
        for(trial in 1:5) {
          for(good in 1:4) {
            for(outcome in 1:2) {
              for(cheeky in c(0.5, 1)) {
                # play_1 determines if the player will play or not (from craving type)
                play_1 = rbinom(1, 1, craving_type[1])
                if(play_1 == 1) {
                  
                  # -0.7 to initiate play
                  total_sum = total_sum - 0.7
                  
                  # play_2 determines if play will happen based on cheeky Aaron
                  play_2 = rbinom(1, 1, cheeky)
                  if(play_2 == 1) {
                    
                    # If play, player gets outcome
                    total_sum = total_sum + outcome*rbinom(1, 1, 0.8)
                  } else {
                    
                    # Else, initial money returned
                    total_sum = total_sum + 0.7
                  }
                }
              }
            }
          }
          
          # Bad blocks (same as earlier, but 0.2 chance of winning)
          for(outcome in 1:2) {
            for(cheeky in c(0.5, 1)) {
              play_1 = rbinom(1, 1, craving_type[2])
              if(play_1 == 1) {
                total_sum = total_sum - 0.7
                play_2 = rbinom(1, 1, cheeky)
                if(play_2 == 1) {
                  total_sum = total_sum + outcome*rbinom(1, 1, 0.2)
                } else {
                  total_sum = total_sum + 0.7
                }
              }
            }
          }
        }
      }
      
      # if optimal play, make a plus variable with run as plus
      plus = 0
      if(col == 1) {
        high <- rbinom(1, 1, .5)
        if(high == 1) {
          plus = run
        } else {
          plus = 2
        }
      }
      
      # Threshold rule applied
      difference = total_sum + plus - 177
      
      # Less than -50 is -50 (or -50 + 'run')
      # -50 to 0 is outcome (plus 'run')
      # If positive, earnings are difference times 24 (plus 'run')
      # Maximum earnings are 110 (plus 'run')
      if(difference <= -50) {
        payouts[row, col] = round(-50, 2)
      } else if(difference <= 0 & difference > -50) {
        payouts[row, col] = round(difference, 2)
      } else {
        earnings = round(difference * 24, 2)
        if(earnings >= 110) {
          payouts[row, col] = 110
        } else {
          payouts[row, col] = earnings
        }
      }
    }
  }
  # Add to run_list
  run_list[[run]] = payouts
  print(run)
}



View(payouts)
  
par(mfrow = c(2, 2))
name_list = c('Optimal', 'Bet all the time', 
              '50% in yellow', '25% in yellow')
for(i in 1:4) {
  hist(payouts[,i], 
       main = paste(name_list[i], ' (M = ', 
                    round(mean(payouts[,i]), 2), ')', sep = ''), 
       xlab = 'Payouts', freq = FALSE)
  abline(v = mean(payouts[,i]), col='red')
}


payouts = run_list[[10]]

payouts_box = data.frame()
for(i in 1:4) {
  payouts_box = rbind(payouts_box, data.frame(rep(name_list[i], nrow(payouts)), payouts[,i]))
}

colnames(payouts_box) = c('Craving', 'Payouts')

cap = '79.96% Optimal players get a positive outcome. For betting 25%, 50% and 100%
in yellow sessions, 32.47%, 13.69% and 1.04% get a positive outcome respectively.' 

ggplot(aes(x = Craving, y = Payouts), data = payouts_box) +
  stat_boxplot(geom ='errorbar', width = 0.3) +
  geom_boxplot() +
  stat_summary(fun = 'mean', col = 'red') +
  scale_y_continuous(breaks = round(seq(min(payouts_box$Payouts), 
                                        max(payouts_box$Payouts), by = 10),1)) +
  labs(title = 'Wager set to $10 (after)',
       caption = cap) +
  theme_minimal() +
  theme(plot.caption = element_text(hjust = 0.2),
        plot.caption.position =  "plot")

ggsave('/Users/sam/Google Drive/Elise Projects/Craving by Design/Plots/box plot wager 8 (after).pdf')

for(i in 5:10) {
  data = run_list[[i]]
  for(col in 1:4) {
    print(paste(name_list[col], mean(data[,col])))
  }
}

for(i in 5:10) {
  data = run_list[[i]]
  for(col in 1:4) {
    print(paste(name_list[col], (sum(data[,col] > 0)/length(data[,col]))))
  }
}

for(col in 1:4) {
  print(paste(name_list[col], (sum(payouts[,col] > 0)/nrow(payouts))))
}





##### New payout with wager #### 
craving_type = c(1, 0)
wager_list = seq(5, 100, by = 5)
wager_out = list()

for(wager in 1:length(wager_list)) {
  payouts = c()
  for(i in 1:10000) {
    total_sum = 0
    
    for(seq in 1:6) {
      for(trial in 1:5) {
        for(good in 1:4) {
          for(outcome in 1:2) {
            for(cheeky in c(0.5, 1)) {
              play_1 = rbinom(1, 1, craving_type[1])
              if(play_1 == 1) {
                total_sum = total_sum - 0.7
                play_2 = rbinom(1, 1, cheeky)
                if(play_2 == 1) {
                  total_sum = total_sum + outcome*rbinom(1, 1, 0.8)
                } else {
                  total_sum = total_sum + 0.7
                }
              }
            }
          }
        }
        for(outcome in 1:2) {
          for(cheeky in c(0.5, 1)) {
            play_1 = rbinom(1, 1, craving_type[2])
            if(play_1 == 1) {
              total_sum = total_sum - 0.7
              play_2 = rbinom(1, 1, cheeky)
              if(play_2 == 1) {
                total_sum = total_sum + outcome*rbinom(1, 1, 0.2)
              } else {
                total_sum = total_sum + 0.7
              }
            }
          }
        }
      }
    }
    
    
    win_wager = rbinom(1, 1, 0.9)
    if(win_wager == 1) {
      plus = wager_list[wager]
    } else {
      plus = -wager_list[wager]
    }
    
    difference = total_sum - 177
      if(difference <= -50) {
        payouts[length(payouts) + 1] = round(-50, 2) + plus
      } else if(difference <= 0 & difference > -50) {
        payouts[length(payouts) + 1] = round(difference, 2) + plus
      } else {
        earnings = round(difference * 24, 2)
        if(earnings >= 110) {
          payouts[length(payouts) + 1] = 110 + plus
        } else {
          payouts[length(payouts) + 1] = earnings + plus
        }
      }
  }

  wager_out[[wager]] = payouts
}


View(payouts)

par(mfrow = c(2, 2))
name_list = c('Optimal', 'Bet all the time', 
              '50% in yellow', '25% in yellow')
for(i in 1:4) {
  hist(payouts[,i], 
       main = paste(name_list[i], ' (M = ', 
                    round(mean(payouts[,i]), 2), ')', sep = ''), 
       xlab = 'Payouts', freq = FALSE)
  abline(v = mean(payouts[,i]), col='red')
}



payouts_box = data.frame()
for(i in 1:4) {
  payouts_box = rbind(payouts_box, data.frame(rep(name_list[i], nrow(payouts)), payouts[,i]))
}

colnames(payouts_box) = c('Craving', 'Payouts')

ggplot(aes(x = Craving, y = Payouts), data = payouts_box) +
  stat_boxplot(geom ='errorbar', width = 0.3) +
  geom_boxplot() +
  stat_summary(fun = 'mean', col = 'red') +
  scale_y_continuous(breaks = round(seq(min(payouts_box$Payouts), 
                                        max(payouts_box$Payouts), by = 10),1)) +
  theme_minimal()


for(i in 1:10) {
  data = run_list[[i]]
  for(col in 1:4) {
    print(paste(name_list[col], mean(data[,col])))
  }
}

for(i in 1:10) {
  data = run_list[[i]]
  for(col in 1:4) {
    print(paste(name_list[col], (sum(data[,col] > 0)/length(data[,col]))))
  }
}

for(col in 1:4) {
  print(paste(name_list[col], (sum(payouts[,col] > 0)/nrow(payouts))))
}


#### Some % of trials ####

# Different craving types
optimal = c(1, 0)
craving_1 = c(1, 1)
craving_2 = c(1, 0.5)
craving_3 = c(1, 0.25)

craving_list = list(optimal, craving_1, craving_2, craving_3)

run_list = list()

# Loop creating data frames for plotting
for(run in c(8, 10)) {
  
  # Make temp data frame for current run and loop through craving types
  payouts = data.frame(optimal = 1, craving_1 = 1, craving_2 = 1, craving_3 = 1)
  for(col in 1:4) {
    craving_type = craving_list[[col]]
    
    # Repeat sampling 'row' times
    for(row in 1:1000) {
      total_sum = 0
      
      # Runs through every bet in the experiment and simulates the outcome
      # There are 6 sequences with 5 trials in each block
      # 4 runs of all good blocks, with 1/2 dollar win and half cheeky
      # After that, the bad blocks are simulated
      for(seq in 1:6) {
        for(trial in 1:5) {
          for(good in 1:4) {
            for(outcome in 1:2) {
              for(cheeky in c(0.5, 1)) {
                # play_1 determines if the player will play or not (from craving type)
                play_1 = rbinom(1, 1, craving_type[1])
                if(play_1 == 1) {
                  
                  # Whether or not the bet is included in payouts
                  out_binom <- rbinom(1, 1, 0.5)
                  if(out_binom == 1) {
                    # -0.7 to initiate play
                    total_sum = total_sum - 0.7
                    
                    # play_2 determines if play will happen based on cheeky Aaron
                    play_2 = rbinom(1, 1, cheeky)
                    if(play_2 == 1) {
                      
                      # If play, player gets outcome
                      total_sum = total_sum + outcome*rbinom(1, 1, 0.8)
                    } else {
                      
                      # Else, initial money returned
                      total_sum = total_sum + 0.7
                    }
                  }
                }
              }
            }
          }
          
          # Bad blocks (same as earlier, but 0.2 chance of winning)
          for(outcome in 1:2) {
            for(cheeky in c(0.5, 1)) {
              play_1 = rbinom(1, 1, craving_type[2])
              if(play_1 == 1) {
                
                out_binom <- rbinom(1, 1, 0.5)
                
                if(out_binom == 1) {
                  total_sum = total_sum - 0.7
                  play_2 = rbinom(1, 1, cheeky)
                  if(play_2 == 1) {
                    total_sum = total_sum + outcome*rbinom(1, 1, 0.2)
                  } else {
                    total_sum = total_sum + 0.7
                  }
                }
              }
            }
          }
        }
      }
      
      # if optimal play, make a plus variable with run as plus
      plus = 0
      if(col == 1) {
        plus = 8
      }
      
      total_sum <- total_sum + plus
      
      if(total_sum < -50) {
        payouts[row, col] = round(-50, 2)
      } else if(total_sum > 110) {
        payouts[row, col] = 110
      } else {
        payouts[row, col] = total_sum
      }
    }
  }
  # Add to run_list
  run_list[[run]] = payouts
  print(run)
}



View(payouts)

payouts = run_list[[10]]

payouts_box = data.frame()
for(i in 1:4) {
  payouts_box = rbind(payouts_box, data.frame(rep(name_list[i], nrow(payouts)), payouts[,i]))
}

colnames(payouts_box) = c('Craving', 'Payouts')

cap = '100% Optimal players get a positive outcome. For betting 25%, 50% and 100%
in yellow sessions, 100%, 100% and 100% get a positive outcome respectively.' 

ggplot(aes(x = Craving, y = Payouts), data = payouts_box) +
  stat_boxplot(geom ='errorbar', width = 0.3) +
  geom_boxplot() +
  stat_summary(fun = 'mean', col = 'red') +
  scale_y_continuous(breaks = round(seq(min(payouts_box$Payouts), 
                                        max(payouts_box$Payouts), by = 10),1)) +
  labs(title = 'Wager set to $10 (50% count)',
       caption = cap) +
  theme_minimal() +
  theme(plot.caption = element_text(hjust = 0.2),
        plot.caption.position =  "plot")

ggsave('/Users/sam/Google Drive/Elise Projects/Craving by Design/Plots/box plot wager 10 (50 percent).pdf')


for(col in 1:4) {
  print(paste(name_list[col], (sum(payouts[,col] > 0)/nrow(payouts))))
}





