
library(tidyverse)

# Get indices of interspersed blocks
idx <- which(
  data$treatment == 'control'
  & data$round_number == 1
  & data$block_type == 'S'
  & data$block_number < 6
)



data$yellow_23 <- NA
for(i in idx) {
  if(data$block_number[i] == 3) {
    
    data$yellow_23[
      data$sequence_number == data$sequence_number[i] &
        data$block_number %in% c(2, 5) &
        data$id == data$id[i]
    ] <- 1
    
    data$yellow_23[
      data$sequence_number == data$sequence_number[i] &
        data$block_number %in% c(1, 4) &
        data$id == data$id[i]
    ] <- 0
    
  } else if(data$block_number[i] == 2) {
    
    data$yellow_23[
      data$sequence_number == data$sequence_number[i] &
        data$block_number %in% c(4, 5) &
        data$id == data$id[i]
    ] <- 1
    
    data$yellow_23[
      data$sequence_number == data$sequence_number[i] &
        data$block_number %in% c(1, 3) &
        data$id == data$id[i]
    ] <- 0
    
  } else if(data$block_number[i] == 4) {
    
    data$yellow_23[
      data$sequence_number == data$sequence_number[i] &
        data$block_number %in% c(2, 3) &
        data$id == data$id[i]
    ] <- 1
    
    data$yellow_23[
      data$sequence_number == data$sequence_number[i] &
        data$block_number %in% c(1, 5) &
        data$id == data$id[i]
    ] <- 0
    
  }
}


compare_inter <- data %>%
  filter(!is.na(yellow_23) & block_type == 'C' &
           treatment == 'control') %>%
  group_by(id, yellow_23, sequence_number) %>%
  summarize(betting_rate = mean(choice)) %>%
  ungroup %>%
  group_by(yellow_23) %>%
  summarize(mean = mean(betting_rate))

# Plot
ggplot(compare_inter, aes(x = sequence_number,
                          y = mean,
                          color = factor(yellow_23))) +
  geom_line() +
  scale_color_manual(name = 'Trial order',
                     breaks = c('0', '1'),
                     labels = c('First (pink)',
                                'Second/third (green)'),
                     values = c('#c40872', '#20b004')) +
  labs(x = 'Sequence number',
       y = 'Betting rate') +
  theme_minimal()

ggsave("bet_by_sequence_and_trial_order.png",
       width = 10, height = 6)


#### Reward asymmetry ####

# Get indices of first trial in session
idx <- which(data$round_number == 1)

data$reward_diff <- NA
for(i in idx[2:length(idx)]) {
  
  # Check if block is after same color and participant
  if(data$id[i-1] == data$id[i] &
     data$block_type[i-1] == data$block_type[i]) {
    
    # Check if reward is up, down or same
    if(data$reward_value[i] > data$reward_value[i-1]) {
      data$reward_diff[
        data$sequence_number == data$sequence_number[i] &
          data$block_number == data$block_number[i] &
          data$id == data$id[i]
      ] <- 'Up'
    } else if(data$reward_value[i] < data$reward_value[i-1]) {
      data$reward_diff[
        data$sequence_number == data$sequence_number[i] &
          data$block_number == data$block_number[i] &
          data$id == data$id[i]
      ] <- 'Down'
    } else {
      data$reward_diff[
        data$sequence_number == data$sequence_number[i] &
          data$block_number == data$block_number[i] &
          data$id == data$id[i]
      ] <- 'Constant'
    }
  }
}

data %>%
  filter(!is.na(reward_diff) & block_type == 'S') %>%
  group_by(id, reward_diff) %>%
  summarize(betting_rate = mean(choice)) %>%
  ungroup %>%
  group_by(reward_diff) %>%
  summarize(mean = mean(betting_rate))





