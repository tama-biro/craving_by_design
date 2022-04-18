
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
  filter(!is.na(yellow_23) & block_type == 'C') %>%
  group_by(id, yellow_23) %>%
  summarize(betting_rate = mean(choice)) %>%
  ungroup %>%
  group_by(yellow_23) %>%
  summarize(mean = mean(betting_rate))


#### Reward assymmetry ####

# Get indices of reward == 2
idx <- which(
  data$treatment == 'control'
  & data$round_number == 1
  & data$block_type == 'S'
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


