
library(tidyverse)

data <- read.csv('/Users/sam/Desktop/Pilot Data CBD/data_combined.csv')

# Make block_type and treatment factors
data <- data %>%
  mutate(block_type = as.factor(block_type),
         treatment = as.factor(treatment),
         craver = as.factor(craver),
         exposure_time = NA,
         previous_choice = NA)

# Number of missed trials (choice = 2)
sum(data$choice == 2)

# Iterate through data and add exposure time
for(i in 1:nrow(data)) {
  
  if(i == 1 & data$block_type[i] == 'S') {
    data$exposure_time[i] <- 0
  } else if(data$block_type[i] == 'S' & data$block_type[i-1] == 'S' &
            data$id[i] == data$id[i-1]) {
    data$exposure_time[i] <- data$exposure_time[i-1] + 1
  } else if(data$block_type[i] == 'S') {
    data$exposure_time[i] <- 0
  }
  
  if(i != 1) {
    if(data$id[i] == data$id[i-1]) {
      data$previous_choice[i] <- data$choice[i - 1]
    }
  }
}

data <- data %>%
  filter(choice != 2 & previous_choice != 2) %>%
  mutate(aaron_mood = factor(aaron_mood, levels = 1:0,
                             labels = c('Low', 'High')),
         reward_value = factor(reward_value, levels = 1:2,
                               labels = c('Low', 'High')),
         gender = as.factor(gender),
         major = as.factor(major))

