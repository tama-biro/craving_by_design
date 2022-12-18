library(tidyverse)


filepath <- '../data/data_full.csv'
data <- read.csv(filepath)

# Iterate through data and add exposure time and previous choice
# Initialize exposure time as 0
e_time <- 0
for (i in 1:nrow(data)) {
  
  # If exposure time is 0, assign zero
  # Else assign craving variable
  if (e_time == 0) {
    data$exposure_time[i] <- 0
  } else {
    # Previous outcomes
    out_hist <- if_else(data$outcome[(i-e_time):(i-1)] > 0, 1, 0)
    out_hist <- data$reward_value[(i-e_time):(i-1)]*out_hist
    
    data$exposure_time[i] <- sum(out_hist*0.9^((e_time-1):0))
  }
  
  # Reset if new sequence
  if(i != nrow(data)) {
    if (data$sequence_number[i+1] != data$sequence_number[i]) {
      e_time <- 0
    } else {
      e_time <- e_time + 1
    }
  }
  
  # Add previous choice
  if(i != 1) {
    if(data$id[i] == data$id[i-1]) {
      data$previous_choice[i] <- data$choice[i - 1]
    }
  }
}

#### Data processing ####

# Make block_type and treatment factors
data <- data %>%
  filter(!(block_type == "S" &
             block_number == 1)) %>%
  mutate(block_type = as.factor(block_type),
         treatment = as.factor(treatment),
         block_number = block_number - 1)


# Remove bets where trials are missed
# Recode reward and uncertainty variable, make gender and major factors
data <- data %>%
  filter(choice != 2 & previous_choice != 2) %>%
  #  filter(sequence_number != 1) %>%
  mutate(aaron_mood = factor(aaron_mood, levels = 1:0,
                             labels = c('Low', 'High')),
         reward_value = factor(reward_value, levels = 1:2,
                               labels = c('Low', 'High')),
         gender = as.factor(gender),
         major = as.factor(major)) %>%
  group_by(id) %>%
  mutate(craver = if_else(sum(choice[block_type == 'C']) > 0, 1, 0),
         craver_2 = if_else(sum(choice[block_type == 'C']) > 1, 1, 0)) %>%
  ungroup


# Check MCQ total per participant
data_mcq <- data %>%
  group_by(id) %>%
  slice_head(n = 1) %>%
  select(!MCQ_Q5) %>%
  ungroup

data_mcq <- data_mcq %>%
  mutate(accuracy = rowMeans(select(data_mcq,
                                    starts_with('MCQ')), na.rm = TRUE)) %>%
  filter(accuracy < 1) %>%
  select(id)


id1 <- data %>%
  filter(craver_2 == 1) %>%
  group_by(id, treatment) %>%
  summarize(post_game = mean(post_game_quiz_correct)) %>%
  ungroup %>%
  filter(post_game == 0) %>%
  select(id)

id2 <- data %>%
  filter(craver_2 == 1) %>%
  group_by(id, treatment) %>%
  slice_head(n = 1) %>%
  ungroup %>%
  mutate(exclude = 0,
         exclude = if_else(odds_guess_one_reply == 1 &
                             odds_guess_two_did_win == 0, 1, exclude),
         exclude = if_else(odds_guess_one_reply == 2 &
                             odds_guess_two_reply == 1 &
                             odds_guess_three_did_win == 0, 1, exclude),
         exclude = if_else(pre_game_strategy == 0, 1, exclude)) %>%
  filter(exclude == 1) %>%
  select(id) %>%
  rbind(id1, data_mcq)


# Remove excluded
data <- data %>%
  filter(!(id %in% id2$id))



# Save engineered data
write.csv(data, file = '../data/data_reduced.csv', row.names = FALSE)








