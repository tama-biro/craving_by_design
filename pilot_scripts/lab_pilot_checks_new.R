
library(tidyverse)
library(effectsize)
library(janitor)

# Loading pilot data
data <- read.csv(file.choose())

# SE function
se <- function (x) {
  se = sd(x, na.rm = TRUE)/sqrt(length(x))
  return(se)
}

#### Data processing ####

# Make block_type and treatment factors
data <- data %>%
  filter(!(block_type == "S" &
           block_number == 1)) %>%
  mutate(block_type = as.factor(block_type),
         treatment = as.factor(treatment),
         exposure_time = NA,
         previous_choice = NA,
         block_number = block_number - 1)


# Get indices of interspersed blocks
idx <- which(
  data$treatment == 'control'
  & data$round_number == 1
  & data$block_type == 'S'
  & data$block_number < 6
)


# Assign interspersed blocks
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



# Remove bets where trials are missed
# Recode reward and uncertainty variable, make gender and major factors
data <- data %>%
  filter(choice != 2) %>%
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

#### Pilot checks ####

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

# Check for excluded participants
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
  summarize(odds_guess = sum(odds_guess_one_did_win,
                             odds_guess_two_did_win,
                             odds_guess_three_did_win,
                             na.rm = TRUE),
            odds_guess_na = sum(!is.na(odds_guess_one_did_win),
                                !is.na(odds_guess_two_did_win),
                                !is.na(odds_guess_three_did_win),
                                na.rm = TRUE)) %>%
  ungroup %>%
  filter(odds_guess < 0.5*odds_guess_na & odds_guess_na > 1) %>%
  select(id) %>%
  rbind(id1, data_mcq)


# Remove excluded
data <- data %>%
  filter(!(id %in% id2$id))


# Removing sequence 1 from dataset and making craving variables
# Comment out filter line if not removing seq 1
data <- data %>%
  filter(!(sequence_number == 1 & batch == 0))


