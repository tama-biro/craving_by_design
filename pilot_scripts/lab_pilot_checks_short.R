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
  filter(!(batch == 1 &
             block_type == "S" &
             block_number == 1)) %>%
  mutate(block_type = as.factor(block_type),
         treatment = as.factor(treatment),
         exposure_time = NA,
         previous_choice = NA,
         block_number = if_else(batch == 1, block_number - 1, as.numeric(block_number)))


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
  summarize(MCQ1 = mean(MCQ_Q1),
            MCQ2 = mean(MCQ_Q2),
            MCQ3 = mean(MCQ_Q3),
            MCQ4 = mean(MCQ_Q4),
#            MCQ5 = mean(MCQ_Q5),
            MCQ6 = mean(MCQ_Q6))

data_mcq <- data_mcq %>%
  mutate(accuracy = rowMeans(select(data_mcq,
                                    starts_with('MCQ')), na.rm = TRUE))

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
  filter(odds_guess < 0.5*odds_guess_na) %>%
  select(id) %>%
  rbind(id1)


# Remove excluded
data <- data %>%
  filter(!(id %in% id2$id))


# Removing sequence 1 from dataset and making craving variables
# Comment out filter line if not removing seq 1
data <- data %>%
  filter(!(sequence_number == 1 & batch == 0))


# Betting rate in blue/yellow for test/control
data_plot <- data %>%
  mutate(yellow_23 = if_else(is.na(yellow_23), 2, yellow_23)) %>%
  filter(
#    craver_2 == 1 &
    yellow_23 != 1
  ) %>%
  group_by(treatment, block_type, id) %>%
  summarize(betting_rate = mean(choice)) %>%
  ungroup %>%
  group_by(treatment, block_type) %>%
  summarize(se = se(betting_rate),
            betting_rate = mean(betting_rate)) %>%
  ungroup


ggplot(data_plot, aes(x = treatment, y = betting_rate, fill = block_type)) +
  geom_bar(stat='identity', position = position_dodge(.9)) +
  geom_errorbar(aes(ymin = betting_rate - se, ymax = betting_rate + se),
                position = position_dodge(.9), width = .2) +
  scale_x_discrete(name = 'Treatment',
                   breaks = c('control', 'test'),
                   labels = c('Control', 'Test')) +
  scale_y_continuous(name = 'Betting rate',
                     breaks = seq(0, 1, 0.05)) +
  scale_fill_manual(name = 'Session color',
                    breaks = c('C', 'S'),
                    labels = c('Yellow', 'Blue'),
                    values = c('#ffd700', '#0057b7')) +
  theme_minimal()


ggsave('betting_rate_by_col_and_treat_all.png', width = 10, height = 8)

# Betting rates in yellow by treat
data_dists <- data %>%
  mutate(yellow_23 = if_else(is.na(yellow_23), 2, yellow_23)) %>%
  filter(yellow_23 != 1) %>%
  filter(block_type == 'C' & craver_2 == 1) %>%
  group_by(id, treatment) %>%
  summarize(betting_rate = mean(choice, na.rm=TRUE)) %>%
  ungroup


# Plot
ggplot(data_dists, aes(x = treatment, y = betting_rate)) +
  geom_boxplot() +
  scale_x_discrete(breaks = c('control', 'test'),
                   labels = c('Control', 'Test'),
                   name = 'Treatment') +
  scale_y_continuous(breaks = seq(0, 1, 0.05),
                     name = 'Betting rate') +
  # scale_y_continuous(breaks = seq(.85, 1, 0.025),
  #                    name = 'Betting rate') +
  labs(title = 'In yellow sessions') +
  theme_minimal()

ggsave('betting_rates_box_yellow_treat_test.png', width = 10, height = 7)





