
library(tidyverse)
library(effectsize)
library(janitor)

# SE function
se <- function (x) {
  se = sd(x, na.rm = TRUE)/sqrt(length(x))
  return(se)
}

# Loading pilot data
data <- read.csv(file.choose())

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


# 1. Fraction of cravers
data %>%
  group_by(id, treatment, craver_2) %>%
  summarize(betting_rate = mean(choice)) %>%
  ungroup %>%
  group_by(treatment) %>%
  summarize(avg = mean(craver_2), n = n()) %>%
  ungroup %>%
  bind_rows(summarize(., treatment = "Total",
                      avg = sum(avg * n) / sum(n),
                      n = sum(n))) %>%
  mutate(cravers = avg * n)


# 2. Betting rates in blue/yellow
data %>%
  filter(craver_2 == 0 & block_type == 'C') %>%
  group_by(id, treatment) %>%
  summarize(betting_rate = mean(choice)) %>%
  ungroup %>%
  group_by(treatment) %>%
  summarize(avg = mean(betting_rate),
            n = n()) %>%
  ungroup %>%
  bind_rows(summarize(., treatment = "Total",
                      avg = sum(avg * n) / sum(n),
                      n = sum(n)))



# 3. Plots plots and more plots

# Betting rate in blue/yellow for test/control
data_plot <- data %>%
#  filter(craver_2 == 1) %>%
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
  scale_y_continuous(name = 'Betting rate') +
  scale_fill_manual(name = 'Session color',
                    breaks = c('C', 'S'),
                    labels = c('Yellow', 'Blue'),
                    values = c('#ffd700', '#0057b7')) +
  theme_minimal()


ggsave('betting_rate_by_col_and_treat_pooled.png', width = 10, height = 8)

# Betting rates in yellow by treat
data_dists <- data %>%
  filter(block_type == 'S' & craver_2 == 1) %>%
  group_by(id, treatment) %>%
  summarize(betting_rate = mean(choice, na.rm=TRUE)) %>%
  ungroup


# Plot
ggplot(data_dists, aes(x = treatment, y = betting_rate)) +
  geom_boxplot() +
  scale_x_discrete(breaks = c('control', 'test'),
                   labels = c('Control', 'Test'),
                   name = 'Treatment') +
  scale_y_continuous(breaks = seq(0, 1, 0.025),
                     name = 'Betting rate') +
  labs(title = 'In blue sessions') +
  theme_minimal()

ggsave('betting_rates_box_blue_treat_test.png', width = 10, height = 7)


# Betting rate by exposure time for cravers and optimals

data_plot <- data %>%
  filter(block_type == 'S') %>%
  mutate(exp_bins = as.numeric(cut_interval(exposure_time, 3)),
         exp_num = cut_interval(exposure_time, 3)) %>%
  group_by(exp_bins, craver_2, id) %>%
  summarize(betting_rate = mean(choice)) %>%
  ungroup %>%
  filter(betting_rate > .6) %>%
  group_by(exp_bins) %>%
  summarize(se = se(betting_rate),
            betting_rate = mean(betting_rate, na.rm = TRUE)) %>%
  ungroup

ggplot(data_plot, aes(x = exp_bins,
                      y = betting_rate)) +
  geom_line() +
  geom_errorbar(aes(ymin = betting_rate - se,
                    ymax = betting_rate + se),
                width = 0.2) +
  labs(x = 'Exposure time', y = 'Betting rate',
       title = "3 bins - outliers removed") +
  scale_x_continuous(breaks = 1:3) +
  theme_minimal()

ggsave('betting_exposure_blue_all_bins_3_outlier_removed.png', width = 10, height = 7)


# 4. Descriptives of betting rates 

# By session color
data_desc <- data %>%
  group_by(id, block_type) %>%
  summarize(betting_rate = mean(choice)) %>%
  ungroup

psych::describeBy(data_desc$betting_rate, group = data_desc$block_type)

# By treatment
data_desc <- data %>%
  filter(block_type == 'C') %>%
  group_by(id, treatment) %>%
  summarize(betting_rate = mean(choice)) %>%
  ungroup

psych::describeBy(data_desc$betting_rate, group = data_desc$treatment)

# By craver/optimal
data_desc <- data %>%
  filter(block_type == 'C') %>%
  group_by(id, craver_2) %>%
  summarize(betting_rate = mean(choice)) %>%
  ungroup

psych::describeBy(data_desc$betting_rate, group = data_desc$craver_2)



