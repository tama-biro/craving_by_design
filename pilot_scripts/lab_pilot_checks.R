
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
             block_number == 1 &
             sequence_number == 1)) %>%
  mutate(block_type = as.factor(block_type),
         treatment = as.factor(treatment),
         exposure_time = NA,
         previous_choice = NA)

# Number of missed trials (choice = 2)
# This is when participants take too long to answer and automatically skip
sum(data$choice == 2)

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

# Remove bets where trials are missed
# Recode reward and uncertainty variable, make gender and major factors
data <- data %>%
  filter(choice != 2) %>%
  mutate(aaron_mood = factor(aaron_mood, levels = 1:0,
                             labels = c('Low', 'High')),
         reward_value = factor(reward_value, levels = 1:2,
                               labels = c('Low', 'High')),
         gender = as.factor(gender),
         major = as.factor(major))

#### Pilot checks ####

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
  filter(!(sequence_number == 1 & batch == 0)) %>%
  group_by(id) %>%
  mutate(craver = if_else(sum(choice[block_type == 'C']) > 0, 1, 0),
         craver_2 = if_else(sum(choice[block_type == 'C']) > 1, 1, 0)) %>%
  ungroup

# 1. Fraction of cravers
data %>%
  group_by(id, treatment, craver) %>%
  summarize(betting_rate = mean(choice)) %>%
  ungroup %>%
  group_by(treatment) %>%
  summarize(avg = mean(craver), n = n()) %>%
  ungroup %>%
  bind_rows(summarize(., treatment = "Total",
                      avg = sum(avg * n) / sum(n),
                      n = sum(n))) %>%
  mutate(cravers = avg * n)


# 2. Betting rates in blue/yellow
data %>%
  filter(craver_2 == 1 & block_type == 'C') %>%
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


# 3. Pre-game strategy and post-game MCQ

# Pre-game questions (control/test)
data %>%
  group_by(id, craver_2) %>%
  slice_head(n = 1) %>%
  ungroup() %>%
  count(pre_game_strategy, craver_2)


# MCQ accuracy test/control/overall
data %>%
  filter(craver_2 == 1) %>%
#  filter(treatment == 'test') %>%
  group_by(id) %>%
  summarize(MCQ1 = mean(MCQ_Q1),
            MCQ2 = mean(MCQ_Q2),
            MCQ3 = mean(MCQ_Q3),
            MCQ4 = mean(MCQ_Q4),
            MCQ5 = mean(MCQ_Q5),
            MCQ6 = mean(MCQ_Q6)) %>%
  ungroup() %>%
  apply(MARGIN = 2, FUN = mean)


# 4. Plots plots and more plots

# Betting rate in blue/yellow for test/control
data_plot <- data %>%
  mutate(yellow_23 = if_else(is.na(yellow_23), 2, yellow_23)) %>%
   filter(
    craver_2 == 1 &
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
  scale_y_continuous(name = 'Betting rate') +
  scale_fill_manual(name = 'Session color',
                    breaks = c('C', 'S'),
                    labels = c('Yellow', 'Blue'),
                    values = c('#ffd700', '#0057b7')) +
  theme_minimal()


ggsave('betting_rate_by_col_and_treat_cravers.png', width = 10, height = 8)

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


# Betting rate by exposure time for cravers and optimals

data_plot <- data %>%
  filter(
    block_type == 'S'
#    & treatment == 'test'
#    & craver_2 == 1
    ) %>%
  mutate(exp_bins = as.numeric(cut_interval(exposure_time, 7)),
         exp_num = cut_interval(exposure_time, 7)) %>%
  group_by(exp_bins, craver_2, id) %>%
  summarize(betting_rate = mean(choice)) %>%
  ungroup %>%
  group_by(exp_bins, craver_2) %>%
  summarize(se = se(betting_rate),
            betting_rate = mean(betting_rate, na.rm = TRUE)) %>%
  ungroup

ggplot(data_plot, aes(x = exp_bins,
                      y = betting_rate,
                      col = factor(craver_2))) +
  geom_line() +
  geom_errorbar(aes(ymin = betting_rate - se,
                    ymax = betting_rate + se),
                width = 0.2) +
  scale_color_manual(name = 'Type', breaks = c(0, 1),
                     labels = c('Optimal', 'Craver'),
                     values = c('#151AD4', '#E32424')) +
  labs(x = 'Exposure time', y = 'Betting rate',
       title = "7 bins") +
  theme_minimal()

ggsave('betting_exposure_blue_all_bins_7.png', width = 10, height = 7)


# Betting rate in first and second half of blue blocks

data_plot <- data %>%
  filter(craver_2 == 0 & block_type == 'S' & block_number > 12) %>%
  group_by(id, treatment) %>%
  summarize(betting_rate = mean(choice)) %>%
  ungroup %>%
  group_by(treatment) %>%
  summarize(avg = mean(betting_rate),
            se = se(betting_rate),
            n = n()) %>%
  ungroup %>%
  bind_rows(summarize(., treatment = "Total",
                      avg = sum(avg * n) / sum(n),
                      se = sum(se * n) / sum(n),
                      n = sum(n)))

ggplot(data_plot, aes(x = treatment, y = avg)) +
  geom_bar(stat = 'identity',
           position = position_dodge(0.7),
           width = 0.5,
           fill = '#151AD4') +
  geom_errorbar(aes(ymin = avg - se, ymax = avg + se),
                position = position_dodge(0.7),
                width = 0.2) +
  scale_x_discrete(name = 'Treatment', breaks = c('control', 'test', 'Total'),
                   labels = c('Control', 'Test', 'Total')) +
  labs(y = 'Betting rate', title = 'Second half of blue session') +
  theme_minimal()

ggsave('betting_rate_blue_halves_second.png', width = 10, height = 7)


# 5. Descriptives of betting rates 

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


