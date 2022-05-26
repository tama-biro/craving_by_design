
library(tidyverse)
library(effectsize)

# Loading pilot data
data <- read.csv(file.choose())

# Make block_type and treatment factors
data <- data %>%
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
  
  data$exposure_time[i] <- e_time
  
  # Increment exposure time if not pass
  if (data$block_type[i] == 'S') {
    if (data$outcome[i] > 0) {
      e_time <- e_time + 1
    }
  } else {
    e_time <- 0
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


# Checking differences between first session and others
data <- data %>%
  mutate(first_sesh = if_else(block_number == 1 & sequence_number > 1,
                              1,
                              if_else(block_number == 1,
                                      2,
                                      0)))


# Betting rate in yellow, seq 1, others and all
data %>%
  filter(block_type == 'C' &
           #sequence_number != 1 &
           treatment == 'control') %>%
  summarize(mean = mean(choice))

# Fraction of yellow sessions with 1 bet
yellow_bet_count <- data %>%
  filter(block_type == 'C' & treatment == 'test') %>%
  group_by(id, block_number, sequence_number) %>%
  summarize(bets = sum(choice))

sum(yellow_bet_count$bets > 0)/nrow(yellow_bet_count)

# Fraction cravers when craving is betting twice
cravers_count <- data %>%
  filter(block_type == 'C' & treatment == 'test') %>%
  group_by(id) %>%
  summarize(bets = sum(choice))

sum(cravers_count$bets > 1)/nrow(cravers_count)


#### Answers to pre-game and MCQ ####

# Pre-game questions (control/test)
data %>%
  group_by(id, treatment) %>%
  summarize(pre_game = mean(pre_game_strategy)) %>%
  ungroup() %>%
  count(pre_game, treatment)


# MCQ accuracy test/control/overall
data %>%
  # filter(treatment == 'control') %>%
  group_by(id) %>%
  summarize(MCQ1 = mean(MCQ_Q1),
            MCQ2 = mean(MCQ_Q2),
            MCQ3 = mean(MCQ_Q3),
            MCQ4 = mean(MCQ_Q4),
            MCQ5 = mean(MCQ_Q5),
            MCQ6 = mean(MCQ_Q6)) %>%
  ungroup() %>%
  apply(MARGIN = 2, FUN = mean)


#### Betting rates control/test vs. yellow/blue for cravers ####
data_plot <- data %>%
  filter(craver == 1) %>%
  group_by(treatment, block_type) %>%
  summarize(betting_rate = mean(choice),
            se = se(choice)) %>%
  ungroup()


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


ggsave('betting_rate_by_col_and_treat.png', width = 10, height = 8)

##### Check distributions of craving in blue/yellow for optimal/cravers #####
data_dists <- data %>%
  group_by(id, block_type, craver) %>%
  summarize(betting_rate = mean(choice, na.rm=TRUE)) %>%
  ungroup() %>%
  filter(block_type == 'C')


# Plot
ggplot(data_dists, aes(x = factor(craver), y = betting_rate)) +
  geom_boxplot() +
  scale_x_discrete(breaks = 0:1, labels = c('Optimal', 'Craver'),
                   name = 'Betting type') +
  scale_y_continuous(breaks = seq(0, 0.55, 0.05),
                     name = 'Betting rate') +
  labs(title = 'In yellow sessions') +
  theme_minimal()

ggsave('betting_rates_pilot_yellow.png', width = 10, height = 7)


ggplot(data_dists, aes(betting_rate)) +
  geom_histogram(bins = 10, col = 'black', fill = 'white') +
  labs(x = 'Betting rate', y = 'Frequency',
       title = 'Betting rate for cravers in yellow sessions') +
  theme_minimal()

ggsave('betting_rates_hist_pilot_yellow_c.png', width = 10, height = 7)


# Betting rates in yellow by treat
data_dists <- data %>%
  filter(block_type == 'C') %>%
  group_by(id, treatment) %>%
  summarize(betting_rate = mean(choice, na.rm=TRUE)) %>%
  ungroup()


# Plot
ggplot(data_dists, aes(x = treatment, y = betting_rate)) +
  geom_boxplot() +
  scale_x_discrete(breaks = c('control', 'test'),
                   labels = c('Control', 'Test'),
                   name = 'Treatment') +
  scale_y_continuous(breaks = seq(0, 0.55, 0.05),
                     name = 'Betting rate') +
  labs(title = 'In yellow sessions') +
  theme_minimal()

ggsave('betting_rates_pilot_yellow_treat_test.png', width = 10, height = 7)

psych::describe(data_dists$betting_rate[data_dists$treatment == 'test'])

# Effect sizes

# Test 1
# Difference between low and high reward in yellow
data_es1 <- data %>%
  filter(block_type == 'C') %>%
  group_by(id) %>%
  summarize(betting_r_high = mean(choice[reward_value == 'High']),
            betting_r_low = mean(choice[reward_value == 'Low']),
            betting_u_high = mean(choice[aaron_mood == 'High']),
            betting_u_low = mean(choice[aaron_mood == 'Low']))

cohens_d(data_es1$betting_r_high,
         data_es1$betting_r_low,
         paired = TRUE)

cohens_d(data_es1$betting_u_high,
         data_es1$betting_u_low,
         paired = TRUE)

# Test 2
# Difference between low and high reward
# Difference between high and low uncertainty
data_es2 <- data %>%
  group_by(id) %>%
  summarize(betting_r_high = mean(choice[reward_value == 'High']),
            betting_r_low = mean(choice[reward_value == 'Low']),
            betting_u_high = mean(choice[aaron_mood == 'High']),
            betting_u_low = mean(choice[aaron_mood == 'Low']))

cohens_d(data_es2$betting_r_high,
         data_es2$betting_r_low,
         paired = TRUE)

cohens_d(data_es2$betting_u_high,
         data_es2$betting_u_low,
         paired = TRUE)

# Test 4
# Control vs Test (yellow)
data_es3 <- data %>%
  filter(block_type == 'C') %>%
  group_by(id, treatment) %>%
  summarize(betting_rate = mean(choice)) %>%
  ungroup()

cohens_d(data_es3$betting_rate[data_es3$treatment == 'test'],
         data_es3$betting_rate[data_es3$treatment == 'control'],
         paired = FALSE,
         pooled_sd = TRUE)


# Test 5
# Control vs Test (blue)
data_es4 <- data %>%
  filter(block_type == 'S') %>%
  group_by(id, treatment) %>%
  summarize(betting_rate = mean(choice)) %>%
  ungroup()

cohens_d(data_es4$betting_rate[data_es4$treatment == 'test'],
         data_es4$betting_rate[data_es4$treatment == 'control'],
         paired = FALSE,
         pooled_sd = TRUE)


# Test 6
# Exposure time
data_es5 <- data %>%
  filter(block_type == 'S')

cohens_d(data_es5$exposure_time[data_es5$choice == 1],
         data_es5$exposure_time[data_es5$choice == 0],
         paired = FALSE,
         pooled_sd = TRUE)


# Test 7
# Betting rate in yellow session in test treatment is positive
data_es6 <- data %>%
  filter(block_type == 'C' & treatment == 'test') %>%
  group_by(id) %>%
  summarize(betting_rate = mean(choice))

cohens_d(data_es6$betting_rate, mu = 0)



#### Differences in betting rates in first/other sessions ####

# Run it by treatment, change line 181 for control/test
# Note that 'control' treatment start sequences with C
# 'test' treatment start sequences with S 
data_sesh <- data %>%
  filter(treatment == 'test' & block_type == 'S') %>%
  group_by(id, first_sesh) %>%
  summarize(betting_rate = mean(choice, na.rm = TRUE)) %>%
  ungroup()

summary(aov(betting_rate ~ factor(first_sesh), data = data_sesh))

ggplot(data_sesh, aes(x = factor(first_sesh), y = betting_rate)) +
  geom_boxplot() +
  scale_x_discrete(name = 'Session',
                   breaks = 0:2,
                   labels('')) +
  labs(title = 'Test treatment (blue sessions)',
       y = 'Betting rate') +
  theme_minimal()






