
library(tidyverse)
library(effectsize)

# Function to calculate standard error
se = function(x) {
  se = sd(x)/sqrt(length(x))
  return(se)
}

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
for(i in 1:nrow(data)) {
  
  if(i == 1 & data$block_type[i] == 'S') {
    data$exposure_time[i] <- 0
  } else if(i == 1 & data$block_type[i] == 'C') {
    data$exposure_time[i] <- NA
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


# Removing first session in sequence 2-6
# Rerun analysis to compare
data <- data %>%
  filter(!(block_number == 1 & sequence_number > 1))




##### Check distributions of craving in blue/yellow for optimal/cravers #####
data_dists <- data %>%
  group_by(id, block_type, craver) %>%
  summarize(betting_rate = mean(choice, na.rm=TRUE)) %>%
  ungroup() %>%
  filter(block_type == 'S')


# Plot
ggplot(data_dists, aes(x = factor(craver), y = betting_rate)) +
  geom_boxplot() +
  scale_x_discrete(breaks = 0:1, labels = c('Optimal', 'Craver'),
                   name = 'Betting type') +
  scale_y_continuous(breaks = seq(0.5, 1, 0.05),
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


##### Check distributions of craving in blue/yellow for test/control #####
data_dists2 <- data %>%
  group_by(block_type, treatment) %>%
  summarize(betting_rate = mean(choice, na.rm=TRUE),
            se = se(choice)) %>%
  ungroup()


# Plot
ggplot(data_dists2, aes(x = treatment, 
                        y = betting_rate,
                        fill = block_type,
                        group = block_type)) +
  geom_bar(stat='identity', position = position_dodge(0.7),
           width = .4) +
  geom_errorbar(aes(ymin = betting_rate - se,
                    ymax = betting_rate + se),
                width = 0.1, position = position_dodge(0.7)) +
  scale_x_discrete(breaks = c('control', 'test'), 
                   labels = c('Control', 'Test'),
                   name = 'Condition') +
  scale_y_continuous(breaks = seq(0, 1, 0.1),
                     name = 'Betting rate') +
  scale_fill_manual(breaks = c('C', 'S'), labels = c('Yellow', 'Blue'),
                    values = c('#ffd500', '#005bbb'),
                    name = 'Session color') +
  theme_minimal() +
  theme(
    panel.grid.major.x = element_blank()
  )

ggsave('betting_rates_pilot_control_test.png', width = 10, height = 7)




##### Effect sizes #####

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






