
library(tidyverse)

# For the pilot data the following column names are the following variables

# block_type = session color, S = blue session, C = yellow session
# craver = optimal/craving strategy, 1 = craving, 0 = optimal
# choice = bet/skip decision, 1 = bet, 0 = skip
# aaron_mood = uncertainty


# Loading pilot data
data <- read.csv(file.choose())

# Make block_type and treatment factors
data <- data %>%
  mutate(block_type = as.factor(block_type),
         treatment = as.factor(treatment),
         craver = as.factor(craver),
         exposure_time = NA,
         previous_choice = NA)

# Number of missed trials (choice = 2)
# This is when participants take too long to answer and automatically skip
sum(data$choice == 2)

# Iterate through data and add exposure time and previous choice
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


### Pilot data checks

# Floor/ceiling effect
data_part <- data %>%
  group_by(id, craver, block_type, treatment) %>%
  summarize(betting_rate = mean(choice, na.rm = T))


# Histogram for betting rates
crave_labs <- c('Optimal', 'Craver')
names(crave_labs) <- 0:1

ggplot(data_part, aes(betting_rate, fill = block_type)) +
  geom_histogram(bins = 15, alpha = .8) +
  scale_fill_manual(name = 'Session color',
                    breaks = c('C', 'S'),
                    labels = c('Yellow', 'Blue'),
                    values = c('#CEB710', '#1715C2')) +
  theme_minimal() +
  facet_grid(vars(craver), vars(treatment), scales = 'free_y',
             labeller = labeller(craver = crave_labs))

# Check number of times people bet in yellow sessions
bet_in_yellow <- data %>%
  filter(block_type == 'C') %>%
  group_by(id) %>%
  summarize(bet_in_y = sum(choice))

ggplot(bet_in_yellow, aes(bet_in_y)) +
  geom_histogram(bins = 20, color = 'black', fill = 'white') +
  labs(title = 'Number of times participants bet in yellow sessions') +
  theme_minimal()


### Betting propensity by craver/block_type

# 16.2% betting by cravers, 0% by optimal in yellow sessions
# 
data %>%
  group_by(craver, block_type) %>%
  summarize(mean = mean(choice, na.rm = T))
  

### Performance in task

# make a data frame with final accumulated outcome
final_outcome <- data.frame(win = 1, wager = 1, treat = 'Test', craver = 1)

inds <- data %>%
  rownames_to_column() %>%
  filter(id != lead(id)) %>%
  dplyr::select(rowname) %>%
  mutate(rowname = as.numeric(rowname)) %>%
  rbind(nrow(data))

final_outcome <- data %>%
  slice(inds$rowname) %>%
  mutate(win = accumulated_outcomes) %>%
  select(win, wager, treatment, craver) %>%
  mutate(payout = if_else(win + wager - 177 > 0, (win + wager - 177) * 24, win + wager - 177)) %>%
  mutate(payout = if_else(payout > 110, 110, payout)) %>%
  mutate(performance = if_else(payout < -50, -50, payout),
         payout = if_else(payout < -50, -50, payout)) %>%
  mutate(payout = if_else(payout < 0, 5, payout + 5)) %>%
  mutate(payout = if_else(craver == 1, payout + 3, payout))


ggplot(final_outcome, aes(win)) +
  geom_density()

# Earnings

# By craving type
ggplot(final_outcome, aes(x = craver, y = performance)) +
  geom_hline(yintercept = 0) +
  geom_boxplot() +
  stat_summary(fun = 'mean', col = 'red') +
  scale_y_continuous(breaks = seq(-50, 120, by = 10),
                     name = 'Final payout') +
  scale_x_discrete(breaks = 0:1, labels = c('Optimal', 'Craver'),
                   name = 'Craving type') +
  theme_minimal()

# By treatment
ggplot(final_outcome, aes(x = treatment, y = performance)) +
  geom_hline(yintercept = 0) +
  geom_boxplot() +
  stat_summary(fun = 'mean', col = 'red') +
  scale_y_continuous(breaks = seq(-50, 120, by = 10),
                     name = 'Final payout') +
  scale_x_discrete(breaks = c('control', 'test'),
                   labels = c('Control', 'Test'),
                   name = 'Treatment') +
  theme_minimal()

# proportion cravers/optimal, 66.23% cravers
sum(final_outcome$craver == 1)/nrow(final_outcome)


### Effect sizes needed for power analysis

# Logistic model
data_part2 <- data_part[data_part$block_type == 'C', ]

(mean(data_part2$betting_rate[data_part2$treatment == 'control']) -
    mean(data_part2$betting_rate[data_part2$treatment == 'test']))/
  sqrt((sd(data_part2$betting_rate[data_part2$treatment == 'control'])^2 + 
          sd(data_part2$betting_rate[data_part2$treatment == 'test'])^2)/2)



### Defining dependent variable for logistic model (test 3)




### Model specification for main analysis

# Variable names follow analysis plan

# Test 1 - paired t-test betting rate on reward value
test1 <- data %>%
  filter(session_color == 'Yellow') %>%
  group_by(id, reward_value) %>%
  summarize(betting_rate = mean(choice, na.rm = T))

t1 <- t.test(betting_rate ~ reward_value, data = test1, paired = T,
             alternative = 'less')

t1

t1$estimate/sd(test1$betting_rate) # Cohen's D


# Test 2 - log mixed model, full data
data2 <- data %>%
  filter(previous_choice != 2)

model2 <- glmer(choice ~ 1 + reward + uncertainty + treat + session_color + previous_choice +
                age + gender + major + reward:session_color + (1 | id),
              data = data2,
              family = binomial('logit'))

# Test 3 - logistic regression predicting craving on participant level
data_log <- data %>%
  filter(session_color == 'Yellow') %>%
  group_by(id, treat, craving_score, craver, age, gender, major) %>%
  summarize(betting_rate = mean(choice))

log_mod = glm(craver ~ 1 + treat + craving_score + accuracy + gender + age + major,
              data = data_log, family = binomial('logit'),
              method = 'brglmFit')


# Test 4 - log mixed model in yellow sessions
data4 <- data %>%
  filter(session_color == 'Yellow')

model4 <- glmer(choice ~ 1 + reward + uncertainty + treat + previous_choice +
                 age + gender + major + (1 | id),
               data = data4,
               family = binomial('logit'))


# Test 5 - two-sample one-tailed t-test betting in blue for test vs control
test5 <- data %>%
  filter(session_color == 'Blue') %>%
  group_by(id, treat) %>%
  summarize(betting_rate = mean(choice, na.rm = T))

t5 <- t.test(betting_rate ~ treat, data = test5,
             alternative = 'less')

t5

# Cohen's D
(mean(test5$betting_rate[test5$treat == 'Test']) -
    mean(test5$betting_rate[test5$treat == 'Test']))/
  sqrt((sd(test5$betting_rate[test5$treat == 'Test'])^2 +
     sd(test5$betting_rate[test5$treat == 'Test'])^2)/2)


# Test 6 - log mixed model in blue sessions with exposure time
data4 <- data %>%
  filter(session_color == 'Blue')

model4 <- glmer(choice ~ 1 + reward + uncertainty + treat + previous_choice +
                  age + gender + exposure_time + (1 | id),
                data = data6,
                family = binomial('logit'))


# Test 7 - one-sample one-tailed t-test for betting in yellow > 0 in Test treatment
test7 <- data %>%
  filter(session_color == 'Yellow' & treat == 'Test') %>%
  group_by(id) %>%
  summarize(betting_rate = mean(choice, na.rm = T))

t7 <- t.test(betting_rate, mu = 0, alternative = 'greater')

t7

mean(test7$betting_rate)/sd(test7$betting_rate) # Cohen's D


# Test 8 - paired t-test for betting rate in yellow on uncertainty
test8 <- data %>%
  filter(session_color == 'Yellow') %>%
  group_by(id, uncertainty) %>%
  summarize(betting_rate = mean(choice, na.rm = T))

t8 <- t.test(betting_rate ~ uncertainty, data = test8, paired = T,
             alternative = 'less')

t8

# Cohen's D
t8$estimate/sd(test8$betting_rate)












