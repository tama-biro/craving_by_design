
library(tidyverse)
library(lme4)
library(Bolstad)
library(MuMIn)
library(brglm2)
library(pscl)
library(GGally)

# Function for standard error
se <- function(x) {
  se <- sd(x)/sqrt(length(x))
  return(se)
}

# Histograms of betting rate in control and test for blue/yellow sessions
hist1 <- data %>%
  group_by(block_type, treatment, id) %>%
  summarize(betting_rate = mean(choice))

ggplot(hist1, aes(betting_rate, color = block_type)) +
  geom_density() +
  facet_wrap('treatment') +
  theme_minimal()

crav_test <- data %>%
  filter(block_type == 'C') %>%
  group_by(id) %>%
  summarize(betting_rate = mean(choice))

sum(crav_test$betting_rate > 0.3)/nrow(crav_test)

mean(data$choice[data$block_type == 'C'])

# Plot for betting in yellow sessions over the experiment
yellow_bet <- data %>%
  filter(block_type == 'C') %>%
  group_by(sequence_number, treatment) %>%
  summarize(betting_rate = mean(choice))

ggplot(yellow_bet, aes(x = sequence_number, y = betting_rate)) +
  geom_line(color = 'orange') +
  theme_minimal() +
  labs(x = 'Sequence number', y = 'Betting rate') +
  facet_wrap('treatment')

ggsave('/Users/sam/Google Drive/Elise Projects/Craving by Design/Pilot/Plots/betting_in_yellow.png',
       width = 5, height = 3)

# Plot for betting rate and exposure rate
exp_plot <- data %>%
  filter(block_type == 'S') %>%
  group_by(exposure_time, treatment) %>%
  summarize(betting_rate = mean(choice))

ggplot(exp_plot, aes(x = exposure_time, y = betting_rate)) +
  geom_point() +
  geom_smooth(formula = y ~ x, method = 'loess') +
  theme_minimal() +
  labs(x = 'Exposure time', y = 'Betting rate') +
  facet_wrap('treatment', scale = 'free')

ggsave('/Users/sam/Google Drive/Elise Projects/Craving by Design/Pilot/Plots/exposure_time.png',
       width = 5, height = 3)

# Strategy distribution
data %>%
  group_by(id, craver) %>%
  summarize(mean = mean(pre_game_strategy)) %>%
  group_by(mean, craver) %>%
  summarize(n = n())

# Test 1
test1 <- data %>%
  filter(block_type == 'C') %>%
  group_by(id, reward_value) %>%
  summarize(betting_rate = mean(choice, na.rm = T))

t1 <- t.test(betting_rate ~ reward_value, data = test1, paired = T,
       alternative = 'less')

t1

# Cohen's D
t1$estimate/sd(test1$betting_rate)

plot1 <- test1 %>%
  group_by(reward_value) %>%
  summarize(se = se(betting_rate),
            betting_rate = mean(betting_rate))

ggplot(plot1, aes(as.factor(reward_value), betting_rate)) +
  geom_bar(stat = 'identity', width = 0.4, fill = 4) +
  geom_errorbar(aes(ymin = betting_rate - se, ymax = betting_rate + se),
                width = 0.1, color = '#323333') +
  labs(x = 'Reward value / AUD', y = 'Betting rate') +
  theme_minimal()

ggsave('/Users/sam/Google Drive/Elise Projects/Craving by Design/Pilot/Plots/plot1.png',
       width = 5, height = 3)

# Test 2
test2 <- data %>%
  filter(block_type == 'C') %>%
  group_by(id, aaron_mood) %>%
  summarize(betting_rate = mean(choice, na.rm = T))

t2 <- t.test(betting_rate ~ aaron_mood, data = test2, paired = T,
       alternative = 'less')

t2

# Cohen's D
t2$estimate/sd(test2$betting_rate)

plot2 <- test2 %>%
  group_by(aaron_mood) %>%
  summarize(se = se(betting_rate),
            betting_rate = mean(betting_rate))

ggplot(plot2, aes(as.factor(aaron_mood), betting_rate)) +
  geom_bar(stat = 'identity', width = 0.4, fill = 4) +
  geom_errorbar(aes(ymin = betting_rate - se, ymax = betting_rate + se),
                width = 0.1, color = '#323333') +
  labs(x = 'Uncertainty', y = 'Betting rate') +
  scale_x_discrete(limits = c('0', '1'), labels = c('High', 'Low')) +
  theme_minimal()

ggsave('/Users/sam/Google Drive/Elise Projects/Craving by Design/Pilot/Plots/plot2.png',
       width = 5, height = 3)

# Test 3
reg1 <- data %>%
  filter(block_type == 'C') %>%
  group_by(id, treatment, craver, age, gender, major) %>%
  summarize(betting_rate = mean(choice))

# lm
mod <- lm((betting_rate + 1) ~ treatment + scale(age) + 
            gender + major,
          data = reg1)

bc <- MASS::boxcox(mod)

lambda <- bc$x[which.max(bc$y)]

new_model <- lm((((betting_rate + 1)^lambda-1)/lambda) ~ treatment + scale(age) + 
                  gender + major,
                data = reg1)

summary(mod)

write.csv(summary(mod)$coefficients, 'reg3_1.csv')

# glm
mod_2 <- glm(craver ~ treatment + scale(age) + 
            as.factor(gender) + as.factor(major),
          data = reg1, family = binomial(link = 'logit'),
          method = 'brglmFit')

summary(mod_2)

write.csv(summary(mod_2)$coefficients, 'reg3_2.csv')

pR2(mod_2)

# Test 4
# Not tested yet before craving propensity score is derived


# Test 5
test5_df <- data %>%
  mutate(choice = factor(choice, levels = 0:1, 
                         labels = c('Skip', 'Bet')),
         previous_choice = factor(previous_choice, levels = 0:1,
                                  labels = c('Skip', 'Bet')),
         age = scale(age))

mixed_log5 <- glmer(choice ~ reward_value + aaron_mood +
                      block_type * treatment + previous_choice +
                      age + gender + major + 
                      (1 | id),
                    data = test5_df,
                    family = binomial(link = 'logit'))

s <- summary(mixed_log5)

write.csv(s$coefficients, 'glmm5.csv')

r.squaredGLMM(mixed_log5)

plot5 <- data %>%
  group_by(block_type, treatment, id) %>%
  summarize(betting_rate = mean(choice)) %>%
  group_by(block_type, treatment) %>%
  summarize(se = se(betting_rate),
            betting_rate = mean(betting_rate))

plot5 <- plot5[1:4,]

ggplot(plot5, aes(x = block_type, y = betting_rate, fill = treatment)) +
  geom_bar(stat = 'identity', position = position_dodge(width = 0.9)) + 
  geom_errorbar(aes(ymin = betting_rate - se, ymax = betting_rate + se),
                width = 0.2, position = position_dodge(width = 0.9),
                color = '#323333') +
  labs(x = 'Block type', y = 'Betting rate') +
  scale_x_discrete(limits = c('C', 'S'), labels = c('Yellow', 'Blue')) +
  theme_minimal()

ggsave('/Users/sam/Google Drive/Elise Projects/Craving by Design/Pilot/Plots/plot5_interaction.png',
       width = 5, height = 3)

# Test 6
reg6 <- data %>%
  filter(block_type == 'C') %>%
  mutate(choice = factor(choice, levels = 0:1, 
                         labels = c('Skip', 'Bet')),
         previous_choice = factor(previous_choice, levels = 0:1,
                                  labels = c('Skip', 'Bet')),
         age = scale(age))

mixed_log6 <- glmer(choice ~ reward_value + aaron_mood +
                      treatment + previous_choice +
                      age + gender + major + 
                      (1 | id),
                    data = reg6,
                    family = binomial(link = 'logit'))

s <- summary(mixed_log6)

write.csv(s$coefficients, 'glmm6.csv')

r.squaredGLMM(mixed_log6)

plot6 <- data %>%
  filter(block_type == 'C') %>%
  group_by(treatment, id) %>%
  summarize(betting_rate = mean(choice)) %>%
  group_by(treatment) %>%
  summarize(se = se(betting_rate),
            betting_rate = mean(betting_rate))

plot6 <- plot6[1:2,]

ggplot(plot6, aes(x = treatment, y = betting_rate)) +
  geom_bar(stat = 'identity', position = position_dodge(width = 0.9),
           width = 0.5, fill = 4) + 
  geom_errorbar(aes(ymin = betting_rate - se, ymax = betting_rate + se),
                width = 0.1, position = position_dodge(width = 0.9),
                color = '#323333') +
  labs(x = 'Treatment', y = 'Betting rate') +
  theme_minimal()

ggsave('/Users/sam/Google Drive/Elise Projects/Craving by Design/Pilot/Plots/plot6_interaction.png',
       width = 5, height = 3)


fix.formula <- choice ~ reward_value * aaron_mood +
  treatment + previous_choice +
  age + gender + major

X <- model.matrix (fix.formula, reg6)

# Forward selection of interactions
mixed_log6_1 <- glmer(choice ~ reward_value + aaron_mood *
                      treatment + previous_choice +
                      age + gender + major + 
                      (1 | id),
                    data = reg6,
                    family = binomial(link = 'logit'))

mixed_log6_2 <- glmer(choice ~ reward_value + aaron_mood *
                        (treatment + previous_choice) +
                        age + gender + major + 
                        (1 | id),
                      data = reg6,
                      family = binomial(link = 'logit'))

mixed_log6_3 <- glmer(choice ~ reward_value + aaron_mood *
                        (treatment + previous_choice +
                        age) + gender + major + 
                        (1 | id),
                      data = reg6,
                      family = binomial(link = 'logit'))

mixed_log6_4 <- glmer(choice ~ reward_value + aaron_mood *
                        (treatment + previous_choice +
                        age + gender) + major + 
                        (1 | id),
                      data = reg6,
                      family = binomial(link = 'logit'))

mixed_log6_5 <- glmer(choice ~ reward_value + aaron_mood *
                        (treatment + previous_choice +
                        age + gender + major) + 
                        (1 | id),
                      data = reg6,
                      family = binomial(link = 'logit'))

AIC(mixed_log6, mixed_log6_1, mixed_log6_2, mixed_log6_3,
    mixed_log6_4, mixed_log6_5)


# Test 7

# Test 8

# Test 9


# Uncertainty and session color
plot10 <- data %>%
  group_by(block_type, aaron_mood, id) %>%
  summarize(betting_rate = mean(choice)) %>%
  group_by(block_type, aaron_mood) %>%
  summarize(se = se(betting_rate),
            betting_rate = mean(betting_rate))

ggplot(plot10, aes(x = block_type, y = betting_rate, fill = aaron_mood)) +
  geom_bar(stat = 'identity', position = position_dodge(width = 0.9)) + 
  geom_errorbar(aes(ymin = betting_rate - se, ymax = betting_rate + se),
                width = 0.2, position = position_dodge(width = 0.9),
                color = '#323333') +
  labs(x = 'Block type', y = 'Betting rate') +
  scale_x_discrete(limits = c('C', 'S'), labels = c('Yellow', 'Blue')) +
  theme_minimal()

ggsave('/Users/sam/Google Drive/Elise Projects/Craving by Design/Pilot/Plots/plot5_interaction.png',
       width = 5, height = 3)

dummy_df <- reg6 %>%
  select(reward_value, aaron_mood, treatment,
         previous_choice, age, gender,
         major)

vif(dummy_df)

ggpairs(dummy_df)


