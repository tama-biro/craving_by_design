
library(tidyverse)

### Pilot data checks and effect sizes


### Betting propensity in yellow sessions
data %>%
  


### Effect sizes needed for power analysis




### Defining dependent variable for logistic model (test 3)




### Model specification

# Test 1 - paired t-test betting rate on reward value

# Test 2 - log mixed model all rows
model2 <- glmer(choice ~ 1 + reward + uncertainty + treat + session_color + previous_choice +
                age + gender + major + reward:session_color + (1 | id),
              data = data2,
              family = binomial('logit'))

# Test 3 - logistic regression predicting craving on participant level
data_log <- data %>%
  filter(block_type == 'C') %>%
  group_by(id, treat, craving_score, craver, age, gender, major) %>%
  summarize(betting_rate = mean(choice))

log_mod = glm(craver ~ 1 + treat + craving_score + accuracy + gender + age + major,
              data = data_log, family = binomial('logit'),
              method = 'brglmFit')


# Test 4 - log mixed model in yellow sessions
model4 <- glmer(choice ~ 1 + reward + uncertainty + treat + previous_choice +
                 age + gender + major + (1 | id),
               data = data4,
               family = binomial('logit'))


# Test 5 - two-sample one-tailed t-test betting in blue for test vs control



# Test 6 - log mixed model in blue sessions with exposure time
model4 <- glmer(choice ~ 1 + reward + uncertainty + treat + previous_choice +
                  age + gender + exposure_time (1 | id),
                data = data6,
                family = binomial('logit'))


# Test 7 - one-sample one-tailed t-test for betting in yellow > 0 in Test treatment



# Test 8 - paired t-test for betting rate in yellow on uncertainty













