library(WebPower)
library(brglm2)
library(simglm)
library(tidyverse)
library(lme4)

set.seed(46)

# Logistic reg power
power = c()
for(i in 1:1000) {
  sim_arguments <- list(
    formula = y ~ 1 + treat + craving_score + accuracy + sex + age,
    fixed = list(treat = list(var_type = 'factor', levels = c('treat', 'control')),
                 craving_score = list(var_type = 'continuous', mean = 0, sd = 1),
                 accuracy = list(var_type = 'continuous', mean = 0, sd = 1),
                 sex = list(var_type = 'factor', levels = c('male', 'female')),
                 age = list(var_type = 'continuous', mean = 0, sd = 1)),
    error = list(variance = 1),
    sample_size = 200,
    # OR = 1.68 is Cohen's D = 0.2, 3.47 is D=0.5, 5 is D=.8
    # https://www.tandfonline.com/doi/abs/10.1080/03610911003650383
    # Thus our beta is log(1.68)*(sqrt(3)/pi) for a small effect
    # From Cohen 1988 converting OR to D
    reg_weights = c(1, rep(log(5)*(sqrt(3)/pi), 5)),
    outcome_type = 'binary'
  )
  
  data_log = simulate_fixed(data = NULL, sim_arguments) %>%
    simulate_error(sim_arguments) %>%
    generate_response(sim_arguments)
  
  log_mod = glm(y ~ 1 + treat + craving_score + accuracy + sex + age,
                data = data_log, family = binomial('logit'),
                method = 'brglmFit')
  
  s = summary(log_mod)
  p = s$coefficients[2, 4]
  b = s$coefficients[2, 1]
  
  if(p < .05) {
    power[length(power) + 1] = 1
  } else {
    power[length(power) + 1] = 0
  }
}

mean(power)

# Multi-level model power
power = c()
power_bf = c()
for(i in 1:10) {
  sim_arguments <- list(
    formula = y ~ 1 + reward + uncertainty + treat + session_color + previous_choice +
      age + gender + major + reward:session_color + (1 | id),
    fixed = list(reward = list(var_type = 'factor', 
                               levels = c('High', 'Low')),
                 uncertainty = list(var_type = 'factor', 
                                    levels = c('High', 'Low')),
                 treat = list(var_type = 'factor', 
                              levels = c('Test', 'Control')),
                 session_color = list(var_type = 'factor', 
                                    levels = c('High', 'Low')),
                 previous_choice = list(var_type = 'factor', 
                                        levels = c('Bet', 'Skip')),
                 age = list(var_type = 'continuous', mean = 0, sd = 1),
                 gender = list(var_type = 'factor',
                               levels = c('Male', 'Female')),
                 major = list(var_type = 'factor',
                               levels = c('Major1', 'Major2', 'Major3', 'Major4'))),
    randomeffect = list(int_id = list(variance = 1, var_level = 2)),
    sample_size = list(level1 = 600, level2 = 200),
    reg_weights = c(1, rep(log(1.68)*(sqrt(3)/pi), 10), -log(1.68)*(sqrt(3)/pi)),
    outcome_type = 'binary'
  )
  
  nested_data <- sim_arguments %>%
    simulate_fixed(data = NULL, .) %>%
    simulate_randomeffect(sim_arguments) %>%
    simulate_error(sim_arguments) %>%
    generate_response(sim_arguments)
  
  model = glmer(y ~ 1 + reward + uncertainty + treat + session_color + previous_choice +
                  age + gender + major + reward:session_color + (1 | id),
               data = nested_data,
               family = binomial('logit'))
  
  null_mod = update(model, formula = ~ . -reward:session_color) 
  
  BF_BIC = exp((BIC(null_mod) - BIC(model))/2)
  
  z = summary(model)[[10]][9,3]
  b = summary(model)[[10]][9,1]
  
  if(z < 1.96) {
    power[length(power) + 1] = 1
  } else {
    power[length(power) + 1] = 0
  }
  
  if(BF_BIC > 10) {
    power_bf[length(power_bf) + 1] = 1
  } else {
    power_bf[length(power_bf) + 1] = 0
  }
  
  print(i)
}

mean(power)
mean(power_bf)

