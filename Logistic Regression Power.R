library(brglm2)
library(simglm)
library(tidyverse)
library(lme4)

set.seed(46) 

# Logistic reg power
power = c()
for(i in 1:1000) {
  sim_arguments <- list(
    formula = y ~ 1 + treat + craving_score + accuracy + sex + age + ctrl1 + ctrl2,
    fixed = list(treat = list(var_type = 'factor', levels = c('treat', 'control')),
                 craving_score = list(var_type = 'continuous', mean = 0, sd = 1),
                 accuracy = list(var_type = 'continuous', mean = 0, sd = 1),
                 sex = list(var_type = 'factor', levels = c('male', 'female')),
                 age = list(var_type = 'continuous', mean = 0, sd = 1),
                 ctrl1 = list(var_type = 'continuous', mean = 0, sd = 1),
                 ctrl2 = list(var_type = 'continuous', mean = 0, sd = 1)),
    error = list(variance = 1),
    sample_size = 200,
    reg_weights = c(1, exp(3.47), exp(3.47), exp(3.47), 
                    exp(3.47), exp(3.47), exp(3.47), exp(3.47)),
    outcome_type = 'binary'
  )
  
  data = simulate_fixed(data = NULL, sim_arguments) %>%
    simulate_error(sim_arguments) %>%
    generate_response(sim_arguments)
  
  log_mod = glm(y ~ 1 + treat + craving_score + accuracy + sex + age + ctrl1 + ctrl2,
                data = data, family = binomial('logit'),
                method = 'brglmFit')
  
  s = summary(log_mod)
  p = s$coefficients[2, 4]
  b = s$coefficients[2, 1]
  
  if(p < .05 & b > 0) {
    power[length(power) + 1] = 1
  } else {
    power[length(power) + 1] = 0
  }
}

mean(power)

# Linear reg power
power = c()
for(i in 1:1000) {
  sim_arguments <- list(
    formula = y ~ 1 + treat + craving_score + accuracy + sex + age + ctrl1 + ctrl2,
    fixed = list(treat = list(var_type = 'factor', levels = c('treat', 'control')),
                 craving_score = list(var_type = 'continuous', mean = 0, sd = 1),
                 accuracy = list(var_type = 'continuous', mean = 0, sd = 1),
                 sex = list(var_type = 'factor', levels = c('male', 'female')),
                 age = list(var_type = 'continuous', mean = 0, sd = 1),
                 ctrl1 = list(var_type = 'continuous', mean = 0, sd = 1),
                 ctrl2 = list(var_type = 'continuous', mean = 0, sd = 1)),
    error = list(variance = 1),
    sample_size = 200,
    reg_weights = c(0, rep(0.5, 7)))
  
  data = simulate_fixed(data = NULL, sim_arguments) %>%
    simulate_error(sim_arguments) %>%
    generate_response(sim_arguments)
  
  full = lm(y ~ 1 + treat + craving_score + accuracy + sex + age + ctrl1 + ctrl2,
                data = data)
  
  b = summary(full)$coefficients[2, 1]
  p = summary(full)$coefficients[2, 4]
  
  if(p < .05 & b > 0) {
    power[length(power) + 1] = 1
  } else {
    power[length(power) + 1] = 0
  }
}

mean(power)


# Multi-level model power
power = c()
for(i in 1:100) {
  sim_arguments <- list(
    formula = y ~ 1 + treat + craving_score + age + uncertainty + reward + exposure + ctrl1 + (1 | id),
    fixed = list(treat = list(var_type = 'factor', 
                              levels = c('Test', 'Control'),
                              var_level = 2),
                 age = list(var_type = 'continuous', mean = 0, sd = 1),
                 craving_score = list(var_type = 'continuous', mean = 0, sd = 1),
                 uncertainty = list(var_type = 'factor', levels = c('High', 'Low'), var_level = 2),
                 reward = list(var_type = 'factor', 
                               levels = c('High', 'Low'),
                               var_level = 2),
                 exposure = list(var_type = 'continuous', mean = 0, sd = 1),
                 ctrl1 = list(var_type = 'factor', 
                              levels = c('High', 'Low'),
                              var_level = 2)),
    randomeffect = list(int_id = list(variance = 1, var_level = 2)),
    sample_size = list(level1 = 24, level2 = 200),
    reg_weights = c(1, exp(3.47), exp(3.47), exp(3.47), 
                    exp(3.47), exp(3.47), exp(3.47), exp(3.47)),
    outcome_type = 'binary'
  )
  
  nested_data <- sim_arguments %>%
    simulate_fixed(data = NULL, .) %>%
    simulate_randomeffect(sim_arguments) %>%
    simulate_error(sim_arguments) %>%
    generate_response(sim_arguments)
  
  model = glmer(y ~ 1 + treat + craving_score + age + uncertainty + 
                 reward + exposure + ctrl1 + (1 | id),
               data = nested_data,
               family = binomial('logit'))
  
  z = summary(model)[[10]][7,3]
  b = summary(model)[[10]][7,1]
  
  if(z > 1.96 & b > 0) {
    power[length(power) + 1] = 1
  } else {
    power[length(power) + 1] = 0
  }
}

mean(power)



