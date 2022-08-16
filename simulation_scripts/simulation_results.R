
library(rjson)
library(tidyverse)

# Different file depending on OS
# json_file <- 'C:/Users/samue/Documents/Elise Projects/simulation_v6_2.json'
json_file <- '/Users/sam/Documents/simulation_v7_1.json'

power_list1 <- json_file %>%
  readLines %>%
  paste0 %>%
  fromJSON



power_list_mean <- list(
  't1' = list('t' = mean(unlist(power_list1[["t1"]][["t"]])),
              'D' = mean(power_list1[["t1"]][["D"]]),
              'power' = sum(power_list1[["t1"]][["power"]] < .05)/
                length(power_list1[["t1"]][["power"]])),
  't2' = list(
    'b_rew' = mean(power_list1[["t2"]][["b_rew"]]),
    'b_unc' = mean(power_list1[["t2"]][["b_unc"]]),
    'b_int' = mean(power_list1[["t2"]][["b_int"]]),
    'power_r' = sum(abs(power_list1[["t2"]][["power_r"]]) > 1.96)/
      length(power_list1[["t2"]][["power_r"]]),
    'power_u' = sum(abs(power_list1[["t2"]][["power_u"]]) > 1.96)/
      length(power_list1[["t2"]][["power_u"]]),
    'power_i' = sum(abs(power_list1[["t2"]][["power_i"]]) > 1.96)/
      length(power_list1[["t2"]][["power_i"]])
  ),
  't3' = list('beta_log' = mean(power_list1[["t3"]][["beta_log"]]),
              'power_log' = sum(abs(power_list1[["t3"]][["power_log"]]) > 1.96)/
                length(power_list1[["t3"]][["power_log"]]), 
              'beta_lin' = mean(power_list1[["t3"]][["beta_lin"]]),
              'power_lin' = sum(abs(power_list1[["t3"]][["power_lin"]]) > 1.96)/
                length(power_list1[["t3"]][["power_lin"]])
  ),
  't4' = list(
    'b_rew' = mean(power_list1[["t4"]][["b_rew"]]),
    'b_unc' = mean(power_list1[["t4"]][["b_unc"]]),
    'b_treat' = mean(power_list1[["t4"]][["b_treat"]]),
    'power_r' = sum(abs(power_list1[["t4"]][["power_r"]]) > 1.96)/
      length(power_list1[["t4"]][["power_r"]]),
    'power_u' = sum(abs(power_list1[["t4"]][["power_u"]]) > 1.96)/
      length(power_list1[["t4"]][["power_u"]]),
    'power_t' = sum(abs(power_list1[["t4"]][["power_t"]]) > 1.96)/
      length(power_list1[["t4"]][["power_t"]])
  ),
  't5' = list('t' = mean(unlist(power_list1[["t5"]][["t"]])),
              'D' = mean(power_list1[["t5"]][["D"]]),
              'power' = sum(power_list1[["t5"]][["power"]] < .05)/
                length(power_list1[["t5"]][["power"]])),
  't6' = list('beta' = mean(power_list1[["t6"]][["beta"]]),
              'power' = sum(abs(power_list1[["t6"]][["power"]]) > 1.96)/
                length(power_list1[["t6"]][["power"]])),
  't7' = list('t' = mean(unlist(power_list1[["t7"]][["t"]])),
              'D' = mean(power_list1[["t7"]][["D"]]),
              'power' = sum(power_list1[["t7"]][["power"]] < .05)/
                length(power_list1[["t7"]][["power"]])),
  't8' = list('t' = mean(unlist(power_list1[["t8"]][["t"]])),
              'D' = mean(power_list1[["t8"]][["D"]]),
              'power' = sum(power_list1[["t8"]][["power"]] < .05)/
                length(power_list1[["t8"]][["power"]])),
  't8_b' = list('t' = mean(unlist(power_list1[["t8_b"]][["t"]])),
                'D' = mean(power_list1[["t8_b"]][["D"]]),
              'power' = sum(power_list1[["t8_b"]][["power"]] < .05)/
                length(power_list1[["t8_b"]][["power"]])),
  't8_y' = list('t' = mean(unlist(power_list1[["t8_y"]][["t"]])),
                'D' = mean(power_list1[["t8_y"]][["D"]]),
              'power' = sum(power_list1[["t8_y"]][["power"]] < .05)/
                length(power_list1[["t8_y"]][["power"]]))
)



test_parallel <- read.csv('/Users/sam/test_parallel220815_46_1.csv')

for(i in 2:5) {
  test_parallel2 <- read.csv(paste0('/Users/sam/test_parallel220815_46_', i, '.csv'))
  
  test_parallel <- rbind(test_parallel, test_parallel2)
}


# From mclapply
power_list_mean <- list(
  't1' = list('t' = mean(test_parallel[,1]),
              'power' = sum(test_parallel[,2] < .05)/nrow(test_parallel),
              'D' = mean(test_parallel[,3])),
  't2' = list(
    'b_rew' = mean(test_parallel[,4]),
    'power_r' = sum(abs(test_parallel[,5]) > 1.68 & test_parallel[,4] > 0)/nrow(test_parallel),
    'b_unc' = mean(test_parallel[,6]),
    'power_u' = sum(abs(test_parallel[,7]) > 1.68 & test_parallel[,6] < 0)/nrow(test_parallel),
    'b_int' = mean(test_parallel[,8]),
    'power_i' = sum(abs(test_parallel[,9]) > 1.68 & test_parallel[,8] < 0)/nrow(test_parallel)
  ),
  't3' = list('b_exp' = mean(test_parallel[,10]),
              'power_exp' = sum(abs(test_parallel[,11]) > 1.68 & test_parallel[,10] > 0)/nrow(test_parallel)),
  't4' = list('beta_log' = mean(test_parallel[,12]),
              'power_log' = sum(abs(test_parallel[,13]) > 1.68 & test_parallel[,12] > 0)/nrow(test_parallel), 
              'beta_lin' = mean(test_parallel[,14]),
              'power_lin' = sum(abs(test_parallel[,15]) > 1.68 & test_parallel[,14] > 0)/nrow(test_parallel)
  ),
  't5' = list(
    'b_rew' = mean(test_parallel[,16]),
    'power_r' = sum(abs(test_parallel[,17]) > 1.68 & test_parallel[,16] > 0)/nrow(test_parallel),
    'b_unc' = mean(test_parallel[,18]),
    'power_u' = sum(abs(test_parallel[,19]) > 1.68 & test_parallel[,18] < 0)/nrow(test_parallel),
    'b_treat' = mean(test_parallel[,20]),
    'power_t' = sum(abs(test_parallel[,21]) > 1.68 & test_parallel[,20] > 0)/nrow(test_parallel)
  ),
  't6' = sum(test_parallel[,22] > 3)/nrow(test_parallel),
  't7' = list('t' = mean(test_parallel[,23]),
              'power' = sum(test_parallel[,24] < .05)/nrow(test_parallel),
              'D' = mean(test_parallel[,25]))
)


