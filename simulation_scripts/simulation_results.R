
json_file <- 'simulation_v4.1.json'

power_list1 <- json_file %>%
  readLines %>%
  paste0 %>%
  fromJSON

power_list_mean <- list(
  't1' = list('t' = mean(unlist(power_list1[["t1"]][["t"]])),
              'D' = mean(power_list[["t1"]][["D"]]),
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
              'D' = mean(power_list[["t5"]][["D"]]),
              'power' = sum(power_list1[["t5"]][["power"]] < .05)/
                length(power_list1[["t5"]][["power"]])),
  't6' = list('beta' = mean(power_list1[["t6"]][["beta"]]),
              'power' = sum(abs(power_list1[["t6"]][["power"]]) > 1.96)/
                length(power_list1[["t6"]][["power"]])),
  't7' = list('t' = mean(unlist(power_list1[["t7"]][["t"]])),
              'D' = mean(power_list[["t7"]][["D"]]),
              'power' = sum(power_list1[["t7"]][["power"]] < .05)/
                length(power_list1[["t7"]][["power"]])),
  't8' = list('t' = mean(unlist(power_list1[["t8"]][["t"]])),
              'D' = mean(power_list[["t8"]][["D"]]),
              'power' = sum(power_list1[["t8"]][["power"]] < .05)/
                length(power_list1[["t8"]][["power"]])),
  't8_b' = list('t' = mean(unlist(power_list1[["t8_b"]][["t"]])),
                'D' = mean(power_list[["t8_b"]][["D"]]),
              'power' = sum(power_list1[["t8_b"]][["power"]] < .05)/
                length(power_list1[["t8_b"]][["power"]])),
  't8_y' = list('t' = mean(unlist(power_list1[["t8_y"]][["t"]])),
                'D' = mean(power_list[["t8_y"]][["D"]]),
              'power' = sum(power_list1[["t8_y"]][["power"]] < .05)/
                length(power_list1[["t8_y"]][["power"]]))
)
