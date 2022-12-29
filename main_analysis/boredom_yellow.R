library(tidyverse)


filepath <- '../data/data_reduced.csv'
data <- read.csv(filepath)


# SE function
se <- function (x) {
  se = sd(x, na.rm = TRUE)/sqrt(length(x))
  return(se)
}


# Make indicator of where blue interspersed block is
data <- data %>%
  group_by(sequence_number, id) %>%
  mutate(blue_interspersed = 
           if_else(block_type == 'S' & block_number == 2, 2,
                   if_else(block_type == 'S' & block_number == 3, 3, 
                           if_else(block_type == 'S' & block_number == 4, 4, 1)))) %>%
  ungroup %>%
  mutate(blue_interspersed = replace(blue_interspersed, blue_interspersed == 1, NA)) %>%
  group_by(sequence_number, id) %>%
  mutate(blue_interspersed = mean(blue_interspersed, na.rm = TRUE)) %>%
  ungroup %>%
  mutate(blue_interspersed = if_else(treatment == 'test', 1, blue_interspersed))


# Set order of yellow blocks (first/second == 1 in sequence)
data$yellow_order_ctrl <- 0
for(id in unique(data$id[data$treatment == 'control'])) {
  for(seq in unique(data$sequence_number[data$id == id])) {
    indicator <- data$blue_interspersed[
      data$sequence_number == seq & data$id == id
        ][1]
    
    if(indicator == 2) {
      data$yellow_order_ctrl[
        data$sequence_number == seq &
          data$id == id &
          data$block_type == 'C' &
          data$block_number %in% c(1, 3, 4)
      ] <- 1
      data$yellow_order_ctrl[
        data$sequence_number == seq &
          data$id == id &
          data$block_type == 'C' &
          data$block_number == 5
      ] <- 2
    } else if(indicator == 3) {
      data$yellow_order_ctrl[
        data$sequence_number == seq &
          data$id == id &
          data$block_type == 'C' &
          data$block_number %in% c(1, 2, 4, 5)
      ] <- 1
    } else if(indicator == 4) {
      data$yellow_order_ctrl[
        data$sequence_number == seq &
          data$id == id &
          data$block_type == 'C' &
          data$block_number %in% c(1, 2, 5)
      ] <- 1
      data$yellow_order_ctrl[
        data$sequence_number == seq &
          data$id == id &
          data$block_type == 'C' &
          data$block_number == 3
      ] <- 2
    }

  }
}


# Set order of yellow blocks (first == 1 in sequence)
data$yellow_order_ctrl <- 0
for(id in unique(data$id[data$treatment == 'control'])) {
  for(seq in unique(data$sequence_number[data$id == id])) {
    indicator <- data$blue_interspersed[
      data$sequence_number == seq & data$id == id
    ][1]
    
    if(indicator == 2) {
      data$yellow_order_ctrl[
        data$sequence_number == seq &
          data$id == id &
          data$block_type == 'C' &
          data$block_number %in% c(1, 3)
      ] <- 1
      data$yellow_order_ctrl[
        data$sequence_number == seq &
          data$id == id &
          data$block_type == 'C' &
          data$block_number %in% c(4, 5)
      ] <- 2
    } else if(indicator == 3) {
      data$yellow_order_ctrl[
        data$sequence_number == seq &
          data$id == id &
          data$block_type == 'C' &
          data$block_number %in% c(1, 4)
      ] <- 1
      data$yellow_order_ctrl[
        data$sequence_number == seq &
          data$id == id &
          data$block_type == 'C' &
          data$block_number %in% c(2, 5)
      ] <- 2
    } else if(indicator == 4) {
      data$yellow_order_ctrl[
        data$sequence_number == seq &
          data$id == id &
          data$block_type == 'C' &
          data$block_number %in% c(1, 5)
      ] <- 1
      data$yellow_order_ctrl[
        data$sequence_number == seq &
          data$id == id &
          data$block_type == 'C' &
          data$block_number %in% c(2, 3)
      ] <- 2
    }
    
  }
}


# Check difference in betting rate with bar chart
data_plot <- data %>%
  filter(block_type == 'C' & craver_2 == 1) %>%
  group_by(id, treatment, yellow_order_ctrl) %>%
  summarize(betting_rate = mean(choice)) %>%
  group_by(treatment, yellow_order_ctrl) %>%
  summarize(mean = mean(betting_rate),
            se = se(betting_rate)) %>%
  ungroup %>%
  mutate(group = c("Control\n(first/second)", "Control\n(third)", "Test"))


ggplot(data_plot, aes(x = group, y = mean)) +
  geom_bar(stat = 'identity', fill = '#ffd700') +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 0.2) +
  theme_minimal() +
  labs(x = 'Group', y = 'Mean betting rate') +
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 14))

ggsave('../Plots/main_analysis/betting_rate_boredom_12_3.png',
       width = 10, height = 6)  


# ANOVA between groups
data_boredom_aov <- data %>%
  filter(block_type == 'C' & craver_2 == 1) %>%
  group_by(id, treatment, yellow_order_ctrl) %>%
  summarize(betting_rate = mean(choice)) %>%
  ungroup

mod_boredom <- aov(betting_rate ~ factor(yellow_order_ctrl) +
                     Error(id/factor(yellow_order_ctrl)),
                   data = data_boredom_aov)

summary(mod_boredom)


# Including indicator of block order in test 5

# Set order of yellow blocks (first/second included)
data$yellow_order_ctrl <- 0
for(id in unique(data$id[data$treatment == 'control'])) {
  for(seq in unique(data$sequence_number[data$id == id])) {
    indicator <- data$blue_interspersed[
      data$sequence_number == seq & data$id == id
    ][1]

    if(indicator == 2) {
      data$yellow_order_ctrl[
        data$sequence_number == seq &
          data$id == id &
          data$block_type == 'C' &
          data$block_number == 5
      ] <- 1
    } else if(indicator == 4) {
      data$yellow_order_ctrl[
        data$sequence_number == seq &
          data$id == id &
          data$block_type == 'C' &
          data$block_number == 3
      ] <- 1
    }
    
  }
}


# Set order of yellow blocks (first included)
data$yellow_order_ctrl <- 0
for(id in unique(data$id[data$treatment == 'control'])) {
  for(seq in unique(data$sequence_number[data$id == id])) {
    indicator <- data$blue_interspersed[
      data$sequence_number == seq & data$id == id
    ][1]
    
    if(indicator == 2) {
      data$yellow_order_ctrl[
        data$sequence_number == seq &
          data$id == id &
          data$block_type == 'C' &
          data$block_number %in% c(4, 5)
      ] <- 1
    } else if(indicator == 3) {
      data$yellow_order_ctrl[
        data$sequence_number == seq &
          data$id == id &
          data$block_type == 'C' &
          data$block_number %in% c(2, 5)
      ] <- 1
    } else if(indicator == 4) {
      data$yellow_order_ctrl[
        data$sequence_number == seq &
          data$id == id &
          data$block_type == 'C' &
          data$block_number %in% c(2, 3)
      ] <- 1
    }
    
  }
}


data_5 <- data %>%
  filter(block_type == "C" & craver_2 == 1 & yellow_order_ctrl == 0) %>%
  mutate(reward_value = factor(reward_value, levels = c("Low", "High"),
                               labels = c("Low", "High")),
         uncertainty = factor(aaron_mood, levels = c("Low", "High"),
                              labels = c("Low", "High")),
         treatment = factor(treatment, levels = c("control", "test"),
                            labels = c("Control", "Test")))

log_mod_5 <- bglmer(choice ~ reward_value + uncertainty + treatment +
                      previous_choice + scale(age) + factor(gender) +
                      factor(major) + (1 | id),
                    data = data_5, fixef.prior = t,
                    family = binomial(link = "logit"))

summary(log_mod_5)



