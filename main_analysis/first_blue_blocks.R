library(tidyverse)


filepath <- '../data/data_reduced.csv'
data <- read.csv(filepath)


# SE function
se <- function (x) {
  se = sd(x, na.rm = TRUE)/sqrt(length(x))
  return(se)
}


# Betting in first 4 blue blocks test/ctrl
data_plot <- data %>%
  mutate(first_blue = if_else(treatment == 'control' &
                                block_type == 'S' &
                                block_number %in% 6:9, 1,
                              if_else(treatment == 'test' &
                                        block_type == 'S' &
                                        block_number %in% 1:4, 1, 0))) %>%
  filter(first_blue == 1) %>%
  group_by(id, treatment) %>%
  summarize(betting_rate = mean(choice)) %>%
  group_by(treatment) %>%
  summarize(mean = mean(betting_rate),
            se = se(betting_rate)) %>%
  ungroup


ggplot(data_plot, aes(x = treatment, y = mean)) +
  geom_bar(stat = 'identity', fill = '#0057b7', width = 0.7) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se),
                width = 0.2) +
  labs(x = 'Treatment', y = 'Mean betting rate') +
  theme_minimal() +
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 16))

ggsave('../Plots/main_analysis/betting_rate_first_blue.png',
       width = 10, height = 6)


# Compare with t-test
data_ttest <- data %>%
  mutate(first_blue = if_else(treatment == 'control' &
                                block_type == 'S' &
                                block_number %in% 6:9, 1,
                              if_else(treatment == 'test' &
                                        block_type == 'S' &
                                        block_number %in% 1:4, 1, 0))) %>%
  filter(first_blue == 1) %>%
  group_by(id, treatment) %>%
  summarize(betting_rate = mean(choice)) %>%
  ungroup


t.test(betting_rate ~ treatment, data = data_ttest)

# Cohen's D
(mean(data_ttest$betting_rate[data_ttest$treatment == 'control']) - 
    mean(data_ttest$betting_rate[data_ttest$treatment == 'test']))/
  sqrt(
    (sd(data_ttest$betting_rate[data_ttest$treatment == 'control'])^2 +
       sd(data_ttest$betting_rate[data_ttest$treatment == 'test'])^2)/2
  )
