library(tidyverse)


# SE function
se <- function (x) {
  se = sd(x, na.rm = TRUE)/sqrt(length(x))
  return(se)
}

filepath <- '../data/data_reduced.csv'
data <- read.csv(filepath)

# 1. Betting rate in yellow is higher in control
d1_plot <- data %>%
  filter(block_type == "C") %>%
  mutate(treatment = factor(treatment, levels = c("control", "test"),
                            labels = c("Control", "Test"))) %>%
  group_by(id, treatment) %>%
  summarize(betting_rate = mean(choice)) %>%
  ungroup %>%
  group_by(treatment) %>%
  summarize(mean = mean(betting_rate),
            se = se(betting_rate)) %>%
  ungroup

ggplot(d1_plot, aes(x = treatment, y = mean)) +
  geom_bar(stat = "identity", fill = '#f7ef25', width = 0.6) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se),
                width = 0.1) +
  labs(x = 'Treatment', y = 'Mean betting rate') +
  theme_minimal()


# 2. Betting rate in yellow control by uncertainty
d2_plot <- data %>%
  filter(block_type == "C") %>%
  mutate(treatment = factor(treatment, levels = c("control", "test"),
                            labels = c("Control", "Test")),
         uncertainty = factor(aaron_mood, levels = c("Low", "High"),
                              labels = c("Low", "High"))) %>%
  group_by(id, treatment, uncertainty) %>%
  summarize(betting_rate = mean(choice)) %>%
  ungroup %>%
  group_by(treatment, uncertainty) %>%
  summarize(mean = mean(betting_rate),
            se = se(betting_rate)) %>%
  ungroup

ggplot(d2_plot, aes(x = treatment, y = mean, fill = uncertainty)) +
  geom_bar(stat = "identity", width = 0.6,
           position = position_dodge(preserve = "single")) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se),
                width = 0.1,
                position = position_dodge(width = 0.6, preserve = "single")) +
  labs(x = 'Treatment', y = 'Mean betting rate') +
  theme_minimal()





