
library(tidyverse)

# Best 5 from each group
# Make new df with final accumulated outcome for each participant
final_outcome <- data.frame(email = 'asd', win = 1, wager = 1, treat = 'Test', craver = 1)
for(i in 2:nrow(data)) {
  if(data$id[i] != data$id[i - 1] | i == nrow(data)) {
    final_outcome <- rbind(final_outcome, c(data$email[i-1], 
                                            data$accumulated_outcomes[i-1], data$wager[i-1], 
                                            data$treatment[i-1],
                                            data$craver[i-1]))
  }
}

final_outcome <- final_outcome[2:nrow(final_outcome), ]
final_outcome <- final_outcome %>%
  mutate(wager = as.numeric(wager),
         win = as.numeric(win))

# get top 5
final_outcome %>%
  group_by(treat) %>% 
  slice_max(order_by = win, n = 5)


# Outcome statistics
final_outcome %>%
  group_by(treat) %>%
  summarize(mean = mean(win))

ggplot(final_outcome, aes(x = treat, y = win)) +
  geom_boxplot() +
  scale_x_discrete(limits = 1:2, labels = c('Control', 'Test')) +
  labs(x = 'Treatment', y = 'Final accumulated outcomes') +
  theme_minimal()

ggsave('/Users/sam/Google Drive/Elise Projects/Craving by Design/Pilot/Plots/box_earnings.png',
       height = 3, width = 5)


# Payouts by treatment
payout <- final_outcome %>%
  mutate(payout = if_else(win + wager - 177 > 0, (win + wager - 177) * 24, win + wager - 177)) %>%
  mutate(payout = if_else(payout > 110, 110, payout)) %>%
  mutate(payout = if_else(payout < -50, -50, payout),
         treat = factor(treat, levels = c('1', '2'), labels = c('Control', 'Test')),
         craver = factor(craver, levels = c('1', '2'), labels = c('Optimal', 'Craver'))) %>%
  mutate(payout = if_else(payout < 0, 5, payout + 5)) %>%
  mutate(payout = if_else(craver == 'Craver', payout + 3, payout))

ggplot(payout, aes(treat, payout)) +
  geom_boxplot() +
  stat_summary(fun = 'mean', col = 'red') +
  geom_text(x=1, y=50, label='Mean = -6.22') +
  geom_text(x=2, y=50, label='Mean = 25.59') +
  labs(x = 'Treatment', y = 'Payout', title = 'Wager added before') +
  theme_minimal()

ggsave('/Users/sam/Google Drive/Elise Projects/Craving by Design/Pilot/Plots/payouts_by_treat_before.png',
       height = 3, width = 5)

mean(payout$payout)
mean(payout$payout[payout$treat == 'Control'])
mean(payout$payout[payout$treat == 'Test'])


# Payouts by treatment
ggplot(payout, aes(craver, payout)) +
  geom_boxplot() +
  stat_summary(fun = 'mean', col = 'red') +
  geom_text(x=1, y=50, label='Mean = 37.2') +
  geom_text(x=2, y=50, label='Mean = -6.53') +
  labs(x = 'Craver', y = 'Payout', title = 'Wager added before') +
  theme_minimal()

ggsave('/Users/sam/Google Drive/Elise Projects/Craving by Design/Pilot/Plots/payouts_by_craving_before.png',
       height = 3, width = 5)

mean(payout$payout[payout$craver == 'Craver'])
mean(payout$payout[payout$craver == 'Optimal'])

sum(payout$payout[payout$craver == 'Optimal'] < 0)/sum(payout$craver == 'Optimal')
sum(payout$payout[payout$craver == 'Craver'] < 0)/sum(payout$craver == 'Craver')




