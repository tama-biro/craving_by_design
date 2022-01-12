

x <- data.frame()
for(i in 11:(nrow(data) - 11)) {
  if(data$block_type[i] == 'S' &
     any(data$block_type[(i-5):(i-1)] == 'C') &
     data$block_type[i+10] == 'C' &
     data$treatment[i] == 'control' &
     all(data$id[(i-5):(i+10)] == data$id[i])) {
    
    x <- rbind(x, c(data$choice[i], data$id[i], 'inter', data$treatment[i]))
    
  } else if(data$block_type[i] == 'S' &
            any(data$block_type[(i+1):(i+5)] == 'C') &
            data$block_type[i-10] == 'C' &
            data$treatment[i] == 'control' &
            all(data$id[(i-10):(i+5)] == data$id[i])) {
    
    x <- rbind(x, c(data$choice[i], data$id[i], 'inter', data$treatment[i]))
    
  } else if(data$block_type[i] == 'S') {
    x <- rbind(x, c(data$choice[i], data$id[i], 'other', data$treatment[i]))
  }
}

colnames(x) <- c('choice', 'id', 'type', 'treatment')

by_id_x <- x %>%
  mutate(choice = as.numeric(choice)) %>%
  filter(treatment == '1') %>%
  group_by(id, type) %>%
  summarize(betting_rate = mean(choice, na.rm = T))

t.test(betting_rate ~ type, data = by_id_x)

(mean(by_id_x$betting_rate[by_id_x$type == 'other']) - 
    mean(by_id_x$betting_rate[by_id_x$type == 'inter']))/
  (sqrt((sd(by_id_x$betting_rate[by_id_x$type == 'other'])^2 +
          sd(by_id_x$betting_rate[by_id_x$type == 'inter'])^2)/2))

