library(tidyverse)
library(arrow)
set.seed(971)

df <- read_parquet("gz2_catalog_with_modern_schema_no_paths.parquet")

df <- df %>% select(iauname, `spiral-arm-count-gz2_1`, `spiral-arm-count-gz2_2`, 
              `spiral-arm-count-gz2_3`, `spiral-arm-count-gz2_4`,
              `spiral-arm-count-gz2_more-than-4`, `spiral-arm-count-gz2_cant-tell`,
              `spiral-arm-count-gz2_total-votes`)

names(df)[names(df)=="spiral-arm-count-gz2_1"] <- 'one_arm_votes'
names(df)[names(df)=="spiral-arm-count-gz2_2"] <- 'two_arm_votes'
names(df)[names(df)=="spiral-arm-count-gz2_3"] <- 'three_arm_votes'
names(df)[names(df)=="spiral-arm-count-gz2_4"] <- 'four_arm_votes'
names(df)[names(df)=="spiral-arm-count-gz2_more-than-4"] <- 'more_than_four_arm_votes'
names(df)[names(df)=="spiral-arm-count-gz2_total-votes"] <- 'total_votes'
names(df)[names(df)=="spiral-arm-count-gz2_cant-tell"] <- 'cant_tell_votes'

df <- filter(df, total_votes != 0)
df <- mutate(df, four_or_less_arm_votes = one_arm_votes + two_arm_votes + three_arm_votes + four_arm_votes)
df <- df[, c(1, 9, 6, 7, 8)]

df <- filter(df, more_than_four_arm_votes != four_or_less_arm_votes )
df <- mutate(df, more_than_four_consensus = more_than_four_arm_votes > four_or_less_arm_votes)
df <- mutate(df, voter_confidence = 1 - cant_tell_votes/total_votes)
df <- mutate(df, most_can_tell = voter_confidence >= 0.5)

glimpse(df)
head(df)

library(ggplot2)

# visualization 1 (bar plot)
dfsum1 <- df %>% group_by(most_can_tell, more_than_four_consensus) %>% tally()

dfsum1 %>% ggplot(aes(x = most_can_tell, y = n, fill = more_than_four_consensus, colour = more_than_four_consensus)) + 
  geom_bar(stat = "identity", position = 'dodge', alpha = 0.5) +
  labs(x="Most people were confident voters", y="Number of Galaxies", fill="Most people voted more than 4") +
  guides(more_than_four_consensus = FALSE) + theme_bw()

# visualization 2 (histogram)
df %>% ggplot(aes(x = voter_confidence, fill=more_than_four_consensus)) + geom_histogram(position = "identity", alpha = 0.5, bins=10) +
  labs(x="Voter confidence", y="Number of Galaxies", fill='Most people voted more than 4') + 
  ylim(0, 31000) + theme_bw()


# hypothesis test
param_stat <- sum(df$more_than_four_consensus)/length(df$iauname)

can_stat <- sum(df$more_than_four_consensus & df$most_can_tell)/sum(df$most_can_tell)

cant_stat <- sum(df$more_than_four_consensus & !df$most_can_tell)/sum(!df$most_can_tell)

# null hyp is that can_stat = cant_stat, 
# alt hyp is that can_stat != cant_stat

test_stat <- cant_stat
repetitions <- 5000
simulated_values <- rep(NA, repetitions)

for(i in 1:repetitions){
  # perform a random permutation and compute the simulated test statistic
  simdata <- df %>%
    mutate(more_than_four_consensus = sample(more_than_four_consensus, replace=FALSE))
  
  # re-compute the test statistic
  sim_value <- sum(simdata$more_than_four_consensus & !simdata$most_can_tell)/sum(!simdata$most_can_tell)
  
  # store the simulated value
  simulated_values[i] <- as.numeric(sim_value)
}

# convert vector results to a tibble
sim <- tibble(median_diff = simulated_values)

# plot the results
sim %>% ggplot(aes(x=median_diff)) + geom_histogram()

# Check p-value
alpha <- 0.05
p_val = 2 * sum(simulated_values >= test_stat) / repetitions
print(paste(c('p value:', p_val)))
print(ifelse(p_val < alpha, 'Reject the null', 'Fail to reject the null'))
