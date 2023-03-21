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

df <- mutate(df, more_than_four_consensus = more_than_four_arm_votes > four_or_less_arm_votes)
df <- mutate(df, voter_confidence = 1 - cant_tell_votes/total_votes)
df <- mutate(df, most_can_tell = voter_confidence >= 0.5)

glimpse(df)
head(df)

library(ggplot2)

# visualization 1 (bar plot)
dfsum1 <- df %>% filter(more_than_four_arm_votes != four_or_less_arm_votes) %>%
  group_by(most_can_tell, more_than_four_consensus) %>% tally()

dfsum1 %>% ggplot(aes(x = most_can_tell, y = n)) + geom_col(aes(fill = more_than_four_consensus), width = 0.8) +
  labs(x="Most people were confident voters", y="Number of Galaxies", fill="Most people voted more than 4") +
  guides(more_than_four_consensus = FALSE) + theme_bw()

#note we ignored these galaxies in our bar plot assessment
sum(df$more_than_four_arm_votes == df$four_or_less_arm_votes)
#note for later: probably should be a chi squared diagram instead of a bar plot/ histogram
# also, we should do a chi-squared test instead of a t-test like the code is about to do...

# hypothesis test
param_stat <- sum(df$more_than_four_consensus)/length(df$iauname)
param_stat
can_stat <- sum(df$more_than_four_consensus & df$most_can_tell)/sum(df$most_can_tell)
can_stat
cant_stat <- sum(df$more_than_four_consensus & !df$most_can_tell)/sum(!df$most_can_tell)
cant_stat

# null hyp is that can_stat = cant_stat, 
# alt hyp is that can_stat != cant_stat

test_1 <- chisq.test(df$more_than_four_consensus, df$most_can_tell)
test_1


# 2nd visualization
# note how we're now including galaxies where the num of four-or-less == more-than-four

#relation of % of confident_votes being more-than-4 and % of unconfident votes
df %>% ggplot(aes(x = more_than_four_arm_votes/(total_votes-cant_tell_votes), y = voter_confidence)) + geom_point() +
  geom_smooth(method=lm, se=FALSE)
lm(more_than_four_arm_votes/(total_votes-cant_tell_votes) ~ voter_confidence, data=df) %>% summary()

#relation of % of all_votes being more-than-4 and % of unconfident votes
df %>% ggplot(aes(x = more_than_four_arm_votes/total_votes, y = voter_confidence)) + geom_point() +
  geom_smooth(method=lm, se=FALSE)
lm(more_than_four_arm_votes/total_votes ~ voter_confidence, data=df) %>% summary()
