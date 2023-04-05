# QUESTION 1 CODE
library(tidyverse)
library(arrow)
set.seed(971)

df <- read_parquet("gz2_catalog_with_modern_schema_no_paths.parquet")

# getting relevant variables
df <- df %>% select(iauname, `spiral-arm-count-gz2_1`, `spiral-arm-count-gz2_2`, 
                    `spiral-arm-count-gz2_3`, `spiral-arm-count-gz2_4`,
                    `spiral-arm-count-gz2_more-than-4`, `spiral-arm-count-gz2_cant-tell`,
                    `spiral-arm-count-gz2_total-votes`)

# renaming variables
names(df)[names(df)=="spiral-arm-count-gz2_1"] <- 'one_arm_votes'
names(df)[names(df)=="spiral-arm-count-gz2_2"] <- 'two_arm_votes'
names(df)[names(df)=="spiral-arm-count-gz2_3"] <- 'three_arm_votes'
names(df)[names(df)=="spiral-arm-count-gz2_4"] <- 'four_arm_votes'
names(df)[names(df)=="spiral-arm-count-gz2_more-than-4"] <- 'more_than_four_arm_votes'
names(df)[names(df)=="spiral-arm-count-gz2_total-votes"] <- 'total_votes'
names(df)[names(df)=="spiral-arm-count-gz2_cant-tell"] <- 'cant_tell_votes'

# data cleaning/manipulation
df <- filter(df, total_votes != 0)
df <- mutate(df, less_than_four_arm_votes = one_arm_votes + two_arm_votes + three_arm_votes)
df <- mutate(df, four_or_more_arm_votes = more_than_four_arm_votes + four_arm_votes)
df <- mutate(df, four_or_more_consensus = four_or_more_arm_votes > less_than_four_arm_votes)
df <- mutate(df, voter_confidence = 1 - cant_tell_votes/total_votes)
df <- mutate(df, most_can_tell = voter_confidence > 0.5)

df <- df[, c(1, 7, 8, 9, 10, 11, 12, 13)]

# see the dataframe
glimpse(df)
head(df)

# VISUALIZATIONS
library(ggplot2)

# visualization 1 (bar plot)
dfsum1 <- df %>% filter(four_or_more_arm_votes != less_than_four_arm_votes) %>%
  group_by(most_can_tell, four_or_more_consensus) %>% tally()

dfsum1 %>% ggplot(aes(x = most_can_tell, y = n)) + geom_col(aes(fill = four_or_more_consensus), width = 0.8) +
  labs(x="Most people were confident voters", y="Number of Galaxies", fill="Most people voted it had four or more") +
  guides(four_or_more_consensus = FALSE) + theme_bw()

#note we ignored these galaxies in our bar plot assessment
sum(df$four_or_more_arm_votes == df$less_than_four_arm_votes)
#note for later: probably should be a chi squared diagram instead of a bar plot/ histogram
# also, we should do a chi-squared test instead of a t-test like the code is about to do...

# hypothesis test stats
param_stat <- sum(df$four_or_more_consensus)/length(df$iauname)
param_stat  # ratio of fourormore in every galaxy

can_stat <- sum(df$four_or_more_consensus & df$most_can_tell)/sum(df$most_can_tell)
can_stat  # ratio of fourormore in confident galaxies

cant_stat <- sum(df$four_or_more_consensus & !df$most_can_tell)/sum(!df$most_can_tell)
cant_stat  # ratio of fourormore in non-confident galaxies

# null hyp is that can_stat = cant_stat, 
# alt hyp is that can_stat != cant_stat

# chi sqaure test on the two categorical variables
test_1 <- chisq.test(df$four_or_more_consensus, df$most_can_tell)
test_1

lm(df$four_or_more_consensus ~ df$most_can_tell)


# 2nd visualization
# note how we're now including galaxies where the num of four-or-less == more-than-four
#relation of % of all_votes being more-than-4 and % of unconfident votes
df %>% ggplot(aes(x = four_or_more_arm_votes/total_votes, y = voter_confidence)) + geom_point() +
  geom_smooth(method=lm, se=FALSE) + labs(x="Percentage of all voters who voted it had four or more arms", y="Percentage of confident voters")
lm(four_or_more_arm_votes/total_votes ~ voter_confidence, data=df) %>% summary()

# correlation (we must manually filter out NA values)
df <- df %>% filter(!is.na(four_or_more_arm_votes)) %>%
  filter(!is.na(total_votes)) %>% filter(!is.na(cant_tell_votes)) %>%
  filter(!is.na(voter_confidence))
x = df$four_or_more_arm_votes/(df$total_votes)
y = df$voter_confidence
cor(x, y, method='spearman')


#relation of % of confident_votes being more-than-4 and % of unconfident votes
df %>% ggplot(aes(x = four_or_more_arm_votes/(total_votes - cant_tell_votes), y = voter_confidence)) + geom_point() +
  geom_smooth(method=lm, se=FALSE) + labs(x="Percentage of confident voters who voted it had four or more arms", y="Percentage of confident voters")
lm(four_or_more_arm_votes/(total_votes - cant_tell_votes) ~ voter_confidence, data=df) %>% summary()

# correlation (we must manually filter out NA values)
df <- df %>% filter(!is.na(four_or_more_arm_votes)) %>%
  filter(!is.na(total_votes)) %>% filter(!is.na(cant_tell_votes)) %>%
  filter(!is.na(voter_confidence))
x = df$four_or_more_arm_votes/max(df$total_votes - df$cant_tell_votes, 1)
y = df$voter_confidence
cor(x, y, method='spearman')
