library(tidyverse)
library(arrow)

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

sum(df$more_than_four_consensus)/length(df$iauname)

sum(df$more_than_four_consensus & df$most_can_tell)/sum(df$most_can_tell)

sum(df$more_than_four_consensus & !df$most_can_tell)/sum(!df$most_can_tell)
