library(tidyverse)
library(arrow)

df2 <- read_parquet("nsa_v1_0_1_key_cols.parquet")
glimpse(df2)

# scatterplot
ggplot(data = df2) + 
  aes(x = redshift, y = petro_theta) + 
  geom_point() + 
  labs(title = "Redshift Compared to the Radius of the Galaxy", x = 'Redshift', y = 'Radius of Galaxy')

cor(df2$redshift, df2$petro_theta)

# scatterplot with line of best fit
ggplot(data = df2) + 
  aes(x = redshift, y = petro_theta) + 
  geom_point() + geom_smooth(method=lm, se=FALSE) + 
  labs(title = "Redshift Compared to the Radius of the Galaxy", x = 'Redshift', y = 'Radius of Galaxy')

# boxplot
ggplot(data = df2) +
  aes(x = redshift, y = petro_theta) + 
  geom_boxplot() +
  labs(title = "Redshift Compared to the Radius of the Galaxy", x = 'Redshift', y = 'Radius of Galaxy')


# used to find p-value
cor.test(df2$redshift, df2$petro_theta)
