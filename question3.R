library(tidyverse)
library(arrow)

df2 <- read_parquet("nsa_v1_0_1_key_cols.parquet")
glimpse(df2)

ggplot(data = df2) + 
  aes(x = redshift, y = petro_theta) + 
  geom_point() + 
  labs(title = "Redshift Compared to the Radius of the Galaxy", x = 'Redshift', y = 'Radius of Galaxy')
