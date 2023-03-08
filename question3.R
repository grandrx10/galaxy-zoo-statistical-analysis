library(tidyverse)
library(arrow)

df2 <- read_parquet("nsa_v1_0_1_key_cols.parquet")
glimpse(df2)
