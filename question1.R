library(tidyverse)
library(arrow)

df2 <- read_parquet("gz2_catalog_with_modern_schema_no_paths.parquet")
glimpse(df)

# This is a change
