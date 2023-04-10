library(tidyverse)
library(arrow)

df <- read_parquet("gz2_catalog_with_modern_schema_no_paths.parquet")
nsa <- read_parquet("nsa_v1_0_1_key_cols.parquet")
glimpse(nsa)

no_bulge <- df %>% filter(`bulge-size-gz2_no` > `bulge-size-gz2_just-noticeable` +
                          `bulge-size-gz2_obvious` + `bulge-size-gz2_dominant`) %>% select(iauname)
bulge <- df %>% filter(`bulge-size-gz2_no` < `bulge-size-gz2_just-noticeable` +
                  `bulge-size-gz2_obvious` + `bulge-size-gz2_dominant`) %>% select(iauname)

glimpse(no_bulge)

nsa_no_bulge <- nsa %>% filter(iauname %in% no_bulge$iauname) %>% 
  filter(!is.na(elpetro_absmag_r) & elpetro_absmag_r != Inf) %>% mutate(bulge=FALSE)
nsa_bulge <- nsa %>% filter(iauname %in% bulge$iauname) %>% 
  filter(!is.na(elpetro_absmag_r) & elpetro_absmag_r != Inf) %>% mutate(bulge=TRUE)

ggplot(nsa_no_bulge) + aes(x=elpetro_absmag_r) + geom_histogram(bins=50) + ggtitle("Brightness of Galaxies with no Bulge") +
  xlab("Brightness (Absolute Magnitudes)")

ggplot(nsa_bulge) + aes(x=elpetro_absmag_r) + geom_histogram(bins=50) + ggtitle("Brightness of Galaxies with Bulge") +
  xlab("Brightness (Absolute Magnitudes)")

nsa_total <- full_join(nsa_no_bulge, nsa_bulge)

ggplot(nsa_total) + aes(x=bulge, y=elpetro_absmag_r) + geom_boxplot() + ggtitle("Bulge vs Non-Bulge Galaxies Brightness")

nsa_no_bulge %>% summarize(avg_brightness = mean(elpetro_absmag_r), n=n())
nsa_bulge %>% summarize(avg_brightness = mean(elpetro_absmag_r), n=n())

set.seed(1)  # REQUIRED so the results are reproducible!

glimpse(nsa_total)
N <- 178398

# Test stat
test_stat <- nsa_total %>% group_by(bulge) %>%
  summarise(medians = median(elpetro_absmag_r), .groups="drop") %>%
  summarise(value = diff(medians))
test_stat <- as.numeric(test_stat)
test_stat

# code you answer here
repetitions <- 1000  # feel free to increase this is you want finer precision
simulated_values <- rep(NA, repetitions)

for(i in 1:repetitions){
  # perform a random permutation and compute the simulated test statistic
  simdata <- nsa_total %>% 
    mutate(elpetro_absmag_r = sample(elpetro_absmag_r, replace=FALSE))
  
  # re-compute the test statistic
  sim_value <- simdata %>% group_by(bulge) %>%
    summarise(medians = median(elpetro_absmag_r), .groups="drop") %>%
    summarise(value = diff(medians))
  
  # store the simulated value
  simulated_values[i] <- as.numeric(sim_value)
}

# convert vector results to a tibble
sim <- tibble(median_diff = simulated_values)

# plot the results
sim %>% ggplot(aes(x=median_diff)) + geom_histogram() + ggtitle("Difference in Median Brightness")

p_1side <- sum(simulated_values <= test_stat) / N
p_1side

