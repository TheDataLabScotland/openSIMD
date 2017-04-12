library(dplyr)

source("scripts/utils/helpers.R")

domains <- read.csv("results/domain_ranks.csv")

exponential_domains <- domains %>%
  mutate_at(-data_zone, )
  