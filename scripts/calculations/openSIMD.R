library(readxl)
library(dplyr)

source("scripts/utils/helpers.R")

d <- read_excel("data/00510566.xlsx", sheet = 3, na = "*")
domains <- read.csv("results/domain_ranks.csv")

exponential_domains <- domains %>%
  mutate_at(vars(-data_zone), funs(expoTransform))

with(exponential_domains, {
  simd_score <<- 
    .28 * income +
    .28 * health +
    .14 * education +
    .09 * access +
    .05 * crime +
    .02 * housing
})

simd_rank <- rank(-simd_score)

simd_results <- data.frame(
  data_zone = d$Data_Zone,
  simd_rank = simd_rank
)

write.csv(simd_results, "results/openSIMD_ranks.csv")