library(dplyr)
setwd("openSIMD_analysis") # if running from parent dir
source("scripts/utils/helpers.R")
domains <- read.csv("results/domain_ranks.csv")

invRank <- function(v) rank(-v)

exponential_domains <- domains %>%
  mutate_at(vars(-data_zone), funs(invRank)) %>%
  mutate_at(vars(-data_zone), funs(expoTransform))

with(exponential_domains, {
  simd_score <<-
    .28 * income +
    .28 * employment +
    .14 * health +
    .14 * education +
    .09 * access +
    .05 * crime +
    .02 * housing
})

simd_rank <- rank(-simd_score)

simd_results <- data.frame(
  data_zone = domains$data_zone,
  simd_rank = simd_rank
)

write.csv(simd_results, "results/openSIMD_ranks.csv", row.names = FALSE)
