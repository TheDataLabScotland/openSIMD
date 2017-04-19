library(readxl)
library(dplyr)
library(ggplot2)

sas_results <- read_excel("data/updated SIMD and domain ranks.xlsx", sheet = 1) %>%
  select(DZ, SIMD)
r_results <- read.csv("results/openSIMD_ranks.csv")

names(sas_results) <- c("data_zone", "sas")
names(r_results) <- c("data_zone", "r")

results <- left_join(sas_results, r_results)

pdf("tests/simd_comparison/comparing_simd.pdf")
ggplot(results, aes(x = sas, y = r)) +
  geom_point()
dev.off()
