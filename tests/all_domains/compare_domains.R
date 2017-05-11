library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)

sas_results <- read_excel("data/SAS SIMD and domain ranks.xlsx", sheet = 1)
r_results <- read.csv("results/domain_ranks.csv")

sas_results <- sas_results %>%
  select(-IZ, -LA, -pop, -wapop, -SIMD)
names(sas_results) <- c("data_zone", "income", "employment", "health", 
                        "education", "access", "crime", "housing")

sas_results$source <- "sas"
r_results$source <- "r"

sas_results <- gather(sas_results, domain, rank, -data_zone, -source)
r_results <- gather(r_results, domain, rank, -data_zone, -source)

results <- rbind(sas_results, r_results) %>%
  spread(source, rank)

pdf("tests/all_domains/compare_domains.pdf")
ggplot(results, aes(x = sas, y = r)) +
  geom_point() +
  facet_wrap(~ domain)
dev.off()
