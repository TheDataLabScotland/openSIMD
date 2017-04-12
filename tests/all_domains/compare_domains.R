
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)

sas_results <- read_excel("data/SIMD16_ranks.xlsx", sheet = 2)
r_results <- read.csv("results/domain_ranks.csv")

sas_results <- sas_results %>%
  select(-Intermediate_Zone, -Council_area, -Total_population, 
         -Working_age_population_revised, -Overall_SIMD16_rank)
names(sas_results) <- c("data_zone", "income", "employment", "health", 
                        "education", "housing", "access", "crime")

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
