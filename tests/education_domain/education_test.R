library(readxl)
library(dplyr)
library(ggplot2)
library(purrr)

source("scripts/utils/normalScores.R")
source("scripts/utils/helpers.R")

d <- read_excel("data/00510566.xlsx", sheet = 3, na = "*")

normalised_education <- d %>%
  select(Attendance, Attainment, Noquals, NEET, HESA) %>%
  mutate(Attendance = normalScores(Attendance, forwards = FALSE)) %>%
  mutate(Attainment = normalScores(Attainment, forwards = FALSE)) %>%
  mutate(Noquals    = normalScores(Noquals, forwards = TRUE)) %>%
  mutate(NEET       = normalScores(NEET, forwards = TRUE)) %>%
  mutate(HESA       = normalScores(HESA, forwards = FALSE)) %>%
  mutate_all(funs(replaceMissing))

education_weights <- getFAWeights(
  normalised_education,
  nfactors = 1,
  fm = "ml",
  rotate = "none"
)

education_score <- map2(education_weights, normalised_education, ~ .x * .y) %>% data.frame() %>% rowSums()

education_rank <- rank(-education_score)

########################################################################

# compare to SAS results
sas <- read_excel("data/EDUCATION.xlsx")

results <- data.frame(
  data_zone = d$Data_Zone,
  sas_edu_score = sas$eduscr,
  r_edu_score = education_score,
  sas_edu_rank = sas$edurnk,
  r_edu_rank = education_rank
)

pdf("tests/education_domain/education_results.pdf")
p1 <- ggplot(results, aes(x = sas_edu_score, y = r_edu_score)) +
  geom_point()
p2 <- ggplot(results, aes(x = sas_edu_rank, y = r_edu_rank)) +
  geom_point()
gridExtra::grid.arrange(p1, p2)
dev.off()

differences <- data.frame(
  scores = abs(results$r_edu_score - results$sas_edu_score),
  ranks = abs(results$r_edu_rank - results$sas_edu_rank)
)

pdf("tests/education_domain/rank_differences.pdf")
p1 <- ggplot(filter(differences), aes(x = scores)) +
  geom_histogram(bins = 50)
p2 <- ggplot(filter(differences), aes(x = ranks)) +
  geom_histogram(bins = 50)
gridExtra::grid.arrange(p1, p2)
dev.off()

d$score_differences <- differences$scores
d$rank_differences <- differences$ranks
write.csv(d %>% top_n(10, rank_differences), "tests/education_domain/top_differences.csv")

