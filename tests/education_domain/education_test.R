library(readxl)
library(dplyr)
library(psych)
library(ggplot2)
library(tidyr)

source("scripts/utils/normalScores.R")
source("scripts/utils/helpers.R")

d <- read_excel("data/00510566.xlsx", sheet = 3, na = "*")
str(d)

normalised_education <- d %>%
  select(Attendance, Attainment, Noquals, NEET, HESA) %>%
  mutate(Attendance = normalScores(Attendance, forwards = FALSE)) %>%
  mutate(Attainment = normalScores(Attainment, forwards = FALSE)) %>%
  mutate(Noquals    = normalScores(Noquals, forwards = TRUE)) %>%
  mutate(NEET       = normalScores(NEET, forwards = TRUE)) %>%
  mutate(HESA       = normalScores(HESA, forwards = FALSE)) %>%
  mutate_all(funs(replaceMissing))

education_weights <- getFAWeights(normalised_education)

education_score <- normalised_education %>%
  mutate(Attendance = Attendance * education_weights['Attendance',]) %>%
  mutate(Attainment = Attainment * education_weights['Attainment',]) %>%
  mutate(Noquals = Noquals * education_weights['Noquals',]) %>%
  mutate(NEET = NEET * education_weights['NEET',]) %>%
  mutate(HESA = HESA * education_weights['HESA',]) %>%
  rowSums

education_rank <- rank(-education_score, ties.method = "average")

# compare to SAS results
sas <- read_excel("data/education/EDU_DOMAIN.xls")

results <- data.frame(
  data_zone = d$Data_Zone,
  sas_edu_score = sas$eduscore,
  r_edu_score = education_score,
  sas_edu_rank = sas$edurank,
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
  diffs = abs(education_rank - sas$edurank)
)
pdf("tests/education_domain/rank_differences.pdf")
ggplot(filter(differences, diffs < 100), aes(x = diffs)) +
  geom_histogram(bins = 50)
dev.off()
