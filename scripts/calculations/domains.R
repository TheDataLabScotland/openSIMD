library(readxl)
library(dplyr)

source("scripts/utils/helpers.R")

d <- read_excel("data/00510566.xlsx", sheet = 3, na = "*")

###
### 1. Education
###

normalised_education <- d %>%
  select(Attendance, Attainment, Noquals, NEET, HESA) %>%
  mutate(Attendance = normalScores(Attendance, forwards = FALSE)) %>%
  mutate(Attainment = normalScores(Attainment, forwards = FALSE)) %>%
  mutate(Noquals    = normalScores(Noquals, forwards = TRUE)) %>%
  mutate(NEET       = normalScores(NEET, forwards = TRUE)) %>%
  mutate(HESA       = normalScores(HESA, forwards = FALSE)) %>% # is high value really good here???
  mutate_all(funs(replaceMissing))

education_weights <- getFAWeights(normalised_education)
education_score <- combineWeightsAndNorms(education_weights, normalised_education)
education_rank <- rank(-education_score)

###
### 2. Housing
###

housing_score <- d$overcrowded_rate + d$nocentralheat_rate
housing_rank <- rank(-housing_score)

###
### 3. Health
###

normalised_health <- d %>%
  select(CIF, ALCOHOL, DRUG, SMR, DEPRESS, LBWT, EMERG) %>%
  mutate_all(funs(normalScores)) %>%
  mutate_all(funs(replaceMissing))
  
health_weights <- getFAWeights(normalised_health)
health_score <- combineWeightsAndNorms(health_weights, normalised_health)
health_rank <- rank(-health_score)

###
### 4. Access 
###    (a) Drive
###

normalised_drive <- d %>%
  select(contains("drive")) %>%
  mutate_all(funs(normalScores)) %>%
  mutate_all(funs(replaceMissing))

drive_weights <- getFAWeights(normalised_drive)
drive_score <- combineWeightsAndNorms(drive_weights, normalised_drive)
drive_rank <- rank(drive_score)

###    (b) Public Transport

normalised_publictransport <- d %>%
  select(contains("PT")) %>%
  mutate_all(funs(normalScores)) %>%
  mutate_all(funs(replaceMissing))

publictransport_weights <- getFAWeights(normalised_publictransport)
publictransport_score <- combineWeightsAndNorms(publictransport_weights, normalised_publictransport)
publictransport_rank <- rank(publictransport_score)

###    (c) Combined

drive_exponential <- expoTransform(drive_rank)
publictransport_exponential <- expoTransform(publictransport_rank)
access_score <-  (drive_exponential * 2/3) + (publictransport_exponential * 1/3)
access_rank <- rank(-access_score)

###
### 4, 6, 7. Crime / Income / Employment
###

crime_rank <- rank(-d$crime_rate)
income_rank <- rank(-d$Income_rate)
employment_rank <- rank(-d$Employment_rate)

###
### Collect and reassign ranks
###

domain_ranks <- data.frame(
  data_zone = d$Data_Zone,
  education = education_rank,
  health = health_rank,
  housing = housing_rank,
  access = access_rank,
  crime = crime_rank,
  income = income_rank,
  employment = employment_rank
)

domain_ranks <- domain_ranks %>%
  reassignRank("crime", "S01010206", "max") %>%
  reassignRank("crime", "S01010227", "max", offset = 1) %>%
  reassignRank("income", "S01010206", "max") %>%
  reassignRank("income", "S01010227", "max", offset = 1) %>%
  reassignRank("employment", "S01010206", "max") %>%
  reassignRank("employment", "S01010227", "max")

###
### Export Domains
###

write.csv(domain_ranks, "results/domain_ranks.csv", row.names = FALSE)
