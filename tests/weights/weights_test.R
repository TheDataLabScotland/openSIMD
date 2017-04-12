###
### Can we reproduce the weights derived from factor analysis?
###

library(readxl)
library(dplyr)
library(psych)
library(ggplot2)

source("scripts/utils/normalScores.R")
source("scripts/utils/helpers.R")

d <- read_excel("data/00510566.xlsx", sheet = 3, na = "*")
str(d)

# generate the normalised scores
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

write.csv(education_weights, "tests/weights/r_edu_weights.csv")

