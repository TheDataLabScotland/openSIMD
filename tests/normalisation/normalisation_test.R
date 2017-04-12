### compare the R normalisation proceedure with the SAS one

library(readxl)
library(dplyr)

source("scripts/utils/normalScores.R")
source("scripts/utils/helpers.R")

d <- read_excel("data/00510566.xlsx", sheet = 3, na = "*")
noquals <- read_excel("data/NOQUALSDATA.xls")

source("scripts/utils/normalScores.R")
noquals$r_nnoquals <- normalScores(noquals$Noquals)

pdf("tests/normalisation/normalisation_test.pdf", 12, 6)
par(mfrow = c(1, 3))
hist(noquals$Noquals, breaks = 50)
hist(noquals$nnoquals, breaks = 50)
hist(noquals$r_nnoquals, breaks = 50)
dev.off()

### note that the two processes produce equivalent 
### results to 11 decimal places in this test
lapply(1:16, function(n) {
identical(
  round(noquals$nnoquals, n), 
  round(noquals$r_nnoquals, n)
)
}) %>% 
  unlist %>%
  cbind(1:16)

normalised_education <- d %>%
  select(Attendance, Attainment, Noquals, NEET, HESA) %>%
  mutate(Attendance = normalScores(Attendance, forwards = FALSE)) %>%
  mutate(Attainment = normalScores(Attainment, forwards = FALSE)) %>%
  mutate(Noquals    = normalScores(Noquals, forwards = TRUE)) %>%
  mutate(NEET       = normalScores(NEET, forwards = TRUE)) %>%
  mutate(HESA       = normalScores(HESA, forwards = FALSE)) %>%
  mutate_all(funs(replaceMissing))

write.csv(normalised_education, "tests/normalisation/r_edu_normalised.csv")