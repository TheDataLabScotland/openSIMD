### compare the R normalisation proceedure with the SAS one

library(readxl)
library(dplyr)

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

