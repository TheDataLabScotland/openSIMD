###
### Some utility functions for the openSIMD project
###

# replace missing values with 0
replaceMissing <- function(v) replace(v, is.na(v) | v == Inf | v == -Inf, 0)

getFAWeights <- function(dat) {
  fact <- psych::fa(dat)
  weights <- fact$weights / sum(fact$weights)
  return(weights)
}