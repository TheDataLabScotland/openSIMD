###
### Some utility functions for the openSIMD project
###

# replace missing values with 0
replaceMissing <- function(v) replace(v, is.na(v) | v == Inf | v == -Inf, 0)

# extract the weights from factor analysis
getFAWeights <- function(dat, ...) {
  
  fact <- psych::fa(dat, nfactors = 1, fm = "ml", rotate = "none", ...)
  
  f1_scores <- as.data.frame(fact$weights) %>% select(ML1)
  
  f1_weights <- f1_scores / sum(f1_scores)
  
  return(lapply(seq_along(f1_weights$ML1), function(i) f1_weights$ML1[i]))

  }

# exponential transform for SIMD
expoTransform <- function(ranks) {
  
  prop_ranks <- ranks / max(ranks)
  
  expo <- -23 * log(1 - prop_ranks * (1 - exp( -100 / 23)))
  
  return(expo)
}

# combine the weights and normalised scores by multiplying
combineWeightsAndNorms <- function(weights, norms) {
  
  combined <- purrr::map2(weights, norms, ~ .x * .y)
  
  combined %>% data.frame %>% rowSums
  
}
