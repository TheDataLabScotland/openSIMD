###
### Some utility functions for the openSIMD project
###

### normalScores
### A function to calculate the normal scores as in SIMD 16 SAS code
### The function is defined as follows:
###   yi = 1/theta ((ri) / (n + 1))
###     where:
###       theta = the cumulative normal (probit) function
###       ri = the rank of the i'th observation
###       n = the number of non-missing observations for the ranking variable
###
### The resulting variables should appear normally distributed.
### In SIMD this function is used to transform indicators before combining them.
###
### This R function will take a numeric vector as input and return a numeric vector 
### with the transformation applied.

normalScores <- function(
  v,                  # a numeric vector as the input variable
  ties = "average",   # passed to ties.method argument in rank()
  forwards = TRUE     # smallest numerical value on left? default is TRUE 
) {
  
  r <- rank(v, ties.method = ties)
  n <- length(na.omit(v))
  
  rn <- r / (n + 1)
  
  y <- qnorm(rn, mean = 0, sd = 1, lower.tail = forwards)
  
  return(y)
  
}
#####################################################################################

### replaceMissing
### A function to replace missing values once data has been normalised by 
### normalScores(). The function takes a numeric vector, finds the missing values
### as well as infinity and minus infinity values and replaces them with 0.
### Note that infinity values can be generated in normalScores() from missing values
### in the original data.

replaceMissing <- function(v) replace(v, is.na(v) | v == Inf | v == -Inf, 0)
#####################################################################################

### getFAWeights
### This function performs factor analysis and returns the proportional scores or 
### weights for each variable on the first factor. This becomes the weight for
### combining the normalised indicator scores.
### The function takes a data frame, assumes that all variables are being processed
### in factor analysis and returns a list where each element is a numeric value,
### the weight corresponding to that column in the data.

getFAWeights <- function(dat, ...) {
  
  fact <- psych::fa(dat, nfactors = 1, fm = "ml", rotate = "none", ...)
  
  f1_scores <- as.data.frame(fact$weights) %>% select(ML1)
  
  f1_weights <- f1_scores / sum(f1_scores)
  
  # This is just to make each weight an individual element of a list
  # For compatibility with purrr::map2() in the next step, combineWeightsAndNorms()
  return(lapply(seq_along(f1_weights$ML1), function(i) f1_weights$ML1[i]))

  }
#####################################################################################

### combineWeightsAndNorms
### This function takes the normalised indicator scores and the weights derived from 
### factor analysis, multiplies them out and then takes the sum of these weighted 
### indicator scores to get the final score for that domain.
### The function takes a list of weights (generated by getFAWeights()) and a 
### data.frame of normalised scores, it returns a numeric vector containing the 
### combined domain score.

combineWeightsAndNorms <- function(weights, norms) {
  
  combined <- purrr::map2(weights, norms, ~ .x * .y)
  
  combined %>% data.frame %>% rowSums
  
}
#####################################################################################

### expoTransform
### This function recapitulates the exponential transformation used in SIMD to 
### transform the weighted domain ranks before combining domains.
### The function takes an numeric vector (of the ranks) and returns a numeric vector
### of equal length containing the transformed values.

expoTransform <- function(ranks) {
  
  prop_ranks <- ranks / max(ranks)

  expo <- -23 * log(1 - prop_ranks * (1 - exp( -100 / 23)))
  
  return(expo)
}
#####################################################################################

### reassignRank
### A function to make manual reassignments of ranks to individual data zones, can 
### be used when there are strange exceptions such as empty data zones. The function
### needs a data.frame of ranks containing a column named 'data_zone'. The function 
### will take that data.frame change the rank of the indicator in question (with an
### optional offset) and then re-rank.
### The function returns the corrected data.frame.

# reassign ranks to individual data zones
reassignRank <- function(data, domain, data_zone, end = "max", offset = 0) {
  
  if(end == "max") {
    data[data$data_zone == data_zone, domain] <- 
      max(data[, domain], na.rm = TRUE) - (offset * 0.1)
  }else
    if(end == "min") {
      data[data$data_zone == data_zone, domain] <- 
        min(data[, domain], na.rm = TRUE) + (offset * 0.1)
    }
  
  data[, domain] <- rank(data[, domain])
  
  return(data)
}
#####################################################################################