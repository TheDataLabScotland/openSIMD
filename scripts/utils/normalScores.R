### normalScores
### A function to calculate the normal scores as in SIMD 16 SAS code
### The function is defined as follows:
###   yi = 1/theta * (ri) / (n + 1)
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
   
  rn_mean <- mean(rn, na.rm = TRUE)
  rn_sd <- sd(rn, na.rm = TRUE)
  
  y <- qnorm(rn, mean = 0, sd = 1, lower.tail = forwards)
  
  return(y)
  
}