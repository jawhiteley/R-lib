################################################################
### Comparing values with NAs
### Jonathan A. Whiteley          R v4.3.1          2023-09-12
################################################################

## Adapted from: http://www.cookbook-r.com/Manipulating_data/Comparing_vectors_or_factors_with_NA/
eqNA <- function (v1, v2) {
  same <- (v1 == v2) | (is.na(v1) & is.na(v2))
  same[is.na(same)] <- FALSE
  return(same)
}

## Generalized to use any comparison function (e.g., dplyr::near)
## might not work as expected with comparisons other than 'equals'
compareNA <- function (v1, v2, fun = `==`, ..., nana = TRUE, na. = FALSE) {
  same <- do.call(fun, args = list(v1, v2, ...))
  same[(is.na(v1) & is.na(v2))] <- nana
  same[is.na(same)] <- na.
  return(same)
}

## infix 'equals' operator that automatically applies a tolerance
## (as with all.equal() and dplyr::near()) when comparing `double`s
## and handles NAs & NaNs
`%=%` <- function (v1, v2) {
  if (typeof(v1) == "double" && typeof(v2) == "double") {
    tolerance = .Machine$double.eps^0.5
    same <- (abs(v1 - v2) < tolerance)
  } else {
    same <- (v1 == v2)
  }
  same <- same | (is.na(v1) & is.na(v2))
  same[is.na(same)] <- FALSE
  return(same)
}


##==============================================================
## TESTING

if (F) {
  vec1 <- c(1, NA, 2 , .3 + .6, NaN)
  vec2 <- c(1, NA, NA, .9,      NaN)
  vec3 <- c("a", NA, "2", TRUE)
  vec4 <- c("a", NA,  NA, FALSE)
  compareNA(vec1, vec2)               # TRUE  TRUE FALSE FALSE  TRUE
  compareNA(vec1, vec2, dplyr::near)  # TRUE  TRUE FALSE  TRUE  TRUE
  vec1 %=% vec2                       # TRUE  TRUE FALSE  TRUE  TRUE
  compareNA(vec3, vec4)   # TRUE  TRUE FALSE FALSE
  vec3 %=% vec4           # TRUE  TRUE FALSE FALSE
}

##==============================================================
## Benchmarking

if (F) {
  #set.seed(1)
  
  n = 10000000
  num1 <- runif(n)
  num1[sample(seq_len(length(num1)), length(num1) * .2)] <- NA
  num2 <- runif(n)
  num2[sample(seq_len(length(num2)), length(num2) * .2)] <- NA
  
  system.time( num1 == num2 )
  system.time( eqNA(num1, num2) )
  system.time( compareNA(num1, num2) )
  system.time( compareNA(num1, num2, fun = near) )
  system.time( num1 %=% num2 )
  
  ## reset random seed
  #set.seed(seed = NULL)
}
