################################################################
### Exploring tests of Normality and skewness
### Jonathan Whiteley  	R v3.4.0		2018-01-11
################################################################

library(MASS)
library(car)

## Generate an explicitly asymmetric and non-normal distribution
n <- 1000
x <- rlnorm(n)

truehist(x, prob = T, col = "lightgrey")
lines(density(x), col = "#333399", lwd = 2)
#lines(density(x, adjust = 2), col = "darkred", lwd = 1, lty = "dotted")

qqnorm(x)
qqPlot(x)                              # car package

## Shapiro-Wilk test of Normality: a significant P-value (<0.05) implies the data deviate significantly from a normal distribution.
shapiro.test(x)

## Central Limit Theorem: tells us that the distribution of a *linear combination of variables* approaches normality, regardless of the underlying distribution of those variables.
## i.e., the distribution of **the sample mean** will be normal, regardless of the population distribution, for a large enough sample size (usually >=30).

x.m <- sapply(1:1000, function (i) mean(rlnorm(n)) )

truehist(x.m, prob = T, col = "lightgrey")
lines(density(x.m), col = "#333399", lwd = 2)

shapiro.test(x.m)
