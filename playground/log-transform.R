################################################################
### exploring log-transformations
### Jonathan Whiteley		R v3.2.2 		2016-02-25
################################################################
### INITIALIZE
rm(list=ls())	# clear memory

### Why base e (natural logarithm / ln; log()) instead of base 10 (log; log10())?
## e = Euler's number (aka "Napier's Constant") = limit of (1 + 1/n)^n
## i.e., the limit ratio of 100% compound interest applied over n intervals.
## It comes from the study of compound interest: i.e., cumulative small *proportional changes*.
## https://en.wikipedia.org/wiki/E_(mathematical_constant)

n <- 10^(seq(0, 5, by=0.5))
(1 + (1/n))^n                          # 100% change divided over n intervals
exp(1)
print(exp(1), digits=7)

## Interest rate (prop. change) other than 1 **
P <- 0.1                               # 10% change
(1 + (P/n))^n
exp(P)                                 # e^P == limit (1 + (P/n))^n for large n

## Binomial distribution
## Playing a game n times with a probability of winning = 1/n
(1 - (1/n))^n                           # the probability of winning 0 times
1/exp(1)

m <- 1:100
plot(m, (1 - (1/m))^m, type="l")
lines(m, rep(1/exp(1), times=length(m)), lty=2)

## Standard Normal Distribution **
## probability density function
x <- seq(-4, 4, by=0.1)
normx <- ((1/sqrt(2 * pi)) * exp(-(1/2) * (x^2)))

plot(x, normx, type="l")


## Exponential Growth
t <- 1:100
X0 <- 1
X <- X0 * (1 + P)^t
k <- log(1 + P)

op <- par()
par(mfrow=c(2, 2))
plot(t, X0 * (1 + P)^t, type="l")
plot(t, X0 * exp(log(1 + P) * t), type="l")  # derived from differential equation of exponential growth.
plot(t, log(X), type="l")
plot(t, log(X0) + (t * log(1 + P)), type="l")
par(op)

## log-linear regression: 
##   log(X) = log(X0) + t * log(1 + P)
##   log(y) = log(y0) + x * log(1 + P)
##   if log(y) = B0 + B1 * x, then
##   B1 = log(1 + P), therefore
##   P = exp(B1) -1


### Proportional change: easily interpreting coefficients in linear regression models with a log-transformed resposne variable
## for *small values of x*, exp(x) is roughly equal to 1+x
## conversely, log(1+x) is roughly equal to x
p <- 0.01
exp(p)
(1+p)
log(1+p)                               # usually a little smaller
round(exp(p) - (1+p), 3)
exp(p) == (1+p)                        # but not exactly

## This relationship is not true for base-10 ***
10^p
(1+p)
log10(1+p)
round(10^(p) - (1+p), 3)

## the strict equality:
p == exp(log(1+p)) -1
p == round( exp(log(1+p)) -1, 16)       # yes, there are rounding errors in R,
p == round( exp(log(1+p)) -1, 17)       # but they are small ;)

## log() is preferred over log10(), because of the prevalence of base e in interest, and proportional changes in general.
## I guess that's why it's the default base in R.
## The base itself doesn't really matter to model-fitting, but it just makes certain interpretations and conversions of coefficients simpler.
## http://stats.stackexchange.com/questions/27682/what-is-the-reason-why-we-use-natural-logarithm-ln-rather-than-log-to-base-10

## Therefore, in a linear regression where response variables are log-transformed (or ln-transformed, more precisely), small coefficients can be interpreted as proportional changes in the response, for a 1-unit change in the explanatory variable.
## In a regression where both response and explanatory variables are log (ln) transformed, the coefficients can be interpreted as: exp(coef * log(1.01)) = the proportional change in response for a 1% change in the explanatory variable.

## www.kenbenoit.net/courses/ME104/logmodels2.pdf
## https://www.cscu.cornell.edu/news/statnews/stnews83.pdf




op <- par()
par(mfrow=c(2, 2))
plot(1:100, exp(1:100), type="l")
plot(1:100, 10^(1:100), type="l")
plot(1:100, 1/exp(1:100), type="l")
plot(1:100, 1/(10^(1:100)), type="l")
plot(1:100, log(1:100), type="l")
plot(1:100, log10(1:100), type="l")
plot(1:100, 1/log(1:100), type="l")
plot(1:100, 1/log10(1:100), type="l")
par(op)


y <- rlnorm(100, mean=0, sd=1)
## Arithmetic mean: Expected value in linear regression
mean(y)
sum(y)/length(y)
## Geometric mean: the Expected value in log-linear regression (log-transformed response) is mean(log(y)) == log(geomtric_mean(y))
prod(y)^(1 / length(y))
exp( mean(log(y)) )
## Harmonic mean: for a properly-weighted average of rates or ratios (price-earnings ratio, velocity over a trip)
1 / (mean(1/y))

## http://www.jerrydallal.com/lhsp/logs.htm
## Mathematicians like natural logs because they have properties that are not shared by other types of logarithms. For example, if you apply any logarithmic transformation to a set of data, the mean (average) of the logs is approximately equal to the log of the original mean, whatever type of logarithms you use. 
mean(log(y))
log(mean(y))                           # ~ 0
mean(log10(y))
log10(mean(y))                           # ~ 0
## However, only for natural logs is the measure of spread called the standard deviation (SD) approximately equal to the coefficient of variation (the ratio of the SD to the mean) in the original scale.
sd(log(y))
sd(y) / mean(y)
sd(log10(y))
