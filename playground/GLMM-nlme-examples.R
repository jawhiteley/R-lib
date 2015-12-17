##################################################
### Syntax examples of GLMMs in R (package nlme)
### GLMM (regression)
### Jonathan Whiteley     R v2.12     2011-10-14
##################################################
## Based mostly by studying R code for Chapter 8 of "Analyzing Ecological Data"
## by Alain F. Zuur et al. (2007)
## <http://www.highstat.com/book1.htm>

library(nlme)
library(car)                           # Companion to Applied Regression

data(Muscle)                           # load some data to play with
dat <- Muscle
dat <- within(dat, {
              X    <- log(conc)
              Y    <- length
              rfac <- Strip
})

## Simple Model
m1 <- lm(Y ~ X, data = dat)
opar <- par(mfrow=c(2,2))
plot(m1)                               # check residuals
par(opar)
plot(dat$X, dat$Y, main = "Simple Linear Model")
abline(m1)
summary(m1)
anova(m1)

## should use models fit with method="ML" when comparing models 
##  with same Random structure, but different Fixed structures
## should use method="REML" to compare models 
##  with same Fixed structure, but different Random structures

## Model with Random intercept, Fixed slope (similar to ANCOVA)
RiFs <- lme(Y ~ X, data = dat, random = ~ 1 | rfac) # "1" refers to the intercept
plot(RiFs)                             # check residuals
summary(RiFs)
anova(RiFs)

## Model with Fixed  intercept, Random slope (not sure why one would do this...)
FiRs <- lme(Y ~ X, data = dat, random = ~ -1 + X | rfac) # "-1" means "drop intercept"
plot(FiRs)                             # check residuals
summary(FiRs)
anova(FiRs)
anova(RiFs, FiRs)

## Model with Random intercept & slope
RiRs <- lme(Y ~ X, data = dat, random = ~ 1 + X | rfac)  # The "1 +" in the random term is optional
summary(RiRs)
anova(RiRs)

plot(RiRs)                             # check residuals
## ordinary residuals: apparent heterogeneity
plot(fitted(RiRs), resid(RiRs), col="blue1")
abline(h=0)
## standardised (normalized) residuals: heterogeneity still apparent (not corrected in this model)
plot(fitted(RiRs), resid(RiRs, type="pearson"), col="blue1")
abline(h=0)

## Normality (different residual types)
qqnorm(resid(RiRs))                    # 'ordinary residuals'
qqnorm(resid(RiRs, type="p"))          # 'standardised residuals' = resid / sd
qqnorm(resid(RiRs, type="normalized")) # same as type="p" in this case ?residuals.lme
## qqnorm(rstandard(RiRs))                # no method for "lme"
## qqnorm(rstudent(RiRs))                 # no method for "lme"



RiRs2 <- lme(Y ~ X, data = dat, random = ~ X | rfac)  # "1 +" in the random term is optional
summary(RiRs2)
anova(RiRs2)

anova(RiRs, RiRs2)
all.equal(RiRs, RiRs2)                 # the only differences are the formulae
anova(RiFs, RiRs)


## Model with Random intercept, slope, and error terms
lmc <- lmeControl(niterEM = 5000, msMaxIter = 1000) # takes a while to converge...
Rise <- lme(Y ~ X, data = dat, 
            random = ~ X | rfac,
            weights = varIdent(form = ~ 1|rfac),
            control = lmc
            )
## ordinary residuals: apparent heterogeneity
plot(fitted(Rise), resid(Rise), col="blue1")
abline(h=0)
## standardised (normalized) residuals: show no heterogeneity (corrected in this model)
plot(fitted(Rise), resid(Rise, type="normalized"), col="blue1")
abline(h=0)
plot(fitted(Rise), resid(Rise, type="p"), col="blue1") # "pearson" (standardised)
abline(h=0)
## studentised residuals?  Apparently not.  Error: should be one of "response", "pearson", "normalized"
## plot(fitted(Rise), studres(Rise), col="blue1") # studres() from MASS library: still no luck
## abline(h=0)
## standardised residuals: default method
plot(Rise)                             # check residuals

summary(Rise)
anova(Rise)
anova(RiRs, Rise)

## GLMM is a better fit than simple linear model (assuming no assumptions violated), 
## even though the fitted values (plot of predicted values) look similar

AIC(m1, RiFs, FiRs, RiRs, Rise)
## The Random Intercept & Slope model has a lower AIC,
## BUT the residuals show signs of heterogeneity (see above), 
##  and the residuals from the model with random error terms also show better fit to Normality.
StdRes <- resid(RiRs, type="p")
qqPlot(StdRes)                           # library(car)
## histogram with reference normal distribution <http://www.statmethods.net/stats/rdiagnostics.html>
Xnorm <- seq(min(StdRes), max(StdRes), length=40)
hist(StdRes, freq=FALSE)
lines(Xnorm, dnorm(Xnorm), col="grey70", lwd=2)

StdRes <- resid(Rise, type="p")
qqPlot(StdRes)                           # library(car)
## histogram with reference normal distribution <http://www.statmethods.net/stats/rdiagnostics.html>
Xnorm <- seq(min(StdRes), max(StdRes), length=40)
hist(StdRes, freq=FALSE)
lines(Xnorm, dnorm(Xnorm), col="grey70", lwd=2)


## plot best model
anova(Rise, m1)                        
lvls <- length(levels(dat$rfac))
Fit1 <- fitted(RiRs, level = 1)
plot(dat$X, dat$Y, col = rainbow(lvls)[dat$rfac], main = "Best GLMM Model")
if (FALSE) {                           # Cheating
  for (i in 1:lvls) abline(coef(RiRs)[i, 1], 
                         coef(RiRs)[i, 2], 
                         lty = 3, col = rainbow(lvls)[i]
                         )
} else {                               # Proper way: lines only span X values in each group
  for (i in 1:lvls) {
    x1 <- dat$X[ dat$rfac == levels(dat$rfac)[i] ]
    y1 <- Fit1[ dat$rfac == levels(dat$rfac)[i] ]
    K  <- order(x1)
    lines(sort(x1), y1[K],
          lty = 3, col = rainbow(lvls)[i]
    )
  }
}  
abline(RiRs$coefficients$fixed, lwd=3)
