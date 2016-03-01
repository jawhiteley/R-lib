################################################################
### lm Sums of Squares (SS): Type I, II, III
### Jonathan Whiteley		R v3.2.2 		2016-03-01
################################################################
### INITIALIZE
rm(list=ls())	# clear memory

data()

## http://myowelt.blogspot.de/2008/05/obtaining-same-anova-results-in-r-as-in.html
## http://stats.stackexchange.com/questions/60362/choice-between-type-i-type-ii-or-type-iii-anova
## https://mcfromnz.wordpress.com/2011/03/02/anova-type-iiiiii-ss-explained/
## https://afni.nimh.nih.gov/sscc/gangc/SS.html
## Affects summary() output more than anova(), at least in this case.
op <- options()
options(contrasts=c(unordered='contr.treatment', ordered='contr.poly')) # R default
options(contrasts=c(unordered='contr.sum', ordered='contr.poly')) # more like SPSS?
options(op)

## Full model
options(op)
lm.default <- lm(model, data=data.)
## Type I SS: adding each term in sequence.  Order of terms matters!
anova(lm.default)
## Omitting each term, one at a time: is this Type II or Type III?
drop1(lm.default, test="F")   # slow

## Type II SS? SS for each term after all other terms, except related interactions (marginality).
options(contrasts=c('contr.treatment', 'contr.poly'))
lm.trt <- lm(model, data=data.)
drop1(lm.trt, test="F")       # only interaction appears, not related main effects.
## Some say drop1 produces Type II, but it is also used to demonstrate Type 3 :/
## - might depend on contrasts

## Type III SS? SS for each term if it were entered last into the model (even after related interactions)
options(contrasts=c('contr.sum', 'contr.poly'))
lm.sum <- lm(model, data=data.)
drop1(lm.sum, .~., test="F")  # main effects and interactions appear.

## anova()s are the same, regardless of contrasts
all.equal(anova(lm.trt), anova(lm.sum))
all.equal(anova(lm.default), anova(lm.trt))
all.equal(anova(lm.default), anova(lm.sum))

## Using 'car' package: I trust these more (does not match above, but I don't know why)
library(car)
options(contrasts=c('contr.sum', 'contr.poly'))
lm.car <- lm(model, data=data.)
anova(lm.car)
Anova(lm.car, type=2)               # ***
Anova(lm.car, type=3, singular.ok=TRUE)


