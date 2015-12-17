## Simulating a Binomial Process (success / failure)
## Jonathan Whiteley    R v2.12     2011-10-16
library(MASS)                          # truehist is WAY better than hist
## library(mgcv)                          # smoother graphs

P1      <- 0.05
Pfail   <- 1 - P1
Nobs    <- 10000
Ntrials <- 20

op <- par(mfrow=c(2,2))                # prepare plotting area with 2x2 grid

## Simulations
Obs <- rbinom(Nobs, Ntrials, P1)

## Expected # successes
Exp1.T <- P1 * Ntrials                 # Theoretical (true) value.
Exp1 <- mean(Obs)

## number, p of observation of all failures
i0   <- which(Obs == 0)
num0 <- length(i0)
p0   <- num0/Nobs

## theoretical distribution
Xdist <- seq(0, Ntrials, by=1)         # need integers (binomial distribution)
Ydist <- exp(dbinom(Xdist, Ntrials, P1, log=TRUE))
p0.T  <- Ydist[1]                      # from binomial distribution
## p1.T  <- sum(Ydist[-1])             # sum of all expected # successes, except 0
p1.T  <- 1 - p0.T                      # the easy way (equal to 15 decimal places)

if (FALSE) {
  hist(Obs, freq=FALSE, main="distribution of successes")
  lines( Xdist, Ydist, col="grey", lwd=2 )
}

truehist(Obs, col="grey70", h=1, prob=TRUE,
         xlab="# successes",
         ylab="relative frequency (probability)",
         main="distribution of # successes")
lines( Xdist, Ydist, col="grey30", lwd=2 )


## Number of consecutive failures before a success
i1    <- which(Obs > 0)
num1  <- length(i1)
num1a <- length(Obs[-i0])
p1    <- num1/Nobs                     # prob. of at least one success
i1s   <- c(0, i1)                      # include initial index (0) for later calculations
Nto1  <- c()
for(j in 2:length(i1s)) {
  this1 <- i1s[j]
  prev1 <- i1s[j-1]
  Nto1[j-1] <- this1 - prev1 -1        # number of failures before next success
}
Nto1E <- mean(Nto1)
# Theoretical solution?

## hist(Nto1, breaks=max(Nto1)+1, main="Number of failures before success")
truehist(Nto1, col="grey70", prob=TRUE, h=1,
         xlab="Number of consecutive failures",
         ylab="relative frequency (probability)",
         main="Number of failures before success"
         )


## Probability of at least one success, as a function of the number of trials.
NTrialsE1.T <- 1 / P1        # # trials needed to expect 1 success
P1range     <- c(0.05, 0.07, 0.08, 0.10)
MaxTrialsE1 <- 1 / min(P1range)
MaxTrials   <- max(10, Ntrials *2, NTrialsE1.T+1 ) # 100%, 150%, or 200%?
MinTrials   <- 1

NumTrials <- 0:MaxTrials
Pgt1      <- 1 - dbinom(0, NumTrials, P1) # theoretical solution
Expected  <- P1 * NumTrials # Expected # success for given # trials
TrialsDF  <- data.frame(Ntrials=NumTrials, Pgt1=Pgt1, Expected=Expected)

if (FALSE) {                           # simulations: overkill
  ## could make it iterative, to keep searching until solutions are found for all desired metrics?
  ## Probably seriously overkill, I'm just too lazy to figure out the closed-form solution.
  TrialsDF    <- data.frame(Ntrials=0, Pgt1=0, Expected=0) # can't win if you don't play ;)
  for (N in MinTrials:MaxTrials) {
    ObsN  <- rbinom(Nobs, N, P1)
    i1N   <- which(ObsN > 0)
    num1N <- length(i1N)
    p1N   <- num1N/Nobs                  # prob. of at least one success
    for (r in 1:10) {                    # did not expect 10x to be so fast!
      ObsN  <- rbinom(Nobs, N, P1)
      i1N   <- which(ObsN > 0)
      num1N <- length(i1N)
      p1N   <- c(p1N, num1N/Nobs)        # larger sample size for a smoother curve?
    }    
    TrialsDF$Ntrials[N+1] <- c(N, mean(p1N), 0)
  }
}


## Plot of probability of ≥1 success for # trials
plot(TrialsDF$Ntrials, TrialsDF$Pgt1, type="l", lwd=2,
     ylim=c(0,1),
     xlab="Number of trials",
     ylab="Probability of at least 1 success",
     main=""
     )
abline(h=1, lty=3)                     # reference line P = 100%
mtext(paste(" P(success)", "=", P1), side=3, line=0, adj=0)
## add a smoother
if (FALSE) { ## Unnecessary with theoretical solutions
  modelP <- gam(Pgt1 ~ s(Ntrials), data = TrialsDF)
  TrialsDF$PredictedP <- predict(modelP)
  lines(TrialsDF$Ntrials, TrialsDF$PredictedP, col="darkred", lwd=1)
}

## Plot of Expected # successes for # trials
plot(TrialsDF$Ntrials, TrialsDF$Expected, 
     type="l", lwd=2,
     xlab="Number of trials",
     ylab="Expected # successes (on average)",
     main=""
     )
## abline(h=1, lty=3)                     # reference line Expected = 1
mtext(paste(" P(success)", "=", P1), side=3, line=0, adj=0)

## smoothers allow more acurate predictions, too
TrialsP50  <- which(TrialsDF$Pgt1 > 0.5)
NTrialsP50 <- TrialsDF$Ntrials[TrialsP50[1]]
TrialsP80  <- which(TrialsDF$Pgt1 > 0.8)
NTrialsP80 <- TrialsDF$Ntrials[TrialsP80[1]]
NTrialsE1  <- ceiling(NTrialsE1.T)     # practical solution.
if (is.na(NTrialsP80)) NTrialsP80 <- paste(">", MaxTrials, sep="")
if (is.na(NTrialsP50)) NTrialsP50 <- paste(">", MaxTrials, sep="")
if (is.na(NTrialsE1) ) NTrialsE1  <- paste(">", MaxTrials, sep="")

Etrials <- P1 * NTrialsE1
## reference line Expected = 1
lines(x=  c(0, NTrialsE1)   , y=rep(1, 2) , lty=3)
## reference line for # trials where Expected ≥ 1
lines(x=rep(NTrialsE1, 2)   , y=  c(0, Etrials) , lty=3)
## reference line for # trials where Expected ~ 1
lines(x=rep(NTrialsE1-1, 2) , y=  c(0, 1) , lty=3, col="grey40")
Exp1Label <- paste(NTrialsE1, "trials", "have", "≥1 avg. success")
text(x=0, y=1, labels=Exp1Label, adj=c(0, -0.2), cex=0.8)

## ENtrials <- TrialsDF$Expected[TrialsDF$Ntrials==Ntrials]
ENtrials <- Exp1.T                     # Expected # successes for specified # trials
## reference line Expected = 1
lines(x=  c(0, Ntrials)   , y=rep(ENtrials, 2) , lty=3)
## reference line for # trials where Expected ≥ 1
lines(x=rep(Ntrials, 2)   , y=  c(0, ENtrials) , lty=3)
ExpLabel <- paste(Ntrials, "trials", "=", format(ENtrials, digits=2), "avg. successes")
text(x=0, y=ENtrials, labels=ExpLabel, adj=c(0, 1.25), cex=0.8)


## Expected successes, P(≥1 success), as a function of # trials AND P(success)
PTrials <- expand.grid(NTrials=NumTrials, P1=P1range)
PTrials <- within(PTrials, {
                  Pgt1     <- 1 - dbinom(0, NTrials, P1) # theoretical solution
                  Expected <- P1 * NTrials # Expected # success for given # trials
})
PTrialsE1  <- ceiling(1 / P1range)
PTrialsP50 <- aggregate(PTrials$Pgt1, by=list(P1=PTrials$P1), function(x) min(which(x > 0.5)) )
names(PTrialsP50) <- c("P1", "Ntrials")
PTrialsP80 <- aggregate(PTrials$Pgt1, by=list(P1=PTrials$P1), function(x) min(which(x > 0.8)) ) 
names(PTrialsP80) <- c("P1", "Ntrials")
PTrialsP0   <- dbinom(0, PTrialsE1, P1range) 
PTrialsPgt1 <- 1 - PTrialsP0



## Numerical output
numdig  <- 2
intpad  <- paste(rep(" ", numdig), collapse="")
results <- c("# observations", paste(Nobs, intpad),
             "# trials / observation", paste(Ntrials, intpad),
             "P(success) for each trial", format(P1, digits=numdig),
             "Expected # successes", format(Exp1.T, digits=numdig),
             "Average # failures before success *", format(Nto1E, digits=numdig),
             "P( ≥1 success )", format(p1.T, digits=numdig),
             "P(all failures)", format(p0.T, digits=numdig),
             "# Trials with 1 expected success", paste(NTrialsE1, intpad),
             "# Trials with 50% chance of ≥1 success", paste(NTrialsP50, intpad),
             "# Trials with 80% chance of ≥1 success", paste(NTrialsP80, intpad)
###          "# Trials: P(≥1 success) ≥50%", paste(NTrialsP50, intpad),
###          "# Trials: P(≥1 success) ≥80%", paste(NTrialsP80, intpad)
             )

results.ncol <- length(P1range)+1
results.tab <- c("P(success) for each trial", format(P1range, digits=numdig),
                 "# Trials with 1 expected success",       PTrialsE1,
                 "# Trials with 50% chance of ≥1 success", PTrialsP50$Ntrials,
                 "# Trials with 80% chance of ≥1 success", PTrialsP80$Ntrials,
                 "P( ≥1 success )", format(PTrialsPgt1, digits=numdig),
                 "P(all failures)", format(PTrialsP0  , digits=numdig)
                 )
results.table <- array(results.tab, dim = c(results.ncol, length(results.tab)/results.ncol))
results.table <- t(results.table)      # transpose
results.table[, -1] <- format(results.table[, -1], digits=numdig, justify="right")

## format results for pretty output
results.out <- matrix(results, length(results)/2, 2, byrow=TRUE)
results.out[, 1] <- format(results.out[, 1], digits=NULL, justify="left")
iDec <- 3:7
results.out[iDec, 2] <- format(as.numeric(results.out[iDec, 2]), digits=numdig, justify="right")
results.out[, 2] <- format(results.out[, 2], digits=NULL, justify="right")
## use as.numeric() to convert numerical columns, if you want a consistent number of digits.
## is there a way to automatically align numbers along decimal, but not print the 0 decimals for integers?  
## I faked it above by padding them with the same number of spaces 
## (paste then adds an extra space to correspond with the decimal character)

## drop dimnames / indices
prettyDim <- " "
results.out <- rbind(results.out, rep("", ncol(results.out)))
dimnames(results.out) <- list(rep(prettyDim, dim(results.out)[1]), 
                              rep(prettyDim, dim(results.out)[2]))

results.table <- rbind(results.table, rep("", ncol(results.table)))
dimnames(results.table) <- list(rep(prettyDim, dim(results.table)[1]), 
                              rep(prettyDim, dim(results.table)[2]))

## output results
print(results.out  , print.gap=2, quote=FALSE)
print(results.table, print.gap=2, quote=FALSE)
par(op)                                # restore plotting parameters
