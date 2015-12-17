####################################################
# Fun with paired observations
# Jonathan Whiteley		2011-02-16		R v2.12
####################################################
## INITIALISE
####################################################
rm(list=ls())	# clear memory

## LOAD PACKAGES


####################################################
## LOAD DATA
####################################################
base <- 5
stdev <- 2
num_obs <- 100
diff <- 2

obs1 <- rnorm( num_obs, mean=base, sd=stdev )
obs2 <- obs1 + diff
# obs2 <- rnorm( num_obs, mean=base+diff, sd=stdev )


####################################################
## ANALYSIS
####################################################

mean1 <- mean(obs1)
mean2 <- mean(obs2)

####################################################
## OUTPUT
####################################################

cat("means differ by:", mean2, "-", mean1, "=", mean2-mean1)
cat("mean difference:", mean(obs2 - obs1))
