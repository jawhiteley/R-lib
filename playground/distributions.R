#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Playing with distributions from the 
# Central Limit Theorem
# v 0
# Jonathan Whiteley
# 2010-09-23	R.version 2.11.1
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
rm(list=ls())	# clear memory

## INITIALIZE values
num_vars <- 10
num_samples <- 100
values <- 1:10
var_data	 <- data.frame(random1=1:num_samples)
var_data_2	 <- data.frame(random1=1:num_samples)
diff <- 0.4	# constant to be added to second data frame (0.4 will induce different t-test results for the combinations of variables)

## Generate data frame of random variates
# could also use runif() instead (Random UNIForm distribution)
for( i in 1:num_vars ) {
	var_data <- within( var_data ,
		assign( 
			paste( "random", i , sep="" ) , 
			sample( values , num_samples , replace=TRUE )	#	floor(runif( num_samples , min=min(values) , max=max(values)+1 ) )
		)
	)
	var_data_2 <- within( var_data_2 ,
		assign( 
			paste( "random", i , sep="" ) , 
			floor(runif( num_samples , min=min(values)+diff , max=max(values)+1+diff ) )
		)
	)
}

# Adding random variates produces a Normal distribution
var_sum		 <- apply( var_data , 1 , 'sum' ) 	
# Multiplying random variates produces a LOG-Normal distribution
var_prod	 <- apply( var_data , 1 , 'prod' )	
# Recover normal distribution by taking log of products.
var_ln_prod	 <- log( var_prod , base=exp(1) )	
# same thing, different base.
var_log_prod <- log( var_prod , base=10 )		

# Same operations on second distribution (with difference)
var_sum2	 <- apply( var_data_2 , 1 , 'sum' ) 
var_prod2	 <- apply( var_data_2 , 1 , 'prod' ) 
var_ln2	 	 <- log( var_prod2 , base=exp(1) )
var_log2 	 <- log( var_prod2 , base=10 )

# change graphing parameters, and store original settings
op <- par( mfrow=c(2,2))

hist(var_sum)
hist(var_prod)
hist(var_ln_prod)
hist(var_log_prod)

# reset graphing parameters to original settings
par(op)

# Shapiro-Wilk test for normality
shapiro.test(var_sum)
shapiro.test(var_prod)
shapiro.test(var_ln_prod)
shapiro.test(var_log_prod)

# t-test for differences between means
t.test(var_sum,var_sum2)
t.test(var_prod,var_prod2)
t.test(var_ln_prod,var_ln2)


## Reference plots
op <- par( mfrow=c(3,2) )

Nsamples <- 1000
normal.distribution <- rnorm(Nsamples, mean=10, sd=1)
log.transformed.normal <- log(normal.distribution, base=10)
log.normal.distribution <- rlnorm(Nsamples, meanlog=1, sdlog=1)
tansformed.log.normal <- log(log.normal.distribution)
poisson.distribution <- rpois(Nsamples, lambda=4)
log.poisson <- log(poisson.distribution +1, base=10)
hist(normal.distribution)
shapiro.test(normal.distribution)
hist(log.transformed.normal)
shapiro.test(log.transformed.normal)
hist(log.normal.distribution)
shapiro.test(log.normal.distribution)
hist(tansformed.log.normal)
shapiro.test(tansformed.log.normal)
hist(poisson.distribution)
shapiro.test(poisson.distribution)
hist(log.poisson)
shapiro.test(log.poisson)

par(op)
