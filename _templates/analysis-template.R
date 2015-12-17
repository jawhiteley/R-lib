##################################################
# R analysis script template
# Description
# Jonathan Whiteley		2011-MM-DD		R v2.12
##################################################
## INITIALISE
##################################################
rm(list=ls())	# clear memory
# I usually just use a default working directory on each computer, but this can speed up the process, for projects with files in a different directory:
# setwd( getwd() )	# Set Working Directory: replace getwd() with a path in quotes "".
getwd()	# check that we're in the right place.
library(car)	# load external package 'car', for recode()
# help(package = car)	#library(help = car)	# get more info about a package, such as who wrote it, what functions are included, and what they do.

##################################################
## LOAD DATA
##################################################
dataset <- read.csv( "dataset.csv" , na.string="." )	# or read.table() for a text file 
	# specify mising values with na.string=""
	# file.choose() opens a file browser pop-up window (on Macs)

##################################################
## PROCESS DATA: planned?
##################################################
## Generate factor columns, re-order factor levels, etc.
dataset <- within( dataset, {	## instead of attach, this places focus within the indicated data frame / object, and returns object with changes applied (in reverse order...)
	levels(Factor) <- list( "level 1"=1, "level 2"=2 )	# rename (and set order of) factor levels using a named list('New'='old') of value pairs.  Unspecified values converted to 'NA'
	Factor <- factor( Factor, levels=c( 'level 1', 'level 2' ) )	# safely reorder factor levels (previous command does this already in one step).  See reorder() for fancy sorting of factor levels by summary stats or other values.
	## rename and reorder factor levels the easy way - maintains empty values if empty factor specified, otherwise converts to 'NA'.  Requires package 'car'
	Factor <- recode( Factor, 
		"'1'='level 1'; '2'='level 2'; else=''", 
		as.factor.result=TRUE, 
		levels=c( "level 1", "level 2", "" ) 
	)
})
#=================================================
# PROCESS DATA: unplanned?
# Transformations, calculated columns, etc.  May depend on the results of exploratory graphs (& assumptions), below.
dataset <- within( dataset, {
	Y.ln  <- log( Y +1 )	# defaults to base e=exp(1).
	Y.log <- log( Y +1 , 10 )	# base 10.
})

##################################################
## CHECK DATA
##################################################
head(dataset)		# have a peek at the first 6 rows & columns: is this what you expected?
str(dataset)		# check structure: are the appropriate variables factors, numeric, etc.?
summary(dataset)	# summary statistics


##################################################
## EXPLORE: PLOTS
##################################################
## make some meaningful plots of data to check for predicted (expected) patterns.
plot(dataset)		# all bivariate plots (might want to specify which columns to compare to avoid an overwhelming graph
## Is the response variable normally-distributed? Check skew in histograms, linearity on QQ-plots
par( mfrow=c(2,2), cex=0.8)	# panel of figures: 2 rows & 2 columns
with( dataset, {
	hist( Y )
	hist( Y.log )
	qqnorm( Y, main="untransformed" )
	qqline( Y )
	qqnorm( Y.log, main="log-10 transformed" )
	qqline( Y.log )
})


##################################################
## DEFINE MODEL FORMULA
##################################################
Y.mf <- with( dataset, Y ~ Factor )	# What Is Your Question? 

##################################################
## ANALYSIS: design
##################################################
Y.model <- 	# Which function to use for desired analysis?

##################################################
## CHECK ASSUMPTIONS: analyse residuals, standard diagnostic plots
##################################################
par( mfrow=c(3,2) )	# panel of figures: 3 rows & 2 columns
plot(Y.model)
hist(resid(Y.model))	# plot residuals

##################################################
## ANALYSIS: GET RESULTS
##################################################
names(Y.model)
anova(Y.model)		# anova table (may or may not be informative)
summary(Y.model)		# summary statistics


##################################################
## FINAL GRAPHICS
##################################################
graph.legend <- data.frame( label=Factor, pch=1:length(Factor), col=1:length(Factor) )	# map point characters & colors to treatment levels or groups
layout( matrix(1:n, rows, cols), respect=TRUE )	# layout of plots within graphics frame
