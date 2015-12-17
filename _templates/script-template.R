####################################################
# R generic script template
# Description
# Jonathan Whiteley		2011-MM-DD		R v2.12
####################################################
## INITIALISE
####################################################
rm(list=ls())	# clear memory
# I usually just use a default working directory on each computer, but this can speed up the process, for projects with files in a different directory:
# setwd( file.choose() )	# Set Working Directory: 
	# replace file.choose() with a path in quotes "".
	# file.choose() opens a file browser: can't choose a folder per se, but maybe it won't mind if a file is specified?
getwd()	# check that we're in the right place.

## LOAD PACKAGES
library(car)	# load external package 'car', for recode()
# help(package = car)	#library(help = car)	# get more info about a package, such as who wrote it, what functions are included, and what they do.



####################################################
## LOAD DATA
####################################################
dataset <- read.csv( file.choose() , na.string="." )	# or read.table() for a text file with more generic options
	# specify mising values with na.string=""
	# file.choose() opens a file browser pop-up window (on Macs)

##==================================================
## PROCESS DATA


##==================================================
## CHECK DATA
str(dataset)		# check structure: are the appropriate variables factors, numeric, etc.?
# invisible(edit(dataset))	# opens spreadsheet and returns changes invisibly.  
# Use fix() to make permanent changes, or assign the result of edit() to an object.
head(dataset)		# have a peek at the first 6 rows & columns: is this what you expected?
summary(dataset)	# summary statistics



####################################################
## DATA EXPLORATION
####################################################




####################################################
## ANALYSIS
####################################################



####################################################
## OUTPUT
####################################################



##==================================================

##__________________________________________________

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

##--------------------------------------------------

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

