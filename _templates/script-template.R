################################################################################
# R generic script template
# Description
# Jonathan Whiteley                202X-MM-DD                R v4.x.x
################################################################################
## INITIALISE
####################################################
rm(list=ls())	# clear memory

## LOAD PACKAGES
library(tidyverse)
# get more info about a package, such as who wrote it, what functions are included, and what they do.
# help(package = tidyverse)	#library(help = tidyverse)



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

