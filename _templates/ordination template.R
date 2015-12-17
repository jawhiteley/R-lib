##################################################
# Ordination template
# Description
# Name	Date	R v 2.11
##################################################
## INITIALISE & HOUSEKEEPING
##################################################
rm(list=ls())	# clear memory
# setwd(getwd())	# Set Working Directory: replace getwd() with a path in quotes "".
# install.packages('vegan')
library(vegan)	# load external package
# help(package = vegan)	#library(help = vegan)	# get more info about a package, such as who wrote it, what functions are included, and what they do.

##################################################
## LOAD DATA
##################################################
# data(package='vegan')	# available datasets including in vegan
dataset <- read.csv( file.choose() , na.string="." )	# or read.table() for a text file with more generic options
	# specify mising values with na.string=""
	# file.choose() opens a file browser pop-up window (on Macs)

#_________________________________________________
## Process Data

# select subsets of full data frame, and reshape if necessary.
# switch between 'long' & 'wide' formats
data.wide <- reshape( dataset[,], direction="wide" )
	# 'wide' data has each variable in a separate column and each row represents a sample object. (n samples x p variables)
	# 'long' data has each combination of sample object and variable as a separate row, with the value as a single column.	
#_________________________________________________
## Check Data
str(dataset)		# check structure: are the appropriate variables factors, numeric, etc.?  Any unexpected or missing columns?
invisible(edit(dataset))	# Peek at the data: opens spreadsheet and returns changes invisibly.    
# Use fix() to make permanent changes, or assign the result of edit() to an object.
summary(dataset)	# summary statistics


##################################################
## TRANFORMATIONS
##################################################
boxplot( data.wide )	# do variables have approximately equal ranges?  Is this a problem for your analysis?

##==================================================
## Standardization - entries are transformed relative to other entries.  
# The need for standardization will depend on your choice of distance metric and ordination method.
data.norm <- decostand(data.wide, method="standardize")	# normalize all columns to zero mean and unit variance.  See ?decostand for other options
boxplot(data.norm)


##==================================================
## Transformation : a function is applied to each entry, independent of other entries
data.trans <- sqrt(data.wide)	# or your preferred transformation function, applied elementwise to all values in the data frame.
# This data frame object will be used for the rest of the script, so if you want to see the effect of a different transformation, you only have to change this line and re-run the rest.
boxplot(data.trans)


##################################################
## SIMILARITY MATRIX
##################################################
# Unless you are doing a PCA, which has this step "built-in".
data.dist <- vegdist(data.trans, method="bray")	# More ecologically-relevant distance metrics included in the vegan package.
# ?vegdist for more options
# data.dist <- dist(data.trans)	# base distance metrics included in R, defaults to euclidean distance.


##################################################
## ORDINATION ANALYSIS
##################################################
# Unconstrained:	samples x response variables
# Constrained:  	samples x explanatory x response variables (typically a separate data frame for explanatory & response variables)
data.ordi <- metaMDS( data.dist, autotransform=FALSE )

##################################################
## ORDINATION PLOTS
##################################################
ordiplot( data.ordi, type='t')	# identify all points by their row number / name


##################################################
## HYPOTHESIS-TESTING
##################################################

#_________________________________________________
## ANOSIM
data.anosim <- anosim( data.dist, group=group.var )	# specify a vector of a priori groups for each row.
# Get Output
data.anosim


##################################################
## FINAL GRAPHICS for publication
##################################################

