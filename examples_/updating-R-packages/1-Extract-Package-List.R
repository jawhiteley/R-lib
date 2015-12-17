####################################################
## Extract a list of installed packages
# Use BEFORE installing a new version of R
# to generate a list of all the packages installed in the previous version.
#*** 1) Run this script in the old version
#*** 2) Run "Install-Packages.R" in the new version of R
## Based on code and idea from Etienne Low-Décarie
## Jonathan Whiteley	2011-01-18
## R v2.12
####################################################

rm(list=ls())	# clear memory
# setwd( getwd() )	# check & set working directory

package.list<-rownames(installed.packages())
save(package.list, file="package_list.R")
