####################################################
## Install Packages from list
# Use when installing a new version of R
# to re-install all the packages you had in the previous version.
#*** 1) Run "Extract-Package-List.R" in the old version
#*** 2) Run this script in the new version of R
## Based on code and idea from Etienne Low-Décarie
## Jonathan Whiteley	2011-01-18
## R v2.12
####################################################

rm(list=ls())	# clear memory
# setwd( getwd() )	# check & set working directory

load("package_list.R")
install.packages(unique(package.list))
