# Courtesy of Etienne Low-Décarie
## BEFORE installing a new version of R,
## Run the following in the old version:
# setwd( getwd() )	# check & set working directory
package.list<-rownames(installed.packages())
save(package.list, file="package_list.R")

## After installing a new version of R, run the following:
load("package_list.R")
install.packages(package.list)

