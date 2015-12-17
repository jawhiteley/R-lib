################################################################
### Project Title
### Initialization script
### Start every analysis script for this project by running this one: source("lib/init.R")
### Jonathan Whiteley		R v3.2.2 		2015-12-16
################################################################
cat("Project Name ...")

## Clear working memory
rm(list=ls())

##==============================================================
## Working Directory: where are we?

## Set the working directory to a working script file with \rd in vim
## in RStudio: Session > Set Working Directory > To Source File Location
##   or open the Rstudio project file.

BASE_DIR  <- "~/Project-Name/"      # base working directory
## Get R to tell me where this file is (by giving me the location on an anonymous function that is defined here).
## This ONLY works when the file is source()d - you have to know where to source it from first!
lib_dir   <- getSrcDirectory(function(x) {x}) # base working directory
BASE_DIR  <- gsub("/[^/]*$", "", BASE_DIR)   # strip this dir
## Set the working directory
if (getwd() != BASE_DIR) setwd(BASE_DIR)

## Load custom functions
source("lib/fun.R")

## Common parameters
DATA_DIR  <- "data/" # all data is in here, usually in sub-folders (including processed output from R)
OUT_DIR   <- ""      # destination for output
GRAPH_DIR <- ""      # destination for graphs, and related data

## Load other project-specific goodies

cat(" project initialized.\n")
