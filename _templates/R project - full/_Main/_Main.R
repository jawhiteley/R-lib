# Main control Script

## INITIALIZE
rm(list=ls())  # clear memory
#source("./lib/load.R")  # (re-)load data
#source("./lib/init.R")  # Initialize - all analysis scripts should start with this.

## GO
source("./do.R")    # Perform Analyses
source("./lib/out.R")   # Produce Outputs: graphs, reports, export.
