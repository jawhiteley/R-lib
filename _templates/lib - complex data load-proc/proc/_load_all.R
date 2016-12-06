################################################################
### PROJECT NAME
### Load and process raw data, then export for analysis
### Jonathan Whiteley		R v3.3.2 		2016-12-05
################################################################

reload_all_data <- function (raw.download=TRUE)
{   # reload, and process, all data for the project
## INITIALIZE
if (exists(BASE_DIR)) { ## check if project initialized already
  source("/lib/init.R")
} else {
  ## set the working directory to the directory of this file: \rd in Vim
  source("../init.R")
}

if (raw.download) {
  source("/lib/proc/0_get.R")
}
source("/lib/proc/1_load.R")
source("/lib/proc/2_clean.R")   # source() check and calc scripts, as needed.
source("/lib/proc/3_save.R")

}

if (FALSE) {                           # Do NOT run when source()d
  reload_all_data()
}

