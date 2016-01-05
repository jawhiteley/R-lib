##################################################
# Miscellaneous useful R functions
# Jonathan A. Whiteley	2012-04-10
# R v2.12
##################################################
## INITIALISE
##################################################

clmem <- rmall <- function() {  rm(list=ls()) }


##################################################
## DATA
##################################################
peek <- function( data="" ) {
	if ( class(data)== "data.frame" )
		invisible( edit(data) )
	else
		data.entry(data=data)
}




