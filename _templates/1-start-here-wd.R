## This script sets the working directory to the location of this file when source()d. A good place to start.

## Get R to tell me where this file is by giving me the location of an anonymous function that is defined here.
## This ONLY works when the file is source()d
## http://stackoverflow.com/a/30306616
setwd( normalizePath(getSrcDirectory(function(x) {x})) )

