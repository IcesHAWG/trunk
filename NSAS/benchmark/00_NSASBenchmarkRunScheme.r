######################################################################################################
# NSH Assessment
#
# Author: Niels Hintzen
# IMARES, The Netherlands
#
#
####################################################################################################

### ======================================================================================================
### Initialise system, including convenience functions and title display
### ======================================================================================================
rm(list=ls()); graphics.off(); start.time <- proc.time()[3]

path <- "N:/Projecten/ICES WG/Haring werkgroep HAWG/2012/assessment/NSAS/"
try(setwd(path))

#in need of something extra

options(stringsAsFactors=FALSE)
log.msg     <-  function(string) {
	cat(string)
}
log.msg("\nNSH FLICA Assessment\n=====================\n")

### ======================================================================================================
### Incorporate external sources
### Uses the common HAWG FLICA Assessment module to do the graphing, diagnostics and output, together with
### the common NSAS object setups
### ======================================================================================================
source(file.path("..","_Common","HAWG Common assessment module.r"))
source("Setup_objects.r")
