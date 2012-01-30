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
path <- "/media/n/Projecten/ICES WG/Haring werkgroep HAWG/2012/assessment/NSAS/"
try(setwd(path))

#in need of something extra

options(stringsAsFactors=FALSE)
log.msg     <-  function(string) {cat(string);}
log.msg("\nNSH FLICA Assessment\n=====================\n")

### ======================================================================================================
### Incorporate external sources
### Uses the common HAWG FLICA Assessment module to do the graphing, diagnostics and output, together with
### the common NSAS object setups
### ======================================================================================================
source(file.path("..","_Common","HAWG Common assessment module.r"))
source(file.path("benchmark","Setup_objects.r"))

### ======================================================================================================
### Define parameters for use in the assessment code here
### ======================================================================================================
output.dir          <-  file.path(".","results")                #Output directory
output.base         <-  file.path(output.dir,"NSH Assessment")  #Output base filename, including directory. Other output filenames are built by appending onto this one

### ======================================================================================================
### Three step approach to benchmark assessment
###   step 1): a) Run ICA for comparison purposes
###            b) Run SAM, find most appropriate setting
###            c) Compare ICA and SAM
###
###   step 2): a) Which surveys to include, LOOI
###            b) Natural mortality
###            c) Other data change?
###
###   step 3): a) Run SAM, find most appropriate setting
###            b) Run retrospective
### ======================================================================================================

