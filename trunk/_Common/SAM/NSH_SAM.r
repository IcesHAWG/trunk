######################################################################################################
# NSH_SAM Assessment
#
# $Rev$
# $Date$
#
# Author: HAWG model devlopment group
#
# Acts as a basic wrapper around the SAM framework for use in the assessment of NSAS herring
# Most of the interfacing is taken care of by the FLR packages
#
# Developed with:
#   - R version 2.8.1
#   - FLCore 2.2
#   - FLAssess, version 1.99-102
#
# To be done:
#
# Notes: Have fun running this assessment!
#
####################################################################################################

### ======================================================================================================
### Initialise system, including convenience functions and title display
### ======================================================================================================
rm(list=ls()); gc(); graphics.off(); start.time <- proc.time()[3]
options(stringsAsFactors=FALSE)
FnPrint     <-  function(string) {
	cat(string)
}
FnPrint("\nNSH SAM Assessment Wrapper\n=====================\n")

### ======================================================================================================
### Import externals
### ======================================================================================================
library(FLCore)
#Load NSH assessessment objects
load(file.path("..","..","NSAS","results","NSH Assessment Assessment.RData"))

#FLSAM package (uncompiled)
FLSAM.dir <- file.path("..","Pkgs","FLSAM")
FLSAM.r.srcs <- dir(file.path(FLSAM.dir,"R"),pattern="r$|R$",full.names=TRUE)
dmp <- lapply(FLSAM.r.srcs,source)

### ======================================================================================================
### Save files to ADMB
### ======================================================================================================
stck <- NSH
tun <- NSH.tun

write.ADMB.dat(NSH,NSH.tun,file.path(".","run","ssass.dat"))

