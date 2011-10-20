################################################################################
# Development wrapper for SUSAM Diagnostics
#
# $Rev: 522 $
# $Date: 2011-10-03 10:19:53 +0200 (Mon, 03 Oct 2011) $
#
# Author: HAWG model devlopment group
#
# Development routine for the FLSAM diagnostics
#
# Developed with:
#   - R version 2.13.1
#   - FLCore 2.4
#
# To be done:
#
# Notes:
#
################################################################################

### ============================================================================
### Initialise system, including convenience functions and title display
### ============================================================================
rm(list=ls()); gc(); graphics.off(); start.time <- proc.time()[3]
options(stringsAsFactors=FALSE)
log.msg     <-  function(string) {
	cat(string);flush.console()
}
log.msg("\nSUSAM Development\n=================\n")

### ============================================================================
### Run code 
### ============================================================================
library(FLSAM)
data(NSH.sam)   #Load result
source("SUSAM_diagnostics.r")
par(ask=TRUE)

#Now run the code
residual.diagnostics(NSH.sam)
