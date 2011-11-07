################################################################################
# Looi run
#
# $Rev$
# $Date$
#
# Author: HAWG model devlopment group
#
# Applies a systematic scan of leave-one-in, leave-one-out and various subsets
# there of.
#
# Developed with:
#   - R version 2.13.2
#   - FLCore 2.4
#
# To be done:
#
# Notes: Have fun running this assessment!
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
log.msg("\nLOOI run\n===========================\n")

### ============================================================================
### Import externals
### ============================================================================
log.msg("IMPORTING EXTERNAL RESOURCES...\n")
library(FLSAM)
source("Setup_objects.r")
source("Setup_default_FLSAM_control.r")

### ============================================================================
### Run the assessment
### ============================================================================
#Perform assessment
LOOI.sams <- looi(NSH,NSH.tun,NSH.ctrl,type="full")

### ============================================================================
### Analyse the results
### ============================================================================

### ============================================================================
### Save results
### ============================================================================
save(LOOI.sams,file=file.path(resdir,"LOOI.sams.RData"))
log.msg(paste("COMPLETE IN",sprintf("%0.1f",round(proc.time()[3]-start.time,1)),"s.\n\n"))
