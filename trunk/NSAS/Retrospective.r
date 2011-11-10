################################################################################
# NSH_SAM Retrospective
#
# $Rev$
# $Date$
#
# Author: HAWG model devlopment group
#
# Performs a retrospective analysis of the NSAS Herring stock using the SAM method
#
# Developed with:
#   - R version 2.13.1
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
log.msg("\nNSH SAM Retrospective Analysis\n==========================\n")

### ============================================================================
### Import externals
### ============================================================================
library(FLSAM)
source("Setup_objects.r")
source("Setup_default_FLSAM_control.r")

### ============================================================================
### Run the assessment
### ============================================================================
#Perform assessment
NSH.retros <- retro(NSH,NSH.tun,NSH.ctrl,retro=2)

### ============================================================================
### Save results
### ============================================================================
save(NSH,NSH.tun,NSH.retros,file=file.path(resdir,"Retrospective.RData"))

### ============================================================================
### Analysis
### ============================================================================
#Make FLStocks
NSH.stcks <- NSH + NSH.retros

pdf(file.path(resdir,"Retrospective.pdf"))
#Plot result
print(plot(NSH.stcks,key=TRUE,main="Retrospective analysis"))
dev.off()

### ============================================================================
### Finish
### ============================================================================
save(NSH.sam,NSH.ctrl,file=file.path(resdir,"Retrospective.RData"))
log.msg(paste("COMPLETE IN",sprintf("%0.1f",round(proc.time()[3]-start.time,1)),"s.\n\n"))
