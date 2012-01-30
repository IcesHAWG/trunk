################################################################################
# Scan HERAS bindings
#
# $Rev: 630 $
# $Date: 2012-01-18 10:22:41 +0100 (wo, 18 jan 2012) $
#
# Author: HAWG model devlopment group
#
# The FLSAM model has the ability to bind the observation variances (weightings)
# and catchabilities of age groups together, effectively using one parameter 
# for many age groups. 
# The appropriate bindings can be identified by performing a series of assessments 
# for each combination and then comparing the results using AIC / LR tests.
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
rm(list=ls()); graphics.off(); start.time <- proc.time()[3]
options(stringsAsFactors=FALSE)
log.msg     <-  function(string) {
	cat(string);flush.console()
}
log.msg("\nNSH SAM HERAS Bindings     \n===========================\n")

### ============================================================================
### Setup assessment
### ============================================================================
#Scanning parameters
age.list <- lapply(1:9,seq,to=9)
scan.surv <- "HERAS"

#Somewhere to store results
resdir <- file.path("benchmark","resultsSAM")
respref <- sprintf("04a_%s_bindings",scan.surv) #Prefix for output files

#Import externals
library(FLSAM)
source(file.path("benchmark","Setup_objects.r"))
source(file.path("benchmark","02_Setup_All_in_runs.r"))
source(file.path("benchmark","Scan_survey_bindings.r"))

### ============================================================================
### Perform assessments
### ============================================================================
#Use automatic tool
scan.sams <- scan.bindings(NSH,NSH.tun,NSH.ctrl,scan.surv,age.list,resdir,respref) 

#Save results
save(NSH,NSH.tun,scan.sams,file=file.path(resdir,paste(respref,".RData",sep="")))

### ============================================================================
### Finish
### ============================================================================
log.msg(paste("COMPLETE IN",sprintf("%0.1f",round(proc.time()[3]-start.time,1)),"s.\n\n"))
