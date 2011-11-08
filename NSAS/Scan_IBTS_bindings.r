################################################################################
# Scan IBTS bindings
#
# $Rev$
# $Date$
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
rm(list=ls()); gc(); graphics.off(); start.time <- proc.time()[3]
options(stringsAsFactors=FALSE)
log.msg     <-  function(string) {
	cat(string);flush.console()
}
log.msg("\nScan IBTS bindings\n===========================\n")

### ============================================================================
### Import externals
### ============================================================================
log.msg("IMPORTING EXTERNAL RESOURCES...\n")
library(FLSAM)
source("Setup_objects.r")
source("Setup_default_FLSAM_control.r")

### ============================================================================
### Modify the default assessment
### ============================================================================
#Now scan through the IBTS ages, tying them sequentlly together
IBTS.ctrls <- list()
for(i in 1:5) {
  ctrl <- NSH.ctrl
  ctrl@obs.vars["IBTS-Q1",ac(i:5)] <- 100
  ctrl@catchabilities["IBTS-Q1",ac(i:5)] <- 100
  ctrl@name <- sprintf("%i+",i)
  ctrl@desc <- sprintf("Age %i+ params bound together",i)
  IBTS.ctrls[[i]] <- update(ctrl)
}
names(IBTS.ctrls) <- sapply(IBTS.ctrls,slot,"name")

### ============================================================================
### Run the assessment
### ============================================================================
#Perform assessment
IBTS.res <- lapply(IBTS.ctrls,FLSAM,stck=NSH,tun=NSH.tun,batch.mode=TRUE)

#Drop any that failed to converge, then create an FLSAMs object
IBTS.sams <- FLSAMs(IBTS.res[!sapply(IBTS.res,is.null)])

### ============================================================================
### Analyse the results
### ============================================================================
#Build stock objects
IBTS.stcks <- NSH+IBTS.sams

#Extract AICs
IBTS.AICs  <- AIC(IBTS.sams)

#Plot
pdf(file.path(resdir,"IBTS_bindings_scan.pdf"))
plot(IBTS.AICs,main="IBTS bindings scan",ylab="AIC",xaxt="n",xlab="Model",pch=16)
axis(1,labels=names(IBTS.AICs),at=seq(IBTS.AICs))
plot(IBTS.stcks,main="IBTS bindings scan",key=TRUE)

dev.off()

### ============================================================================
### Compare results
### ============================================================================
save(IBTS.sams,file=file.path(resdir,"IBTS.sams.RData"))
log.msg(paste("COMPLETE IN",sprintf("%0.1f",round(proc.time()[3]-start.time,1)),"s.\n\n"))
