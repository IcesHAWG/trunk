################################################################################
# Scan HERAS bindings
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
log.msg("\nScan HERAS bindings\n===========================\n")

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
#Now scan through the HERAS ages, tying them sequentlly together
HERAS.ctrls <- list()
for(i in 1:9) {
  ctrl <- NSH.ctrl
  ctrl@obs.vars["HERAS",ac(i:9)] <- 100
  ctrl@catchabilities["HERAS",ac(i:9)] <- 100
  ctrl@name <- sprintf("%i+",i)
  ctrl@desc <- sprintf("Age %i+ params bound together",i)
  HERAS.ctrls[[i]] <- update(ctrl)
}
names(HERAS.ctrls) <- sapply(HERAS.ctrls,slot,"name")

### ============================================================================
### Run the assessment
### ============================================================================
#Perform assessment
HERAS.sams <- lapply(HERAS.ctrls,FLSAM,stck=NSH,tun=NSH.tun,batch.mode=TRUE)

#Convert to FLSAMs
HERAS.sams <- do.call(FLSAMs,HERAS.sams)

### ============================================================================
### Analyse the results
### ============================================================================
#Drop any that failed to converge
HERAS <- HERAS.sams[!sapply(HERAS.sams,is.null)]

#Build stock objects
HERAS.stcks <- do.call(FLStocks,lapply(HERAS,"+",NSH))

#Extract AICs
HERAS.AICs  <- sapply(HERAS,AIC)

#Plot
pdf(file.path(resdir,"HERAS_bindings_scan.pdf"))
plot(HERAS.AICs,main="HERAS",ylab="AIC")
plot(HERAS.stcks,main="HERAS bindings scan")

dev.off()

### ============================================================================
### Compare results
### ============================================================================
save(HERAS.sams,file=file.path(resdir,"HERAS.sams.RData"))
log.msg(paste("COMPLETE IN",sprintf("%0.1f",round(proc.time()[3]-start.time,1)),"s.\n\n"))
