################################################################################
# Scan observation variance bindings
#
# $Rev$
# $Date$
#
# Author: HAWG model devlopment group
#
# The FLSAM model has the ability to bind the observation variances (weightings)
# of age groups together, effectively using one parameter for many age groups. 
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
log.msg("\nScan obs. variance bindings\n===========================\n")

### ============================================================================
### Import externals
### ============================================================================
log.msg("IMPORTING EXTERNAL RESOURCES...\n")
library(FLSAM)
objdir <- file.path(".","objects")
load(file.path(objdir,"NSH.RData"))
load(file.path(objdir,"NSH.tun.RData"))
load(file.path(objdir,"NSH.ctrl.RData"))

### ============================================================================
### Modify the default assessment
### ============================================================================
#Now scan through the HERAS ages, tying them sequentlly together
HERAS.ctrls <- list()
for(i in 1:9) {
  ctrl <- NSH.ctrl
  ctrl@obs.vars["HERAS",ac(1:9)] <- 5:13
  ctrl@obs.vars["HERAS",ac(i:9)] <- 4+i
  ctrl@name <- sprintf("%i+",i)
  ctrl@desc <- sprintf("Age %i+ observation variances bound together",i)
  HERAS.ctrls[[i]] <- ctrl
}
names(HERAS.ctrls) <- sapply(HERAS.ctrls,slot,"name")

#And ditto for the IBTS ages
IBTS.ctrls <- list()
for(i in 1:5) {
  ctrl <- NSH.ctrl
  ctrl@obs.vars["IBTS-Q1",ac(1:5)] <- 3:7
  ctrl@obs.vars["IBTS-Q1",ac(i:5)] <- 2+i
  ctrl@obs.vars["HERAS",ac(1:9)] <- 3+i
  ctrl@name <- sprintf("%i+",i)
  ctrl@desc <- sprintf("Age %i+ observation variances bound together",i)
  IBTS.ctrls[[i]] <- ctrl
}
names(IBTS.ctrls) <- sapply(IBTS.ctrls,slot,"name")

### ============================================================================
### Run the assessment
### ============================================================================
#Perform assessment
HERAS.sams <- lapply(HERAS.ctrls,FLSAM,stck=NSH,tun=NSH.tun,batch.mode=TRUE)
IBTS.sams <- lapply(IBTS.ctrls,FLSAM,stck=NSH,tun=NSH.tun,batch.mode=TRUE)

### ============================================================================
### Analyse the results
### ============================================================================
#Drop any that failed to converge
HERAS <- HERAS.sams[!sapply(HERAS.sams,is.null)]
IBTS <- IBTS.sams[!sapply(IBTS.sams,is.null)]

#Build stock objects
HERAS.stcks <- do.call(FLStocks,lapply(HERAS,"+",NSH))
IBTS.stcks <- do.call(FLStocks,lapply(IBTS,"+",NSH))

#Extract AICs
HERAS.AICs <- sapply(HERAS,AIC)
IBTS.AICs  <- sapply(IBTS,AIC)

#Plot
pdf(file.path(resdir,"Obs_var_scan.pdf"))
plot(HERAS.AICs,main="HERAS",ylab="AIC")
plot(HERAS.stcks,main="HERAS obs var scan")
plot(IBTS.AICs,main="IBTS",ylab="AIC")
plot(IBTS.stcks,main="IBTS obs var scan")

dev.off()

### ============================================================================
### Compare results
### ============================================================================
save(HERAS.sams,IBTS.sams,file=file.path(resdir,"Obs_var_scan.RData"))
log.msg(paste("COMPLETE IN",sprintf("%0.1f",round(proc.time()[3]-start.time,1)),"s.\n\n"))
