################################################################################
# Scan Catchability Binding
#
# $Rev$
# $Date$
#
# Author: HAWG model devlopment group
#
# The FLSAM model has the ability to bind the catchability of age groups
# together, effectively using one parameter for many age groups. The appropriate
# bindings can be identified by performing a series of assessments for each
# combination and then comparing the results using AIC / LR tests.
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
log.msg("\nScan Catchability Bindings\n==========================\n")

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
log.msg("CONFIGURING ASSESSMENT......\n")

#Scan through the HERAS ages, tying them sequentlly together
HERAS.ctrls <- list()
for(i in 8:9) {
  ctrl <- NSH.ctrl
  ctrl@catchabilities["HERAS",ac(i:9)] <- 101
  ctrl@name <- ac(i)
  ctrl@desc <- sprintf("Age %i+ catchabilities bound together",i)
  HERAS.ctrls[[ac(i)]] <- update(ctrl)
}
names(HERAS.ctrls) <- sapply(HERAS.ctrls,slot,"name")

#And ditto for the IBTS ages
IBTS.ctrls <- list()
for(i in 4:5) {
  ctrl <- NSH.ctrl
  ctrl@catchabilities["IBTS-Q1",ac(i:5)] <- 101
  ctrl@name <- ac(i)
  ctrl@desc <- sprintf("Age %i+ catchabilities bound together",i)
  IBTS.ctrls[[ac(i)]] <- update(ctrl)
}
names(IBTS.ctrls) <- sapply(IBTS.ctrls,slot,"name")

### ============================================================================
### Run the assessment
### ============================================================================
#Perform assessments
HERAS.sams <- lapply(HERAS.ctrls,FLSAM,stck=NSH,tun=NSH.tun,batch.mode=TRUE)
IBTS.sams <- lapply(IBTS.ctrls,FLSAM,stck=NSH,tun=NSH.tun,batch.mode=TRUE)

#Drop any that failed to converge
HERAS.sams <- FLSAMs(HERAS.sams[!sapply(HERAS.sams,is.null)])
IBTS.sams <- FLSAMs(IBTS.sams[!sapply(IBTS.sams,is.null)])

### ============================================================================
### Save results
### ============================================================================
save(NSH,NSH.tun,HERAS.sams,IBTS.sams,file=file.path(resdir,"Scan_catchability_binding.RData"))

### ============================================================================
### Analyse and plot the results
### ============================================================================
#Build stock objects
HERAS.stcks <- HERAS.sams+NSH
IBTS.stcks <- IBTS.sams + NSH

#Extract AICs
HERAS.AICs <- AIC(HERAS.sams)
IBTS.AICs  <- AIC(IBTS.sams)

#Extract catchabilities to plot
HERAS.qs <- catchabilities(HERAS.sams)
IBTS.qs  <- catchabilities(IBTS.sams)

#Plot
pdf(file.path(resdir,"Scan_catchability_binding.pdf"))
plot(HERAS.AICs,main="HERAS bindings scan",ylab="AIC",xaxt="n",xlab="Model",pch=16)
axis(1,labels=names(HERAS.AICs),at=seq(HERAS.AICs))
print(plot(HERAS.stcks,main="HERAS catchability scan",key=TRUE))
print(xyplot(value ~ age,data=HERAS.qs,subset=fleet=="HERAS",
      type="l",groups=name,main="HERAS catchabilities",auto.key=TRUE))

plot(IBTS.AICs,main="IBTS bindings scan",ylab="AIC",xaxt="n",xlab="Model",pch=16)
axis(1,labels=names(IBTS.AICs),at=seq(IBTS.AICs))
print(plot(IBTS.stcks,main="IBTS catchability scan",key=TRUE))
print(xyplot(value ~ age,IBTS.qs,subset=fleet=="IBTS-Q1",
      type="l",groups=name,main="IBTS catchabilities",auto.key=TRUE))
dev.off()

### ============================================================================
### Compare results
### ============================================================================
log.msg(paste("COMPLETE IN",sprintf("%0.1f",round(proc.time()[3]-start.time,1)),"s.\n\n"))
