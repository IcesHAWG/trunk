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
rm(list=ls()); gc(); graphics.off(); start.time <- proc.time()[3]
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
source("Setup_FLSAM_control.r")

### ============================================================================
### Modify the default assessment
### ============================================================================
log.msg("CONFIGURING ASSESSMENT......\n")

#Scan through the HERAS ages, tying them sequentlly together
HERAS.ctrls <- list()
for(i in 1:9) {
  ctrl <- NSH.ctrl
  ctrl@catchabilities["HERAS",ac(1:9)] <- 7:15
  ctrl@catchabilities["HERAS",ac(i:9)] <- 6+i
  ctrl@name <- ac(i)
  ctrl@desc <- sprintf("Age %i+ catchabilities bound together",i)
  HERAS.ctrls[[i]] <- ctrl
}
names(HERAS.ctrls) <- sapply(HERAS.ctrls,slot,"name")

#And ditto for the IBTS ages
IBTS.ctrls <- list()
for(i in 1:5) {
  ctrl <- NSH.ctrl
  ctrl@catchabilities["IBTS-Q1",ac(1:5)] <- 2:6
  ctrl@catchabilities["IBTS-Q1",ac(i:5)] <- 1+i
  ctrl@catchabilities["HERAS",ac(1:9)] <- 1+i+c(1:4, rep(5,5))
  ctrl@name <- ac(i)
  ctrl@desc <- sprintf("Age %i+ catchabilities bound together",i)
  IBTS.ctrls[[i]] <- ctrl
}
names(IBTS.ctrls) <- sapply(IBTS.ctrls,slot,"name")

### ============================================================================
### Run the assessment
### ============================================================================
#Perform assessments
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

#Extract catchabilities to plot
HERAS.qs <- do.call(rbind,lapply(HERAS,catchabilities))
IBTS.qs  <- do.call(rbind,lapply(IBTS,catchabilities)) 

#Plot
pdf(file.path(resdir,"Catchability_scan.pdf"))
plot(HERAS.AICs,main="HERAS",ylab="AIC")
plot(HERAS.stcks,main="HERAS catchability scan")
p<-xyplot(value ~ age,HERAS.qs,subset=fleet=="HERAS",
      type="l",groups=name)
print(p)
plot(IBTS.AICs,main="IBTS",ylab="AIC")
plot(IBTS.stcks,main="IBTS catchability scan")
p<-xyplot(value ~ age,IBTS.qs,subset=fleet=="IBTS-Q1",
      type="l",groups=name)
print(p)
dev.off()

### ============================================================================
### Compare results
### ============================================================================
save(HERAS.sams,IBTS.sams,file=file.path(resdir,"Catchability_scan.RData"))
log.msg(paste("COMPLETE IN",sprintf("%0.1f",round(proc.time()[3]-start.time,1)),"s.\n\n"))