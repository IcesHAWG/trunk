################################################################################
# Scan IBTS bindings
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
  IBTS.ctrls[[ac(i)]] <- update(ctrl)
}
names(IBTS.ctrls) <- sapply(IBTS.ctrls,slot,"name")

### ============================================================================
### Run the assessment
### ============================================================================
#Perform assessment
IBTS.sams <- lapply(IBTS.ctrls,FLSAM,stck=NSH,tun=NSH.tun,batch.mode=TRUE)

#Drop any that failed to converge, then create an FLSAMs object
IBTS.sams <- FLSAMs(IBTS.res[!sapply(IBTS.sams,is.null)]); ifelse(length(which(sapply(IBTS.sams,is.null)==T)>0),warnings("IBTS obs.vars+catchability binding run(s) failed"),"")

### ============================================================================
### Save results
### ============================================================================
save(NSH,NSH.tun,IBTS.sams,file=file.path(".","benchmark","resultsSAM","Scan_IBTS_bindings.RData"))

### ============================================================================
### Analyse the results
### ============================================================================
#Build stock objects
IBTS.stcks <- NSH+IBTS.sams

#Extract AICs
IBTS.AICs  <- AIC(IBTS.sams)

#Plot
pdf(file.path(".","benchmark","resultsSAM","Scan_IBTS_bindings.pdf"))
plot(IBTS.AICs,main="IBTS bindings scan",ylab="AIC",xaxt="n",xlab="Model",pch=16)
axis(1,labels=names(IBTS.AICs),at=seq(IBTS.AICs))
print(plot(IBTS.stcks,main="IBTS bindings scan",key=TRUE))

dev.off()

### ============================================================================
### Finish
### ============================================================================
log.msg(paste("COMPLETE IN",sprintf("%0.1f",round(proc.time()[3]-start.time,1)),"s.\n\n"))
