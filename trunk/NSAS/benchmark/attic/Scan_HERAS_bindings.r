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
  HERAS.ctrls[[ac(i)]] <- update(ctrl)
}
names(HERAS.ctrls) <- sapply(HERAS.ctrls,slot,"name")

### ============================================================================
### Run the assessment
### ============================================================================
#Perform assessment
HERAS.res <- lapply(HERAS.ctrls,FLSAM,stck=NSH,tun=NSH.tun,batch.mode=TRUE)

#Drop any that failed to converge, then create an FLSAMs object
HERAS.sams <- FLSAMs(HERAS.res[!sapply(HERAS.res,is.null)]); ifelse(length(which(sapply(HERAS.sams,is.null)==T)>0),warnings("HERAS obs.vars+catchability binding run(s) failed"),"")

### ============================================================================
### Save results
### ============================================================================
save(NSH,NSH.tun,HERAS.sams,file=file.path(".","benchmark","resultsSAM","Scan_HERAS_bindings.RData"))

### ============================================================================
### Analyse the results
### ============================================================================
#Build stock objects
HERAS.stcks <- HERAS.sams + NSH

#Extract AICs
HERAS.AICs  <- AIC(HERAS.sams)

#Plot
pdf(file.path(".","benchmark","resultsSAM","Scan_HERAS_bindings.pdf"))
plot(HERAS.AICs,main="HERAS bindings scan",ylab="AIC",xaxt="n",xlab="Model",pch=16)
axis(1,labels=names(HERAS.AICs),at=seq(HERAS.AICs))
print(plot(HERAS.stcks,main="HERAS bindings scan",key=TRUE))

dev.off()

### ============================================================================
### Finish
### ============================================================================
log.msg(paste("COMPLETE IN",sprintf("%0.1f",round(proc.time()[3]-start.time,1)),"s.\n\n"))
