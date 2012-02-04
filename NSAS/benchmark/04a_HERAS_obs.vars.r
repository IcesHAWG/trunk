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
scan.surv <- "HERAS"
scan.slot <- "obs.vars"

#Somewhere to store results
resdir <- file.path("benchmark","resultsSAM")
respref <- sprintf("04a_%s_%s",scan.surv,scan.slot) #Prefix for output files
resfile <- file.path(resdir,paste(respref,".RData",sep=""))

#Import externals
library(FLSAM)
source(file.path("benchmark","Setup_objects.r"))
source(file.path("benchmark","03_Setup_selected_surveys.r"))

### ============================================================================
### Setup control objects
### ============================================================================
#Setup defaults
ctrls <- list()
NSH.ctrl@timeout <- 2700

#Scan through the survey ages, tying them sequentlly together
binding.list <- lapply(1:8,seq,to=9)
names(binding.list) <- lapply(binding.list,function(x) sprintf("%i+",min(x)))
for(bnd.name in names(binding.list)) {
   ctrl.obj <- NSH.ctrl
   ctrl.obj@name <- bnd.name
   ctrl.obj@desc <- paste(scan.surv,scan.slot,bnd.name)
   bindings <- binding.list[[bnd.name]]
   slot(ctrl.obj,scan.slot)[scan.surv,ac(bindings)] <- 100
   ctrls[[ctrl.obj@name]] <- update(ctrl.obj)
}

#Consider some good guesses based on selection pattern
ends.free <- new("FLSAM.control",NSH.ctrl,name="1,28,9")
ends.free@catchabilities["HERAS",ac(1:9)] <- c(101,rep(102,7),109) 
ends.free2 <- new("FLSAM.control",NSH.ctrl,name="1,27,89")
ends.free2@catchabilities["HERAS",ac(1:9)] <- c(101,rep(102,6),108,108) 
ends.free3 <- new("FLSAM.control",NSH.ctrl,name="1,26,79")
ends.free3@catchabilities["HERAS",ac(1:9)] <- c(101,rep(102,5),108,108,108) 

#Update and finish control objects
ctrls <- c(ctrls,NSH.ctrl,ends.free,ends.free2,ends.free3)
ctrls <- lapply(ctrls,update)
names(ctrls) <- lapply(ctrls,function(x) x@name)

### ============================================================================
### Run the assessments
### ============================================================================
#Only do the assessment if we are running in batch mode, or
#if the results file is missing
if(!file.exists(resfile) | !interactive()) {
   #Perform assessment
   ass.res <- lapply(ctrls,FLSAM,stck=NSH,tun=NSH.tun,batch.mode=TRUE)
     
   #Drop any that failed to converge, then create an FLSAMs object
   scan.sams <- FLSAMs(ass.res[!sapply(ass.res,is.null)]); 
   
   #Save results
   save(NSH,NSH.tun,scan.sams,file=resfile)
} else {
  #Load the file
  load(resfile)
}

### ============================================================================
### Outputs
### ============================================================================
pdf(file.path(resdir,paste(respref,".pdf",sep="")))
#Plot AICs
scan.AICs  <- AIC(scan.sams)
plot(scan.AICs,main=sprintf("%s %s scan",scan.surv,scan.slot),
   ylab="AIC",xaxt="n",xlab="Model",pch=16)
axis(1,labels=names(scan.AICs),at=seq(scan.AICs),las=3)

#Plot all assessments on one plot
print(plot(scan.sams,main=sprintf("%s %s scan",scan.surv,scan.slot)))

#Write likelihood test table
#lr.tbl <- lr.test(scan.sams)
#write.table(lr.tbl,file=file.path(resdir,paste(respref,".txt",sep="")))

#Plot all observation variances
obvs <- obs.var(scan.sams)
print(xyplot(value ~ age,data=obvs,groups=name,
          scale=list(alternating=FALSE),as.table=TRUE,
          type="l",auto.key=list(space="right",points=FALSE,lines=TRUE),
          subset=fleet %in% c("HERAS"),
          main="HERAS observation variances",ylab="Observation Variance",xlab="Age"))

### ============================================================================
### Finish
### ============================================================================
dev.off()
log.msg(paste("COMPLETE IN",sprintf("%0.1f",round(proc.time()[3]-start.time,1)),"s.\n\n"))
