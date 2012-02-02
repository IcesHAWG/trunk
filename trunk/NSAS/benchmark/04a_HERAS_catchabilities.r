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
#Configuration
scan.surv <- "HERAS"
scan.slot <- "catchabilities"

#Somewhere to store results
resdir <- file.path("benchmark","resultsSAM")
respref <- sprintf("04a_%s_%s",scan.surv,scan.slot) #Prefix for output files

#Import externals
library(FLSAM)
source(file.path("benchmark","Setup_objects.r"))
source(file.path("benchmark","03_Setup_selected_surveys.r"))

### ============================================================================
### Setup control objects
### ============================================================================
#Setup defaults
NSH.ctrl@timeout <- 2700
ctrls <- list()

#Scan through the survey ages, tying them sequentlly together from bottom
binding.list <- lapply(2:9,seq,from=1)
names(binding.list) <- lapply(binding.list,function(x) sprintf("-%i",max(x)))
for(bnd.name in names(binding.list)) {
   ctrl.obj <- NSH.ctrl
   ctrl.obj@name <- bnd.name
   ctrl.obj@desc <- paste(scan.surv,scan.slot,bnd.name)
   bindings <- binding.list[[bnd.name]]
   slot(ctrl.obj,scan.slot)[scan.surv,ac(bindings)] <- 100
   ctrls[[ctrl.obj@name]] <- update(ctrl.obj)
}

#Consider some good guesses based on selection pattern
two.steps <- new("FLSAM.control",NSH.ctrl,name="17,89")
two.steps@catchabilities["HERAS",ac(1:9)] <- c(rep(101,7),102,102) 
three.steps <- new("FLSAM.control",NSH.ctrl,name="14,57,89")
three.steps@catchabilities["HERAS",ac(1:9)] <- c(rep(101,4),rep(102,3),103,103) 
two.by.two <- new("FLSAM.control",NSH.ctrl,name="12,34,56,78,9")
two.by.two@catchabilities["HERAS",ac(1:9)] <- c(101,101,103,103,105,105,107,107,109)

#Update and finish control objects
ctrls <- c(NSH.ctrl,ctrls,two.steps,three.steps,two.by.two)
ctrls <- lapply(ctrls,update)
names(ctrls) <- lapply(ctrls,function(x) x@name)

### ============================================================================
### Run the assessments
### ============================================================================
#Perform assessment
ass.res <- lapply(ctrls,FLSAM,stck=NSH,tun=NSH.tun,batch.mode=TRUE)
  
#Drop any that failed to converge, then create an FLSAMs object
scan.sams <- FLSAMs(ass.res[!sapply(ass.res,is.null)]); 

#Save results
save(NSH,NSH.tun,NSH.ctrl,scan.sams,file=file.path(resdir,paste(respref,".RData",sep="")))

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

#Plot variable catchabilities for each set of bindings
qs <- catchabilities(scan.sams)
print(xyplot(value ~ age,data=qs,groups=name,
          scale=list(alternating=FALSE),as.table=TRUE,
          type="l",auto.key=list(space="right",points=FALSE,lines=TRUE),
          subset=fleet %in% c("HERAS"),
          main="HERAS catchability parameters",ylab="Catchability",xlab="Age"))


#Write likelihood test table
#lr.tbl <- lr.test(scan.sams)
#write.table(lr.tbl,file=file.path(resdir,paste(respref,".txt",sep="")))

### ============================================================================
### Finish
### ============================================================================
dev.off()
log.msg(paste("COMPLETE IN",sprintf("%0.1f",round(proc.time()[3]-start.time,1)),"s.\n\n"))
