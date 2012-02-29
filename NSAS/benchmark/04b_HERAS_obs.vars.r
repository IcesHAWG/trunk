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
respref <- sprintf("04b_%s_%s",scan.surv,scan.slot) #Prefix for output files
resfile <- file.path(resdir,paste(respref,".RData",sep=""))

#Import externals
library(FLSAM)
source(file.path("benchmark","Setup_objects.r"))
source(file.path("benchmark","04_Setup_refined_data.r"))

### ============================================================================
### Setup control objects
### ============================================================================
#Setup defaults
ctrls <- list()
NSH.ctrl@timeout <- 2700


#Setup bindings - all free is the default
binding.list <- list()
for(i in 1:7) {
   base <- slot(NSH.ctrl,scan.slot)[scan.surv,]
   base[] <- c(NA,1:8)
   base[ac(i:8)] <- 100
   binding.list[[sprintf("%i8",i)]] <- base  
}
#HERAS 1 is a bit ugly, so set it free and then consider binding others
binding.list[["27"]]     <- c(NA,101,rep(102,6),108) 
binding.list[["26,78"]] <- c(NA,101,rep(102,5),rep(107,2))
binding.list[["25,68"]] <- c(NA,101,rep(102,4),rep(106,3))
binding.list[["24,58"]] <- c(NA,101,rep(102,3),rep(105,4))
binding.list[["23,48"]] <- c(NA,101,rep(102,2),rep(104,5))
binding.list[["23,45,67"]] <- c(NA,101,rep(102,2),rep(104,2),rep(106,2),108)
binding.list[["23,45,68"]] <- c(NA,101,rep(102,2),rep(104,2),rep(106,3))

#Convert bindings to ctrl objects
for(bnd.name in names(binding.list)) {
   ctrl.obj <- NSH.ctrl
   ctrl.obj@name <- bnd.name
   ctrl.obj@desc <- paste(scan.surv,scan.slot,bnd.name)
   bindings <- binding.list[[bnd.name]]
   slot(ctrl.obj,scan.slot)[scan.surv,] <- bindings
   ctrls[[ctrl.obj@name]] <- update(ctrl.obj)
}

#Update and finish control objects
ctrls <- c(ctrls,NSH.ctrl)
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
pdf(file.path(resdir,paste(respref,".pdf",sep="")),pointsize=18)
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
obvs$name <- factor(obvs$name)
obvs$str <- factor(paste(obvs$fleet,obvs$age))
plot(as.numeric(obvs$str),obvs$value,type="n",xaxt="n",xlab="",ylab="Observation variance",
   ylim=range(pretty(c(0,obvs$value))))
grid()
text(as.numeric(obvs$str),obvs$value,as.numeric(obvs$name),col=as.numeric(obvs$name))
axis(1,at=seq(levels(obvs$str)),labels=levels(obvs$str),las=3)
legend("topright",legend=sprintf("%i - %s",seq(nlevels(obvs$name)),levels(obvs$name)),
  text.col=seq(nlevels(obvs$name)))

print(xyplot(value ~ age,data=obvs,groups=name,
          scale=list(alternating=FALSE),as.table=TRUE,
          type="l",auto.key=list(space="right",points=FALSE,lines=TRUE),
          subset=fleet %in% c("HERAS"),
          main="HERAS observation variances",ylab="Observation Variance",xlab="Age"))

#Plot HERAS catchabilities for each model
HERAS.qs <- subset(catchabilities(scan.sams),fleet=="HERAS")
print(xyplot(value ~ age, data=HERAS.qs, groups=name,
          type="l",auto.key=list(space="right",points=FALSE,lines=TRUE),
          xlab="Age",ylab="Catchabilities",main="HERAS catchabilities"))

### ============================================================================
### Finish
### ============================================================================
dev.off()
log.msg(paste("COMPLETE IN",sprintf("%0.1f",round(proc.time()[3]-start.time,1)),"s.\n\n"))
