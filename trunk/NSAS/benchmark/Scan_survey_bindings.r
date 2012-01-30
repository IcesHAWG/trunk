################################################################################
# Scan survey bindings
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

scan.bindings <- function(stck,tun,ctrl,surv,age.list,resdir,respref) {

#scan through the survey ages, tying them sequentlly together
   ctrls <- list()
   for(ages in age.list) {
     ctrl.obj <- ctrl
     ctrl.obj@obs.vars[surv,ac(ages)] <- 100
     ctrl.obj@catchabilities[surv,ac(ages)] <- 100
     ctrl.obj@name <- sprintf("%i+",min(ages))
     ctrl.obj@desc <- sprintf("Age %i+ params bound together",min(ages))
     ctrls[[ctrl.obj@name]] <- update(ctrl.obj)
   }
   
   #Perform assessment
   ass.res <- lapply(ctrls,FLSAM,stck=stck,tun=tun,batch.mode=TRUE)
   
   #Drop any that failed to converge, then create an FLSAMs object
   scan.sams <- FLSAMs(ass.res[!sapply(ass.res,is.null)]); 

   #Save results
   save(stck,tun,scan.sams,file=file.path(resdir,paste(respref,".RData",sep="")))

   #Plot
   scan.AICs  <- AIC(scan.sams)
   pdf(file.path(resdir,paste(respref,".pdf",sep="")))
   plot(scan.AICs,main=sprintf("%s bindings scan",surv),ylab="AIC",xaxt="n",xlab="Model",pch=16)
   axis(1,labels=names(scan.AICs),at=seq(scan.AICs))
   print(plot(scan.sams,main=sprintf("%s bindings scan",surv)))

   dev.off()

   #Write likelihood test table
   lr.tbl <- lr.test(scan.sams)
   write.table(lr.tbl,file=file.path(resdir,paste(respref,".txt",sep="")))
   
   return(scan.sams)
}

