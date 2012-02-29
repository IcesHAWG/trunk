################################################################################
# Scan Catch f.var bindings
#
# $Rev$
# $Date$
#
# Author: HAWG model devlopment group
#
# The FLSAM model has the ability to bind parameters together, effectively using one parameter 
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
log.msg("\nNSH SAM Catch f.vars     \n===========================\n")

### ============================================================================
### Setup assessment
### ============================================================================
#Scanning parameters
scan.surv <- "catch"
scan.slot <- "f.vars"

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

#Scan through the survey ages, tying them sequentlly together
binding.list <- list("08"=rep(1,9),
                     "01,28"=c(rep(1,2),rep(2,7)),
                     "01,24,58"=c(rep(1,2),rep(2,3),rep(3,4)),
                     "23,45,68"=c(1,2,3,3,4,4,5,5,5),
                     "01,23,45,68"=c(1,1,2,2,3,3,4,4,4))
for(bnd.name in names(binding.list)) {
   ctrl.obj <- NSH.ctrl
   ctrl.obj@name <- bnd.name
   ctrl.obj@desc <- paste(scan.surv,scan.slot,bnd.name)
   slot(ctrl.obj,scan.slot)[scan.surv,] <- binding.list[[bnd.name]]
   ctrls[[ctrl.obj@name]] <- update(ctrl.obj)
}

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

#Plot obs.var as a measure of stability of the model
obvs <-  obs.var(scan.sams)
obvs$name <- factor(obvs$name)
obvs$str <- factor(paste(obvs$fleet,obvs$age))
plot(as.numeric(obvs$str),obvs$value,type="n",xaxt="n",xlab="",ylab="Observation variance",
   ylim=range(pretty(c(0,obvs$value))))
grid()
text(as.numeric(obvs$str),obvs$value,as.numeric(obvs$name),col=as.numeric(obvs$name))
axis(1,at=seq(levels(obvs$str)),labels=levels(obvs$str),las=3)
legend("topright",legend=sprintf("%i - %s",seq(nlevels(obvs$name)),levels(obvs$name)),
  text.col=seq(nlevels(obvs$name)))

#Plot resulting f.vars
f.vars <- lapply(scan.sams,function(x) {
             f.vars <- subset(coef(x),name=="logSdLogFsta")
             f.vars <- f.vars[x@control@f.vars["catch",],]
             f.vars$age <- colnames(x@control@f.vars)
             f.vars$object.name <- x@name
             return(f.vars)})
f.vars <- do.call(rbind,f.vars)
f.vars$vars <- exp(f.vars$value)
print(xyplot(vars ~ as.numeric(age),f.vars,groups=object.name,
        type="l",auto.key=list(space="right",points=FALSE,lines=TRUE),
        xlab="Age",ylab="F variance",main="F variances"))

#Correlation matrices
library(RColorBrewer)
cols <- brewer.pal(11,"Spectral")
for(sm in scan.sams) {
  n.coefs <- nrow(coef(sm))   
  cor.mat <- cov2cor(sm@vcov[1:n.coefs,1:n.coefs]) 
  var.names <- colnames(cor.mat)
  var.names <- factor(var.names,unique(var.names))
  var.names <- paste(var.names,do.call(c,lapply(table(var.names),seq,from=1)),sep=".")
  dimnames(cor.mat) <- list(Var2=var.names,Var1=var.names)
  plt.dat <- as.data.frame(as.table(cor.mat),responseName="cor")
  print(levelplot(cor ~ Var2 * Var1,plt.dat,
      at=seq(-1,1,by=0.02),zlim=c(-1,1),
      scales=list(x=list(rot=90)),
      xlab="",ylab="",main=sm@name,
      col.regions=colorRampPalette(cols,space="Lab")(102)))
}

#Plot all assessments on one plot
print(plot(scan.sams,main=sprintf("%s %s scan",scan.surv,scan.slot)))

#Write likelihood test table
#lr.tbl <- lr.test(scan.sams)
#write.table(lr.tbl,file=file.path(resdir,paste(respref,".txt",sep="")))


### ============================================================================
### Finish
### ============================================================================
dev.off()
log.msg(paste("COMPLETE IN",sprintf("%0.1f",round(proc.time()[3]-start.time,1)),"s.\n\n"))
