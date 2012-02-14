################################################################################
# NSH_SAM "All-in" Assessment
#
# $Rev$
# $Date$
#
# Author: HAWG model devlopment group
#
# Performs an assessment of the NSAS Herring stock using the SAM method including
# all surveys
#
# Developed with:
#   - R version 2.13.0
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
log.msg("\nNSH SAM Refined data   \n===========================\n")

### ============================================================================
### Setup assessment
### ============================================================================
#Somewhere to store results
resdir <- file.path("benchmark","resultsSAM")
respref <- "04a_refined_data" #Prefix for output files
resfile <- file.path(resdir,paste(respref,".RData",sep=""))

#Import externals
library(FLSAM)
source(file.path("benchmark","Setup_objects.r"))
source(file.path("benchmark","04_Setup_refined_data.r"))

### ============================================================================
### Run the assessment
### ============================================================================
#Only do the assessment if we are running in batch mode, or
#if the results file is missing
if(!file.exists(resfile) | !interactive()) {
  #Perform assessment
  NSH.sam <- FLSAM(NSH,NSH.tun,NSH.ctrl)
  NSH.sam@name <- "NSAS Herring Refined Data Assessment"

  #Update stock object
  NSH.sam.ass <- NSH + NSH.sam

  # Save results
  save(NSH,NSH.tun,NSH.ctrl,NSH.sam,file=resfile)

} else {
  #Load the file
  load(resfile)
}

### ============================================================================
### Plots
### ============================================================================
#Setup plots
pdf(file.path(resdir,paste(respref,".pdf",sep="")),pointsize=16)

#Plot result
print(plot(NSH.sam))

#Plot uncertainties as a function of time
CV.yrs <- ssb(NSH.sam)$year
CV.dat <- cbind(SSB=ssb(NSH.sam)$CV,
                   Fbar=fbar(NSH.sam)$CV,Rec=rec(NSH.sam)$CV)
matplot(CV.yrs,CV.dat,type="l",ylim=range(pretty(c(0,CV.dat))),yaxs="i",
    xlab="Year",ylab="CV of estimate",main="Uncertainties of key parameters")
legend("topleft",legend=colnames(CV.dat),lty=1:5,col=1:6,bty="n")

#Plot catchabilities values
catch <- catchabilities(NSH.sam)
print(xyplot(value+ubnd+lbnd ~ age | fleet,catch,
          scale=list(alternating=FALSE,y=list(relation="free")),as.table=TRUE,
          type="l",lwd=c(2,1,1),col=c("black","grey","grey"),
          subset=fleet %in% c("HERAS"),
          main="Survey catchability parameters",ylab="Catchability",xlab="Age"))

#Plot obs_variance (weightings)
obv <- obs.var(NSH.sam)
obv$str <- paste(obv$fleet,ifelse(is.na(obv$age),"",obv$age))
obv <- obv[order(obv$value),]
bp <- barplot(obv$value,ylab="Observation Variance",
       main="Observation variances by data source",col=factor(obv$fleet))
axis(1,at=bp,labels=obv$str,las=3,lty=0,mgp=c(0,0,0))
legend("topleft",levels(obv$fleet),pch=15,col=1:nlevels(obv$fleet),pt.cex=1.5)

plot(obv$value,obv$CV,xlab="Observation variance",ylab="CV of estimate",log="x",
  pch=16,col=obv$fleet,main="Observation variance vs uncertainty")
text(obv$value,obv$CV,obv$str,pos=4,cex=0.75,xpd=NA)

#Compare weights against Simmonds wts
sim.wts <- read.csv(file.path(".","data","simmonds_wts.csv"))
wts <- merge(sim.wts,obv)
wts$fit.wts <- 1/wts$value
plot(wts$simmonds_wts,wts$fit.wts,xlab="HAWG 2011 Weightings", 
  ylab="SAM Fitted Weights",type="n",log="xy",main="Comparison of weightings")
text(wts$simmonds_wts,wts$fit.wts,wts$str,xpd=NA)

#Plot time series used 
surv.avail <- lapply(NSH.tun,function(x) {
                 data.frame(name=x@name,as.data.frame(x@index))})
catch.avail <- data.frame(name="catch",as.data.frame(NSH@catch.n))
dat.avail <- do.call(rbind,c(list(catch.avail),surv.avail))
dat.avail <- subset(dat.avail, data>=0)
dat.ts <- split(dat.avail,list(dat.avail$name,dat.avail$age),drop=TRUE)
dat.unique <- lapply(dat.ts,function(x) {
                data.frame(name=unique(x$name),age=unique(x$age),
                         min.year=min(x$year),max.year=max(x$year))
                })
dat.unique <- do.call(rbind,dat.unique)
dat.unique$name <- factor(dat.unique$name,levels=levels(obv$fleet))
dat.unique$str <- paste(dat.unique$name,dat.unique$age)
dat.unique <- dat.unique[order(dat.unique$str),]
dat.unique$str <- factor(dat.unique$str)
par(mar=c(5,6,2,2))
plot(NA,NA,xlim=range(pretty(c(dat.unique$min.year,dat.unique$max.year))),
  ylim=c(1,nrow(dat.unique)),xlab="Year",ylab="",yaxt="n")
arrows(dat.unique$min.year,as.numeric(dat.unique$str),
       dat.unique$max.year,as.numeric(dat.unique$str),
  col=dat.unique$name,code=3,angle=90,length=0.05)
axis(2,at=as.numeric(dat.unique$str),dat.unique$str,las=2)

#Plot selectivity pattern over time
sel.pat <- merge(f(NSH.sam),fbar(NSH.sam),
             by="year",suffixes=c(".f",".fbar"))
sel.pat$sel <- sel.pat$value.f/sel.pat$value.fbar
sel.pat$age <- as.numeric(as.character(sel.pat$age))
print(xyplot(value.f ~ year,sel.pat,groups=sprintf("Age %02i",age),
         type="l",as.table=TRUE,auto.key=list(space="right"),
         main="Fishing pressure over time",xlab="Year",ylab="F",
         scale=list(alternating=FALSE)))
print(xyplot(sel ~ year,sel.pat,groups=sprintf("Age %02i",age),
         type="l",as.table=TRUE,auto.key=list(space="right"),
         main="Selectivity of the Fishery",xlab="Year",ylab="F/Fbar",
         scale=list(alternating=FALSE)))
print(xyplot(sel ~ age|sprintf("%i's",floor((year+2)/5)*5),sel.pat,
         groups=year,type="l",as.table=TRUE,
         scale=list(alternating=FALSE),
         main="Selectivity of the Fishery by Pentad",xlab="Age",ylab="F/Fbar"))

#Survey fits
residual.diagnostics(NSH.sam)

### ============================================================================
### Finish
### ============================================================================
dev.off()
log.msg(paste("COMPLETE IN",sprintf("%0.1f",round(proc.time()[3]-start.time,1)),"s.\n\n"))
