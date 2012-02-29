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
#print(plot(NSH.sam))

#Plot uncertainties as a function of time
#CV.yrs <- ssb(NSH.sam)$year
#CV.dat <- cbind(SSB=ssb(NSH.sam)$CV,
#                   Fbar=fbar(NSH.sam)$CV,Rec=rec(NSH.sam)$CV)
#matplot(CV.yrs,CV.dat,type="l",ylim=range(pretty(c(0,CV.dat))),yaxs="i",
#    xlab="Year",ylab="CV of estimate",main="Uncertainties of key parameters")
#legend("topleft",legend=colnames(CV.dat),lty=1:5,col=1:6,bty="n")


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

#Plot catchabilities values
catch <- catchabilities(NSH.sam)
print(xyplot(value+ubnd+lbnd ~ age | fleet,catch,
          scale=list(alternating=FALSE,y=list(relation="free")),as.table=TRUE,
          type="l",lwd=c(2,1,1),col=c("black","grey","grey"),
          subset=fleet %in% c("HERAS"),
          main="Survey catchability parameters",ylab="Catchability",xlab="Age"))

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

print(levelplot(sel ~ year*age,sel.pat,at=seq(0,2.75,length.out=100),
        col.regions=colorRampPalette(c("black","red","yellow"),space="Lab")(101)))


#Variability in catch residuals over time
catch.resids <- subset(residuals(NSH.sam),fleet=="catch")
catch.resids$pentad <- floor((catch.resids$year+2)/5)*5
catch.resids$decade <- sprintf("%02i",(floor(catch.resids$year/10)*10)%%100)
catch.resids$decade <- factor(catch.resids$decade,levels=unique(catch.resids$decade))
boxplot(std.res ~ pentad,catch.resids,xlab="Pentad",ylab="Standardised residuals",
  main="Variability of catch residuals by pentad",las=3)

print(bwplot(std.res ~ decade | sprintf("Age %02i",age),catch.resids,
        xlab="Decade",ylab="Standardised residuals",main="Catch-residual variability",
        as.table=TRUE,horizontal=FALSE,pch="|",lty=1,fill="grey",
        par.settings=list(box.rectangle=list(lty=1,col=1),
                          box.umbrella=list(lty=1,col=1),
                          plot.symbol=list(col=1)),
        scale=list(alternating=FALSE),
        panel=function(...) {
          panel.abline(h=-3:3,col="lightgrey")
          panel.bwplot(...)}))

#Standard deviation of each time series
resids <- residuals(NSH.sam)
resids$str <- paste(resids$fleet,resids$age)
ob.bindings <- as.data.frame(as.table(NSH.sam@control@obs.vars),responseName="binding")
ob.bindings <- subset(ob.bindings,!is.na(binding))
resids <- merge(resids,ob.bindings)
ts.sds <- tapply(resids$std.res,resids$str,sd)
bind.sds <- tapply(resids$std.res,resids$binding,sd)
bp <- barplot(ts.sds,ylab="Residual Standard deviations",
       main="Residual standard deviations by time series",xaxt="n")
axis(1,at=bp,labels=names(ts.sds),las=3,lty=0,mgp=c(0,0,0))
bp <- barplot(bind.sds,ylab="Residual Standard deviations",xaxt="n",
       main="Residual standard deviations by bindings",xlab="Binding parameter")
axis(1,at=bp,labels=names(bind.sds),las=3,lty=0,mgp=c(0,0,0))

#Correlation matrices
library(RColorBrewer)
cols <- brewer.pal(11,"Spectral")
n.coefs <- nrow(coef(NSH.sam))   
cor.mat <- cov2cor(NSH.sam@vcov[1:n.coefs,1:n.coefs]) 
var.names <- colnames(cor.mat)
var.names <- factor(var.names,unique(var.names))
var.names <- paste(var.names,do.call(c,lapply(table(var.names),seq,from=1)),sep=".")
dimnames(cor.mat) <- list(Var2=var.names,Var1=var.names)
plt.dat <- as.data.frame(as.table(cor.mat),responseName="cor")
print(levelplot(cor ~ Var2 * Var1,plt.dat,
    at=seq(-1,1,by=0.02),zlim=c(-1,1),
    scales=list(x=list(rot=90)),
    xlab="",ylab="",main=NSH.sam@name,
    col.regions=colorRampPalette(cols,space="Lab")(102)))

#Survey fits
residual.diagnostics(NSH.sam)

### ============================================================================
### Finish
### ============================================================================
dev.off()
log.msg(paste("COMPLETE IN",sprintf("%0.1f",round(proc.time()[3]-start.time,1)),"s.\n\n"))
