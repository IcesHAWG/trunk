################################################################################
# NSH_SAM Assessment
#
# $Rev$
# $Date$
#
# Author: HAWG model devlopment group
#
# Performs an assessment of the NSAS Herring stock using the SAM method
#
# Developed with:
#   - R version 2.13.1
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
log.msg("\nNSH SAM Assessment\n==========================\n")

### ============================================================================
### Import externals
### ============================================================================
library(FLSAM)
source("Setup_objects.r")
source("Setup_default_FLSAM_control.r")

### ============================================================================
### Run the assessment
### ============================================================================
#Perform assessment
NSH.sam <- FLSAM(NSH,NSH.tun,NSH.ctrl)

#Update stock object
NSH.sam.ass <- NSH + NSH.sam

### ============================================================================
### Save results
### ============================================================================
save(NSH,NSH.tun,file=file.path(resdir,"NSH.RData"))
save(NSH.sam,NSH.ctrl,file=file.path(resdir,"NSH.sam.RData"))

### ============================================================================
### Plots
### ============================================================================
#Load ICA assessment
load(file.path(".","data","NSH.ica.RData"))
NSH.stocks <- FLStocks(FLSAM=NSH.sam.ass, FLICA=NSH.ica)

#Setup plots
pdf(file.path(resdir,"NSH_SAM.pdf"))

#Plot result
NSH.sam@name <- "North Sea Herring FLSAM Assessment"
print(plot(NSH.sam))
print(plot(NSH.stocks,key=TRUE,main="Comparison of assessments"))

#Plot catchabilities values
catch <- catchabilities(NSH.sam)
print(xyplot(value+ubnd+lbnd ~ age | fleet,catch,
          scale=list(alternating=FALSE,y=list(relation="free")),as.table=TRUE,
          type="l",lwd=c(2,1,1),col=c("black","grey","grey"),
          subset=fleet %in% c("HERAS","IBTS-Q1"),
          main="Survey catchability parameters",ylab="Catchability",xlab="Age"))

#Plot obs_variance (weightings)
obv <- obs.var(NSH.sam)
print(barchart(value ~ sprintf("%s",age)| fleet,obv,
       col="grey",ylim=range(pretty(c(0,obv$value))),
       as.table=TRUE,scale=list(alternating=FALSE),horizontal=FALSE,
       main="Observation Variances",ylab="Observation Variances",xlab="Age"))

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
print(xyplot(sel ~ age|sprintf("%i's",floor(year/5)*5),sel.pat,
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
