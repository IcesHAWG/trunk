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
source("Setup_FLSAM_control.r")
source("SUSAM_diagnostics.r")

### ============================================================================
### Run the assessment
### ============================================================================
#Perform assessment
NSH.sam <- FLSAM(NSH,NSH.tun,NSH.ctrl)

#Update stock object
NSH.sam.ass <- NSH + NSH.sam
NSH.stocks <- FLStocks(FLSAM=NSH.sam.ass, FLICA=NSH)

### ============================================================================
### Plots
### ============================================================================
pdf(file.path(resdir,"NSH_SAM_assessment.pdf"))

#Plot result
NSH.sam@name <- "North Sea Herring FLSAM Assessment"
print(plot(NSH.sam))
print(plot(NSH.stocks,key=TRUE,main="Comparison of assessments"))

#Plot catchabilities values
catch <- catchabilities(NSH.sam)
catch$ubn <- catch$value + 1.96*catch$std.dev
catch$lbn <- catch$value - 1.96*catch$std.dev
print(xyplot(exp(value)+exp(ubn)+exp(lbn) ~ age | fleet,catch,
          scale=list(alternating=FALSE,y=list(relation="free")),as.table=TRUE,
          type="l",lwd=c(2,1,1),col=c("black","grey","grey"),
          subset=fleet %in% c("HERAS","IBTS-Q1"),
          main="Survey catchability parameters",ylab="Catchability",xlab="Age"))

#Plot obs_variance (weightings)
obv <- obs.var(NSH.sam)
print(barchart(exp(value) ~ age | fleet,obv,
       col="grey",ylim=range(pretty(c(0,exp(obv$value)))),
       as.table=TRUE,scale=list(alternating=FALSE),
       main="Observation Variances",ylab="Observation Variances",xlab="Age"))

#Plot selectivity pattern over time
sel.pat <- merge(as.data.frame(harvest(NSH.sam)),as.data.frame(fbar(NSH.sam)),
             by="year",suffixes=c(".f",".fbar"))
sel.pat$sel <- sel.pat$data.f/sel.pat$data.fbar
print(xyplot(sel ~ year|sprintf("Age %02i",age.f),sel.pat,
         type="l",as.table=TRUE,
         main="Selectivity of the Fishery",xlab="Year",ylab="F/Fbar",
         scale=list(alternating=FALSE)))
print(xyplot(sel ~ age.f|sprintf("%i's",floor(year/5)*5),sel.pat,
         groups=year,type="l",as.table=TRUE,
         scale=list(alternating=FALSE),
         main="Selectivity of the Fishery by Pentade",xlab="Age",ylab="F/Fbar"))

#Survey fits
residual.diagnostics(NSH.sam)

### ============================================================================
### Finish
### ============================================================================
dev.off()
save(NSH.sam,NSH.ctrl,NSH.sam.ass,file=file.path(resdir,"NSH.sam.RData"))
log.msg(paste("COMPLETE IN",sprintf("%0.1f",round(proc.time()[3]-start.time,1)),"s.\n\n"))
