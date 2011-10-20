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
source("SUSAM_diagnostics.r")

### ============================================================================
### Run the assessment
### ============================================================================
#Perform assessment
NSH.sam <- FLSAM(NSH,NSH.tun,NSH.ctrl)

#Update stock object
NSH.sam.ass <- NSH + NSH.sam
NSH.stocks <- FLStocks(NSH.sam.ass, NSH)

### ============================================================================
### Plots
### ============================================================================
pdf(file.path(resdir,"NSH_SAM_assessment.pdf"))

#Plot result
print(plot(NSH.sam.ass))
print(plot(NSH.stocks))

#Survey fits
residual.diagnostics(NSH.sam)

#Bubble plots - bit rough at moment, but anyway
res.dat <- residuals(NSH.sam)
res.dat$data <- res.dat$std.res
p <-bubbles(age~year | fleet,res.dat)
print(p)

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

### ============================================================================
### Finish
### ============================================================================
dev.off()
save(NSH.sam,NSH.ctrl,NSH.sam.ass,file=file.path(resdir,"NSH.sam.RData"))
log.msg(paste("COMPLETE IN",sprintf("%0.1f",round(proc.time()[3]-start.time,1)),"s.\n\n"))
