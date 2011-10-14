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

### ============================================================================
### Run the assessment
### ============================================================================
#Perform assessment
NSH.sam <- FLSAM(NSH,NSH.tun,NSH.ctrl)

#Update stock object
NSH.sam.ass <- NSH + NSH.sam

### ============================================================================
### Plots
### ============================================================================
pdf(file.path(resdir,"NSH_SAM_assessment.pdf"))

#Survey fits
#survey.diagnostics(sam.out)

#Bubble plots - bit rough at moment, but anyway
res.dat <- residuals(NSH.sam)
res.dat$data <- res.dat$std.res
p <-bubbles(age~year | fleet,res.dat)
print(p)

#Plot result
plot(NSH.sam.ass)

### ============================================================================
### Finish
### ============================================================================
dev.off()
save(NSH.sam,NSH.ctrl,NSH.sam.ass,file=file.path(resdir,"NSH.sam.RData"))
log.msg(paste("COMPLETE IN",sprintf("%0.1f",round(proc.time()[3]-start.time,1)),"s.\n\n"))
