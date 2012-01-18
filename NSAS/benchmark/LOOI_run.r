################################################################################
# Looi run
#
# $Rev: 618 $
# $Date: 2011-11-11 16:42:31 +0100 (vr, 11 nov 2011) $
#
# Author: HAWG model devlopment group
#
# Applies a systematic scan of leave-one-in, leave-one-out and various subsets
# there of.
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
log.msg("\nLOOI run\n===========================\n")

### ============================================================================
### Import externals
### ============================================================================
log.msg("IMPORTING EXTERNAL RESOURCES...\n")
library(FLSAM)
source("Setup_objects.r")
source("Setup_default_FLSAM_control.r")

### ============================================================================
### Run the assessment
### ============================================================================
#Perform assessment
LOI.sams <- looi(NSH,NSH.tun,NSH.ctrl,type="LOI")
LOO.sams <- looi(NSH,NSH.tun,NSH.ctrl,type="LOO")

### ============================================================================
### Save results
### ============================================================================
save(NSH,NSH.tun,LOI.sams,LOO.sams,file=file.path(resdir,"LOOI.sams.RData"))

### ============================================================================
### Generate plots
### ============================================================================
#Extract terminal SSB and Fbars
LOI.ssbs <- subset(ssb(LOI.sams[-1]),year==2010)
LOI.fbars <- subset(fbar(LOI.sams[-1]),year==2010)
LOO.ssbs <- subset(ssb(LOO.sams[-1]),year==2010)
LOO.fbars <- subset(fbar(LOO.sams[-1]),year==2010)
full.ssbs <- subset(ssb(LOO.sams[1]),year==2010)
full.fbars <- subset(fbar(LOO.sams[1]),year==2010)

#Setup plots
xlims <- range(pretty(c(LOO.fbars$value,LOI.fbars$value)))
ylims <- range(pretty(c(LOO.ssbs$value,LOI.ssbs$value)))
pdf(file.path(resdir,"LOOI_run.pdf"))

#Generate plots
plot(NA,NA,xlab="Fbar",ylab="SSB",xlim=xlims,ylim=ylims,main="LOI / LOO runs")
points(full.fbars$value,full.ssbs$value,pch=15,col="green",cex=1.5)
points(LOI.fbars$value,LOI.ssbs$value,pch=16,col="red")
text(LOI.fbars$value,LOI.ssbs$value,LOI.ssbs$name,col="red",cex=0.5,pos=1,xpd=NA)
points(LOO.fbars$value,LOO.ssbs$value,pch=16,col="blue")
text(LOO.fbars$value,LOO.ssbs$value,LOO.ssbs$name,col="blue",cex=0.5,pos=1,xpd=NA)
legend("topright",pch=16,col=c("red","blue"),legend=c("Leave-in","Leave-out"))
dev.off()

### ============================================================================
### Save results
### ============================================================================
log.msg(paste("COMPLETE IN",sprintf("%0.1f",round(proc.time()[3]-start.time,1)),"s.\n\n"))
