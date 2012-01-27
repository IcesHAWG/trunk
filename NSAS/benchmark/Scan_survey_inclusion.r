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
### Run the assessment
### ============================================================================
#Perform assessment
LOOI.sams <- looi(NSH,NSH.tun,NSH.ctrl,type="full")

### ============================================================================
### Save results
### ============================================================================
save(NSH,NSH.tun,LOOI.sams,file=file.path(".","benchmark","resultsSAM","Scan_survey_inclusion.RData"))

### ============================================================================
### Generate plots
### ============================================================================
#Extract terminal SSB and Fbars
LOOI.ssbs    <- subset(ssb(LOOI.sams[-1]),  year==2010)
LOOI.fbars   <- subset(fbar(LOOI.sams[-1]), year==2010)

#Setup plots
xlims <- range(pretty(c(LOOI.fbars$value,LOOI.fbars$value)))
ylims <- range(pretty(c(LOOI.ssbs$value,LOOI.ssbs$value)))
pdf(file.path(".","benchmark","resultsSAM","LOOI_run.pdf"))

#Generate plots
plot(NA,NA,xlab="Fbar",ylab="SSB",xlim=xlims,ylim=ylims,main="LOI / LOO runs")
points(LOOI.fbars$value,LOOI.ssbs$value,pch=15,col="red",cex=1.5)
text(LOOI.fbars$value,LOOI.ssbs$value,LOOI.ssbs$name,col="red",cex=0.5,pos=1,xpd=NA)
dev.off()

### ============================================================================
### Save results
### ============================================================================
log.msg(paste("COMPLETE IN",sprintf("%0.1f",round(proc.time()[3]-start.time,1)),"s.\n\n"))
