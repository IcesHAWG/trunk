################################################################################
# NSH_SAM Retrospective
#
# $Rev: 618 $
# $Date: 2011-11-11 16:42:31 +0100 (vr, 11 nov 2011) $
#
# Author: HAWG model devlopment group
#
# Performs a retrospective analysis of the NSAS Herring stock using the SAM method
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
### Run the assessment
### ============================================================================
#Perform assessment
NSH.retros <- retro(NSH,NSH.tun,NSH.ctrl,retro=10)

### ============================================================================
### Save results
### ============================================================================
save(NSH,NSH.tun,NSH.retros,file=file.path(".","benchmark","resultsSAM","Retrospective.RData"))

### ============================================================================
### Analysis
### ============================================================================
#Make FLStocks
NSH.stcks <- NSH + NSH.retros

pdf(file.path(".","benchmark","resultsSAM","Retrospective.pdf"))
#Plot result
print(plot(NSH.stcks,key=TRUE,main="Retrospective analysis"))
dev.off()

### ============================================================================
### Finish
### ============================================================================
log.msg(paste("COMPLETE IN",sprintf("%0.1f",round(proc.time()[3]-start.time,1)),"s.\n\n"))
