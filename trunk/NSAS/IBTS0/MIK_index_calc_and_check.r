##############################################################################
# MIK Index Calculation and Check
#
# $Rev: 619 $
# $Date: 2011-11-25 14:59:55 +0100 (Fri, 25 Nov 2011) $
#
# Author: Mark Payne, mpa@aqua.dtu.dk
# DTU-Aqua, Charlottenlund, DK
# 
# Calculates the MIK index in the standard manner (e.g. following the SAS script
# of P. Munk) and compares the results with the results of those calculations
#
# Developed with:
#   - R version 2.13.0
#   - sp package version 0.9-84
#
# Changes:
#
# To be done:
#
# Notes:
#
# ----------------------------------------------------------------------------
# "THE BEER-WARE LICENSE" (Revision 42):
# Mark Payne wrote this file. As long as you retain this notice, you can do
# whatever you want with this stuff - just don't hold me responsible for it.
# If we meet some day, and you think still this stuff is worth it, you
# can buy me a beer in return. Mark Payne (with kudos to Poul-Henning Kamp)
# ----------------------------------------------------------------------------
##############################################################################

### ============================================================================
### Initialise system, including convenience functions and title display
### ============================================================================
rm(list=ls()); graphics.off(); start.time <- proc.time()[3]
options(stringsAsFactors=FALSE)
log.msg     <-  function(string) {
	cat(string);flush.console()
}

#Source externals
source(file.path(".","IBTS0_common_elements.r"))

log.msg("\nMIK Index Calculation and Check\n===============================\n")

### ==========================================================================
### Parameters
### ==========================================================================
#Output file
pdf(file.path(out.dir,"MIK_index_checks.pdf"),width=210/25.4,height=210/25.4)

### ==========================================================================
### Retrieve data and calculate index
### ==========================================================================
#load data
db <- read.IBTS0()

#Calculate index using defaults
calc.MIK.idx <- calc.MIK.index(db) 

### ==========================================================================
### Compare the old and the new calculations
### ==========================================================================
#There exists two different versions of the MIK. The first is the "historic"
#MIK, ie that which was published. However, applying the SAS code to the
#database doesn't give quite the same answer, due to various modifications
#that were made along the way. Hence, we talk about the "recalculated" and
#the "historic" data sets
#Load historic data
hist.MIK.index <- read.csv(file.path(dat.dir,"Historic_MIK_index.csv"))

#Merge data sources
calc.MIK.idx$calc.here <- calc.MIK.idx$index
MIK.idxs <- merge(calc.MIK.idx[,c("year","calc.here")],hist.MIK.index)

#Compare the as-calculated here with the re-calculated MIK (the most appropriate)
lims <- range(pretty(MIK.idxs$calc.here,MIK.idxs$Recalculated.MIK))
plot(MIK.idxs$Recalculated.MIK,MIK.idxs$calc.here,xlim=lims,ylim=lims,pch=16,
    ylab="MIK index calculated here",xlab="Recalculated (SAS) MIK",
    main="Comparison of MIK calculated here and using SAS script")
text(MIK.idxs$Recalculated.MIK,MIK.idxs$calc.here,sprintf("%02i",MIK.idxs$year%%100),pos=4)
abline(a=0,b=1)

#Deviations between as recalculated MIK and as calculated here
devs <- (MIK.idxs$calc.here-MIK.idxs$Recalculated.MIK)/MIK.idxs$Recalculated.MIK*100
plot(MIK.idxs$year,devs,type="h",xlab="Year",ylab="Error in calcs here (%)",lwd=2,
   main="Error between MIK calculated here and using SAS script")
abline(h=0,lwd=2)

#Relationship between as published MIK and recalculated
lims <- range(pretty(MIK.idxs$hist.MIK,MIK.idxs$Recalculated.MIK))
plot(MIK.idxs$hist.MIK,MIK.idxs$Recalculated.MIK,xlim=lims,ylim=lims,pch=16,
    xlab="As Published HAWG MIK",ylab="Recalculated MIK",
    main="Comparison of as published MIK and SAS-script MIK")
text(MIK.idxs$hist.MIK,MIK.idxs$Recalculated.MIK,sprintf("%02i",MIK.idxs$year%%100),pos=4)
abline(a=0,b=1)

#Deviations between as published MIK and recalculated
devs <- (MIK.idxs$Recalculated.MIK-MIK.idxs$hist.MIK)/MIK.idxs$hist.MIK*100
plot(MIK.idxs$year,devs,type="h",xlab="Year",ylab="Change in MIK upon recalculation (%)",
   lwd=2,main="Error between as-published MIK and SAS-script MIK")
abline(h=0,lwd=2)

### ==========================================================================
### write output and finish
### ==========================================================================
calc.MIK.idx$calc.here <- NULL
write.csv(calc.MIK.idx,file=file.path(out.dir,"MIK_indices.csv"),row.names=FALSE,na="")

#Finish
dev.off()
log.msg(paste("COMPLETE IN",sprintf("%0.1f",round(proc.time()[3]-start.time,1)),"s.\n\n"))

