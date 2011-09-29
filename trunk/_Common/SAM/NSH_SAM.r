################################################################################
# NSH_SAM Assessment
#
# $Rev$
# $Date$
#
# Author: HAWG model devlopment group
#
# Acts as a basic wrapper around the SAM framework for use in the assessment of 
# NSAS herring. Most of the interfacing is taken care of by the FLR packages
#
# Developed with:
#   - R version 2.8.1
#   - FLCore 2.2
#   - FLAssess, version 1.99-102
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
FnPrint     <-  function(string) {
	cat(string)
}
FnPrint("\nNSH SAM Assessment Wrapper\n==========================\n")

### ============================================================================
### Import externals
### ============================================================================
library(FLSAM)

#Load old ICA-based NSH assessessment objects from FLSAM package
#Normally you would use the FLCore functions to read the data
#files and create the objects. This is just a shortcut to achieve that
data(NSH)

### ============================================================================
### Configure assessment
### ============================================================================
#Setup configuration - creates an empty control object with appropriate structure
NSH.ctrl <- FLSAM.control(NSH,NSH.tun)

#Fishing mortality random walk coupling
NSH.ctrl@states["catch",] <- c(1:5,rep(6,5))            #Couple age 5+ Fs
NSH.ctrl@f.vars["catch",] <- 1                          #All have the same variance
#Log N random walk variances
NSH.ctrl@logN.vars <- c(1,rep(2,9))
#Catchability models
NSH.ctrl@catchabilities["IBTS0","0"] <- 1
NSH.ctrl@catchabilities["IBTS-Q1",ac(1:5)] <- 2:6
NSH.ctrl@catchabilities["HERAS",ac(1:9)] <- c(7:10, rep(11,5))    #Set linear catchability model
#Observation model parameters
NSH.ctrl@obs.vars["catch",] <- c(1,rep(2,9))
NSH.ctrl@obs.vars["IBTS0",] <- 3
NSH.ctrl@obs.vars["IBTS-Q1",] <- 4
NSH.ctrl@obs.vars["HERAS",] <- 5

#Remove 2010 data to be fully comparable with development version (baserun)
NSH@catch.n[,"2010"] <- NA
NSH.tun[["HERAS"]]@index[,"2010"] <- NA
NSH.tun[["MLAI"]]@index[,"2010"] <- NA

### ============================================================================
### Run the assessment
### ============================================================================
#Perform assessment
sam.out <- FLSAM(NSH,NSH.tun,NSH.ctrl)

#Update stock object
stck <- NSH + sam.out

### ============================================================================
### Diagnostic plots
### ============================================================================
#Survey fits
#survey.diagnostics(sam.out)

#Bubble plots - bit rough at moment, but anyway
res.dat <- sam.out@residuals
res.dat$data <- res.dat$std.res
p <-bubbles(age~year | fleet,res.dat)
print(p)

### ============================================================================
### Compare results
### ============================================================================
save(sam.out,file="NSH_sam_assessment.RData")
FnPrint(paste("COMPLETE IN",sprintf("%0.1f",round(proc.time()[3]-start.time,1)),"s.\n\n"))
