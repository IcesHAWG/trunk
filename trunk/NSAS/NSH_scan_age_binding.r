################################################################################
# NSH Scan Age Binding
#
# $Rev$
# $Date$
#
# Author: HAWG model devlopment group
#
# The FLSAM model has the ability to bind the catchability of age groups
# together, effectively using one parameter for many age groups. The appropriate
# bindings can be identified by performing a series of assessments for each
# combination and then comparing the results using AIC / LR tests.
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
rm(list=ls()); gc(); graphics.off(); start.time <- proc.time()[3]
options(stringsAsFactors=FALSE)
log.msg     <-  function(string) {
	cat(string);flush.console()
}
log.msg("\nNSH scan age bindings\n=====================\n")

### ============================================================================
### Import externals
### ============================================================================
log.msg("IMPORTING EXTERNAL RESOURCES...\n")
library(FLSAM)
source("NSH_setup_objects.r")

### ============================================================================
### Configure the default assessment
### ============================================================================
log.msg("CONFIGURING ASSESSMENT......\n")
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

#Now scan through the HERAS ages, tying them sequentlly together
HERAS.ctrls <- list()
for(i in 1:9) {
  ctrl <- NSH.ctrl
  ctrl@catchabilities["HERAS",ac(1:9)] <- 7
  ctrl@catchabilities["HERAS",ac(i:9)] <- 6+i
  ctrl@name <- ac(i)
  ctrl@desc <- sprintf("Age %i+ catchabilities bound together",i)
  HERAS.ctrls[[i]] <- ctrl
}

### ============================================================================
### Run the assessment
### ============================================================================
#Perform assessment
HERAS.sams <- lapply(HERAS.ctrls,FLSAM,stck=NSH,tun=NSH.tun)

### ============================================================================
### Analyse the results
### ============================================================================


### ============================================================================
### Compare results
### ============================================================================
save(HERAS.sams,file="HERAS_age_scan.RData")
FnPrint(paste("COMPLETE IN",sprintf("%0.1f",round(proc.time()[3]-start.time,1)),"s.\n\n"))
