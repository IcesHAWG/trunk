################################################################################
# FLSAM_Setup_Objects
#
# $Rev: 606 $
# $Date: 2011-11-10 19:50:13 +0100 (do, 10 nov 2011) $
#
# Author: HAWG model devlopment group
#
# Sets up the default FLSAM control object necessary to perform a 
# stock assessment. Script is intended to be called
# from other external sources.
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
### Setup default assessment configuration
### ============================================================================
#Setup configuration - creates an empty control object with appropriate structure
NSH.ctrl <- FLSAM.control(NSH,NSH.tun)

#Fishing mortality random walk coupling
#NSH.ctrl@states["catch",] <- c(1:7,rep(8,3))            #Couple age 7+ Fs
#NSH.ctrl@f.vars["catch",] <- 1                          #All have the same variance
#
##Log N random walk variances
#NSH.ctrl@logN.vars <- c(1,rep(2,9))
#
##Catchability models
#NSH.ctrl@catchabilities["IBTS-Q1",ac(3:5)] <- 101
#NSH.ctrl@catchabilities["HERAS",ac(6:9)] <- 102
#
##Observation model parameters
#NSH.ctrl@obs.vars["IBTS-Q1",ac(3:5)] <- 103
#NSH.ctrl@obs.vars["HERAS",ac(6:9)] <- 104
#NSH.ctrl@obs.vars["catch",ac(6:9)] <- 105
#
##Update
NSH.ctrl <- update(NSH.ctrl)

