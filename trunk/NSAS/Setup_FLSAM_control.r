################################################################################
# FLSAM_Setup_Objects
#
# $Rev$
# $Date$
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
NSH.ctrl@states["catch",] <- c(1:5,rep(6,5))            #Couple age 5+ Fs
NSH.ctrl@f.vars["catch",] <- 1                          #All have the same variance

#Log N random walk variances
NSH.ctrl@logN.vars <- c(1,rep(2,9))

#Catchability models
NSH.ctrl@catchabilities["IBTS0","0"] <- 1
NSH.ctrl@catchabilities["IBTS-Q1",ac(1:5)] <- 2:6
#NSH.ctrl@catchabilities["HERAS",ac(1:9)] <- c(7:10, rep(11,5))    #Set linear catchability model
NSH.ctrl@catchabilities["HERAS",ac(1:9)] <- 7:15
#NSH.ctrl@catchabilities["MLAI","6"] <- 16
#NSH.ctrl@power.law.exps["MLAI","6"] <- 1

#Observation model parameters
#NSH.ctrl@obs.vars["catch",] <- c(1,rep(2,9))
#NSH.ctrl@obs.vars["IBTS0","0"] <- 3
#NSH.ctrl@obs.vars["IBTS-Q1",ac(1:5)] <- 4
#NSH.ctrl@obs.vars["HERAS",ac(1:9)] <- 5
NSH.ctrl@obs.vars["catch",] <- 1:10
NSH.ctrl@obs.vars["IBTS0","0"] <- 11
NSH.ctrl@obs.vars["IBTS-Q1",ac(1:5)] <- 12:16
NSH.ctrl@obs.vars["HERAS",ac(1:9)] <- 17:25
#NSH.ctrl@obs.vars["MLAI","6"] <- 6

