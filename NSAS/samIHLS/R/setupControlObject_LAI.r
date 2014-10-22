################################################################################
# NSH_SAM Control for Assessment
#
# $Rev: 705 $
# $Date: 2012-02-14 19:02:57 +0100 (di, 14 feb 2012) $
#
# Author: HAWG model devlopment group
#
# Sets up a control object for use by Step 04 assessments i.e. the "refined data" run
#
# Developed with:
#   - R version 2.13.0
#   - FLCore 2.4
#
# To be done:
#
# Notes: Have fun running this assessment!
#
################################################################################

NSH.ctrl <- FLSAM.control(NSH,NSH.tun[1:3])
NSH.ctrl@fleets <- c("catch"=0,"HERAS"=2,"IBTS-Q1"=2,"IBTS0"=2,"LAI"=5)

NSH.ctrl@states <- rbind(NSH.ctrl@states,"LAI"=rep(NA,9))
NSH.ctrl@catchabilities <- rbind(NSH.ctrl@catchabilities,"LAI"=rep(NA,9))
NSH.ctrl@power.law.exps <- rbind(NSH.ctrl@power.law.exps,"LAI"=rep(NA,9))
NSH.ctrl@f.vars <- rbind(NSH.ctrl@f.vars,"LAI"=rep(NA,9))
NSH.ctrl@obs.vars <- rbind(NSH.ctrl@obs.vars,"LAI"=rep(NA,9))

#Set the variances. Separate variance for recruitment and plus group
#Fishing mortality RWs are set from an analysis of ICA VPA results
NSH.ctrl@logN.vars[]      <- c(1,rep(2,dims(NSH)$age-1))
NSH.ctrl@f.vars["catch",] <- c(rep(1,2),rep(2,2),rep(3,2),rep(4,3))

#All fishing mortality states are free except
#oldest ages to ensure stablity
NSH.ctrl@states["catch",] <- seq(dims(NSH)$age)
NSH.ctrl@states["catch",ac(7:8)] <- 101

#Group observation variances of catches to ensure stability
NSH.ctrl@obs.vars["catch",ac(0)]  <- 201
NSH.ctrl@obs.vars["catch",ac(1:5)]  <- 202
NSH.ctrl@obs.vars["catch",ac(6:8)]  <- 203
#NSH.ctrl@obs.vars["catch",ac(8)]    <- 204

NSH.ctrl@obs.vars["HERAS",ac(1)]    <- 301
NSH.ctrl@obs.vars["HERAS",ac(2:5)]  <- 302
NSH.ctrl@obs.vars["HERAS",ac(6:8)]  <- 303

NSH.ctrl@obs.vars["LAI",ac(0:3)]  <- 401:401

#Group catchability parametesr
NSH.ctrl@catchabilities["HERAS",ac(1:8)]  <- c(rep(101,2),rep(102,2),rep(103,4))
#NSH.ctrl@catchabilities["HERAS",ac(1:8)]  <- 101:108

NSH.ctrl@sam.binary <- "ADMB/sam.exe"


#Finalise
NSH.ctrl@name <- "Final Assessment"
NSH.ctrl <- update(NSH.ctrl)

