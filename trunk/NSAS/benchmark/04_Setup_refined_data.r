################################################################################
# NSH_SAM Control for "refined data" Assessment (Step 04)
#
# $Rev$
# $Date$
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

### ============================================================================
### Setup data objects
### ============================================================================
#Select surveys
NSH.tun  <- NSH.tun[setdiff(names(NSH.tun),c("MLAI","IBTS-Q3"))] 
NSH.tun[["IBTS-Q1"]]  <- trim(NSH.tun[["IBTS-Q1"]],age=1)

#Set plus group to age 8
pg <- 8
NSH <- setPlusGroup(NSH,pg)
NSH.tun[["HERAS"]]@index[ac(pg),] <- quantSums(NSH.tun[["HERAS"]]@index[ac(pg:9),])
NSH.tun[["HERAS"]] <- trim(NSH.tun[["HERAS"]],age=1:pg)
NSH.tun[["HERAS"]]@range["plusgroup"] <- pg

#Drop 1970s problematic catches
NSH@catch.n[,ac(1978:1979)] <- NA

### ============================================================================
### Setup control object
### ============================================================================
#Get default settings of control object
NSH.ctrl <- FLSAM.control(NSH,NSH.tun)

#Set the variances. Separate variance for recruitment and plus group
#Fishing mortality RWs are set from an analysis of ICA VPA results
NSH.ctrl@logN.vars[] <- c(1,rep(2,dims(NSH)$age-1)) 
NSH.ctrl@f.vars["catch",] <- c(rep(1,2),rep(2,7))

#All fishing mortality states are free except 
#oldest ages to ensure stablity
NSH.ctrl@states["catch",] <- seq(dims(NSH)$age) 
NSH.ctrl@states["catch",ac(7:8)] <- 101

#Group observation variances of catches to ensure stability
NSH.ctrl@obs.vars["catch",ac(0:1)] <- 201
NSH.ctrl@obs.vars["catch",ac(2:5)] <- 202
NSH.ctrl@obs.vars["catch",ac(6:7)] <- 203
NSH.ctrl@obs.vars["catch",ac(8)]   <- 204

#Finalise
NSH.ctrl@name <- "Refined_data"
NSH.ctrl <- update(NSH.ctrl)

