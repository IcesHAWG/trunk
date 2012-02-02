################################################################################
# NSH_SAM Control for "All-in" Assessment
#
# $Rev$
# $Date$
#
# Author: HAWG model devlopment group
#
# Sets up a control object for use by Step 02 assessments i.e. the "All_in" run
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
### Setup assessment
### ============================================================================
#Exclude MLAI index
NSH.tun  <- NSH.tun[setdiff(names(NSH.tun),"MLAI")] 

#Get default settings of control object
NSH.ctrl <- FLSAM.control(NSH,NSH.tun)

#Set the variances. Separate variance for recruitment and plus group
#Fishing mortality RWs are set from an analysis of ICA VPA results
NSH.ctrl@logN.vars[] <- c(1,rep(2,dims(NSH)$age-1)) 
NSH.ctrl@f.vars["catch",] <- c(rep(1,2),rep(2,8))

#All fishing mortality states are free except 
#oldest ages to ensure stablity
NSH.ctrl@states["catch",] <- seq(dims(NSH)$age) 
NSH.ctrl@states["catch",ac(8:9)] <- 101

#Group observation variances of catches to ensure stability
NSH.ctrl@obs.vars["catch",ac(0:1)] <- 201
NSH.ctrl@obs.vars["catch",ac(2:5)] <- 202
NSH.ctrl@obs.vars["catch",ac(6:8)] <- 203
NSH.ctrl@obs.vars["catch",ac(9)]   <- 204

#Update control
NSH.ctrl <- update(NSH.ctrl)
NSH.ctrl@name <- "Step02"
