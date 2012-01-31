################################################################################
# NSH_SAM Control for "All-in" Assessment
#
# $Rev: 618 $
# $Date: 2011-11-11 16:42:31 +0100 (vr, 11 nov 2011) $
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
NSH.tun  <- NSH.tun[setdiff(names(NSH.tun),c("MLAI","IBTS-Q3"))] 
NSH.tun[["IBTS-Q1"]]  <- trim(NSH.tun[["IBTS-Q1"]],age=1)
NSH.ctrl <- FLSAM.control(NSH,NSH.tun)

#Modify default settings of control object
NSH.ctrl@states["catch",] <- seq(dims(NSH)$age) #number at age states move freely
NSH.ctrl@logN.vars[] <- c(1,rep(2,dims(NSH)$age-2),3) #Recruitment gets separate variance
NSH.ctrl@f.vars["catch",] <- c(rep(1,2),rep(2,5),rep(3,3))

#Update control
NSH.ctrl <- update(NSH.ctrl)

