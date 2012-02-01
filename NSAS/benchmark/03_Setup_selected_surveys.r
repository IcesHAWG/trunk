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
#Exclude MLAI index, nearly all of IBTS-Q1 and all of IBTS-Q3
NSH.tun  <- NSH.tun[setdiff(names(NSH.tun),c("MLAI","IBTS-Q3"))] 
NSH.tun[["IBTS-Q1"]]  <- trim(NSH.tun[["IBTS-Q1"]],age=1)

#Use Step02 as the basis for the default settings
source(file.path("benchmark","02_Setup_All_in_runs.r"))

#Modifications for Step03
NSH.ctrl@name <- "Step03"
