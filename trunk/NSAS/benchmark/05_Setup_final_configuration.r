################################################################################
# NSH_SAM Control for "Final" Assessment
#
# $Rev$
# $Date$
#
# Author: HAWG model devlopment group
#
# Sets up a control object for use by Step 05 assessments i.e. the "final" run
# Configuration is based on the 04_refined_data setup configuration
# with some further refinements to the model parameters
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
#Use Step04 as the basis for the default settings
source(file.path("benchmark","04_Setup_refined_data.r"))

#Release f.vars, bind obs.variances
NSH.ctrl@f.vars["catch",] <-  c(1,1,2,2,3,3,4,4,4)
NSH.ctrl@obs.vars["catch",] <- c(100,rep(101,5),rep(106,3))

#Bind HERAS 
NSH.ctrl@obs.vars["HERAS",ac(1:8)] <- c(101,rep(102,4),rep(106,3))
NSH.ctrl@catchabilities["HERAS",ac(1:8)] <- c(rep(101,2),rep(103,2),rep(105,4))

#Round off changes
NSH.ctrl <- update(NSH.ctrl)
NSH.ctrl@name <- "Final_cfg"

