################################################################################
# NSH_SAM Control for "Final" Assessment
#
# $Rev$
# $Date$
#
# Author: HAWG model devlopment group
#
# Sets up a control object for use by Step 05 assessments i.e. the "final" run
# Configuration is based on the 03_selected_surveys setup configuration
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
#Use Step03 as the basis for the default settings
source(file.path("benchmark","03_Setup_selected_surveys.r"))

#Bind HERAS observation variances
NSH.ctrl@obs.vars["HERAS",ac(2:9)] <- 301

#Round off changes
NSH.ctrl <- update(NSH.ctrl)
NSH.ctrl@name <- "Step05"

