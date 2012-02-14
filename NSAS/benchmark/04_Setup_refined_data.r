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
### Setup assessment
### ============================================================================
#Use Step03 as the basis for the default settings
source(file.path("benchmark","03_Setup_selected_surveys.r"))

#Modifications for Step04
NSH.ctrl@name <- "Step04"

#Set plus group to age 8
pg <- 8
NSH <- setPlusGroup(NSH,pg)
NSH.tun[["HERAS"]]@index[ac(pg),] <- quantSums(NSH.tun[["HERAS"]]@index[ac(pg:9),])
NSH.tun[["HERAS"]] <- trim(NSH.tun[["HERAS"]],age=1:pg)
NSH.tun[["HERAS"]]@range["plusgroup"] <- pg
NSH.ctrl <- drop.from.control(NSH.ctrl,ages=pg:NSH.ctrl@range["max"]+1)
NSH.ctrl@states["catch",ac((pg-1):pg)] <- 101
NSH.ctrl@obs.vars["catch",ac(pg)] <- 201
NSH.ctrl@range[c("max","plusgroup")] <- pg
NSH.ctrl <- update(NSH.ctrl)

#Drop 1970s problematic catches
NSH@catch.n[,ac(1978:1979)] <- NA

