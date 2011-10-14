################################################################################
# NSH_Setup_Objects
#
# $Rev$
# $Date$
#
# Author: HAWG model devlopment group
#
# Sets up the data objects (specifically an FLStock and an FLIndices object)
# necessary to perform a stock assessment. Necessary preprocessing, such as
# the smoothing of weights, is performed here. Script is intended to be called
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
### Setup object
### ============================================================================
#Load old ICA-based NSH assessessment objects from FLSAM package
#Normally you would use the FLCore functions to read the data
#files and create the objects. This is just a shortcut to achieve that
data(NSH)

#Remove 2010 data to be fully comparable with development version (baserun)
NSH@catch.n[,"2010"] <- NA
NSH.tun[["HERAS"]]@index[,"2010"] <- NA
NSH.tun[["MLAI"]]@index[,"2010"] <- NA

