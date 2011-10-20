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
### Misc
### ============================================================================
resdir <- file.path(".","results")
data.source         <-  file.path(".","data")                   #Data source, not code or package source!!!

### ======================================================================================================
### Prepare stock object for assessment
### ======================================================================================================
log.msg("PREPARING STOCK OBJECT...\n")
NSH                               <- readFLStock(file.path(data.source, "index.txt"),no.discards=TRUE)

#Catch is calculated from: catch.wt * catch.n, however, the reported landings are
#normally different (due to SoP corrections). Hence we overwrite the calculate landings
NSH@catch                         <- NSH@landings
units(NSH)[1:17]                  <- as.list(c(rep(c("tonnes","thousands","kg"),4), rep("NA",5)))

#Set object details
NSH@name                          <- "North Sea Herring"
range(NSH)[c("minfbar","maxfbar")]<- c(2,6)
NSH                               <- setPlusGroup(NSH,NSH@range["max"])

#No catches of age 10 in 1977 so stock.wt does not get filled there.
# Hence, we copy the stock weight for that age from the previous year
NSH@stock.wt[10,"1977"]           <- NSH@stock.wt[10,"1976"]

### ======================================================================================================
### Prepare index object for assessment
### ======================================================================================================
log.msg("PREPARING INDEX OBJECT...\n")

#Load and modify all index data
NSH.tun                         <- readFLIndices(file.path(data.source,"/fleet.txt"),file.path(data.source,"/ssb.txt"),type="ICA")

#Set names
NSH.tun[[1]]@name               <- "HERAS"
NSH.tun[[2]]@name               <- "IBTS-Q1"
NSH.tun[[3]]@name               <- "IBTS0"
NSH.tun[[4]]@name               <- "MLAI"
names(NSH.tun)                  <- lapply(NSH.tun,name)

#Set parameters etc
NSH.tun[["HERAS"]]@type               <- "number"
NSH.tun[["IBTS-Q1"]]@type               <- "number"
NSH.tun[["IBTS0"]]@type               <- "number"
NSH.tun[["IBTS0"]]@range["plusgroup"] <- NA

### give 1/weighting factors as variance
NSH.tun[["HERAS"]]@index.var[]        <- 1.0/FLQuant(c(0.63,0.62,0.17,0.10,0.09,0.08,0.07,0.07,0.05),dimnames=dimnames(NSH.tun[["HERAS"]]@index)) #Acoustic
NSH.tun[["IBTS-Q1"]]@index.var[]        <- 1.0/FLQuant(c(0.47,0.28,0.01,0.01,0.01),dimnames=dimnames(NSH.tun[["IBTS-Q1"]]@index)) #IBTS
NSH.tun[["IBTS0"]]@index.var[]        <- 1.0/FLQuant(0.63,dimnames=dimnames(NSH.tun[["IBTS0"]]@index)) #MIK
NSH.tun[["MLAI"]]@index.var[]        <- 1.0/FLQuant(0.60,dimnames=dimnames(NSH.tun[["MLAI"]]@index)) #MLAI

#FLICA requires that biomass indices ie MLAI, is the first index
NSH.tun                         <- rev(NSH.tun)
if (NSH.tun[[1]]@name != "MLAI") print("Error - MLAI not as the first index")

