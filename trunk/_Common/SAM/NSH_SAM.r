######################################################################################################
# NSH_SAM Assessment
#
# $Rev$
# $Date$
#
# Author: HAWG model devlopment group
#
# Acts as a basic wrapper around the SAM framework for use in the assessment of NSAS herring
# Most of the interfacing is taken care of by the FLR packages
#
# Developed with:
#   - R version 2.8.1
#   - FLCore 2.2
#   - FLAssess, version 1.99-102
#
# To be done:
#
# Notes: Have fun running this assessment!
#
####################################################################################################

### ======================================================================================================
### Initialise system, including convenience functions and title display
### ======================================================================================================
rm(list=ls()); gc(); graphics.off(); start.time <- proc.time()[3]
options(stringsAsFactors=FALSE)
FnPrint     <-  function(string) {
	cat(string)
}
FnPrint("\nNSH SAM Assessment Wrapper\n==========================\n")

### ======================================================================================================
### Import externals
### ======================================================================================================
library(FLCore);
#Load NSH assessessment objects
load(file.path("..","..","NSAS","results","NSH Assessment Assessment.RData"))

#FLSAM package (uncompiled)
FLSAM.dir <- file.path(".","FLSAM")
FLSAM.r.srcs <- dir(file.path(FLSAM.dir,"R"),pattern="r$|R$",full.names=TRUE)
dmp <- lapply(FLSAM.r.srcs,source)

### ======================================================================================================
### Configure assessment
### ======================================================================================================
#Setup configuration
NSH.ctrl <- FLSAM.control(NSH,NSH.tun)
#Fishing mortality random walk coupling
NSH.ctrl@states["catch",] <- c(1:5,rep(6,5))            #Couple age 5+ Fs
NSH.ctrl@f.vars["catch",] <- 1                          #All have the same variance
#Log N random walk variances
NSH.ctrl@logN.vars <- c(1,rep(2,9))
#Catchability models
NSH.ctrl@catchabilities["HERAS",ac(1:9)] <- c(1:4, rep(5,5))    #Set linear catchability model
NSH.ctrl@catchabilities["IBTS-Q1",ac(1:5)] <- 6:10
NSH.ctrl@catchabilities["IBTS0","0"] <- 11
#Observation model parameters
NSH.ctrl@obs.vars["catch",] <- c(1,rep(2,9))
NSH.ctrl@obs.vars["IBTS0",] <- 5
NSH.ctrl@obs.vars["IBTS-Q1",] <- 4
NSH.ctrl@obs.vars["HERAS",] <- 3

### ======================================================================================================
### Run the assessment
### ======================================================================================================
stck <- NSH
tun  <- NSH.tun
ctrl <- NSH.ctrl

#Remove 2010 data to be fully comparable with development version
stck@catch.n[,"2010"] <- NA
tun[["HERAS"]]@index[,"2010"] <- NA
tun[["MLAI"]]@index[,"2010"] <- NA

#Write configuration file
wkdir <- file.path(".","run")
write.ADMB.dat(stck,tun,file.path(wkdir,"ssass.dat"))
write.ADMB.cfg(ctrl,file.path(wkdir,"model.cfg"))

#Run the assessment
olddir <- setwd(wkdir)
if(.Platform$OS.type=="windows") {
  shell("ssass.exe",mustWork=TRUE)
} else {
  system("ssass",mustWork=TRUE)
}
setwd(olddir)

#Load results
NSH.sam.out <- read.ADMB.outputs(file.path(".","run","ssass"),stck,ctrl)

#Update stock object
NSH.sam <- NSH + NSH.sam.out

### ======================================================================================================
### Compare results
### ======================================================================================================
stcks <- FLStocks(SAM=NSH.sam,ICA=NSH)
plot(stcks,key=TRUE)
save(NSH.sam.out,stcks,file="NSH_sam_assessment.RData")