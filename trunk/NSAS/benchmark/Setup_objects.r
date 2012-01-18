################################################################################
# NSH_Setup_Objects
#
# $Rev: 634 $
# $Date: 2012-01-18 14:21:05 +0100 (wo, 18 jan 2012) $
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
resdir              <- file.path(".","results")
data.source         <- file.path(".","data")    #Data source, not code or package source!!!

### ============================================================================
### Prepare stock object for assessment
### ============================================================================
NSH                 <- readFLStock(file.path(data.source, "index.txt"),no.discards=TRUE)

#Catch is calculated from: catch.wt * catch.n, however, the reported landings are
#normally different (due to SoP corrections). Hence we overwrite the calculate landings
NSH@catch           <- NSH@landings
units(NSH)[1:17]    <- as.list(c(rep(c("tonnes","thousands","kg"),4), rep("NA",5)))

#Set object details
NSH@name                              <- "North Sea Herring"
range(NSH)[c("minfbar","maxfbar")]    <- c(2,6)
NSH                                   <- setPlusGroup(NSH,NSH@range["max"])

#No catches of age 9 in 1977 so stock.wt does not get filled there.
#Hence, we copy the stock weight for that age from the previous year.
#Note that because we use a fixed stock.wt prior to 1983, there is no
#need to use averaging or anything fancier
NSH@stock.wt["9","1977"]              <- NSH@stock.wt["9","1976"]

#Use mean(y-2,y-1,y) of west_raw to calculate west
NSH@stock.wt[,3:dim(NSH@stock.wt)[2]] <- (NSH@stock.wt[,3:(dim(NSH@stock.wt)[2]-0)] +
                                          NSH@stock.wt[,2:(dim(NSH@stock.wt)[2]-1)] +
                                          NSH@stock.wt[,1:(dim(NSH@stock.wt)[2]-2)]) / 3


### ============================================================================
### Prepare index object for assessment
### ============================================================================
#Load and modify all index data
NSH.tun             <- readFLIndices(file.path(data.source,"/fleet.txt"),
                                     file.path(data.source,"/ssb.txt"),type="ICA")
                           
#Set names
NSH.tun[[1]]@name   <- "HERAS"
NSH.tun[[2]]@name   <- "IBTS-Q1"
NSH.tun[[3]]@name   <- "IBTS-Q3"
NSH.tun[[4]]@name   <- "IBTS0"
NSH.tun[[5]]@name   <- "MLAI"
names(NSH.tun)      <- lapply(NSH.tun,name)

#Load additional SCAI index
NSH.scai            <- readFLIndices(file.path(data.source,"/fleet.txt"),
                                     file.path(data.source,"/scai.txt"),type="ICA")
#Combine all surveys into one object
NSH.tot             <- FLIndices()
for(iSurv in names(NSH.tun))
  NSH.tot[[iSurv]]  <- NSH.tun[[iSurv]]
NSH.tot[["SCAI"]]   <- NSH.scai[[which(unlist(lapply(NSH.scai,name))=="SCAI")]]
NSH.tun             <- FLIndices(NSH.tot)


#Set parameters etc
NSH.tun[["HERAS"]]@type               <- "number"
NSH.tun[["IBTS-Q1"]]@type             <- "number"
NSH.tun[["IBTS-Q3"]]@type             <- "number"
NSH.tun[["IBTS0"]]@type               <- "number"
NSH.tun[["IBTS0"]]@range["plusgroup"] <- NA

#Set a-priori weighting factors from Simmonds 2003. These are included
#as variances in the indices object
NSH.tun[["HERAS"]]@index.var[] <- 1.0/FLQuant(c(0.63,0.62,0.17,0.10,
  0.09,0.08,0.07,0.07,0.05),dimnames=dimnames(NSH.tun[["HERAS"]]@index)) #Acoustic
NSH.tun[["IBTS-Q1"]]@index.var[]  <- 1.0/FLQuant(c(0.47,0.28,0.01,0.01,0.01),
  dimnames=dimnames(NSH.tun[["IBTS-Q1"]]@index)) #IBTS
NSH.tun[["IBTS-Q3"]]@index.var[]  <- 1.0/FLQuant(c(0.47,0.28,0.01,0.01,0.01),
  dimnames=dimnames(NSH.tun[["IBTS-Q1"]]@index)) #IBTS-Q3: assume same variance as IBTS-Q1
NSH.tun[["IBTS0"]]@index.var[]  <- 1.0/FLQuant(0.63,
  dimnames=dimnames(NSH.tun[["IBTS0"]]@index)) #MIK
NSH.tun[["MLAI"]]@index.var[]   <- 1.0/FLQuant(0.60,
  dimnames=dimnames(NSH.tun[["MLAI"]]@index)) #MLAI
NSH.tun[["MLAI"]]@index.var[]   <- 1.0/FLQuant(0.60,
  dimnames=dimnames(NSH.tun[["SCAI"]]@index)) #SCAI

#FLICA requires that biomass indices ie MLAI, is the first index
NSH.tun  <- rev(NSH.tun)
if (NSH.tun[[1]]@name != "MLAI") print("Error - MLAI not as the first index")

