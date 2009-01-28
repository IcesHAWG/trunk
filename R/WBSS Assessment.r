######################################################################################################
# WBSS FLICA Assessment
#
# Version 5.00 27/01/2009 18:03:13
#
# Author: Mark Payne
# DIFRES, Charlottenlund, DK
#
# Performs an assessment of Western Baltic Spring Spawning Herring (WBSS) in IIIa using the
# FLICA package.
#
# Developed with:
#   - R version 2.8.0
#   - FLCore 1.99-111
#   - FLICA, version 1.4-3
#   - FLAssess, version 1.99-102
#   - FLSTF, version 1.99-1
#
# Changes:
# V 5.00 - Compatiable with Google Code version
# V 0.20 - Modifications
# V 0.10 - Initial version, based on code inherited from Tomas Grösler
#
# To be done:
#
# Notes:
#
####################################################################################################

### ======================================================================================================
### Initialise system  
### ======================================================================================================
# Start with house cleaning
rm(list=ls()); gc(); graphics.off(); start.time <- proc.time()[3]
options(stringsAsFactors=FALSE)
#Load libraries
library(FLCore)
library(FLAssess)
library(FLICA)
library(FLSTF)

### ======================================================================================================
### Functions
### ======================================================================================================
FnPrint     <-  function(string) {
	cat(string)
	flush.console()
}

### ======================================================================================================
### Display Info
### ======================================================================================================
ver <- "\nWBSS FLICA Assessment v 5.00\n";
ver.datetime   <- "27/01/2009 18:03:13\n\n";
FnPrint(ver)
FnPrint(ver.datetime)

### ======================================================================================================
### Define parameters for use in the generic assessment code here
### ======================================================================================================
n.retro.yrs         <-  3               #Number of years for which to run the retrospective
filename            <-  file.path("..","Output","WBSS","WBSS Assessment") #Output base filename, including directory
table.fmt.str       <-  "TABLE 3.6.%i WBSS HERRING."   #The table number formatting string for the ica.out file

### ======================================================================================================
### Output setup
### ======================================================================================================
win.metafile(paste(filename,"figures - %02d.wmf"),height=180/25.4,width=130/25.4,pointsize=10,restoreConsole=FALSE)

### ======================================================================================================
### Parameters specific to this code
### ======================================================================================================
source.path         <-  file.path("..","Source Data","WBSS")                     #Data source, not code or package source!!!

### ======================================================================================================
### Prepare control object for assessment
### ======================================================================================================
FnPrint("PREPARING CONTROL OBJECTS...\n")
ctrl   <-  FLICA.control(sep.nyr=5,
                              sep.age=4,
                              sep.sel=1.0,
                              lambda.yr=1,
                              lambda.age=c(0.1,1,1,1,1,1,1,1,1),
                              lambda.sr=0,
                              sr=FALSE,
                              sr.age=0,
                              index.model=c("l","l","l"),
                              index.cor=1)

### ======================================================================================================
### Prepare stock object for assessment
### ======================================================================================================
FnPrint("PREPARING STOCK OBJECT...\n")
stck                        <- readFLStock(file.path(source.path, "index.dat"),name="WBSS Herring")
#Assume no discards
stck@catch.n                <- stck@landings.n
stck@catch                  <- stck@landings
stck@catch.wt               <- stck@landings.wt
units(stck)[1:17]           <- as.list(c(rep(c("tonnes","thousands","kg"),4), "NA", "NA", "NA", "NA", "NA"))
#Set fbar
range(stck)[c("minfbar","maxfbar")] <- c(3,6)
#Set plus group
stck                        <- setPlusGroup(stck,stck@range["max"])

### ======================================================================================================
### Prepare index object for assessment
### ======================================================================================================
FnPrint("PREPARING INDEX OBJECT...\n")
#Load and modify all index data
idxs   <- readFLIndices(file.path(source.path, "tun.txt"))
idxs   <- lapply(idxs,function(idx) {
          							idx@type 				       <- 	"number"
          							idx@index.var[] 		   <-	1
                        idx@range["plusgroup"] <- NA
          							return(idx)})
idxs[[1]]@range["plusgroup"] <- 8
idxs[[8]]      <-  trim(idxs[[1]],age=3:6,year=1993:2007) #HERAS 3-6 wr
idxs[[9]]      <-  trim(idxs[[5]],age=1:3,year=1994:2007) #GerAS 1-3 wr
idxs[[9]]@index[,"2001"] <- -1     #2001 is excluded from GerAS due to lack of coverage in SD23

#Name them all appropriately
idxs[[1]]@name <- "HERAS 0-8+ wr"
idxs[[2]]@name <- "Ger AS 0-8 wr (SD 22-24)"
idxs[[3]]@name <- "IYFS Katt Q1 1-5 wr"
idxs[[4]]@name <- "IYFS Katt Q3 1-5 wr"
idxs[[5]]@name <- "Ger AS 0-8 wr (SD 21-24)"
idxs[[6]]@name <- "N20"
idxs[[7]]@name <- "N30"
idxs[[8]]@name <- "HERAS 3-6 wr"
idxs[[9]]@name <- "GerAS 1-3 wr";
names(idxs) <-  sapply(idxs,name)

#Only use the relevant data sets
idxs  <- idxs[c("HERAS 3-6 wr","GerAS 1-3 wr","N20")]




### ======================================================================================================
### Perform the assessment
### Uses the common HAWG FLICA Assessment module to perform the actual assessment
### ======================================================================================================
FnPrint("PERFORMING ASSESSMENT...\n")
source(file.path(".","Common","HAWG Common assessment module.r"))
### ======================================================================================================




### ======================================================================================================
### Short Term Forecast
### The STF for each stock can be a bit different, and is usually based on the outcome of the results of
### the assessment itself (especially for what value to choose for the recruitment in the coming year)
### It is therefore easier to treat each one separately, in the postprocessing section (I think).
### ======================================================================================================
FnPrint("PERFORMING SHORT TERM FORECAST...\n")
#Make forecast
gm.recs         <- exp(mean(log(rec(trim(stck,year=2002:2006)))))
stf.ctrl        <- FLSTF.control(nyrs=1,catch.constraint=1000,f.rescale=TRUE,rec=gm.recs)
stck.stf        <- FLSTF(stock=stck,control=stf.ctrl,survivors=NA,quiet=TRUE,sop.correct=FALSE)

#Write the stf results out in the lowestoft VPA format for further analysis eg MFDP
writeFLStock(stck.stf,output.file=paste(filename,"with STF"))

### ======================================================================================================
### Convert objects to better names and Save workspace
### ======================================================================================================
FnPrint("SAVING WORKSPACES...\n")
herIIIa      <- stck
herIIIa.stf  <- stck.stf
herIIIa.tun  <- idxs
herIIIa.ctrl <- ctrl
#Save assessment objects
save(herIIIa,herIIIa.stf,herIIIa.tun,herIIIa.ctrl,file=paste(filename,"Assessment.RData"))
save.image(file=paste(filename,"Assessment Workspace.RData"))

### ======================================================================================================
### Finish Up
### ======================================================================================================
dev.off()


FnPrint(paste("COMPLETE IN",sprintf("%0.1f",round(proc.time()[3]-start.time,1)),"s.\n\n"))