######################################################################################################
# WBSS FLICA Assessment
#
# Version 5.10 29/01/2009 13:04:11
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
# V 5.10 - Reflects modifications to Common Module to work as functions, rather than as a single script
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
#library(FLCore)
#library(FLAssess)
#library(FLICA)
#library(FLSTF)
#
### ======================================================================================================
### Convenience Functions
### ======================================================================================================
FnPrint     <-  function(string) {
	cat(string)
	flush.console()
}

### ======================================================================================================
### Display Info
### ======================================================================================================
ver <- "\nWBSS FLICA Assessment v 5.01\n";
ver.datetime   <- "28/01/2009 11:39:37\n\n";
FnPrint(ver)
FnPrint(ver.datetime)

### ======================================================================================================
### Define parameters for use in the assessment code here
### ======================================================================================================
data.source         <-  file.path("..","Source Data","WBSS")                     #Data source, not code or package source!!!
filename            <-  file.path("..","Output","WBSS","WBSS Assessment") #Output base filename, including directory
nretroyrs           <-  3               #Number of years for which to run the retrospective

### ======================================================================================================
### Incorporate Common modules
### Uses the common HAWG FLICA Assessment module to do the graphing, diagnostics and output
### ======================================================================================================
FnPrint("CALLING COMMON MODULE...\n")
source(file.path(".","Common","HAWG Common assessment module.r"))
FnPrint("COMMON MODULE COMPLETE.\n")
### ======================================================================================================

### ======================================================================================================
### Output setup
### ======================================================================================================
win.metafile(paste(filename,"figures - %02d.wmf"),height=180/25.4,width=130/25.4,pointsize=10,restoreConsole=FALSE)

### ======================================================================================================
### Prepare control object for assessment
### ======================================================================================================
FnPrint("PREPARING CONTROL OBJECTS...\n")
WBSS.ctrl   <-  FLICA.control(sep.nyr=5,
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
WBSS                        <- readFLStock(file.path(data.source, "index.dat"))
#Assume no discards
WBSS@catch.n                <- WBSS@landings.n
WBSS@catch                  <- WBSS@landings
WBSS@catch.wt               <- WBSS@landings.wt
units(WBSS)[1:17]           <- as.list(c(rep(c("tonnes","thousands","kg"),4), "NA", "NA", "NA", "NA", "NA"))
#Set fbar
range(WBSS)[c("minfbar","maxfbar")] <- c(3,6)
#Set plus group
WBSS                        <- setPlusGroup(WBSS,WBSS@range["max"])
#Set stock object name - this is propagated through into the figure titles
WBSS@name    <- "WBSS Herring"

### ======================================================================================================
### Prepare index object for assessment
### ======================================================================================================
FnPrint("PREPARING INDEX OBJECT...\n")
#Load and modify all index data
WBSS.tun   <- readFLIndices(file.path(data.source, "tun.txt"))
WBSS.tun   <- lapply(WBSS.tun,function(idx) {
          							idx@type 				       <- 	"number"
          							idx@index.var[] 		   <-	1
                        idx@range["plusgroup"] <- NA
          							return(idx)})
WBSS.tun[[1]]@range["plusgroup"] <- 8
WBSS.tun[[8]]      <-  trim(WBSS.tun[[1]],age=3:6,year=1993:2007) #HERAS 3-6 wr
WBSS.tun[[9]]      <-  trim(WBSS.tun[[5]],age=1:3,year=1994:2007) #GerAS 1-3 wr
WBSS.tun[[9]]@index[,"2001"] <- -1     #2001 is excluded from GerAS due to lack of coverage in SD23

#Name them all appropriately
WBSS.tun[[1]]@name <- "HERAS 0-8+ wr"
WBSS.tun[[2]]@name <- "Ger AS 0-8 wr (SD 22-24)"
WBSS.tun[[3]]@name <- "IYFS Katt Q1 1-5 wr"
WBSS.tun[[4]]@name <- "IYFS Katt Q3 1-5 wr"
WBSS.tun[[5]]@name <- "Ger AS 0-8 wr (SD 21-24)"
WBSS.tun[[6]]@name <- "N20"
WBSS.tun[[7]]@name <- "N30"
WBSS.tun[[8]]@name <- "HERAS 3-6 wr"
WBSS.tun[[9]]@name <- "GerAS 1-3 wr";
names(WBSS.tun) <-  sapply(WBSS.tun,name)

#Only use the relevant data sets
WBSS.tun  <- WBSS.tun[c("HERAS 3-6 wr","GerAS 1-3 wr","N20")]

### ======================================================================================================
### Perform the assessment
### ======================================================================================================
FnPrint("PERFORMING ASSESSMENT...\n")
#Now perform the asssessment
WBSS.ica   <-  FLICA(WBSS,WBSS.tun,WBSS.ctrl)
WBSS       <-  WBSS + WBSS.ica

### ======================================================================================================
### Use the standard code from the common modules to produce outputs
### ======================================================================================================
do.summary.plots(WBSS,WBSS.ica)
WBSS.retro <- do.retrospective.plots(WBSS,WBSS.tun,WBSS.ctrl,nretroyrs)



### ======================================================================================================
### Document Assessment
### ======================================================================================================
FnPrint("GENERATING DOCUMENTATION...\n")
#Document the run with alternative table numbering
options("width"=80)
ica.out.file <- ica.out(WBSS,WBSS.tun,WBSS.ica,format="TABLE 3.6.%i WBSS HERRING.")
write(ica.out.file,file=paste(filename,"ica.out",sep="."))

#And finally, write the results out in the lowestoft VPA format for further analysis eg MFDP
writeFLStock(WBSS,output.file=filename)


### ======================================================================================================
### Short Term Forecast
### ======================================================================================================
FnPrint("PERFORMING SHORT TERM FORECAST...\n")
#Make forecast
gm.recs         <- exp(mean(log(rec(trim(WBSS,year=2002:2006)))))  #WBSS recruitment is based on a geometric mean of the last few years
stf.ctrl        <- FLSTF.control(nyrs=1,catch.constraint=1000,f.rescale=TRUE,rec=gm.recs)
WBSS.stf        <- FLSTF(stock=WBSS,control=stf.ctrl,survivors=NA,quiet=TRUE,sop.correct=FALSE)

#Write the stf results out in the lowestoft VPA format for further analysis eg MFDP
writeFLStock(WBSS.stf,output.file=paste(filename,"with STF"))

### ======================================================================================================
### Save workspace and Finish Up
### ======================================================================================================
FnPrint("SAVING WORKSPACES...\n")
save(WBSS,WBSS.stf,WBSS.tun,WBSS.ctrl,file=paste(filename,"Assessment.RData"))
save.image(file=paste(filename,"Assessment Workspace.RData"))
dev.off()
FnPrint(paste("COMPLETE IN",sprintf("%0.1f",round(proc.time()[3]-start.time,1)),"s.\n\n"))