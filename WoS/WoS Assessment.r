######################################################################################################
# WoS FLICA Assessment
#
# $Rev$
# $Date$
#
# Author: Mark Payne  (and edited for WoS from WBSS by Emma Hatfield with MUCH help from Mark)
# DIFRES, Charlottenlund, DK
#
# Performs an assessment of West of Scotland Herring (WoS) in VIaN using the
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
### Initialise system, including convenience functions and title display
### ======================================================================================================
rm(list=ls()); gc(); graphics.off(); start.time <- proc.time()[3]
options(stringsAsFactors=FALSE)
FnPrint     <-  function(string) {
	cat(string)
	flush.console()
}
FnPrint("\nWoS FLICA Assessment\n=====================\n")

### ======================================================================================================
### Incorporate Common modules
### Uses the common HAWG FLICA Assessment module to do the graphing, diagnostics and output
### ======================================================================================================
source(file.path("..","_Common","HAWG Common assessment module.r"))
### ======================================================================================================

### ======================================================================================================
### Define parameters for use in the assessment code here
### ======================================================================================================
data.source         <-  file.path(".","data")      #Data source, not code or package source!!!
output.dir          <-  file.path(".","res")       #Output directory
output.base         <-  file.path(output.dir,"WoS Assessment") #Output base filename, including directory. Other output filenames are built by appending onto this one
n.retro.years       <-  5                          #Number of years for which to run the retrospective

### ======================================================================================================
### Output setup
### ======================================================================================================
png(paste(output.base,"figures - %02d.png"),units = "px", height=1200,width=800,pointsize = 24, bg = "white")
#Set default lattice fontsize, so that things are actually readible!
trellis.par.set(fontsize=list(text=24,points=20))

### ======================================================================================================
### Prepare control object for assessment
### We use here two different options - the first is the simpler and more normal:, just setting the control
### directly in the code. The second is reading in the configuration from a file - this is normally not
### necessary, but is a handy feature for using the code on stockassessment.org
### ======================================================================================================
FnPrint("PREPARING CONTROL OBJECTS...\n")
#Set control object straight up (option 1)
#-----------------------------------------
# copied from John Simmonds' 2008 HAWG script
WoS.ctrl   <-  FLICA.control(sep.nyr=8,sep.age=4,sep.sel=1.0,sr=FALSE,
                                lambda.yr=c(1,1,1,1,1,1,1,1),
                                lambda.age =c(0.1, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0),
                                lambda.sr=0.01, index.model=c("l"), index.cor=T)


### ======================================================================================================
### Prepare stock object for assessment
### ======================================================================================================
FnPrint("PREPARING STOCK OBJECT...\n")
WoS                        <- readFLStock(file.path(data.source, "index.txt"),no.discards=TRUE)
#Set units
units(WoS)[1:17]           <- as.list(c(rep(c("Tonnes","Thousands","Kg"),4), rep("NA",5)))
#Set fbar
range(WoS)[c("minfbar","maxfbar")] <- c(3,6)
#Set plus group
WoS                        <- setPlusGroup(WoS,WoS@range["max"])           
#Set stock object name - this is propagated through into the figure titles
WoS@name    <- "West of Scotland Herring"

### ======================================================================================================
### Prepare index object for assessment
### ======================================================================================================
FnPrint("PREPARING INDEX OBJECT...\n")
#Load and modify all index data
WoS.tun   <- readFLIndices(file.path(data.source, "fleet.txt"))

#Set names, and parameters etc
WoS.tun[[1]]@index.var[] <- 1
WoS.tun[[1]]@index.var["1",] <- 10
WoS.tun[[1]]@type <- "number"

#  IGNORE THIS - not relevant to WoS
#Define two new indices by truncating current indices etc
#WBSS.tun[[8]]      <-  trim(WBSS.tun[[1]],age=3:6,year=1993:2007) #HERAS 3-6 wr
# WBSS.tun[[9]]      <-  trim(WBSS.tun[[5]],age=1:3,year=1994:2007) #GerAS 1-3 wr
# WBSS.tun[[9]]@index[,"2001"] <- -1     #2001 is excluded from GerAS due to lack of coverage in SD23
# names(WBSS.tun)[8:9] <- c("HERAS 3-6 wr","GerAS 1-3 wr")

#Only use the relevant data sets
# WBSS.tun  <- WBSS.tun[c("HERAS 3-6 wr","GerAS 1-3 wr","N20")]

### ======================================================================================================
### Perform the assessment
### ======================================================================================================
FnPrint("PERFORMING ASSESSMENT...\n")
#Now perform the asssessment
WoS.ica   <-  FLICA(WoS,WoS.tun,WoS.ctrl)
WoS       <-  WoS + WoS.ica

### ======================================================================================================
### Use the standard code from the common modules to produce outputs
### ======================================================================================================
do.summary.plots(WoS,WoS.ica)
WoS.retro <- do.retrospective.plots(WoS,WoS.tun,WoS.ctrl,n.retro.years)
do.SRR.plot(WoS)

### ======================================================================================================
### Custom plots
### ======================================================================================================
# FnPrint("GENERATING CUSTOM PLOTS...\n")
#Catch and TAC
# TACs    <- data.frame(year=1991:2008,TAC=1000*c(155,174,210,191,183,163,100,97,99,101,101,101,101,91,120,102+47.5,69+49.5,51.7+45))
# TAC.plot.dat <- data.frame(year=rep(TACs$year,each=2)+c(-0.5,0.5),TAC=rep(TACs$TAC,each=2))
# catch   <- as.data.frame(WBSS@catch)
# plot(0,0,pch=NA,xlab="Year",ylab="Catch",xlim=range(pretty(catch$year,TACs$year)),ylim=range(pretty(c(0,TACs$TAC,catch$data))))
# rect(catch$year-0.5,0,catch$year+0.5,catch$data,col="grey")
# lines(TAC.plot.dat,lwd=5)
# legend("topright",legend=c("Catch","TAC"),lwd=c(1,5),lty=c(NA,1),pch=c(22,NA),col="black",pt.bg="grey",pt.cex=c(2))
# title(main=paste(WBSS@name,"Catch and TAC"))


### ======================================================================================================
### Document Assessment
### ======================================================================================================
FnPrint("GENERATING DOCUMENTATION...\n")
#Document the run with alternative table numbering and a reduced width
old.opt <- options("width")
options("width"=80)
ica.out.file <- ica.out(WoS,WoS.tun,WoS.ica,format="TABLE 5.6.1.%i HERRING in VIa (N).")
write(ica.out.file,file=paste(output.base,"ica.out",sep="."))
options("width"=old.opt$width)

#And finally, write the results out in the lowestoft VPA format for further analysis eg MFDP
writeFLStock(WoS,output.file=output.base)


### ======================================================================================================
### Short Term Forecast
### ======================================================================================================
# FnPrint("PERFORMING SHORT TERM FORECAST...\n")
#Make forecast
# gm.recs         <- exp(mean(log(rec(trim(WBSS,year=2002:2006)))))  #WBSS recruitment is based on a geometric mean of the last few years
# stf.ctrl        <- FLSTF.control(nyrs=1,catch.constraint=1000,f.rescale=TRUE,rec=gm.recs)
# WBSS.stf        <- FLSTF(stock=WBSS,control=stf.ctrl,survivors=NA,quiet=TRUE,sop.correct=FALSE)

#Write the stf results out in the lowestoft VPA format for further analysis eg MFDP
# writeFLStock(WBSS.stf,output.file=paste(output.base,"with STF"))

### ======================================================================================================
### Save workspace and Finish Up
### ======================================================================================================
FnPrint("SAVING WORKSPACES...\n")
save(WoS,WoS.stf,WoS.tun,WoS.ctrl,file=paste(output.base,"Assessment.RData"))
save.image(file=paste(output.base,"Assessment Workspace.RData"))
dev.off()
FnPrint(paste("COMPLETE IN",sprintf("%0.1f",round(proc.time()[3]-start.time,1)),"s.\n\n"))