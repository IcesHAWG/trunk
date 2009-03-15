######################################################################################################
# CS.herring FLICA Assessment
#  February 2009
#
# $Rev$
# $Date$
#
# Author: Afra Egan
# Ireland
#
# Performs an assessment of Celtic Sea Herring (cs.herring) using the FLICA package.
#
# Developed with:
#   - R version 2.8.0
#   - FLCore 1.99-111
#   - FLICA, version 1.4-5
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
FnPrint("\nCeltic Sea Herring FLICA Assessment\n===================================\n")

### ======================================================================================================
### Incorporate Common modules
### Uses the common HAWG FLICA Assessment module to do the graphing, diagnostics and output
### ======================================================================================================

source(file.path("..","_Common","HAWG Common assessment module.r"))

### ======================================================================================================

### ======================================================================================================
### Define parameters for use in the assessment code here
### ======================================================================================================
data.source         <-  file.path("data")      #Data source, not code or package source!!!
output.dir          <-  file.path("res")       #Output directory
output.base         <-  file.path(output.dir,"cs.herring Assessment") #Output base filename, including directory. Other output filenames are built by appending onto this one
n.retro.years       <- 7                          #Number of years for which to run the retrospective

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
cs.herring.ctrl   <-  FLICA.control(sep.nyr=6,
                             sep.age=3,
                             sep.sel=1.0,
                             lambda.yr=1,
                             lambda.age=c(0.1,1,1,1,1,1,1,1,1),
                              lambda.sr=0,
                              sr=FALSE,
                              sr.age=0,
                              index.model=c("l"),
                              index.cor=1)


### ======================================================================================================
### Prepare stock object for assessment
### ======================================================================================================
FnPrint("PREPARING STOCK OBJECT...\n")
cs.herring                        <- readFLStock(file.path(data.source, "index.txt"),no.discards=TRUE)
#Set no discards
cs.herring@catch.n                <- cs.herring@landings.n
cs.herring@catch                  <- cs.herring@landings
cs.herring@catch.wt               <- cs.herring@landings.wt
units(cs.herring)[1:17]           <- as.list(c(rep(c("tonnes","thousands","kg"),4), rep("NA",5)))
#Set fbar
range(cs.herring)[c("minfbar","maxfbar")] <- c(2,5)
#Set plus group
cs.herring                        <- setPlusGroup(cs.herring,cs.herring@range["max"])
#Set stock object name - this is propagated through into the figure titles
cs.herring@name    <- "Celtic Sea Herring"

### ======================================================================================================
### Prepare index object for assessment
### ======================================================================================================
FnPrint("PREPARING INDEX OBJECT...\n")
#Load and modify all index data
cs.herring.tun   <- readFLIndices(file.path(data.source, "fleet.txt"))

#Set names, and parameters etc
names(cs.herring.tun) <-  gsub(":.*$","",names(cs.herring.tun))
cs.herring.tun   <- lapply(cs.herring.tun,function(idx) {
                idx@type 	     <- 	"number"
          		idx@index.var[]  <-	1
                idx@range["plusgroup"] <- NA
          		return(idx)})


names(cs.herring.tun)[1] <- c("Celtic Sea Herring Acoustic")

#Only use the relevant data sets
cs.herring.tun  <- cs.herring.tun[c("Celtic Sea Herring Acoustic")]

### ======================================================================================================
### Perform the assessment
### ======================================================================================================
FnPrint("PERFORMING ASSESSMENT...\n")

#Now perform the asssessment
cs.herring.ica   <-  FLICA(cs.herring,cs.herring.tun,cs.herring.ctrl)
cs.herring       <-  cs.herring + cs.herring.ica

################################################################################
## Change Recruitment to mean value

Rec=exp(mean(log(cs.herring@stock.n[1,as.character(1958:(cs.herring@range['maxyear']-1)),,,,])))

# put recruitment into last fishing year
cs.herring@stock.n['1',(as.character(cs.herring@range['maxyear'])),,,,]=Rec

################################################################################

### ======================================================================================================
### Use the standard code from the common modules to produce outputs
### ======================================================================================================
do.summary.plots(cs.herring,cs.herring.ica)
cs.herring.retro <- do.retrospective.plots(cs.herring,cs.herring.tun,cs.herring.ctrl,n.retro.years)
do.SRR.plot(cs.herring)

### ======================================================================================================
### Custom plots
### ======================================================================================================
FnPrint("GENERATING CUSTOM PLOTS...\n")

#Plot of Catch and TAC
## TAC years specified
## Catch all years
TACs    <- data.frame(year=1974:2008,TAC=1000*c(32,25,10.8,0,0,6,6,6,8,8,13,13,17,18,18,20,17.5,21,21,21,21,21,21,22,22,21,21,20,11,13,13,13,11,9.3,7.9))
TAC.plot.dat <- data.frame(year=rep(TACs$year,each=2)+c(-0.5,0.5),TAC=rep(TACs$TAC,each=2))
catch   <- as.data.frame(cs.herring@catch)
plot(0,0,pch=NA,xlab="Year",ylab="Catch",xlim=range(pretty(c(catch$year,TACs$year))),ylim=range(pretty(c(0,TACs$TAC,catch$data))))
rect(catch$year-0.5,0,catch$year+0.5,catch$data,col="grey")
lines(TAC.plot.dat,lwd=5)
legend("topright",legend=c("Catch","TAC"),lwd=c(1,5),lty=c(NA,1),pch=c(22,NA),col="black",pt.bg="grey",pt.cex=c(2))
title(main=paste(cs.herring@name,"Catch and TAC"))


### ======================================================================================================
### Document Assessment
### ======================================================================================================
FnPrint("GENERATING DOCUMENTATION...\n")
#Document the run with alternative table numbering and a reduced width
old.opt <- options("width","scipen")
options("width"=80,"scipen"=1000)
ica.out.file <- ica.out(cs.herring,cs.herring.tun,cs.herring.ica,format="TABLE 4.6.%i cs.herring HERRING.")
write(ica.out.file,file=paste(output.base,"ica.out",sep="."))
options("width"=old.opt$width,"scipen"=old.opt$scipen)

#And finally, write the results out in the lowestoft VPA format for further analysis eg MFDP
writeFLStock(cs.herring,output.file=output.base)


### ======================================================================================================
### Short Term Forecast
### ======================================================================================================
FnPrint("PERFORMING SHORT TERM FORECAST...\n")
#Make forecast
gm.recs         <- exp(mean(log(rec(trim(cs.herring,year=1958:2006)))))  #cs.herring recruitment is based on geometric mean recruitment
stf.ctrl        <- FLSTF.control(nyrs=1,catch.constraint=1000,f.rescale=TRUE,rec=gm.recs)
cs.herring.stf  <- FLSTF(stock=cs.herring,control=stf.ctrl,survivors=NA,quiet=TRUE,sop.correct=FALSE)

#Write the stf results out in the lowestoft VPA format for further analysis eg MFDP
writeFLStock(cs.herring.stf,output.file=paste(output.base,"with STF"))

### ======================================================================================================
### Save workspace and Finish Up
### ======================================================================================================
FnPrint("SAVING WORKSPACES...\n")
save(cs.herring,cs.herring.stf,cs.herring.tun,cs.herring.ctrl,file=paste(output.base,"Assessment.RData"))
save.image(file=paste(output.base,"Assessment Workspace.RData"))
dev.off()
FnPrint(paste("COMPLETE IN",sprintf("%0.1f",round(proc.time()[3]-start.time,1)),"s.\n\n"))
