######################################################################################################
# NEA Mackerel FLICA Assessment
#
# $Rev: 1 $
# $Date: 2009-04-16 18:43:20 +0000 (Tue, 24 Mar 2009) $
#
# Author: John Simmonds
# FRS, Scotland
#
# Performs an assessment of North East Atlantic Mackerel stock using the
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
# V 0.10 - Initial version, based on code inherited from NEA.Mac 2009 and MEAmac 2008
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
FnPrint("\nNEAMac FLICA Assessment\n=======================\n")

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
output.dir          <-  file.path(".","results")       #Output directory
output.base         <-  file.path(output.dir,"NEAMac Assessment") #Output base filename, including directory. Other output filenames are built by appending onto this one
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
NEA.Mac.ctrl<-FLICA.control(sep.nyr=12,sep.age=5,sep.sel=1.5,sr=FALSE,
                                lambda.yr=c(1,1,1,1,1,1,1,1,1,1,1,1),
                                lambda.age =c(0.0033333, 0.033333, 0.33333,
                                 0.33333, 0.33333, 0.33333, 0.33333, 0.33333, 0.33333, 0.33333,0.33333,0.33333,0.33333),
                                lambda.sr=0.01,index.model=c("l"),index.cor=0)


### ======================================================================================================
### Prepare stock object for assessment
### ======================================================================================================
FnPrint("PREPARING STOCK OBJECT...\n")
NEA.Mac                        <- readFLStock(file.path(data.source, "index.txt"),no.discards=TRUE)
#Set units
units(NEA.Mac)[1:17]           <- as.list(c(rep(c("Tonnes","Thousands","Kg"),4), rep("NA",5)))
#Set fbar
range(NEA.Mac)[c("minfbar","maxfbar")] <- c(4,8)
#Set plus group
NEA.Mac                        <- setPlusGroup(NEA.Mac,NEA.Mac@range["max"])
#Set stock object name - this is propagated through into the figure titles
NEA.Mac@name    <- "NEA Mackerel"

### ======================================================================================================
### Prepare index object for assessment
### ======================================================================================================
FnPrint("PREPARING INDEX OBJECT...\n")
#Load and modify all index data


#line should be as follows but this does not work
#NEA.Mac.tun   <- readFLIndices(file.path(data.source, "/Ssb.txt"),type="ICA")
## add tempory solution --- subroutine
  read.SSB.Index<-function(file.str,catch){
     aa       <-scan(file=file.str,skip=3)
     aa       <-t(matrix(aa,c(3,as.integer(length(aa)/3))))
     dmns     <-dimnames(catch)
     dmns$year<-as.character(aa[,1])
     SSB      <-as.FLQuant(aa[,3],dimnames=dmns)

     SSB      <-FLIndex(index=SSB)
     SSB@type <-"biomass"

     return(SSB)
     }

NEA.Mac.indices  <-read.SSB.Index(paste(data.source, "/Ssb.txt",sep=""),NEA.Mac@catch)
NEA.Mac.tun=FLIndices(NEA.Mac.indices)
###################################### end of non--standard section


#Set names, and parameters etc
NEA.Mac.tun[[1]]@index.var[] <- 0.1
NEA.Mac.tun[[1]]@effort[] <- 1

NEA.Mac.tun[[1]]@type <- "biomass"
names(NEA.Mac.tun) <- "NEA.Mac Egg Survey"  #MPA: Added so that your graphs are a bit prettier

### ======================================================================================================
### Perform the assessment
### ======================================================================================================
FnPrint("PERFORMING ASSESSMENT.\n")
#Now perform the asssessment
NEA.Mac.ica   <-  FLICA(NEA.Mac,NEA.Mac.tun,NEA.Mac.ctrl)
NEA.Mac       <-  NEA.Mac + NEA.Mac.ica
NEA.Mac@stock=computeStock(NEA.Mac) # to get TSB in stock slot

### ======================================================================================================
### Use the standard code from the common modules to produce outputs
### ======================================================================================================
do.summary.plots(NEA.Mac,NEA.Mac.ica)
NEA.Mac.retro <- do.retrospective.plots(NEA.Mac,NEA.Mac.tun,NEA.Mac.ctrl,n.retro.years)
do.SRR.plot(NEA.Mac)

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
old.opt <- options("width","scipen")
options("width"=80,"scipen"=1000)
#Do some tidying up on precision of the ica file
NEA.Mac.orig=NEA.Mac
NEA.Mac.ica@catch.res@.Data <- round(NEA.Mac.ica@catch.res@.Data,3)
NEA.Mac.ica@index.res[[1]]@.Data <- round(NEA.Mac.ica@index.res[[1]]@.Data,3)
NEA.Mac.ica@survivors=round(NEA.Mac.ica@survivors)
NEA.Mac.ica@sel=round(NEA.Mac.ica@sel,3)
NEA.Mac@harvest <- zapsmall(NEA.Mac@harvest,3)
NEA.Mac@stock.n=round(NEA.Mac@stock.n)
NEA.Mac@catch.n=round(NEA.Mac@catch.n)
NEA.Mac.ica@catch.n=round(NEA.Mac.ica@catch.n)
NEA.Mac.ica@index.hat[[1]]@.Data=round(NEA.Mac.ica@index.hat[[1]]@.Data)
NEA.Mac@mat=round(NEA.Mac@mat,2)
NEA.Mac@stock.wt=round(NEA.Mac@stock.wt,3)
NEA.Mac@catch.wt=round(NEA.Mac@catch.wt,3)
NEA.Mac.ica@param[,6:10]=round(NEA.Mac.ica@param[6:10],2)

#Now write the file
ica.out.file <- ica.out(NEA.Mac,NEA.Mac.tun,NEA.Mac.ica,format="TABLE 5.6.2.%i HERRING in VIa (N).")
write(ica.out.file,file=paste(output.base,"ica.out",sep="."))
options("width"=old.opt$width,"scipen"=old.opt$scipen)

#And finally, write the results out in the lowestoft VPA format for further analysis eg MFDP
writeFLStock(NEA.Mac.orig,output.file=output.base)


### ======================================================================================================
### Short Term Forecast
### ======================================================================================================
# FnPrint("PERFORMING SHORT TERM FORECAST...\n")
#Make forecast
gm.recs         <- exp(mean(log(rec(trim(NEA.Mac.orig,year=1989:2006)))))  #WBSS recruitment is based on a geometric mean of the last few years
stf.ctrl        <- FLSTF.control(nyrs=1,fbar.nyrs=1,fbar.min=3,fbar.max=6,catch.constraint=21760,f.rescale=TRUE,rec=gm.recs)
NEA.Mac.orig@catch.n[1,52,,,,]=1    #MPA: This is where the error occurs now - what are you trying to do here?
NEA.Mac.stf        <- FLSTF(stock=NEA.Mac.orig,control=stf.ctrl,quiet=TRUE,sop.correct=FALSE)
writeVPA(NEA.Mac.stf, output.file=output.base,slots=c("stock.n"))
## use the rounder version so report and quality control database have same values
writeFLStock(NEA.Mac,file.path(output.dir,"hawg_her-vian.sum"),type="ICAsum")
# project one year in order to get a single year holding means for YPR output
NEA.Mac.proj=stf(NEA.Mac.orig,nyears=1,wts.nyears=3,fbar.nyears=1,arith.mean=TRUE,na.rm=TRUE)
writeFLStock(NEA.Mac.proj,file.path(output.dir,"hawg_her-vian.ypr"),type="YPR")

#Write the stf results out in the lowestoft VPA format for further analysis eg MFDP
# writeFLStock(WBSS.stf,output.file=paste(output.base,"with STF"))

### ======================================================================================================
### Save workspace and Finish Up
### ======================================================================================================
FnPrint("SAVING WORKSPACES...\n")
save(NEA.Mac.orig,NEA.Mac.tun,NEA.Mac.ctrl,file=paste(output.base,"Assessment.RData"))
save.image(file=paste(output.base,"Assessment Workspace.RData"))
dev.off()
FnPrint(paste("COMPLETE IN",sprintf("%0.1f",round(proc.time()[3]-start.time,1)),"s.\n\n"))