######################################################################################################
# WoS FLICA Assessment
#
# $Rev$
# $Date$
#
# Author: Emma Hatfield / John Simmonds
# FRS, Scotland
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
path <- "C:/Documents and Settings/hatfielde/My Documents/A1 Post Virtualisation/hawgVIaN/2011/hawg/WoS/"
try(setwd(path))
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
n.retro.years       <-  8                          #Number of years for which to run the retrospective

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
WoS.tun[[1]]@index.var[]      <- 1
WoS.tun[[1]]@index.var["1",]  <- 10
WoS.tun[[1]]@type             <- "number"
names(WoS.tun)                <- "WoS Summer Acoustic Survey"  #MPA: Added so that your graphs are a bit prettier
#Set units
units(WoS.tun[[1]])[1:7]      <- as.list(c("number","NA","number","Kg","NA","NA","NA"))

### ======================================================================================================
### Perform the assessment
### ======================================================================================================
FnPrint("PERFORMING ASSESSMENT...\n")
#Now perform the asssessment
WoS.ica   <- FLICA(WoS,WoS.tun,WoS.ctrl)
WoS       <- WoS + WoS.ica
WoS@stock <- computeStock(WoS) # to get TSB in stock slot

WoS@stock.n[1,ac(2010)] <- NA #Take this recruitment out because it is not well estimated in ICA as there is no information to support it

#-Replace the recruitment value in 2010 with a geometric mean recruitment over the years 1986 to 2007
WoS@stock.n[1,ac(2010)] <- exp(mean(log(rec(WoS[,ac(1989:2007)])),na.rm=T))

### ======================================================================================================
### Use the standard code from the common modules to produce outputs
### ======================================================================================================
#this only needs to be left in for the stock summary plot so run everything 
#without the line below but then run line below to produce the picture that I want for the stock summary
#WoS@stock.n[1,ac(2010)] <- NA
do.summary.plots(WoS,WoS.ica)

do.retrospective.plots<- function(stck,idxs,ctrl,n.retro.yrs) {
    cat("GENERATING RETROSPECTIVE ANALYSES...\n");flush.console()

    #Generate a retrospective analysis
    icas    <- retro(stck,idxs,ctrl,retro=n.retro.yrs,return.FLStocks=FALSE)
    stcks   <- do.call(FLStocks,lapply(icas,function(ica) {ica + trim(stck, year=dims(ica@stock.n)$minyear:dims(ica@stock.n)$maxyear)}))

    # Remove last years recruitment value as it gets estimated wrong in ICA, replace it later with a GM estimate
    stcks[[9]]@stock.n[1,ac(2010)] <- NA
    stcks[[9]]@stock.n[1,ac(2010)] <- exp(mean(log(rec(stck[,ac(1989:2007)])),na.rm=T))
    #Now call the retro plotting functions
    retro.plots(stcks,icas,ctrl)

    #Return retrospective object
    return(stcks)
}
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
print(stacked.area.plot(data~year| unit, as.data.frame(pay(WoS@stock.n)),groups="age",main="Proportion of stock.n at age",ylim=c(-0.01,1.01),xlab="years",col=gray(9:0/9)))
print(stacked.area.plot(data~year| unit, as.data.frame(pay(WoS@catch.n)),groups="age",main="Proportion of Catch.n at age",ylim=c(-0.01,1.01),xlab="years",col=gray(9:0/9)))
print(stacked.area.plot(data~year| unit, as.data.frame(pay(WoS@catch.wt)),groups="age",main="Proportion of Catch.wt at age",ylim=c(-0.01,1.01),xlab="years",col=gray(9:0/9)))
print(stacked.area.plot(data~year,as.data.frame(pay(WoS@stock.n*WoS@stock.wt)),groups="age",main="Proportion by weight in the stock",ylim=c(-0.01,1.01),xlab="years",col=gray(9:0/9)))
print(stacked.area.plot(data~year| unit, as.data.frame(pay(WoS.tun[[1]]@index)),groups="age",main="Proportion of Acoustic index at age",ylim=c(-0.01,1.01),xlab="years",col=gray(9:0/9)))

catch.curves(WoS,1990,2010)
WoS.sr <- ref.pts(WoS,"segreg",100000)
cor.tun(WoS.tun)

#Time series of west
west.ts  <- xyplot(data~year,data=window(WoS@stock.wt,1991,2010),
              groups=age,
              auto.key=list(space="right",points=FALSE,lines=TRUE,type="b"),
              type="b",
              xlab="Year",ylab="Weight in the stock (kg)",
              main=paste(WoS@name,"Weight in the Stock"),
              par.settings=list(superpose.symbol=list(pch=as.character(0:8),cex=1.25)))
print(west.ts)

#Time series of west by cohort
west.by.cohort      <- as.data.frame(FLCohort(window(WoS@stock.wt,1992,2010)))
west.by.cohort      <- subset(west.by.cohort,!is.na(west.by.cohort$data))
west.by.cohort$year <- west.by.cohort$age + west.by.cohort$cohort
west.cohort.plot    <- xyplot(data~year,data=west.by.cohort,
              groups=cohort,
              auto.key=list(space="right",points=FALSE,lines=TRUE,type="b"),
              type="b",
              xlab="Year",ylab="Weight in the stock (kg)",
              main=paste(WoS@name,"Weight in the stock by cohort"),
              par.settings=list(superpose.symbol=list(pch=as.character(unique(west.by.cohort$cohort)%%10),cex=1.25)),
              panel=function(...) {
                panel.grid(h=-1,v=-1)
                panel.xyplot(...)
              })
print(west.cohort.plot)


### ======================================================================================================
### Document Assessment
### ======================================================================================================
FnPrint("GENERATING DOCUMENTATION...\n")
#Document the run with alternative table numbering and a reduced width
old.opt <- options("width","scipen")
options("width"=80,"scipen"=1000)


#Now write the file
ica.out.file <- ica.out(WoS,WoS.tun,WoS.ica,format="TABLE 5.6.2.%i HERRING in VIa (N).")
write(ica.out.file,file=paste(output.base,"ica.out",sep="."))
options("width"=old.opt$width,"scipen"=old.opt$scipen)

#And finally, write the results out in the lowestoft VPA format for further analysis eg MFDP
writeFLStock(WoS,output.file=output.base)
 
### ======================================================================================================
### Short Term Forecast
### ======================================================================================================
FnPrint("PERFORMING SHORT TERM FORECAST...\n")
#Make forecast
#WoS recruitment is based on a geometric mean of 1989-2007. Updated by one year, each year
gm.recs                   <- exp(mean(log(rec(trim(WoS.orig,year=1989:2007)))))
stf.ctrl                  <- FLSTF.control(nyrs=1,fbar.nyrs=1,fbar.min=3,fbar.max=6,catch.constraint=22481,f.rescale=TRUE,rec=gm.recs)
WoS@catch.n[1,52,,,,]     <- 1
WoS.stf                   <- FLSTF(stock=WoS.orig,control=stf.ctrl,quiet=TRUE,sop.correct=FALSE)
writeFLStock(WoS.stf,output.file=paste(output.base," WoSaddyr",sep=""))
## use the rounder version so report and quality control database have same values
writeFLStock(WoS,file.path(output.dir,"hawg_her-vian.sum"),type="ICAsum")
# project one year in order to get a single year holding means for YPR output
WoS.proj                  <- stf(WoS.orig,nyears=1,wts.nyears=3,fbar.nyears=1,arith.mean=TRUE,na.rm=TRUE)
writeFLStock(WoS.proj,file.path(output.dir,"hawg_her-vian.ypr"),type="YPR")

# Write the stf results out in the lowestoft VPA format for further analysis eg MFDP
writeFLStock(WoS.stf,output.file=paste(output.base,"with STF"))

### ======================================================================================================
### Save workspace and Finish Up
### ======================================================================================================
FnPrint("SAVING WORKSPACES...\n")
save(WoS,WoS.tun,WoS.ctrl,file=paste(output.base,"Assessment.RData"))
save.image(file=paste(output.base,"Assessment Workspace.RData"))
dev.off()
FnPrint(paste("COMPLETE IN",sprintf("%0.1f",round(proc.time()[3]-start.time,1)),"s.\n\n"))

### ======================================================================================================
### Create the figures for the advice sheet and the summary table and reference points
### ======================================================================================================
WoS.sr <- fmle(FLSR(rec=rec(WoS.orig)[,ac(1959:2010)],ssb=ssb(WoS.orig)[,ac(1957:2007)],model="bevholt"),control=list(parscale=c(0.001,1,0.001)))
#WoS.sr <- fmle(as.FLSR(transform(WoS,stock.n=WoS@stock.n/100000),model="segreg"));
WoS.sr@params <- WoS.sr@params*100000

# Or, for Hockey-Stick
# WoS.sr <- fmle(as.FLSR(transform(WoS,stock.n=WoS@stock.n/100000),model="segreg"));
# WoS.sr@params <- WoS.sr@params*100000

writeStandardOutput(WoS.orig,WoS.sr,WoS.retro,nyrs.=3,output.base,Blim=5e4,Bpa=NULL,Flim=NULL,Fpa=NULL,Bmsy=NULL,Fmsy=0.25)



### ======================================================================================================
### Short Term Forecast data preparation - not used as John has code above
### ======================================================================================================
#gm.recs <- exp(mean(log(rec(trim(WoS,year=1989:2006)))))
#WoS.orig@stock.n["1",ac(2009)] <- gm.recs
#stf.ctrl        <- FLSTF.control(fbar.min=3,fbar.max=6,fbar.nyrs=1,nyrs=1,catch.constraint=1000,f.rescale=TRUE,rec=gm.recs)
#WoS.stf  <- FLSTF(stock=WoS.orig,control=stf.ctrl,survivors=NA,quiet=TRUE,sop.correct=FALSE)

# Write the stf results out in the lowestoft VPA format for further analysis eg MFDP
#writeFLStock(WoS.stf,output.file=paste(output.base,"with STF"))

