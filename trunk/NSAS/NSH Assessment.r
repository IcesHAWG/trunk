######################################################################################################
# NSH FLICA Assessment
#
# $Rev$
# $Date$
#
# Author: Niels Hintzen
# Wageningen IMARES, The Netherlands
# With great compliments to M.Payne, DTU-aqua
#
# Performs an assessment of Western Baltic Spring Spawning Herring (NSH) in IIIa using the
# FLICA package.
#
# Developed with:
#   - R version 2.8.0
#   - FLCore 3.0
#   - FLICA, version 1.4-8
#   - FLAssess, version 1.99-102
#   - FLSTF, version 1.99-1
#   - FLEDA, version 2.0
#   - FLash, version 2.0.0
#   - FLBRP, version 2.0.0
#
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

path <- "N:/Projecten/ICES WG/Haring werkgroep HAWG/2010/assessment2/NSAS/"
try(setwd(path))

#in need of something extra

options(stringsAsFactors=FALSE)
FnPrint     <-  function(string) {
	cat(string)
}
FnPrint("\nNSH FLICA Assessment\n=====================\n")

### ======================================================================================================
### Incorporate Common modules
### Uses the common HAWG FLICA Assessment module to do the graphing, diagnostics and output
### ======================================================================================================
source(file.path("..","_Common","HAWG Common assessment module.r"))
### ======================================================================================================

### ======================================================================================================
### Define parameters for use in the assessment code here
### ======================================================================================================
data.source         <-  file.path(".","data")                   #Data source, not code or package source!!!
output.dir          <-  file.path(".","results")                #Output directory
output.base         <-  file.path(output.dir,"NSH Assessment")  #Output base filename, including directory. Other output filenames are built by appending onto this one
n.retro.years       <-  10                                      #Number of years for which to run the retrospective

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

#Setup FLICA control object
NSH.ctrl <- FLICA.control(sep.nyr=5, sep.age=4, sep.sel=1.0, sr.age=1, sr=TRUE,
                          lambda.age=c(0.1, 0.1, 3.67, 2.87, 2.23, 1.74, 1.37, 1.04, 0.94, 0.91),
                          lambda.yr=c(1.0, 1.0, 1.0, 1.0, 1.0),
                          lambda.sr=0.1,
                          index.model=c("l","l","l","p"), index.cor=FALSE)  #index model: Acoustic, IBTS, MIK, MLAI
### don't forget to reorder the index models too!!                          
NSH.ctrl@index.model <- rev(NSH.ctrl@index.model)

### ======================================================================================================
### Prepare stock object for assessment
### ======================================================================================================
FnPrint("PREPARING STOCK OBJECT...\n")
NSH                               <- readFLStock(file.path(data.source, "index.txt"),no.discards=TRUE)

#- Catch is calculated from: catch.wt * catch.n, however, the reported landings are
#   normally different (due to SoP corrections). Hence we overwrite the calculate landings
NSH@catch                         <- NSH@landings
units(NSH)[1:17]                  <- as.list(c(rep(c("tonnes","thousands","kg"),4), rep("NA",5)))

#- Set fbar ages
range(NSH)[c("minfbar","maxfbar")]<- c(2,6)

#- Set stock object name - this is propagated through into the figure titles
NSH@name                          <- "Autumn spawning herring in IV, VIId, IIIa"

#- Set plus group
NSH                               <- setPlusGroup(NSH,NSH@range["max"])

#- No catches of age 10 in 1977 s0 stock.wt does not get filled there.
#   Hence, we copy the stock weight for that age from the previous year
NSH@stock.wt[10,"1977"]           <- NSH@stock.wt[10,"1976"]


### ======================================================================================================
### Prepare index object for assessment
### ======================================================================================================
FnPrint("PREPARING INDEX OBJECT...\n")
#Load and modify all index data
NSH.tun                         <- readFLIndices(file.path(data.source,"/fleet.txt"),file.path(data.source,"/ssb.txt"),type="ICA")

#Set names, and parameters etc
NSH.tun[[1]]@type               <- "number"
NSH.tun[[2]]@type               <- "number"
NSH.tun[[3]]@type               <- "number"
NSH.tun[[3]]@range["plusgroup"] <- NA

NSH.tun                         <- rev(NSH.tun)
if (NSH.tun[[1]]@name != "MLAI") print("Error - MLAI not as the first index")
### give 1/weighting factors as variance
NSH.tun[[4]]@index.var[]        <- 1.0/FLQuant(c(0.63,0.62,0.17,0.10,0.09,0.08,0.07,0.07,0.05),dimnames=dimnames(NSH.tun[[4]]@index)) #Acoustic
NSH.tun[[3]]@index.var[]        <- 1.0/FLQuant(c(0.47,0.28,0.01,0.01,0.01),dimnames=dimnames(NSH.tun[[3]]@index)) #IBTS
NSH.tun[[2]]@index.var[]        <- 1.0/FLQuant(0.63,dimnames=dimnames(NSH.tun[[2]]@index)) #MIK
NSH.tun[[1]]@index.var[]        <- 1.0/FLQuant(0.60,dimnames=dimnames(NSH.tun[[1]]@index)) #MLAI
#Set names
names(NSH.tun)                  <- lapply(NSH.tun,name)
### ======================================================================================================
### Perform the assessment
### ======================================================================================================
FnPrint("PERFORMING ASSESSMENT...\n")
#Now perform the asssessment
NSH.ica                         <-  FLICA(NSH,NSH.tun,NSH.ctrl)
NSH                             <-  NSH + NSH.ica
range(NSH.ica)                  <-  range(NSH)[1:5]
NSH@stock                       <-  computeStock(NSH)


### ======================================================================================================
### Use the standard code from the common modules to produce outputs
### ======================================================================================================
do.summary.plots(NSH,NSH.ica)
NSH.retro <- do.retrospective.plots(NSH,NSH.tun,NSH.ctrl,n.retro.years)

### ======================================================================================================
### Custom plots
### ======================================================================================================
FnPrint("GENERATING CUSTOM PLOTS...\n")

mat.immat.ratio(NSH)
cpue.survey(NSH.tun,"index") 

print(stacked.area.plot(data~year| unit, as.data.frame(pay(NSH@stock.n)),groups="age",main="Proportion of stock.n at age",ylim=c(-0.01,1.01),xlab="years",col=gray(9:0/9)))
print(stacked.area.plot(data~year| unit, as.data.frame(pay(NSH@catch.n)),groups="age",main="Proportion of Catch.n at age",ylim=c(-0.01,1.01),xlab="years",col=gray(9:0/9)))
print(stacked.area.plot(data~year| unit, as.data.frame(pay(NSH@catch.wt)),groups="age",main="Proportion of Catch.wt at age",ylim=c(-0.01,1.01),xlab="years",col=gray(9:0/9)))
print(stacked.area.plot(data~year,as.data.frame(pay(NSH@stock.n*NSH@stock.wt)),groups="age",main="Proportion by weight in the stock",ylim=c(-0.01,1.01),xlab="years",col=gray(9:0/9)))
print(stacked.area.plot(data~age| unit, as.data.frame(pay(NSH@catch.n)),groups="year",main="Total Historic Catches from an age group",xlab="years",col=gray(9:0/9)))

print(stacked.area.plot(data~year| unit, as.data.frame(pay(NSH.tun[[3]]@index)),groups="age",main="Proportion of IBTS index at age",ylim=c(-0.01,1.01),xlab="years",col=gray(9:0/9)))
print(stacked.area.plot(data~year| unit, as.data.frame(pay(NSH.tun[[4]]@index)),groups="age",main="Proportion of Acoustic index at age",ylim=c(-0.01,1.01),xlab="years",col=gray(9:0/9)))

catch.curves(NSH,1990,2009)
NSH.sr <- ref.pts(NSH,"bevholt",100000)
cor.tun(NSH.tun)

source("./private_diagnostics.r")
LNV.fbar(NSH,0.25,0.1,c(2,6))
LNV.fbar(NSH,0.1,0.04,c(0,1))
LNV.ssb(NSH,1.5e6,0.8e6)
LNV.rec(NSH,NSH.ica)     

NSH.sr <- fmle(as.FLSR(transform(NSH,stock.n=NSH@stock.n/100000),model="bevholt")); 
plot(NSH.sr)
NSH.sr@params <- NSH.sr@params*100000
plot(NSH.sr@rec[,-1]~NSH.sr@ssb[,1:48],type="b",xlab="SSB",ylab="Rec",main="Yearly stock recruitment relationship")
text(NSH.sr@rec[,-1]~NSH.sr@ssb[,1:48],labels=dimnames(NSH.sr@rec)$year[-1],pos=1,cex=0.7)

#Time series of west
west.ts  <- xyplot(data~year,data=window(NSH@stock.wt,1975,2009),
              groups=age,
              auto.key=list(space="right",points=FALSE,lines=TRUE,type="b"),
              type="b",
              xlab="Year",ylab="Weight in the stock (kg)",
              main=paste(NSH@name,"Weight in the Stock"),
              par.settings=list(superpose.symbol=list(pch=as.character(0:8),cex=1.25)))
print(west.ts)

#Time series of west anomalies
anom.plot(trim(NSH@stock.wt,year=1983:dims(NSH)$maxyear,age=0:1),xlab="Year",ylab="Anomaly (std. devs)",
    main=paste(NSH@name,"Weight in the Stock Anomaly (Age 0-1)"),ylim=c(-3,3))
anom.plot(trim(NSH@stock.wt,year=1983:dims(NSH)$maxyear,age=3:6),xlab="Year",ylab="Anomaly (std. devs)",
    main=paste(NSH@name,"Weight in the Stock Anomaly (Age 3-6)"),ylim=c(-3,3))

#Time series of west by cohort
west.by.cohort  <- as.data.frame(FLCohort(window(NSH@stock.wt,1980,2009)))
west.by.cohort  <-  subset(west.by.cohort,!is.na(west.by.cohort$data))
west.by.cohort$year <- west.by.cohort$age + west.by.cohort$cohort
west.cohort.plot  <- xyplot(data~year,data=west.by.cohort,
              groups=cohort,
              auto.key=list(space="right",points=FALSE,lines=TRUE,type="b"),
              type="b",
              xlab="Year",ylab="Weight in the stock (kg)",
              main=paste(NSH@name,"Weight in the stock by cohort"),
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
ica.out.file <- ica.out(NSH,NSH.tun,NSH.ica,format="TABLE 3.6.%i NSH HERRING.")
write(ica.out.file,file=paste(output.base,"ica.out",sep="."))
options("width"=old.opt$width,"scipen"=old.opt$scipen)

#And finally, write the results out in the lowestoft VPA format for further analysis eg MFDP
writeFLStock(NSH,output.file=output.base)


### ======================================================================================================
### Short Term Forecast
### ======================================================================================================
FnPrint("PERFORMING SHORT TERM FORECAST...\n")
REC               <- NSH.ica@param["Recruitment prediction","Value"]
TAC               <- 194233 #overshoot = approximately 13% every year + 1000 tons of transfer
NSH.stf           <- FLSTF.control(fbar.min=2,fbar.max=6,nyrs=1,fbar.nyrs=1,f.rescale=TRUE,rec=REC,catch.constraint=TAC)
NSH.stock09       <- as.FLStock(FLSTF(stock=NSH,control=NSH.stf,unit=1,season=1,area=1,survivors=NA,quiet=TRUE,sop.correct=FALSE))

#A plot on the agreed management plan with the estimated Fbar in 2009
plot(x=c(0,0.8,1.5,2),y=c(0.1,0.1,0.25,0.25),type="l",ylim=c(0,0.4),lwd=2,xlab="SSB in million tonnes",ylab="Fbar",cex.lab=1.3,main="Management plan North Sea Herring")
abline(v=0.8,col="red",lwd=2,lty=2)
abline(v=1.3,col="blue",lwd=2,lty=2)
abline(v=1.5,col="darkgreen",lwd=2,lty=2)
text(0.8,0,labels=expression(B[lim]),col="red",cex=1.3,pos=2)
text(1.3,0,labels=expression(B[pa]),col="blue",cex=1.3,pos=2)
text(1.5,0,labels=expression(B[trigger]),col="darkgreen",cex=1.3,pos=4)

points(y=fbar(NSH.stock09[,ac(2002:2009)]),x=(ssb(NSH.stock09[,ac(2002:2009)])/1e6),pch=19)
lines(y=fbar(NSH.stock09[,ac(2002:2009)]),x=(ssb(NSH.stock09[,ac(2002:2009)])/1e6))
text(y=fbar(NSH.stock09[,ac(2002:2009)]),x=(ssb(NSH.stock09[,ac(2002:2009)])/1e6),labels=ac(2002:2009),pos=3,cex=0.7)

#Write the stf results out in the lowestoft VPA format for further analysis eg MFDP
writeFLStock(NSH.stock09,output.file=paste(output.base,"with STF"))

### ======================================================================================================
### Save workspace and Finish Up
### ======================================================================================================
FnPrint("SAVING WORKSPACES...\n")
save(NSH,NSH.stock09,NSH.tun,NSH.ctrl,file=paste(output.base,"Assessment.RData"))
save.image(file=paste(output.base,"Assessment Workspace.RData"))
dev.off()
FnPrint(paste("COMPLETE IN",sprintf("%0.1f",round(proc.time()[3]-start.time,1)),"s.\n\n"))