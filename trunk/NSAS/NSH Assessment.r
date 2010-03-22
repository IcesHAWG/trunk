######################################################################################################
# NSH FLICA Assessment
#
# $Rev$
# $Date$
#
# Author: Niels Hintzen
# IMARES, The Netherlands
# With great compliments to M.Payne, DTU-aqua
#
# Performs an assessment of Western Baltic Spring Spawning Herring (NSH) in IIIa using the
# FLICA package.
#
# Developed with:
#   - R version 2.8.1
#   - FLCore 2.2
#   - FLICA, version 1.4-12
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
output.base         <-  file.path(output.dir,"NSH Assessment SCAI")  #Output base filename, including directory. Other output filenames are built by appending onto this one
n.retro.years       <-  10                                      #Number of years for which to run the retrospective

### ======================================================================================================
### Output setup
### ======================================================================================================
png(paste(output.base,"figures SCAI - %02d.png"),units = "px", height=1200,width=800,pointsize = 24, bg = "white")
#png(paste(output.base,"figures - 64.png"),units = "px", height=1200,width=800,pointsize = 24, bg = "white")


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
NSH@name                          <- "North Sea Herring"

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

#Load the NSH.tun with the scai rather than the MLAI (exploratory in 2010)
#NSH.tun                         <- readFLIndices(file.path(data.source,"/fleet.txt"),file.path(data.source,"/scai.txt"),type="ICA")

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
# Extra otolith plot with a slightly higher number of samples in there
plot.otolith(NSH,NSH.ica,n=100000)
NSH.retro <- do.retrospective.plots(NSH,NSH.tun,NSH.ctrl,n.retro.years)

### ======================================================================================================
### Custom plots
### ======================================================================================================
FnPrint("GENERATING CUSTOM PLOTS...\n")

# Plot the mature and immature part of the stock
mat.immat.ratio(NSH)
# Plot the cpue for each survey against each other to see if they get through the same signals
cpue.survey(NSH.tun,"index")

# Plot the proportion of catch and weight in numbers and weight to see if the catch is representative for the stock build-up
print(stacked.area.plot(data~year| unit, as.data.frame(pay(NSH@stock.n)),groups="age",main="Proportion of Stock numbers at age",ylim=c(-0.01,1.01),xlab="years",col=gray(9:0/9)))
print(stacked.area.plot(data~year| unit, as.data.frame(pay(NSH@catch.n)),groups="age",main="Proportion of Catch numbers at age",ylim=c(-0.01,1.01),xlab="years",col=gray(9:0/9)))
print(stacked.area.plot(data~year| unit, as.data.frame(pay(NSH@stock.wt)),groups="age",main="Proportion of Stock weight at age",ylim=c(-0.01,1.01),xlab="years",col=gray(9:0/9)))
print(stacked.area.plot(data~year| unit, as.data.frame(pay(NSH@catch.wt)),groups="age",main="Proportion of Catch weight at age",ylim=c(-0.01,1.01),xlab="years",col=gray(9:0/9)))

# Plot the harvest pattern at age as a proportion over time
print(stacked.area.plot(data~year| unit, as.data.frame(pay(NSH@harvest)),groups="age",main="Proportion of harvest pressure at age",ylim=c(-0.01,1.01),xlab="years",col=gray(9:0/9)))

# Plot the proportion of catch in numbers in the indices to see if the indices are having specific yearclass trends
print(stacked.area.plot(data~year| unit, as.data.frame(pay(NSH.tun[[3]]@index)),groups="age",main="Proportion of IBTS index at age",ylim=c(-0.01,1.01),xlab="years",col=gray(9:0/9)))
print(stacked.area.plot(data~year| unit, as.data.frame(pay(NSH.tun[[4]]@index)),groups="age",main="Proportion of Acoustic index at age",ylim=c(-0.01,1.01),xlab="years",col=gray(9:0/9)))

# Plot the catch curves to see if there is a change in the selectivity of the fleet (seperable period) or the age which is targetted the most (seperable age)
catch.curves(NSH,1990,2009)

# Plot the reference points on yield - F curves, and print the reference points
NSH.sr <- ref.pts(NSH,"bevholt",100000)
#NSH.sr <- ref.pts(NSH,"ricker",100000)

# Create a co-plot of the tuning indices, and see if there is correlation between age groups in the survey (there should be)
cor.tun(NSH.tun)

# Plot the historic indices updated with new values against each other to see if similar patterns are coming through
source("./private_diagnostics.r")
#LNV.fbar(NSH,0.25,0.1,c(2,6))
#LNV.fbar(NSH,0.1,0.04,c(0,1))
#LNV.ssb(NSH,1.5e6,0.8e6)
#LNV.rec(NSH,NSH.ica)

# Calculate the stock recruitment fit, and plot it, and also add years to the plot
NSH.sr <- fmle(as.FLSR(transform(NSH,stock.n=NSH@stock.n/100000),model="bevholt")); 
plot(NSH.sr)
NSH.sr@params <- NSH.sr@params*100000
plot(NSH.sr@rec[,-1]~NSH.sr@ssb[,1:c(length(dimnames(NSH.sr@ssb)$year)-1)],type="b",xlab="SSB",ylab="Rec",main="Yearly stock recruitment relationship")
text(NSH.sr@rec[,-1]~NSH.sr@ssb[,1:c(length(dimnames(NSH.sr@ssb)$year)-1)],labels=dimnames(NSH.sr@rec)$year[-1],pos=1,cex=0.7)

# Plot the time series of weight in the stock and catch in the stock
timeseries(window(NSH,1975,2009),slot="stock.wt")
timeseries(window(NSH,1975,2009),slot="catch.wt")
timeseries(window(NSH,2000,2009),slot="harvest")
timeseries(window(NSH,1990,2009),slot="mat")

#Time series of weigth of the stock anomalies
anom.plot(trim(NSH@stock.wt,year=1983:dims(NSH)$maxyear,age=0:1),xlab="Year",ylab="Anomaly (std. devs)",
    main=paste(NSH@name,"Weight in the Stock Anomaly (Age 0-1)"),ylim=c(-3,3))
anom.plot(trim(NSH@stock.wt,year=1983:dims(NSH)$maxyear,age=3:6),xlab="Year",ylab="Anomaly (std. devs)",
    main=paste(NSH@name,"Weight in the Stock Anomaly (Age 3-6)"),ylim=c(-3,3))

#Time series of west by cohort
west.by.cohort      <- as.data.frame(FLCohort(window(NSH@stock.wt,1980,2009)))
west.by.cohort      <- subset(west.by.cohort,!is.na(west.by.cohort$data))
west.by.cohort$year <- west.by.cohort$age + west.by.cohort$cohort
west.cohort.plot    <- xyplot(data~year,data=west.by.cohort,
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

# Plot the TAC's versus the realized catches. The TAC's have to be added manually
par(oma=c(rep(2,4)))
TACs          <- data.frame(year=1987:2009,TAC=c(600,530,514,415,420,430,430,440,440, 156+44,159+24,254+22,265+30,265+36,265+36,265+36,400+52,460+38,535+50,455+43,341+32,201+19,171))
TAC.plot.dat  <- data.frame(year=rep(TACs$year,each=2)+c(-0.5,0.5),TAC=rep(TACs$TAC,each=2))
catch         <- as.data.frame(NSH@catch[,ac(1987:2009)]/1e3)
plot(0,0,pch=NA,xlab="Year",ylab="Catch",xlim=range(c(catch$year,TAC.plot.dat$year)),ylim=range(c(0,TAC.plot.dat$TAC,catch$data)),cex.lab=cl,cex.axis=ca,family=fam,font=fonts)
rect(catch$year-0.5,0,catch$year+0.5,catch$data,col="grey")
lines(TAC.plot.dat,lwd=3)
legend("topright",legend=c("Catch","TAC"),lwd=c(1,5),lty=c(NA,1),pch=c(22,NA),col="black",pt.bg="grey",pt.cex=c(2),box.lty=0)
box()
title(main=paste(NSH@name,"Catch and TAC"))
mtext("Working group estimate",side=1,outer=F,line=5,cex=ca)



### ======================================================================================================
### Document Assessment
### ======================================================================================================
FnPrint("GENERATING DOCUMENTATION...\n")
#Document the run with alternative table numbering and a reduced width
old.opt           <- options("width","scipen")
options("width"=80,"scipen"=1000)
ica.out.file      <- ica.out(NSH,NSH.tun,NSH.ica,format="TABLE 3.6.%i North Sea Herring HERRING.")
write(ica.out.file,file=paste(output.base,"ica.out",sep="."))
options("width"=old.opt$width,"scipen"=old.opt$scipen)

#And finally, write the results out in the lowestoft VPA format for further analysis eg MFDP
writeFLStock(NSH,output.file=output.base)

### ======================================================================================================
### Short Term Forecast
### ======================================================================================================
FnPrint("PERFORMING SHORT TERM FORECAST...\n")
REC               <- NSH.ica@param["Recruitment prediction","Value"]
TAC               <- 164300 #overshoot = approximately 13% every year + 1000 tons of transfer             #194233 in 2009  #It does not matter what you fill out here as it only computes the suvivors
NSH.stf           <- FLSTF.control(fbar.min=2,fbar.max=6,nyrs=1,fbar.nyrs=1,f.rescale=TRUE,rec=REC,catch.constraint=TAC)
NSH.stock10       <- as.FLStock(FLSTF(stock=NSH,control=NSH.stf,unit=1,season=1,area=1,survivors=NA,quiet=TRUE,sop.correct=FALSE))

#A plot on the agreed management plan with the estimated Fbar in 2010
plot(x=c(0,0.8,1.5,2),y=c(0.1,0.1,0.25,0.25),type="l",ylim=c(0,0.4),lwd=2,xlab="SSB in million tonnes",ylab="Fbar",cex.lab=1.3,main="Management plan North Sea Herring")
abline(v=0.8,col="red",lwd=2,lty=2)
abline(v=1.3,col="blue",lwd=2,lty=2)
abline(v=1.5,col="darkgreen",lwd=2,lty=2)
text(0.8,0,labels=expression(B[lim]),col="red",cex=1.3,pos=2)
text(1.3,0,labels=expression(B[pa]),col="blue",cex=1.3,pos=2)
text(1.5,0,labels=expression(B[trigger]),col="darkgreen",cex=1.3,pos=4)

points(y=fbar(NSH.stock10[,ac(2002:2010)]), x=(ssb(NSH.stock10[,ac(2002:2010)])/1e6),pch=19)
lines(y=fbar(NSH.stock10[,ac(2002:2010)]),  x=(ssb(NSH.stock10[,ac(2002:2010)])/1e6))
text(y=fbar(NSH.stock10[,ac(2002:2010)]),   x=(ssb(NSH.stock10[,ac(2002:2010)])/1e6),labels=ac(2002:2010),pos=3,cex=0.7)



#Write the stf results out in the lowestoft VPA format for further analysis eg MFDP
writeFLStock(NSH.stock10,output.file=paste(output.base,"with STF"))

### ======================================================================================================
### Save workspace and Finish Up
### ======================================================================================================
FnPrint("SAVING WORKSPACES...\n")
save(NSH,NSH.stock10,NSH.tun,NSH.ctrl,file=paste(output.base,"Assessment.RData"))
save.image(file=paste(output.base,"Assessment Workspace.RData"))
dev.off()
FnPrint(paste("COMPLETE IN",sprintf("%0.1f",round(proc.time()[3]-start.time,1)),"s.\n\n"))

### ======================================================================================================
### Create the figures for the advice sheet and the summary table and reference points
### ======================================================================================================

writeStandardOutput(NSH,NSH.sr,NSH.retro,nyrs.=3,output.base,Blim=0.8e6,Bpa=1.3e6,Flim=NULL,Fpa=0.25,Bmsy=NULL,Fmsy=NULL)
