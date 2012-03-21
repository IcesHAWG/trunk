################################################################################
# ISH_Setup_Objects
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
rm(list=ls()); graphics.off(); start.time <- proc.time()[3]
options(stringsAsFactors=FALSE)
log.msg     <-  function(string) {cat(string);}
log.msg("\nISH Final Assessment\n=====================\n")
library(FLCore)
library(FLSAM)
library(FLEDA)
packageDescription("FLSAM") #check version of FLSAM used

### ============================================================================
### Misc
### ============================================================================
my.path<-file.path("C:","Users","PJSchon","Documents","AESD","Irish Sea herring","HAWG2012","SAM")
output.dir              <- file.path(my.path,"results")
data.source         <- file.path(my.path,"data")    #Data source, not code or package source!!!



### ============================================================================
### Setup assessment
### ============================================================================
### ============================================================================
### Prepare stock object for assessment
### ============================================================================
ISH                 <- readFLStock(file.path(data.source,"index.txt"),no.discards=TRUE)


#Catch is calculated from: catch.wt * catch.n, however, the reported landings are
#normally different (due to SoP corrections). Hence we overwrite the calculate landings
ISH@catch           <- ISH@landings
units(ISH)[1:17]    <- as.list(c(rep(c("tonnes","thousands","kg"),4), rep("NA",2),"f",rep("NA",2)))

#Set object details
ISH@name                              <- "Irish Sea herring"
range(ISH)[c("minfbar","maxfbar")]    <- c(4,6)
ISH                                   <- setPlusGroup(ISH,ISH@range["max"])

#Replace zero or missign values in catch.wt and stock.wt
#which(is.na(ISH@stock.wt)==T,arr.ind=T) #check for NA data
ISH@stock.wt[8,49] <- (ISH@stock.wt[8,48]+ISH@stock.wt[8,50])/2
#which(ISH@stock.wt==0,arr.ind=T) #generate array of values=0
ISH@stock.wt[7,4] <- (ISH@stock.wt[7,3]+ISH@stock.wt[7,5])/2 #manual replacement avg if 0 
ISH@stock.wt[8,6:9] <- c(0.264,0.275,0.264,0.27) #otherwise replace with values in input files

#which(is.na(ISH@catch.wt)==T,arr.ind=T) #check for NA data
ISH@catch.wt[7,4] <- (ISH@catch.wt[7,3]+ISH@catch.wt[7,5])/2
ISH@catch.wt[6,7] <- 0.243
ISH@catch.wt[7,7:8] <- c(0.227,0.234)
ISH@catch.wt[1:8,49] <- c((ISH@catch.wt[1:8,48]+ISH@catch.wt[1:8,50])/2)
#which(ISH@catch.wt==0,arr.ind=T) #generate array of values=0
ISH@catch.wt[8,6:9] <- c(0.264,0.275,0.264,0.270)

#which(is.na(ISH@landings.wt)==T,arr.ind=T) #check for NA data
ISH@landings.wt[8,49] <- (ISH@landings.wt[8,48]+ISH@landings.wt[8,50])/2
#which(ISH@landings.wt==0,arr.ind=T) 
ISH@landings.wt[7,4] <- (ISH@landings.wt[7,3]+ISH@landings.wt[7,5])/2 
ISH@landings.wt[8,6:9] <- c(0.264,0.275,0.264,0.27) 

### ============================================================================
### Prepare index object for assessment
### ============================================================================
#Load and modify all index data
ISH.tun             <- readFLIndices(file.path(data.source,"fleet.txt"),
                                     file.path(data.source,"ssb.txt"),type="ICA")
                                
#Set names
ISH.tun[[1]]@name   <- "AC(VIIaN)"
ISH.tun[[2]]@name   <- "NINEL"
names(ISH.tun)      <- lapply(ISH.tun,name)

#Set parameters etc
ISH.tun[["AC(VIIaN)"]]@type    <- "number"
ISH.tun[["NINEL"]]@type   <- "biomass"

## ============================================================================
### Setup default assessment configuration
### ============================================================================
#Setup configuration - creates an empty control object with appropriate structure
ISH.ctrl <- FLSAM.control(ISH,ISH.tun)

#FISHing mortality random walk coupling
ISH.ctrl@states["catch",] <- c(1:6,7,7)               #Couple age 8+ Fs
ISH.ctrl@f.vars["catch",] <- c(rep(1,8))                   #All have the same variance across ages

#Log N random walk variances
ISH.ctrl@logN.vars        <- c(1,rep(2,7))

#Survey Power law application
# ctrl@power.law.exps

#Catchability models
ISH.ctrl@catchabilities["AC(VIIaN)",ac(1:8)]    <- c(1:3,rep(4,5))
#ISH.ctrl@catchabilities["NINEL",ac(1:8)]  <- c(rep(NA,8)) #values NA by default if not defined

#Observation model parameters
ISH.ctrl@obs.vars["catch",ac(1:8)]     <- c(1,2,3,3,4,4,4,4)
ISH.ctrl@obs.vars["AC(VIIaN)",ac(1:8)]  <- c(5,6,6,7,7,8,8,8) #1-8

# # Stock recruitment model code (0=RW, 1=Ricker, 2=BH, ... more in time
ISH.ctrl@srr <- as.integer(0)

### ============================================================================
### Run the assessment
### ============================================================================
#Run assessment
ISH.sam <- FLSAM(ISH,ISH.tun,ISH.ctrl)
name(ISH.sam) <- "Irish Sea Herring"
#Update stock object
ISH     <- ISH + ISH.sam

#Quick look
plot(ISH.sam)

# Save results
save(ISH,ISH.tun,ISH.ctrl,ISH.sam,file=file.path(output.dir,paste(name(ISH.sam),".RData",sep="")))

### ============================================================================
### Outputs
### ============================================================================
### ============================================================================
### Plots
### ============================================================================
#Import externals
source(file.path(my.path,"_Common","HAWG_Common_module.r"))
# Need to have following list of r files in same folder: "Stacked Area plot.r", "WriteIcaSum.r",
#"writeStandardOutput.r","Taylor_diagram.r"

#Setup plots
pdf(file.path(output.dir,paste(name(ISH.sam),".pdf",sep="")))
### ============================================================================
### Input data
### ============================================================================


# Plot the mature and immature part of the stock
print(mat.immat.ratio(ISH))

# Plot the overlay of tuning series
print(overlayTimeseries(lapply(ISH.tun,index),nyrs=10,ages=0:4)) #not much change in ages 5+

# Plot the overlay by year and age
print(surveyTimeseries(ISH.tun))


#plot(tmp.tun,type="pairwise")
plot(ISH.tun[["AC(VIIaN)"]],type="internal")

# Plot the proportion of catch and weight in numbers and weight to see if the catch is representative for the stock build-up
print(stacked.area.plot(data~year| unit, as.data.frame(pay(ISH@catch.n)),groups="age",main="Proportion of Catch numbers at age",ylim=c(-0.01,1.01),xlim=c(range(ISH)["minyear"]-2,range(ISH)["maxyear"]+1),xlab="years",col=gray(9:0/9)))
#print(stacked.area.plot(data~year| unit, as.data.frame(pay(ISH@stock.wt)),groups="age",main="Proportion of Stock weight at age",ylim=c(-0.01,1.01),xlab="years",col=gray(9:0/9))) #not very informative plot#
#print(stacked.area.plot(data~year| unit, as.data.frame(pay(ISH@catch.wt)),groups="age",main="Proportion of Catch weight at age",ylim=c(-0.01,1.01),xlab="years",col=gray(9:0/9))) #not very informative plot#

# Plot the proportion of catch in numbers in the indices to see if the indices are having specific yearclass trends
print(stacked.area.plot(data~year| unit, as.data.frame(pay(ISH.tun[["AC(VIIaN)"]]@index)),groups="age",main="Proportion of Acoustic index at age",ylim=c(-0.01,1.01),xlim=c(1993.5,range(ISH.tun)["maxyear"]+0.5),xlab="years",col=gray(9:0/9)))

# Plot the proportion of natural mortality - only informative if time variant M
#print(stacked.area.plot(data~year| unit, as.data.frame(pay(ISH@m)),groups="age",main="Proportion of natural at age",ylim=c(-0.01,1.01),xlab="years",col=gray(9:0/9)))

# Plot the time series of weight in the stock and catch in the stock
timeseries(window(ISH,1961,range(ISH)["maxyear"]),slot="stock.wt")
timeseries(window(ISH,1961,range(ISH)["maxyear"]),slot="catch.wt")
timeseries(window(ISH,2000,range(ISH)["maxyear"]),slot="harvest")
timeseries(window(ISH,1990,range(ISH)["maxyear"]),slot="mat")

# Plot the time series of the surveys
timeseries(ISH.tun[["AC(VIIaN)"]],slot="index")
       # need to run function below to get rid of age labels for NINEL as oppose to source code - need to reload source code if doing more timeseries plots
timeseries <- function(stck.,slot.){
  assign("stck.",stck.,envir=.GlobalEnv);assign("slot.",slot.,envir=.GlobalEnv);
  print(xyplot(data~year,data=slot(stck.,slot.),
               groups=age,type="b",
               xlab="Year",ylab=paste("Time series of",slot.,ifelse(units(slot(stck.,slot.))=="NA","",paste("(",units(slot(stck.,slot.)),")",sep=""))),
               main=paste(stck.@name,"timeseries of",slot.)))}
timeseries(ISH.tun[["NINEL"]],slot="index")

#Time series of west by cohort
west.by.cohort      <- as.data.frame(FLCohort(window(ISH@stock.wt,1980,range(ISH)["maxyear"])))
west.by.cohort      <- subset(west.by.cohort,!is.na(west.by.cohort$data))
west.by.cohort$year <- west.by.cohort$age + west.by.cohort$cohort
west.cohort.plot    <- xyplot(data~year,data=west.by.cohort,
                              groups=cohort,
                              auto.key=list(space="right",points=FALSE,lines=TRUE,type="b"),
                              type="b",
                              xlab="Year",ylab="Weight in the stock (kg)",
                              main=paste(ISH@name,"Weight in the stock by cohort"),
                              par.settings=list(superpose.symbol=list(pch=as.character(unique(west.by.cohort$cohort)%%10),cex=1.25)),
                              panel=function(...) {
                                panel.grid(h=-1,v=-1)
                                panel.xyplot(...)
                              })
print(west.cohort.plot)

### ============================================================================
### Model fit
### ============================================================================

#Survey fits
residual.diagnostics(ISH.sam)

# Plot the harvest pattern at age as a proportion over time & the stock.n as proportion over time
print(stacked.area.plot(data~year| unit, as.data.frame(pay(ISH@harvest)),groups="age",main="Proportion of harvest pressure at age",ylim=c(-0.01,1.01),xlab="years",col=gray(9:0/9)))
print(stacked.area.plot(data~year| unit, as.data.frame(pay(ISH@stock.n)),groups="age",main="Proportion of Stock numbers at age",ylim=c(-0.01,1.01),xlab="years",col=gray(9:0/9)))

#Plot result
print(plot(ISH.sam))

#Plot uncertainties as a function of time
CV.yrs <- ssb(ISH.sam)$year
CV.dat <- cbind(SSB=ssb(ISH.sam)$CV,
                Fbar=fbar(ISH.sam)$CV,Rec=rec(ISH.sam)$CV)
matplot(CV.yrs,CV.dat,type="l",ylim=range(pretty(c(0,CV.dat))),yaxs="i",
        xlab="Year",ylab="CV of estimate",main="Uncertainties of key parameters")
legend("topleft",legend=colnames(CV.dat),lty=1:5,col=1:6,bty="n")

#Plot catchabilities values
catch <- catchabilities(ISH.sam)
print(xyplot(value+ubnd+lbnd ~ age | fleet,subset(catch,fleet=="AC(VIIaN)"),
             scale=list(alternating=FALSE,y=list(relation="free")),as.table=TRUE,
             type="l",lwd=c(2,1,1),col=c("black","grey","grey"),
             main="Survey catchability parameters",ylab="Catchability",xlab="Age"))

#Plot obs_variance (weightings)
obv <- obs.var(ISH.sam)
obv$str <- paste(obv$fleet,ifelse(is.na(obv$age),"",obv$age))
obv <- obv[order(obv$value),]
bp <- barplot(obv$value,ylab="Observation Variance",
              main="Observation variances by data source",col=factor(obv$fleet),ylim=c(0,1.0))
axis(1,at=bp,labels=obv$str,las=3,lty=0,mgp=c(0,0,0))
legend("topleft",levels(obv$fleet),pch=15,col=1:nlevels(obv$fleet),pt.cex=1.5)

plot(obv$value,obv$CV,xlab="Observation variance",ylab="CV of estimate",log="x",
     pch=16,col=obv$fleet,main="Observation variance vs uncertainty")
text(obv$value,obv$CV,obv$str,pos=4,cex=0.75,xpd=NA)

#Plot fishery selectivity pattern over time
sel.pat <- merge(f(ISH.sam),fbar(ISH.sam),
                 by="year",suffixes=c(".f",".fbar"))
sel.pat$sel <- sel.pat$value.f/sel.pat$value.fbar
sel.pat$age <- as.numeric(as.character(sel.pat$age))
print(xyplot(sel ~ age|sprintf("%i's",floor((year+2)/5)*5),sel.pat,
             groups=year,type="l",as.table=TRUE,
             scale=list(alternating=FALSE),
             main="Selectivity of the Fishery by Pentad",xlab="Age",ylab="F/Fbar"))

#Plot correlation matrix
cor.plot(ISH.sam)

#Plot otholith
#plot.otolith(NSH.sam,n=100000)

###accessing the numbers...viewing it
slotNames(ISH.sam) # list slot names
ISH.sam@nlogl # to view slot "nlogl"= Objective function value
ISH.sam@params


### ======================================================================================================
### Document Assessment
### ======================================================================================================
log.msg     <-  function(string) {cat(string);}
log.msg("\nISH Final Assessment\n=====================\n")
log.msg("GENERATING DOCUMENTATION...\n")
#Document the run with alternative table numbering and a reduced width
old.opt           <- options("width","scipen")
options("width"=old.opt$width,"scipen"=old.opt$scipen)

#And finally, write the results out in the lowestoft VPA format for further analysis eg MFDP
#writeFLStock(NSH,output.file=output.base)

#And for incorporation into the standard graphs
#writeFLStock(NSH,file.path(output.dir,"hawg_her-47d3.sum"),type="ICAsum")
#The YPR curves based on the same values as the projection - therefore use WBSS.proj
#writeStandardOutput(NSH,NSH.sr,NSH.retro,recImY=NSH.ica@survivors[1,1],nyrs.=3,output.base,Blim=0.8e6,Bpa=1.3e6,Flim=NULL,Fpa=0.25,Bmsy=NULL,Fmsy=NULL)

### ============================================================================
### Finish
### ============================================================================
dev.off()
log.msg(paste("COMPLETE IN",sprintf("%0.1f",round(proc.time()[3]-start.time,1)),"s.\n\n"))


### ============================================================================
### Short term forecast
### ============================================================================
### ============================================================================
### Input data
###=============================================================================


#survivors
dmns                <- dims(ISH@stock.n)
gm.recruitmentEstimate <- exp(mean(log(ISH@stock.n[1,as.character(1995:(ISH@range['maxyear']-2)),,,,])))
survivors           <- FLQuant(c(gm.recruitmentEstimate,stock.n(ISH)[,ac(ISH@range['maxyear'])] * exp(-harvest(ISH[,ac(ISH@range['maxyear'])])-m(ISH[,ac(2011)]))),
                               dimnames=list(ages=dmns$min:(dmns$max+1),year=ISH@range['maxyear']+1,unit=dmns$unit,season=dmns$season,area=dmns$area,iter=dmns$iter))

## plusgroup
survivors[ac(dmns$max),ac(ISH@range['maxyear']+1)] <- quantSums(survivors[ac(dmns$max:(dmns$max+1)),ac(ISH@range['maxyear']+1)])
survivors           <- survivors[ac(dmns$min:dmns$max),]


#Update
#ISH.ctrl <- update(ISH.ctrl)

# useful commmands

#getwd()

#FLR2SAM(stck,tun,ctrl,run.dir=".")  #makes the wd the rund directory

#packageDescription("FLSAM")

#save(stck,ctrl,tun,file="IrishSH.RData") # as I changed the wd then it saves the files to that directory