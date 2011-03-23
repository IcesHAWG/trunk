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

path <- "N:/Projecten/ICES WG/Haring werkgroep HAWG/2011/assessment2/NSAS/"
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
png(paste(output.base,"figures - %02d.png"),units = "px", height=1200,width=900,pointsize = 24, bg = "white")
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
NSH.tun[[1]]@name               <- "MLAI"
NSH.tun[[2]]@name               <- "IBTS0"
NSH.tun[[3]]@name               <- "IBTS-Q1"
NSH.tun[[4]]@name               <- "HERAS"
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
NSH.retro <- do.retrospective.plots(NSH,NSH.tun,NSH.ctrl,4)

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
catch.curves(NSH,1990,2010)

# Plot the reference points on yield - F curves, and print the reference points
NSH.sr <- ref.pts(NSH,"bevholt",100000)
#NSH.sr <- ref.pts(NSH,"ricker",100000)

# Create a co-plot of the tuning indices, and see if there is correlation between age groups in the survey (there should be)
cor.tun(NSH.tun)

# Plot the historic indices updated with new values against each other to see if similar patterns are coming through

# Calculate the stock recruitment fit, and plot it, and also add years to the plot
NSH.srcontrol <- FLSR(
  rec = rec(transform(NSH,stock.n=NSH@stock.n/100000))[,-1],
  ssb = ssb(transform(NSH,stock.n=NSH@stock.n/100000))[,1:(length(dimnames(ssb(NSH))$year)-1)],
  model = 'bevholt')
NSH.sr <- fmle(NSH.srcontrol,control=list(parscale=c(0.001,1,0.001)))

plot(NSH.sr)
NSH.sr@params <- NSH.sr@params*100000
plot(NSH.sr@rec~NSH.sr@ssb,type="b",xlab="SSB",ylab="Rec",main="Yearly stock recruitment relationship")
text(NSH.sr@rec~NSH.sr@ssb,labels=dimnames(NSH.sr@rec)$year[-1],pos=1,cex=0.7)

# Plot the time series of weight in the stock and catch in the stock
timeseries(window(NSH,1975,2010),slot="stock.wt")
timeseries(window(NSH,1975,2010),slot="catch.wt")
timeseries(window(NSH,2000,2010),slot="harvest")
timeseries(window(NSH,1990,2010),slot="mat")

#Time series of weigth of the stock anomalies
anom.plot(trim(NSH@stock.wt,year=1983:dims(NSH)$maxyear,age=0:1),xlab="Year",ylab="Anomaly (std. devs)",
    main=paste(NSH@name,"Weight in the Stock Anomaly (Age 0-1)"),ylim=c(-3,3))
anom.plot(trim(NSH@stock.wt,year=1983:dims(NSH)$maxyear,age=3:6),xlab="Year",ylab="Anomaly (std. devs)",
    main=paste(NSH@name,"Weight in the Stock Anomaly (Age 3-6)"),ylim=c(-3,3))

#Time series of west by cohort
west.by.cohort      <- as.data.frame(FLCohort(window(NSH@stock.wt,1980,2010)))
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
TACs          <- read.csv("data/historic data/TAC-historic.csv")
TAC.plot.dat  <- data.frame(year=rep(TACs$year,each=2)+c(-0.5,0.5),TAC=rep(rowSums(TACs[,c("Agreed_A","Bycatch_B")],na.rm=T),each=2))
catch         <- as.data.frame(NSH@catch[,ac(1987:2010)]/1e3)
plot(0,0,pch=NA,xlab="Year",ylab="Catch",xlim=range(c(catch$year,TAC.plot.dat$year)),ylim=range(c(0,TAC.plot.dat$TAC,catch$data)),cex.lab=1.2,cex.axis=1.1,font=2)
rect(catch$year-0.5,0,catch$year+0.5,catch$data,col="grey")
lines(TAC.plot.dat,lwd=3)
legend("topright",legend=c("Catch","TAC"),lwd=c(1,5),lty=c(NA,1),pch=c(22,NA),col="black",pt.bg="grey",pt.cex=c(2),box.lty=0)
box()
title(main=paste(NSH@name,"Catch and TAC"))
mtext("Working group estimate",side=1,outer=F,line=5,cex=1.1)

# Plot MIK versus IBTS 1wr
  IBTS0 <- NSH.tun[[2]]@index[,ac(1992:(range(NSH.tun[[2]])["maxyear"]-1))]@.Data
  IBTS1 <- NSH.tun[[3]]@index[ac(1),ac(1993:range(NSH.tun[[3]])["maxyear"])]@.Data
plot(IBTS1 ~ IBTS0,pch=19,ylab="IBTS 1wr",xlab="MIK 0wr")
lines(predict(lm(IBTS1~IBTS0),newdata=data.frame(IBTS0=range(IBTS0)))~range(IBTS0),lwd=2)
mtext(bquote(R^2 [(.(round(summary(lm(IBTS1~IBTS0))$r.squared,2)))]),side=3,line=-2,outer=F,at=40,font=2,cex=1.2)
points(rev(IBTS0)[1],rev(IBTS1)[1],col="red",pch=19); text(rev(IBTS0)[1],rev(IBTS1)[1],labels=c(range(NSH.tun[[2]])["maxyear"]-1),col="red",pos=3)
lines(x=c(rep(rev(IBTS0)[1],2)),y=c(0,max(IBTS1,na.rm=T)/5),col="darkgreen",lwd=2); text(rev(IBTS0)[1],max(IBTS1,na.rm=T)/8,labels=range(NSH.tun[[2]])["maxyear"],col="darkgreen",pos=2)
legend("bottomright",c("MIK 0wr vs. IBTS 1wr",paste(range(NSH.tun[[2]])["maxyear"]-1,"yearclass"),paste(range(NSH.tun[[2]])["maxyear"],"yearclass")),col=c("black","red","darkgreen"),lty=c(0,0,1),pch=c(19,19,-1),lwd=3,box.lty=0)



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

#And for incorporation into the standard graphs
writeFLStock(NSH,file.path(output.dir,"hawg_her-47d3.sum"),type="ICAsum")
#The YPR curves based on the same values as the projection - therefore use WBSS.proj
writeFLStock(NSAS.proj,file.path(output.dir,"hawg_her-47d3.ypr"),type="YPR")

### ======================================================================================================
### Intermediate year
### ======================================================================================================
FnPrint("PERFORMING INTERMEDIATE YEAR CALCULATION...\n")
REC               <- NSH.ica@param["Recruitment prediction","Value"]
TAC               <- 215000 #overshoot = approximately 13% every year + 1000 tons of transfer             #194233 in 2009  #It does not matter what you fill out here as it only computes the suvivors
NSH.stf           <- FLSTF.control(fbar.min=2,fbar.max=6,nyrs=1,fbar.nyrs=1,f.rescale=TRUE,rec=REC,catch.constraint=TAC)
NSH.stock11       <- as.FLStock(FLSTF(stock=NSH,control=NSH.stf,unit=1,season=1,area=1,survivors=NA,quiet=TRUE,sop.correct=FALSE))

#New style complement Intermediate year
#target            <- fwdTarget(list(year=2010,value=TAC,quantity="catch"))
#NSH.stock10       <- fwd(NSH,target,sr=list(model="geomean",params=FLPar(NSH.ica@param["Recruitment prediction","Value"])))

#A plot on the agreed management plan with the estimated Fbar in 2010
plot(x=c(0,0.8,1.5,2),y=c(0.1,0.1,0.25,0.25),type="l",ylim=c(0,0.4),lwd=2,xlab="SSB in million tonnes",ylab="Fbar",cex.lab=1.3,main="Management plan North Sea Herring")
abline(v=0.8,col="red",lwd=2,lty=2)
abline(v=1.3,col="blue",lwd=2,lty=2)
abline(v=1.5,col="darkgreen",lwd=2,lty=2)
text(0.8,0,labels=expression(B[lim]),col="red",cex=1.3,pos=2)
text(1.3,0,labels=expression(B[pa]),col="blue",cex=1.3,pos=2)
text(1.5,0,labels=expression(B[trigger]),col="darkgreen",cex=1.3,pos=4)

points(y=fbar(NSH.stock11[,ac(2002:2011)]), x=(ssb(NSH.stock11[,ac(2002:2011)])/1e6),pch=19)
lines(y=fbar(NSH.stock11[,ac(2002:2011)]),  x=(ssb(NSH.stock11[,ac(2002:2011)])/1e6))
text(y=fbar(NSH.stock11[,ac(2002:2011)]),   x=(ssb(NSH.stock11[,ac(2002:2011)])/1e6),labels=ac(2002:2011),pos=3,cex=0.7)

#Write the results out in the lowestoft VPA format
writeFLStock(NSH.stock11,output.file=paste(output.base,"with STF"))

#And for incorporation into the standard graphs
writeFLStock(NSH,file.path(output.dir,"hawg_her-47d3.sum"),type="ICAsum")
#The YPR curves based on the same values as the projection - therefore use WBSS.proj
writeFLStock(NSH.stock11,file.path(output.dir,"hawg_her-47d3.ypr"),type="YPR")

### ======================================================================================================
### Write summary table for use with State Space Framework
### ======================================================================================================
tbl <- merge(as.data.frame(rec(NSH))[,c("year","data")],as.data.frame(ssb(NSH))[,c("year","data")],by="year")
tbl <- merge(tbl,as.data.frame(fbar(NSH))[,c("year","data")],by="year")
colnames(tbl) <- c("year","rec","ssb","fbar")
write.table(tbl,file=paste(output.base,"Summary Table.txt"),row.names=FALSE,quote=FALSE)

### ======================================================================================================
### Save workspace and Finish Up
### ======================================================================================================
FnPrint("SAVING WORKSPACES...\n")
save(NSH,NSH.stock11,NSH.tun,NSH.ctrl,file=paste(output.base,"Assessment.RData"))
save.image(file=paste(output.base,"Assessment Workspace.RData"))
dev.off()
FnPrint(paste("COMPLETE IN",sprintf("%0.1f",round(proc.time()[3]-start.time,1)),"s.\n\n"))

### ======================================================================================================
### Create the figures for the advice sheet and the summary table and reference points
### ======================================================================================================

writeStandardOutput(NSH,NSH.sr,NSH.retro,recImY=NSH.ica@survivors[1,1],nyrs.=3,output.base,Blim=0.8e6,Bpa=1.3e6,Flim=NULL,Fpa=0.25,Bmsy=NULL,Fmsy=NULL)
