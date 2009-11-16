######################################################################################################
# WBSS FLICA Assessment
#
# $Rev$
# $Date$
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
### Initialise system, including convenience functions and title display
### ======================================================================================================
rm(list=ls()); gc(); graphics.off(); start.time <- proc.time()[3]
options(stringsAsFactors=FALSE)
FnPrint     <-  function(string) {
	cat(string)
	flush.console()
}
FnPrint("\nWBSS FLICA Assessment\n=====================\n")

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
output.base         <-  file.path(output.dir,"WBSS Assessment") #Output base filename, including directory. Other output filenames are built by appending onto this one
n.retro.years       <-  8                          #Number of years for which to run the retrospective

### ======================================================================================================
### Output setup
### ======================================================================================================
#Output figure sizes for use in report
win.metafile(paste(output.base,"landscape figures - %02d.wmf"),height=100/25.4,width=130/25.4,pointsize=10)
trellis.par.set(fontsize=list(text=10,points=8))    #Set default lattice fontsize, so that things are actually readible!
win.metafile(paste(output.base,"portrait figures - %02d.wmf"),height=180/25.4,width=130/25.4,pointsize = 10)
trellis.par.set(fontsize=list(text=10,points=8))    #Set default lattice fontsize, so that things are actually readible!
#Square output
#png(paste(output.base,"figures - %02d.png"),units = "px", height=720,width=720,pointsize = 16, bg = "white")
#win.metafile(paste(output.base,"figures - %02d.wmf"),height=130/25.4,width=130/25.4,pointsize = 8)
#trellis.par.set(fontsize=list(text=18,points=16))     #Set default lattice fontsize, so that things are actually readible!

### ======================================================================================================
### Prepare control object for assessment
### ======================================================================================================
FnPrint("PREPARING CONTROL OBJECTS...\n")
WBSS.ctrl   <-  FLICA.control(sep.nyr=5,                            #Number of years in the separable period
                              sep.age=4,                            #Age at which catchability is fixed to equal 1.0
                              sep.sel=1.0,                          #Selectivity of the plus group and last true age
                              lambda.yr=1,                          #Weighting for each year in the separable period
                              lambda.age=c(0.1,1,1,1,1,1,1,1,1),    #Weighting for each age group
                              sr=FALSE,                             #Fit a stock recruitment relationship? (TRUE, FALSE)
                              lambda.sr=0,                          #Weighting given to the stock recruitment relationship in the fit
                              sr.age=0,                             #Lag (in years) between SSB and recruitment
                              index.model=c("l","l","l"),           #Catchability models for each index. (absolute, linear, power)
                              index.cor=1)                          #Correlations within each index

### ======================================================================================================
### Prepare stock object for assessment
### ======================================================================================================
FnPrint("PREPARING STOCK OBJECT...\n")
#Read in data from VPA files
WBSS <- readFLStock(file.path(data.source, "index.dat"),no.discards=TRUE)
#Setup rest of object
units(WBSS)[1:17] <- as.list(c(rep(c("tonnes","thousands","kg"),4), rep("NA",5)))
range(WBSS)[c("minfbar","maxfbar")] <- c(3,6)
WBSS         <- setPlusGroup(WBSS,WBSS@range["max"])
WBSS@name    <- "WBSS Herring"  #Set stock object name - this is propagated through into the figure titles

### ======================================================================================================
### Prepare index object for assessment
### ======================================================================================================
FnPrint("PREPARING INDEX OBJECT...\n")
#Load and modify all index data
WBSS.tun.in   <- readFLIndices(file.path(data.source, "tun.txt"))

#Set names, and parameters etc
names(WBSS.tun.in) <-  gsub(":.*$","",names(WBSS.tun.in))
for(nme in names(WBSS.tun.in)) { #Most indices have the same characteristics
  WBSS.tun.in[[nme]]@type 	             <- "number"
  WBSS.tun.in[[nme]]@index.var[]         <-	1
  WBSS.tun.in[[nme]]@range["plusgroup"]  <- NA
}
WBSS.tun.in[["HERAS 0-8+ wr"]]@range["plusgroup"] <- 8

#Generate two new indices by truncating current indices etc
WBSS.tun.in[["HERAS 3-6 wr"]]      <-  trim(WBSS.tun.in[["HERAS 0-8+ wr"]],age=3:6,year=1993:2008)           #HERAS 3-6 wr
WBSS.tun.in[["GerAS 1-3 wr"]]      <-  trim(WBSS.tun.in[["GerAS 0-8 wr (SD 21-24)"]],age=1:3,year=1994:2008) #GerAS 1-3 wr
WBSS.tun.in[["GerAS 1-3 wr"]]@index[,"2001"] <- -1     #2001 is excluded from GerAS due to lack of coverage in SD23

#Only use the relevant data sets in the assessment
WBSS.tun  <- WBSS.tun.in[c("HERAS 3-6 wr","GerAS 1-3 wr","N20")]

#Update the names in the trimmed object
for(nme in names(WBSS.tun)) {
      WBSS.tun[[nme]]@desc     <-    ""   #Wipe description, so it doesn't end up in the ica.out
      WBSS.tun[[nme]]@name     <-    nme #Reset index name in the object to that in the list (for consistency)
}

### ======================================================================================================
### Perform the assessment
### ======================================================================================================
FnPrint("PERFORMING ASSESSMENT...\n")
#Now perform the asssessment
WBSS.ica   <-  FLICA(WBSS,WBSS.tun,WBSS.ctrl)
WBSS       <-  WBSS + WBSS.ica
#Update the stock total biomass
WBSS@stock <- computeStock(WBSS)

### ======================================================================================================
### Use the standard code from the common modules to produce outputs
### ======================================================================================================
do.summary.plots(WBSS,WBSS.ica)
WBSS.retro <- do.retrospective.plots(WBSS,WBSS.tun,WBSS.ctrl,n.retro.years)
do.SRR.plot(WBSS)
ref.pts(WBSS,"bevholt",1e6)

### ======================================================================================================
### Custom plots
### ======================================================================================================
FnPrint("GENERATING CUSTOM PLOTS...\n")

#Time series of each index
index.ts.dat  <- lapply(WBSS.tun,slot,"index")
index.ts.dat  <- as.data.frame(index.ts.dat)
index.ts.dat$id <- paste(index.ts.dat$qname,", Age ",index.ts.dat$age,sep="")
index.ts.dat$data[index.ts.dat$data<0] <- NA
index.ts.plot <- xyplot(data~year|id,data=index.ts.dat,
                    type="b",
                    xlab="Year",ylab="Index Value",
                    prepanel=function(...) list(ylim=range(pretty(c(0,list(...)$y)))),
                    pch=19,
                    as.table=TRUE,
                    strip=strip.custom(par.strip.text=list(cex=0.8)),
                    main=paste(WBSS@name,"Input Indices"),
                    scales=list(alternating=1,y=list(relation="free")),
                    panel=function(...) {
                        panel.grid(h=-1,v=-1)
                        panel.xyplot(...)
                    })
print(index.ts.plot)

#Catch and TAC
catch.dat <- read.csv(file.path(".","data","Herring catches by area.csv"))
catch.dat$data <- catch.dat$data/1000
xlims <- range(pretty(catch.dat$year))
par(mfrow=c(3,1),mar=c(0,0,1,0),oma=c(5,4,4,2))
plot.dat <- subset(catch.dat,area=="Div. IIIa")
IIIa.catches <- tapply(plot.dat[plot.dat$quantity=="Catch","data"],plot.dat[plot.dat$quantity=="Catch","year"],sum)
IIIa.catches <- data.frame(year=as.numeric(names(IIIa.catches)),data=IIIa.catches)
plot(0,0,pch=NA,xaxt="n",xlim=xlims,ylim=range(pretty(c(0,IIIa.catches$data))),ylab="Catch (kt)",xlab="",xpd=NA)
grid()
rect(IIIa.catches$year-0.5,0,IIIa.catches$year+0.5,IIIa.catches$data,col="lightgrey")
plot.dat <- subset(catch.dat,area=="Div. IIIa"&stock=="WBSS")
rect(plot.dat$year-0.5,0,plot.dat$year+0.5,plot.dat$data,col="darkgrey")
IIIa.TACs <- subset(catch.dat, area=="Div. IIIa"&quantity=="TAC")
plot.dat <-  data.frame(year=rep(IIIa.TACs$year,each=2)+c(-0.5,0.5),TAC=rep(IIIa.TACs$data,each=2))
lines(plot.dat,lwd=5)
legend("topright",legend=c("WBSS Catch in IIIa","NSAS Catch in IIIa","Div. IIIa TAC"),lwd=c(1,1,5),lty=c(NA,NA,1),pch=c(22,22,NA),
    col="black",pt.bg=c("darkgrey","lightgrey",NA),pt.cex=c(2),bg="white")
axis(1,labels=FALSE)

plot.dat <- subset(catch.dat,area=="Sub-div. 22-24" & quantity=="Catch")
plot(0,0,pch=NA,xaxt="n",xlim=xlims,ylim=range(pretty(c(0,plot.dat$data))),ylab="Catch (kt)",xlab="",xpd=NA)
grid()
rect(plot.dat$year-0.5,0,plot.dat$year+0.5,plot.dat$data,col="darkgrey")
sd22.TACs <- subset(catch.dat,area=="Sub-div. 22-24" & quantity=="TAC")
plot.dat <-  data.frame(year=rep(sd22.TACs$year,each=2)+c(-0.5,0.5),TAC=rep(sd22.TACs$data,each=2))
lines(plot.dat,lwd=5)
legend("topright",legend=c("WBSS Catch in SD. 22-24","SD. 22-24 TAC"),lwd=c(1,5),lty=c(NA,1),pch=c(22,NA),
    col="black",pt.bg=c("darkgrey",NA),pt.cex=c(2),bg="white")
axis(1,labels=FALSE)

plot.dat      <- subset(catch.dat,quantity=="Catch" & stock=="WBSS")
WBSS.catches  <- tapply(plot.dat[plot.dat$quantity=="Catch","data"],plot.dat[plot.dat$quantity=="Catch","year"],sum)
WBSS.catches  <- data.frame(year=as.numeric(names(WBSS.catches)),data=WBSS.catches)
plot(0,0,pch=NA,xaxt="n",xlim=xlims,ylim=range(pretty(c(0,WBSS.catches$data))),ylab="Catch (kt)",xlab="",xpd=NA)
grid()
rect(WBSS.catches$year-0.5,0,WBSS.catches$year+0.5,WBSS.catches$data,col="darkgrey")
legend("topright",legend=c("Total WBSS Catch"),pch=c(22,NA),pt.bg=c("darkgrey"),pt.cex=c(2),bg="white")
axis(1,labels=TRUE)

title(main=paste(WBSS@name,"Catch and TAC"),outer=TRUE)
title(xlab="Year",xpd=NA)

#Other Custom plots should be in landscape orientation, so close the portrait ones here
dev.off()
plot.new()  #Space filler, to make up for the fact that we've moved the Catch TAc plot to portrait mode

#Proportion at age in (numbers) in the catch
prop.num.catch  <- stacked.area.plot(data~year,data=as.data.frame(pay(WBSS@catch.n)),groups="age",
                      xlab="Year",
                      ylab="Proportion at age (by numbers) in catch",
                      main=paste(WBSS@name,"Proportion at Age by numbers in Catch"))
print(prop.num.catch)

#Proportion at age (in weight) in the catch
prop.wt.catch  <- stacked.area.plot(data~year,data=as.data.frame(pay(WBSS@catch.n*WBSS@catch.wt)),groups="age",
                      xlab="Year",
                      ylab="Proportion at age (by weight) in catch",
                      main=paste(WBSS@name,"Proportion at Age by weight in Catch"))
print(prop.wt.catch)

#Time series of west
west.ts  <- xyplot(data~year,data=WBSS@stock.wt,
              groups=age,
              auto.key=list(space="right",points=FALSE,lines=TRUE,type="b",cex=0.8),
              type="b",
              xlab="Year",ylab="Weight in the stock (kg)",
              main=paste(WBSS@name,"Weight in the Stock"),
              par.settings=list(superpose.symbol=list(pch=as.character(0:8),cex=1.25)))
print(west.ts)

#Time series of west anomalies
anom.plot(trim(WBSS@stock.wt,age=0:2),xlab="Year",ylab="Anomaly (std. devs)",
    main=paste(WBSS@name,"Weight in the Stock Anomaly (Age 0-2)"),ylim=c(-3,3))
anom.plot(trim(WBSS@stock.wt,age=3:7),xlab="Year",ylab="Anomaly (std. devs)",
    main=paste(WBSS@name,"Weight in the Stock Anomaly (Age 3-7)"),ylim=c(-3,3))

#Time series of west by cohort
west.by.cohort  <- as.data.frame(FLCohort(WBSS@stock.wt))
west.by.cohort  <-  subset(west.by.cohort,!is.na(west.by.cohort$data))
west.by.cohort$year <- west.by.cohort$age + west.by.cohort$cohort
west.cohort.plot  <- xyplot(data~year,data=west.by.cohort,
              groups=cohort,
              auto.key=list(space="right",points=FALSE,lines=TRUE,type="b",cex=0.8),
              type="b",
              xlab="Year",ylab="Weight in the stock (kg)",
              main=paste(WBSS@name,"Weight in the stock by cohort"),
              par.settings=list(superpose.symbol=list(pch=as.character(unique(west.by.cohort$cohort)%%10),cex=1.25)),
              panel=function(...) {
                panel.grid(h=-1,v=-1)
                panel.xyplot(...)
              })
print(west.cohort.plot)

#Cohort growth rates
cohort.growth  <- xyplot(data~age,data=west.by.cohort,
                  groups=cohort,
                  auto.key=list(space="right",points=FALSE,lines=TRUE,type="b",cex=0.8,title="Cohort"),
                  type="b",
                  xlab="Year",ylab="Weight in the stock (kg)",
                  main=paste(WBSS@name,"Growth by Cohort"),
                  par.settings=list(superpose.symbol=list(pch=as.character(unique(west.by.cohort$cohort)%%10),cex=1)),
                  panel=function(...) {
                    panel.grid(h=-1,v=-1)
                    panel.xyplot(...)
                  })
print(cohort.growth)

#SSB structure Analysis - force the age 9 year class to be zero, to give pretty graphs
ssb.dat          <- FLQuant(0,dimnames=c(list(age=c(dimnames(WBSS@stock.n)[[1]],"9")),dimnames(WBSS@stock.n)[2:6]))
ssb.dat[1:9,]    <- WBSS@stock.wt*WBSS@stock.n*exp(-WBSS@harvest*WBSS@harvest.spwn - WBSS@m*WBSS@m.spwn)*WBSS@mat
ssb.cohorts      <- as.data.frame(FLCohort(ssb.dat))
ssb.cohorts$prop <- as.data.frame(FLCohort(pay(ssb.dat)))$data
ssb.cohorts$year <- ssb.cohorts$cohort+ssb.cohorts$age
ssb.cohorts      <- subset(ssb.cohorts,!is.na(ssb.cohorts$data))

#Contribution to ssb by age
ssb.by.age <- stacked.area.plot(data~year,as.data.frame(ssb.dat[as.character(0:8)]),groups="age",
                  ylab="Spawning Biomass (kt)",xlab="Year",
                  main=paste(WBSS@name,"Contribution of ages to SSB"))
print(ssb.by.age)

#Proportion of ssb by age
ssb.prop.by.age <- stacked.area.plot(data~year,as.data.frame(pay(ssb.dat[as.character(0:8)])),groups="age",
                        ylab="Proportion of SSB",xlab="Year",
                        main=paste(WBSS@name,"Proportion of ages in SSB"))
print(ssb.prop.by.age)

#Individual cohort strengths vs age
cohort.ssb <- xyplot(data~age,as.data.frame(FLCohort(ssb.dat)),groups=cohort,
                type="b",xlab="Age",ylab="SSB",
                main=paste(WBSS@name,"SSB of Cohorts by age"),
                auto.key=list(space="right",points=FALSE,lines=TRUE,type="b",title="Cohort",cex=0.8),
                par.settings=list(superpose.symbol=list(cex=1,
                  pch=as.character(unique(as.data.frame(FLCohort(ssb.dat))$cohort)%%10))))
print(cohort.ssb)

#Proportion of SSB by cohort
ssb.prop.by.cohort.plot  <- stacked.area.plot(prop~year,ssb.cohorts,groups="cohort",
                                ylab="Proportion of SSB",xlab="Year",
                                main=paste(WBSS@name,"Prop of SSB by Cohorts"))
print(ssb.prop.by.cohort.plot)

##Contribution to ssb by cohort
ssb.by.cohort.plot  <- stacked.area.plot(data/1000~year,ssb.cohorts,groups="cohort",
                          ylab="SSB [kt]",xlab="Year",
                          main=paste(WBSS@name,"SSB by Cohorts"))
print(ssb.by.cohort.plot)

#
### ======================================================================================================
### Projections
### ======================================================================================================
FnPrint("CALCULATING PROJECTIONS...\n")

#Define years
TaY <- dims(WBSS)$maxyear   #Terminal assessment year
ImY <- TaY+1                #Intermediate Year
AdY <- TaY+2                #Advice year
CtY <- TaY+3                #Continuation year - not of major concern but used in calculations in places
tbl.yrs     <- as.character(c(ImY,AdY,CtY))   #Years to report in the output table

#Deal with recruitment - a geometric mean of the five years prior to the terminal assessment year
rec.years <- (TaY-5):(TaY-1)
gm.recs  <- exp(mean(log(rec(WBSS)[,as.character(rec.years)])))
WBSS.srr <- list(model="geomean",params=FLPar(gm.recs))

#Expand stock object
WBSS.proj <- stf(WBSS,nyears=4,wts.nyears=3,arith.mean=TRUE,na.rm=TRUE)
WBSS.proj@stock.n[,ac(ImY)]  <- WBSS.ica@survivors
WBSS.proj@stock.n[1,as.character(c(ImY,AdY,CtY))] <- gm.recs

#Define some constants
ImY.catch <- 45087
AdY.catch <- 56627   #Based on 100% uptake of 2009 TAC

#Setup options
options.l <- list(#Zero catch
                  "Catch(2010) = Zero"=
                    fwdControl(data.frame(year=c(ImY,AdY,CtY),
                                          quantity="catch",
                                          val=c(ImY.catch,0,0))),
                  #2009 Catch is 45087, followed by -15% Catch reduction => 2010 Catch 38324
                  "Catch(2010) = 2009 TACs -15% (48 133 t)"=
                    fwdControl(data.frame(year=c(ImY,AdY,CtY),
                                          quantity=c("catch","catch","f"),
                                          rel=c(NA,NA,AdY),
                                          val=c(ImY.catch,AdY.catch*0.85,1))),
                  #2009 Catch is 45087, followed by +15% Catch increase => 2010 Catch 51850
                  "Catch(2010) = 2009 TACs (56 627 t)"=
                    fwdControl(data.frame(year=c(ImY,AdY,CtY),
                                          quantity=c("catch","catch","f"),
                                          rel=c(NA,NA,AdY),
                                          val=c(ImY.catch,AdY.catch*1,1))),
                  #2009 Catch is 45087, followed by +15% Catch increase => 2010 Catch 51850
                  "Catch(2010) = 2009 TACs +15% (65 121 t)"=
                    fwdControl(data.frame(year=c(ImY,AdY,CtY),
                                          quantity=c("catch","catch","f"),
                                          rel=c(NA,NA,AdY),
                                          val=c(ImY.catch,AdY.catch*1.15,1))),
                  #Constant Catch 45087
                  "Catch(2010) = 2009 Catch (45 087 t)"=
                    fwdControl(data.frame(year=c(ImY,AdY,CtY),
                                          quantity="catch",
                                          val=ImY.catch)),
                  #2009 Catch is 45087, followed Fbar= 0.25
                  "Fbar(2010) = 0.25"=
                    fwdControl(data.frame(year=c(ImY,AdY,CtY),
                                          quantity=c("catch","f","f"),
                                          rel=c(NA,NA,AdY),
                                          val=c(ImY.catch,0.25,1)))
) #End options list

#Multi-options table
fmult.targs  <- seq(0,2,by=0.1)
mult.opts.l <- lapply(as.list(fmult.targs),function(fmult) {
                          fwdControl(data.frame(year=c(ImY,AdY,CtY),
                                          quantity=c("catch","f","f"),
                                          rel=c(NA,ImY,AdY),
                                          val=c(ImY.catch,fmult,1)))
                  })
names(mult.opts.l) <- sprintf("Fmult(2010) = %4.3f",fmult.targs)

#Calculate options
WBSS.options   <- lapply(options.l,function(ctrl) {fwd(WBSS.proj,ctrl=ctrl,sr=WBSS.srr)})
WBSS.mult.opts <- lapply(mult.opts.l,function(ctrl) {fwd(WBSS.proj,ctrl=ctrl,sr=WBSS.srr)})

### ======================================================================================================
### Write Options Tables
### ======================================================================================================
FnPrint("WRITING OPTIONS TABLES...\n")

#Document input settings
input.tbl.file <-paste(output.base,"options - input.csv",sep=".")
write.table(NULL,file=input.tbl.file,col.names=FALSE,row.names=FALSE)
input.tbl.list <- list(N="stock.n",M="m",Mat="mat",PF="harvest.spwn",
                       PM="m.spwn",SWt="stock.wt",Sel="harvest",CWt="catch.wt")
for(yr in c(ImY,AdY,CtY)){
    col.dat <- sapply(input.tbl.list,function(slt) slot(WBSS.proj,slt)[,as.character(yr),drop=TRUE])
    write.table(yr,file=input.tbl.file,col.names=FALSE,row.names=FALSE,append=TRUE,sep=",")
    write.table(t(c("Age",colnames(col.dat))),file=input.tbl.file,col.names=FALSE,row.names=FALSE,append=TRUE,sep=",")
    write.table(col.dat,file=input.tbl.file,col.names=FALSE,row.names=TRUE,append=TRUE,sep=",",na="-")
    write.table("",file=input.tbl.file,col.names=FALSE,row.names=FALSE,append=TRUE,sep=",")
}

#Detailed options table
options.file <-paste(output.base,"options - details.csv",sep=".")
write.table(NULL,file=options.file,col.names=FALSE,row.names=FALSE)
for(i in 1:length(WBSS.options)) {
    opt <- names(WBSS.options)[i]
    stk <- WBSS.options[[opt]]
    #Now the F and N by age
    nums.by.age <- stk@stock.n[,tbl.yrs,drop=TRUE]
    colnames(nums.by.age) <- sprintf("N(%s)",tbl.yrs)
    f.by.age    <- stk@harvest[,tbl.yrs,drop=TRUE]
    colnames(f.by.age) <- sprintf("F(%s)",tbl.yrs)
    age.tbl     <- cbind(Age=rownames(f.by.age),N=nums.by.age,F=f.by.age)
    #And now the summary tbl
    sum.tbl     <- cbind(Year=tbl.yrs,SSB=ssb(stk)[,tbl.yrs],
                        F.bar=fbar(stk)[,tbl.yrs],Yield=computeCatch(stk)[,tbl.yrs])
    #Now, bind it all together
    sum.tbl.padding <- matrix("",nrow=nrow(age.tbl)-nrow(sum.tbl),ncol=ncol(sum.tbl))
    comb.tbl    <- cbind(age.tbl," ",rbind(sum.tbl,sum.tbl.padding))
    #And write it - hdr first, then the rest
    write.table(sprintf("%s). %s",letters[i],opt),options.file,append=TRUE,col.names=FALSE,row.names=FALSE,sep=",")
    write.table(t(colnames(comb.tbl)),options.file,append=TRUE,col.names=FALSE,row.names=FALSE,sep=",")
    write.table(comb.tbl,options.file,append=TRUE,col.names=FALSE,row.names=FALSE,sep=",")
    write.table(c(""),options.file,append=TRUE,col.names=FALSE,row.names=FALSE,sep=",")
}

#Options summary table
opt.sum.tbl <- function(stcks,fname) {
    options.sum.tbl <- sapply(as.list(1:length(stcks)),function(i) {
                          opt <- names(stcks)[i]
                          stk <- stcks[[opt]]
                          #Build up the summary
                          sum.tbl     <- data.frame(Rationale=opt,
                                          F.ImY=fbar(stk)[,as.character(ImY),drop=TRUE],
                                          Catch.ImY=computeCatch(stk)[,as.character(ImY),drop=TRUE],
                                          SSB.ImY=ssb(stk)[,as.character(ImY),drop=TRUE],
                                          F.AdY=fbar(stk)[,as.character(AdY),drop=TRUE],
                                          Catch.AdY=computeCatch(stk)[,as.character(AdY),drop=TRUE],
                                          SSB.AdY=ssb(stk)[,as.character(AdY),drop=TRUE],
                                          SSB.CtY=ssb(stk)[,as.character(CtY),drop=TRUE])
                          })
    options.sum.tbl <- t(options.sum.tbl)
    colnames(options.sum.tbl) <- c("Rationale",
                                    sprintf("Fbar (%i)",ImY),sprintf("Catch (%i)",ImY),sprintf("SSB (%i)",ImY),
                                    sprintf("Fbar (%i)",AdY),sprintf("Catch (%i)",AdY),sprintf("SSB (%i)",AdY),
                                    sprintf("SSB (%i)",CtY))
    write.csv(options.sum.tbl,file=fname,row.names=FALSE)
}
opt.sum.tbl(stcks=WBSS.options,fname=paste(output.base,"options - summary.csv",sep="."))
opt.sum.tbl(stcks=WBSS.mult.opts,fname=paste(output.base,"multi-options - summary.csv",sep="."))


### ======================================================================================================
### Document Assessment
### ======================================================================================================
FnPrint("GENERATING DOCUMENTATION...\n")
#Document the run with alternative table numbering and a reduced width
old.opt <- options("width","scipen","digits")
options("width"=80,"scipen"=1000,"digits"=3)
#Fix up some of the uglier values with too many decimal places
WBSS.ica@catch.res <- zapsmall(WBSS.ica@catch.res,digits=5)
WBSS.ica@ica.out <- ica.out(WBSS,WBSS.tun,WBSS.ica,format="TABLE 3.6.%i WBSS HERRING.")
write(WBSS.ica@ica.out,file=paste(output.base,"ica.out",sep="."))
do.call(options,old.opt)

#Write the results out in the lowestoft VPA format for further analysis eg MFDP
writeFLStock(WBSS,output.file=output.base)

#And for incorporation into the standard graphs
writeFLStock(WBSS,file.path(output.dir,"hawg_her-3a22.sum"),type="ICAsum")
#The YPR curves based on the same values as the projection - therefore use WBSS.proj
writeFLStock(WBSS.proj,file.path(output.dir,"hawg_her-3a22.ypr"),type="YPR")

### ======================================================================================================
### Save workspaces
### ======================================================================================================
FnPrint("SAVING WORKSPACES...\n")
save(WBSS,WBSS.ica,WBSS.tun,WBSS.ctrl,file=paste(output.base,"- Key Objects.RData"))
save.image(file=paste(output.base,"- Complete Workspace.RData"))

### ======================================================================================================
### Finish off
### ======================================================================================================
dev.off()
FnPrint(paste("COMPLETE IN",sprintf("%0.1f",round(proc.time()[3]-start.time,1)),"s.\n\n"))