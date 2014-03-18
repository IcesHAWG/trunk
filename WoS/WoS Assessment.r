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
path <- "C:/Documents and Settings/hatfielde/My Documents/A1 Post Virtualisation/hawgVIaN/2014/hawg/WoS/"
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

WoS@stock.n[1,ac(2013)] <- NA #Take this latest year recruitment out because it is not well estimated in ICA as there is no information to support it

#-Replace the recruitment value in the last data year with a geometric mean recruitment over the years 1989 to the year prior to the last data year
WoS@stock.n[1,ac(2013)] <- exp(mean(log(rec(WoS[,ac(1989:2012)])),na.rm=T))

### ======================================================================================================
### Use the standard code from the common modules to produce outputs
### ======================================================================================================
#this only needs to be left in for the stock summary plot so run everything 
#without the line below but then run line below to produce the picture that I want for the stock summary
#WoS@stock.n[1,ac(2013)] <- NA
do.summary.plots(WoS,WoS.ica)

do.retrospective.plots<- function(stck,idxs,ctrl,n.retro.yrs) {
    cat("GENERATING RETROSPECTIVE ANALYSES...\n");flush.console()

    #Generate a retrospective analysis
    icas    <- retro(stck,idxs,ctrl,retro=n.retro.yrs,return.FLStocks=FALSE)
    stcks   <- do.call(FLStocks,lapply(icas,function(ica) {ica + trim(stck, year=dims(ica@stock.n)$minyear:dims(ica@stock.n)$maxyear)}))

    # Remove the recruitment value in the last data year as it gets estimated wrong in ICA, replace it later with a GM estimate
#    stcks[[9]]@stock.n[1,ac(2011)] <- NA
#    stcks[[9]]@stock.n[1,ac(2011)] <- exp(mean(log(rec(stck[,ac(1989:2010)])),na.rm=T))
#
    stcks <- lapply(stcks,function(x){ x@stock.n[1,ac(range(x)["maxyear"])] <-
                                          exp(mean(log(rec(x[,ac(1989:(range(x)["maxyear"]-1))])),na.rm=T));
                          return(x)})
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

catch.curves(WoS,1990,2013)
WoS.sr <- ref.pts(WoS,"segreg",100000)
cor.tun(WoS.tun)

#Time series of west
west.ts  <- xyplot(data~year,data=window(WoS@stock.wt,1991,2013),
              groups=age,
              auto.key=list(space="right",points=FALSE,lines=TRUE,type="b"),
              type="b",
              xlab="Year",ylab="Weight in the stock (kg)",
              main=paste(WoS@name,"Weight in the Stock"),
              par.settings=list(superpose.symbol=list(pch=as.character(0:8),cex=1.25)))
print(west.ts)

#Time series of west by cohort
west.by.cohort      <- as.data.frame(FLCohort(window(WoS@stock.wt,1992,2013)))
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
ica.out.file <- ica.out(WoS,WoS.tun,WoS.ica,format="TABLE 5.6.%i HERRING in VIa (N).")
write(ica.out.file,file=paste(output.base,"ica.out",sep="."))
options("width"=old.opt$width,"scipen"=old.opt$scipen)

#And finally, write the results out in the lowestoft VPA format for further analysis eg MFDP
writeFLStock(WoS,output.file=output.base)
 
#And for incorporation into the standard graphs
writeFLStock(WoS,file.path(output.dir,"hawg_her-vian.sum"),type="ICAsum")
writeFLStock(WoS,file.path(output.dir,"hawg_her-vian.ypr"),type="YPR")

 
### ======================================================================================================
### Standard Graphs Input
### ======================================================================================================
FnPrint("PERFORMING Standard Graphs text...\n")

#WoS recruitment is based on a geometric mean of 1989-one year prior to the last data year. Updated by one year, each year
gm.recs                   <- exp(mean(log(rec(trim(WoS,year=1989:2012)))))
stf.ctrl                  <- FLSTF.control(nyrs=1,fbar.nyrs=1,fbar.min=3,fbar.max=6,catch.constraint=28067,f.rescale=TRUE,rec=gm.recs)
WoS@catch.n[1,57,,,,]     <- 1     #should the second number here be the number of data years, i.e., 55 for 2012 as last data year
#WoS.stf                   <- FLSTF(stock=WoS,control=stf.ctrl,quiet=TRUE,sop.correct=FALSE)
#writeFLStock(WoS.stf,output.file=paste(output.base," WoSaddyr",sep=""))
## use the rounder version so report and quality control database have same values
writeFLStock(WoS,file.path(output.dir,"hawg_her-vian.sum"),type="ICAsum")
# project one year in order to get a single year holding means for YPR output
WoS.proj                  <- stf(WoS,nyears=1,wts.nyears=3,fbar.nyears=1,arith.mean=TRUE,na.rm=TRUE)
writeFLStock(WoS.proj,file.path(output.dir,"hawg_her-vian.ypr"),type="YPR")

# Write the stf results out in the lowestoft VPA format for further analysis eg MFDP
#writeFLStock(WoS.stf,output.file=paste(output.base,"with STF"))

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
#create stock-recruitment relationship (Bev-Holt)
WoS.srcontrol <- FLSR(
  rec = rec(transform(WoS,stock.n=WoS@stock.n/100000))[,ac(1959:2013)],
  ssb = ssb(transform(WoS,stock.n=WoS@stock.n/100000))[,ac(1957:2011)],
  model = 'bevholt')
WoS.sr <- fmle(WoS.srcontrol,control=list(parscale=c(0.001,1,0.001)))

plot(WoS.sr)
WoS.sr@params <- WoS.sr@params*100000

writeStandardOutput(WoS,WoS.sr,WoS.retro,recImY=gm.recs,nyrs.=3,output.base,Blim=5e4,Bpa=NULL,Flim=NULL,Fpa=NULL,Bmsy=NULL,Fmsy=0.25)


### ======================================================================================================
### Projections
### ======================================================================================================
FnPrint("CALCULATING PROJECTIONS...\n")

#Define years
TaY <- dims(WoS)$maxyear   #Terminal assessment year
ImY <- TaY+1                      #Intermediate Year
AdY <- TaY+2                      #Advice year
CtY <- TaY+3                      #Continuation year - not of major concern but used in calculations in places
tbl.yrs <- as.character(c(ImY,AdY,CtY))   #Years to report in the output table

#-Replace the recruitment value in the last data year with a geometric mean recruitment over the years 1989 to the year prior to the last data year
rec.years <- (1989:(WoS@range['maxyear']-1));

gm.recs  <- exp(mean(log(rec(WoS)[,as.character(rec.years)])));
WoS.srr <- list(model="geomean",params=FLPar(gm.recs));

#Expand stock object
WoS.proj <- stf(WoS,nyears=4,wts.nyears=3,arith.mean=TRUE,na.rm=TRUE);
WoS.proj@stock.n[,ac(ImY)]  <- WoS.ica@survivors;
WoS.proj@stock.n[1,as.character(c(ImY,AdY,CtY))] <- gm.recs;
WoS.proj@stock.n[2,as.character(ImY)] <- WoS.proj@stock.n[1,as.character(TaY)] * exp(-WoS.proj@harvest[1,as.character(TaY)] - WoS.proj@m[1,as.character(TaY)])

#Define some constants 

#For 2013
ImY.catch <- 28067;
AdY.catch <- 28067;
numFmsy <- 0.25;
basisFImY <- yearMeans(fbar(WoS[,ac((ImY-3):(ImY-1))]))

#Setup options
options.l <- list(#Zero catch
                  "Catch(2015) = Zero"=
                    fwdControl(data.frame(year=c(ImY,AdY,CtY),
                                          quantity=c("f","catch","catch"),
                                          val=c(basisFImY,0,0))),
                  # TAC, -20% 
                  "Catch(2015) = 2014 TAC -20%)"=
                    fwdControl(data.frame(year=c(ImY,AdY,CtY),
                                          quantity=c("f","catch","f"),
                                          rel=c(NA,NA,AdY),
                                          val=c(basisFImY,AdY.catch*0.8,1))),
                  #TAC sq
                  "Catch(2015) = 2014 TAC sq"=
                    fwdControl(data.frame(year=c(ImY,AdY,CtY),
                                          quantity=c("f","catch","f"),
                                          rel=c(NA,NA,AdY),
                                          val=c(basisFImY,AdY.catch*1,1))),
                  #TAC +20%
                  "Catch(2015) = 2014 TAC +20%"=
                    fwdControl(data.frame(year=c(ImY,AdY,CtY),
                                          quantity=c("f","catch","f"),
                                          rel=c(NA,NA,AdY),
                                          val=c(basisFImY,AdY.catch*1.20,1))),
                  #F =0.25	#Fmsy
                  "Fbar(2015) = 0.25"=
                    fwdControl(data.frame(year=c(ImY,AdY,CtY),
                                          quantity=c("f","f","f"),
                                          rel=c(NA,NA,AdY),
                                          val=c(basisFImY,numFmsy,1)))

) #End options list



#Multi-options table
#fmult.targs  <- seq(0,2,by=0.1)
#mult.opts.l <- lapply(as.list(fmult.targs),function(fmult) {
#                          fwdControl(data.frame(year=c(ImY,AdY,CtY),
#                                          quantity=c("f","f","f"),
#                                          rel=c(NA,ImY,AdY),
#                                          val=c(basisFImY,fmult,1)))
#                  })
#names(mult.opts.l) <- sprintf("Fmult(2010) = %4.3f",fmult.targs)

#Calculate options
WoS.options   <- lapply(options.l,function(ctrl) {fwd(WoS.proj,ctrl=ctrl,sr=WoS.srr)})
#WoS.mult.opts <- lapply(mult.opts.l,function(ctrl) {fwd(WoS.proj,ctrl=ctrl,sr=WoS.srr)})


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
    col.dat <- sapply(input.tbl.list,function(slt) slot(WoS.proj,slt)[,as.character(yr),drop=TRUE])
    write.table(yr,file=input.tbl.file,col.names=FALSE,row.names=FALSE,append=TRUE,sep=",")
    write.table(t(c("Age",colnames(col.dat))),file=input.tbl.file,col.names=FALSE,row.names=FALSE,append=TRUE,sep=",")
    write.table(col.dat,file=input.tbl.file,col.names=FALSE,row.names=TRUE,append=TRUE,sep=",",na="-")
    write.table("",file=input.tbl.file,col.names=FALSE,row.names=FALSE,append=TRUE,sep=",")
}

#Detailed options table
options.file <-paste(output.base,"options - details.csv",sep=".")
write.table(NULL,file=options.file,col.names=FALSE,row.names=FALSE)
for(i in 1:length(WoS.options)) {
    opt <- names(WoS.options)[i]
    stk <- WoS.options[[opt]]
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
opt.sum.tbl(stcks=WoS.options,fname=paste(output.base,"options - summary.csv",sep="."))


### ======================================================================================================
### Save workspace and Finish Up
### ======================================================================================================
FnPrint("SAVING WORKSPACES...\n")
save(WoS,WoS.proj,WoS.tun,WoS.ctrl,file=paste(output.base,"AssessmentForecast.RData"))
save.image(file=paste(output.base,"AssessmentForecast Workspace.RData"))
dev.off()
FnPrint(paste("COMPLETE IN",sprintf("%0.1f",round(proc.time()[3]-start.time,1)),"s.\n\n"))

