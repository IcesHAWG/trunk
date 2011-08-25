######################################################################################################
# NEA Mackerel FLICA Assessment


## Notes on updating stock files on the 'data' directory
# Caton.txt / Canum.txt / Weca.txt  supplied by stock coordinator either as new files or for one line to be added and year to be incremented
# Fprop.txt and Mprop.txt require change of last year increment by one
# Natmor.txt requires one more line added at the end and increment of last year
# Matprop.txt - based on satandard maturities for areas prpotiopned by most recent surveys 2008 value = 2007 value
# but new caculation will be required next year (2010) for 2009 value
# for matprop proceedure see section 2.4.6 in the report
# West.txt based on catch weights at age in 2nd quarter from spawing area from stock coordinator but examinewd critically
# see section 2.4.5 in the report
# Future need to look at this at benchmark - how reliable is this how variable and is the variability correct?
# but also to check that SSB method used to give stock from egg surveys comatible with SSB cacluations in the assessment.
# Because ICA fits to SSB using numbers maturies and weights and egg survey use numbers maturities and weight they should be in accord.
# No change this year Ssb.txt file will nedd updating in 2010 following new egg survey if value acceptable. (quality check of completed data from spanish required)
# Index.txt and indexMFDP.txt not required to be updated, just file lists
# Ctrl.txt only required of MFDP required to be run  - update catches and recruitment


# $Rev: 1 $
# $Date: 2009-05-27 $
# Author: John Simmonds
# FRS, Scotland
#
# $Rev: 2 $
# $Date: 2010-09-01 $
# Author: Teunis Jansen (DTU-AQUA, Denmark), Thomas Brunel (IMARES, Nederlands), Charlie Main( FRS, Scotland)
#
# $Rev: 3 $
# $Date: 2011-09-01 $
# Author: Teunis Jansen (DTU-AQUA, Denmark), Thomas Brunel (IMARES, Nederlands), Emma Hatfield (Neo-FRS, is it called Marine Scotland now or?)
#                    
# Performs an assessment of North East Atlantic Mackerel stock using the
# FLICA package.
# and provides short term options that comare with MFDP to 4 figures
#
# Developed with:
#   - R version 2.8.1
#   - FLCore 3.0
#   - FLICA, version 1.4-10
#   - FLAssess, version 1.99-102
#   - FLSTF, version 1.99-1
#   - FLASH, version 2.0
#   - FLEDA, version 2.0.0
# Changes:
# V 5.10 - Reflects modifications to Common Module to work as functions, rather than as a single script
# V 5.00 - Compatiable with Google Code version
# V 0.20 - Modifications
# V 0.10 - Initial version, based on code inherited from NEA.Mac 2009 and MEAmac 2008
#
# To be done:
#
# Notes:
# This sets the working directory on Teunis's machine:
path <- "C:/Assessment/NEAMackerel/"
try(setwd(path),silent=TRUE)

####################################################################################################
### ======================================================================================================
### Initialise system, including convenience functions and title display
### ======================================================================================================
rm(list=ls()); gc(); graphics.off(); start.time <- proc.time()[3]
options(stringsAsFactors=FALSE)
objBeginTime <- proc.time()[3]
FnPrint     <-  function(string) {
	cat(string)
	flush.console()
}
FnPrint("\nNEAMac FLICA Assessment - dont forget to set working directory to stock\n=======\n")

### ======================================================================================================
### Incorporate Common modules
### Uses the common HAWG FLICA Assessment module to do the graphing, diagnostics and output
### ======================================================================================================
#Here the herring-people use: source(file.path("..","_Common","HAWG Common assessment module.r"))
source("NEAM assessment module change summary plot.r")
source("NEAMac Stock summary plot.r")
### ======================================================================================================

### ======================================================================================================
### Define parameters for use in the assessment code here
### ======================================================================================================
data.source         <-  file.path(".","data")      #Data source, not code or package source!!!
output.dir          <-  file.path(".","results")       #Output directory - some questions regarding use old "res" or "results"
# these next two lines are stock specific - others are standard across stocks
output.base         <-  file.path(output.dir,"NEAMac Assessment") #Output base filename, including directory. Other output filenames are built by appending onto this one
n.retro.years       <-  6                          #Number of years for which to run the retrospective: 7 in 2009

### ======================================================================================================
### Output setup creates png here look at others if wmf required.
### ======================================================================================================
png(paste(output.base,"figures - %02d.png"),units = "px", height=1200,width=800,pointsize = 24, bg = "white")
#Set default lattice fontsize, so that things are actually readible!
trellis.par.set(fontsize=list(text=24,points=20))

### ======================================================================================================
### Prepare control object for assessment
### ======================================================================================================
FnPrint("PREPARING CONTROL OBJECTS...\n")
# single 12 years separable period
# F on oldest true age = 1.5 * F on reference age of 5
# do not fit s/r relationship
# flat weighting across years to fit separable period
# down weightong of fittimg at age 0 (1/100) and age 1 (1/10) then flat weigtiong all other ages (1)
# survey weight not set here but in indices object (30)
# all weights divided by 3 ----- to match requirements in ICA and a max weight of 10 on the survey.

NEA.Mac.ctrl<-FLICA.control(sep.nyr=12,sep.age=5,sep.sel=1.5,sr=FALSE,
                                lambda.yr=c(1,1,1,1,1,1,1,1,1,1,1,1),
                                lambda.age =c(0.0033333, 0.033333, 0.33333,
                                 0.33333, 0.33333, 0.33333, 0.33333, 0.33333, 0.33333, 0.33333,0.33333,0.33333,0.33333),
                                lambda.sr=0.01,index.model=c("l"),index.cor=0)

## ======================================================================================================
### Prepare stock object for assessment - standard FL stock object
### ======================================================================================================
FnPrint("PREPARING STOCK OBJECT...\n")
NEA.Mac <- readFLStock(file.path(data.source, "index.txt"),no.discards=TRUE)
units(NEA.Mac)[1:17] <- as.list(c(rep(c("Tonnes","Thousands","Kg"),4), rep("NA",5))) #Set units
range(NEA.Mac)[c("minfbar","maxfbar")] <- c(4,8) #Set fbar
NEA.Mac <- setPlusGroup(NEA.Mac,NEA.Mac@range["max"]) #Set plus group
NEA.Mac@name    <- "NEA Mackerel" #Set stock object name - this is propagated through into the figure titles

### ======================================================================================================
### Prepare index object for assessment
### ======================================================================================================
FnPrint("PREPARING INDEX OBJECT...\n")
#Load and modify all index data


# Ideally line should be as follows but this does not work because the call expects at least one age disaggregated index
# NEA.Mac.tun   <- readFLIndices(file.path(data.source, "/Ssb.txt"),type="ICA")
# here we only have an SSB index so we use a short routine to load iot and comple the indices object
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
NEA.Mac.tun[[1]]@index.var[] <- 0.1 # implies a weighting of 10 which was chosen in 2007 Benchmark  - overall the weightinmg set here is 30 relative to catch weight of 1
NEA.Mac.tun[[1]]@effort[] <- 1 # just a standard number - realy ignored if 1
NEA.Mac.tun[[1]]@index[NEA.Mac.tun[[1]]@index==0] <- NA #Set the 0 values to NAs
NEA.Mac.tun[[1]]@type <- "biomass"
names(NEA.Mac.tun) <- "NEA.Mac Egg Survey"

### ======================================================================================================
### Perform the assessment
### ======================================================================================================
FnPrint("PERFORMING ASSESSMENT.    \n")
#Now perform the asssessment
NEA.Mac.ica   <-  FLICA(NEA.Mac,NEA.Mac.tun,NEA.Mac.ctrl)


# replace the recruitment value in the last year by the geometric mean of recruitment over the period 1972 to two year before assessment year
nyears<-dim(NEA.Mac.ica@stock.n)[2]
NEA.Mac.ica@stock.n[1,nyears]<-prod(NEA.Mac.ica@stock.n[1,1:(nyears-2)])^(1/(nyears-2))
NEA.Mac.ica@survivors[1,]<-prod(NEA.Mac.ica@stock.n[1,1:(nyears-2)])^(1/(nyears-2)) # do the same for the survivors
NEA.Mac.ica@survivors[2,]<-NEA.Mac.ica@stock.n[1,nyears]*exp(-NEA.Mac.ica@harvest[1,nyears]-NEA.Mac@m[1,nyears]) # recalculate age 1 for the survivors given the new recruitment for the last year and the fishing mortality

NEA.Mac <-  NEA.Mac + NEA.Mac.ica # put assessment results in stock object
# calculate total stock biomass for use in output tables
NEA.Mac@stock=computeStock(NEA.Mac) # to get TSB in stock slot
NEA.Mac.ica@index.res[[1]]@.Data[NEA.Mac.ica@index.res[[1]]@.Data==-99] <- NA # set flagged index resituals set to -99 as missing residuals to 0

### ======================================================================================================
### Use the standard code from the common modules to produce outputs
### ======================================================================================================
NEAmac.stock.summary.plot(NEA.Mac) #Make the customised NEA mackerel stock summary plot
#Do the other common plots
do.summary.plots(NEA.Mac,NEA.Mac.ica)
NEA.Mac.retro <- do.retrospective.plots(NEA.Mac,NEA.Mac.tun,NEA.Mac.ctrl,n.retro.years)
do.SRR.plot(NEA.Mac)

### ======================================================================================================
### Document Assessment
### ======================================================================================================
FnPrint("GENERATING DOCUMENTATION...\n")
#Document the run with alternative table numbering and a reduced width
old.opt <- options("width","scipen")
options("width"=80,"scipen"=1000)
#Do some tidying up on precision of the ica file
# making sure precision is either integer or 3 decimal places in most cases
# to avoid altering the final data make a copy then change that one
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

#Now write the file set up table number you require in the report
# change table number here as required
ica.out.file <- ica.out(NEA.Mac,NEA.Mac.tun,NEA.Mac.ica,format="TABLE 2.7.1.%i NEA Mackerel.")
write(ica.out.file,file=paste(output.base,"ica.out",sep="."))
options("width"=old.opt$width,"scipen"=old.opt$scipen)

#And finally, write the results out in the lowestoft VPA format for further analysis eg MFDP
writeFLStock(NEA.Mac.orig,output.file=output.base)


### ======================================================================================================
### Short Term Forecast
### ======================================================================================================

# MUST BE UPDATED FOR THE CURRENT YEAR       
# Current status: NOT FINAL ESTIMATES FOR 2011 
# see for example section 2.8 short term  prediction inputs
ImY.catch <- 929758  # estimated catches expected due to TAC, discards, payback, overfishing, unilateral quotas etc.
# see table in text of section 2.8
ImY.TAC <- 934990
# to be checked from NEAFC and Coatal states agreement (not EU TAC regulations whicvh is only part of this
# final number should be very close REF TAC + southern TAC = 0.0700767 * CS Ref TAC   + NEAFC = 57,884
# 2009 example: IMY.TAC = REF TAC + southern TAC + NEAFC + NOFO unilateral, where REF TAC = 511287t  Southern = 35829  NEAFC = 57844 NOFO unilateral = 35819
# 2010 no coastal state agreement only uni- and bilateral agreements, therefore set to ImY.Catch - transfer from 2009 - Discard - expected overcatch
# 2011 no coastal state agreement only uni- and bilateral agreements, therefore set to ImY.Catch minus (transfers+paybacks+discards+expected overcatch)

FnPrint("YPR and stock summary for standard graphs...\n")
#Define years
TaY <- dims(NEA.Mac)$maxyear#Terminal assessment year
ImY <- TaY+1                #Intermediate Year
AdY <- TaY+2                #Advice year
CtY <- TaY+3                #Continuation year - not of major concern but used in calculations in places
tbl.yrs     <- as.character(c(ImY,AdY,CtY))   #Years to report in the output table

#OLD STF
#Deal with recruitment - a geometric mean of the time series 1972 to 2 years prior to the terminal catch year
# if terminal catch year is 2008 last year for recruits is 2006 if catrc year was 2007 it use up to 2005
#rec.years   <- (dims(NEA.Mac)$minyear:(TaY-2))
#gm.recs     <- exp(mean(log(rec(NEA.Mac)[,as.character(rec.years)])))

#stf.ctrl    <- FLSTF.control(nyrs=1,fbar.nyrs=1,fbar.min=3,fbar.max=6,catch.constraint=ImY.catch,f.rescale=TRUE,rec=gm.recs)
#NEA.Mac.stf <- FLSTF(stock=NEA.Mac.orig,control=stf.ctrl,quiet=TRUE,sop.correct=FALSE)

# just in case you want to run MFDP the write line here gives correct file with n  1st jan last year
# over-write the stock.n file with suvivors added to last line
#writeVPA(NEA.Mac.stf, output.file=output.base,slots=c("stock.n"))

# write standard files for graphs
# use the rounder version so report and quality control database have same values
#writeFLStock(NEA.Mac,file.path(output.dir,"wide_mac-nea.sum"),type="ICAsum")

# project one year in order to get a single year holding means for YPR output
#NEA.Mac.proj=stf(NEA.Mac.orig,nyears=1,wts.nyears=3,fbar.nyears=1,arith.mean=TRUE,na.rm=TRUE)
#writeFLStock(NEA.Mac.proj,file.path(output.dir,"wide_mac-nea.ypr"),type="YPR")


### ======================================================================================================
# NEW PROJECTION routine
### ======================================================================================================
FnPrint("CALCULATING PROJECTIONS...\n")

rec.years <- (dims(NEA.Mac)$minyear:(TaY-2))
gm.recs  <- exp(mean(log(rec(NEA.Mac)[,as.character(rec.years)])))
NEA.Mac.srr <- list(model="geomean",params=FLPar(gm.recs))

#Expand stock object
#NEA.Mac.proj <- stf(NEA.Mac,nyears=4,wts.nyears=3,arith.mean=TRUE,na.rm=TRUE)
NEA.Mac.proj <- stf(NEA.Mac.orig,nyears=4,wts.nyears=3,arith.mean=TRUE,na.rm=TRUE) # to use object saved before rounding
NEA.Mac.proj@stock.n[,ac(ImY)]  <- NEA.Mac.ica@survivors
NEA.Mac.proj@stock.n[1,as.character(c(TaY,ImY,AdY,CtY))] <- gm.recs

#Setup options to suite the advice sheet  the choice here is based on the following:
#0 catch,  role over + and - 20% on TAC, F=0.20,0.21,0.22 from management plan  

options.l <- list(#2011 Catch, followed by Zero catch
                  "Catch(2012) = Zero"=
                    fwdControl(data.frame(year=c(ImY,AdY,CtY),
                                          quantity="catch",
                                          val=c(ImY.catch,0,0))),
                  #2011 Catch, followed by 20% reduction in declared TACs
                  "Catch(2012) = 2011 catch (excl. interannual transfers, paybacks and discard) -20%"=
                    fwdControl(data.frame(year=c(ImY,AdY,CtY),
                                          quantity=c("catch","catch","f"),
                                          rel=c(NA,NA,AdY),
                                          val=c(ImY.catch,ImY.TAC*0.80,1))),
                  #2011 Catch, followed by unchanged declared TACs
                  "Catch(2012) = 2011 catch (excl. interannual transfers, paybacks and discard)"=
                    fwdControl(data.frame(year=c(ImY,AdY,CtY),
                                          quantity=c("catch","catch","f"),
                                          rel=c(NA,NA,AdY),
                                          val=c(ImY.catch,ImY.TAC,1))),
                  #2011 Catch, followed by 20% increase in declared TACs
                  "Catch(2012) = 2010 catch (excl. interannual transfers, paybacks and discard) +20%"=
                    fwdControl(data.frame(year=c(ImY,AdY,CtY),
                                          quantity=c("catch","catch","f"),
                                          rel=c(NA,NA,AdY),
                                          val=c(ImY.catch,ImY.TAC*1.20,1))),
                 #2011 Catch, followed by Fbar= 0.20
                 "Fbar(2012) = 0.20"=
                    fwdControl(data.frame(year=c(ImY,AdY,CtY),
                                          quantity=c("catch","f","f"),
                                          val=c(ImY.catch,0.20,0.20))),
                 #2011 Catch, followed by Fbar= 0.21
                  "Fbar(2012) = 0.21"=
                    fwdControl(data.frame(year=c(ImY,AdY,CtY),
                                          quantity=c("catch","f","f"),
                                          val=c(ImY.catch,0.21,0.21))),
                 #2011 Catch, followed by Fbar= 0.22
                  "Fbar(2012) = 0.22 (Fmsy)"=
                    fwdControl(data.frame(year=c(ImY,AdY,CtY),
                                          quantity=c("catch","f","f"),
                                          val=c(ImY.catch,0.22,0.22))),
                 #2011 Catch, followed by EC/ICES transition F (0.292)
                  "Fbar(2012) = 0.292 (EC and ICES transition F)"=
                    fwdControl(data.frame(year=c(ImY,AdY,CtY),
                                          quantity=c("catch","f","f"),
                                          val=c(ImY.catch,0.292,0.22)))
) #End options list

#Multi-options table - standard one to show wider range of options for the report
# F multipliers from 0 to 2 *roll over F
fmult.targs  <- seq(0,2,by=0.1)
mult.opts.l <- lapply(as.list(fmult.targs),function(fmult) {
                          fwdControl(data.frame(year=c(ImY,AdY,CtY),
                                          quantity=c("catch","f","f"),
                                          rel=c(NA,ImY,AdY),
                                          val=c(ImY.catch,fmult,1)))
                  })
names(mult.opts.l) <- sprintf("Fmult(2012) = %4.3f",fmult.targs)

#Calculate options for two option tables
NEA.Mac.options   <- lapply(options.l,function(ctrl) {fwd(NEA.Mac.proj,ctrl=ctrl,sr=NEA.Mac.srr)})
NEA.Mac.mult.opts <- lapply(mult.opts.l,function(ctrl) {fwd(NEA.Mac.proj,ctrl=ctrl,sr=NEA.Mac.srr)})

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
    col.dat <- sapply(input.tbl.list,function(slt) slot(NEA.Mac.proj,slt)[,as.character(yr),drop=TRUE])
    write.table(yr,file=input.tbl.file,col.names=FALSE,row.names=FALSE,append=TRUE,sep=",")
    write.table(t(c("Age",colnames(col.dat))),file=input.tbl.file,col.names=FALSE,row.names=FALSE,append=TRUE,sep=",")
    write.table(col.dat,file=input.tbl.file,col.names=FALSE,row.names=TRUE,append=TRUE,sep=",",na="-")
    write.table("",file=input.tbl.file,col.names=FALSE,row.names=FALSE,append=TRUE,sep=",")
}

#Detailed options table
options.file <-paste(output.base,"options - details.csv",sep=".")
write.table(NULL,file=options.file,col.names=FALSE,row.names=FALSE)
for(i in 1:length(NEA.Mac.options)) {
    opt <- names(NEA.Mac.options)[i]
    stk <- NEA.Mac.options[[opt]]
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
opt.sum.tbl(stcks=NEA.Mac.options,fname=paste(output.base,"options - summary.csv",sep="."))
opt.sum.tbl(stcks=NEA.Mac.mult.opts,fname=paste(output.base,"multi-options - summary.csv",sep="."))



### ======================================================================================================
### Save workspace and Finish Up
### ======================================================================================================
FnPrint("SAVING WORKSPACES...\n")
save(NEA.Mac.orig,NEA.Mac.tun,NEA.Mac.ctrl,file=paste(output.base,"Assessment.RData"))
save.image(file=paste(output.base,"Assessment Workspace.RData"))
dev.off()
FnPrint(paste("\nFinished After", round(((proc.time()[3] - objBeginTime)/60), 1), "minutes.\n"))
