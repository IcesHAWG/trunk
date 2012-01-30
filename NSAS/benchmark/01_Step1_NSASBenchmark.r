######################################################################################################
# NSH Assessment
#
# Author: Niels Hintzen
# IMARES, The Netherlands
#
#
####################################################################################################

rm(list=ls()); graphics.off(); start.time <- proc.time()[3]

path <- "D:/Repository/HAWG/HAWGrepository/NSAS/"
#path <- "/media/n/Projecten/ICES WG/Haring werkgroep HAWG/2012/assessment/NSAS/"
try(setwd(path))

options(stringsAsFactors=FALSE)
log.msg     <-  function(string) {cat(string);}
log.msg("\nNSH Benchmark Assessment\n=====================\n")
an <- function(x){return(as.numeric(x))}

### ======================================================================================================
### ICA first to go
### ======================================================================================================
if(floor(an(R.Version()$minor))==8){
  source(file.path("..","_Common","HAWG Common assessment module.r"))
  source(file.path("benchmark","Setup_objects.r"))

  ### ======================================================================================================
  ### Prepare control object for assessment
  ### ======================================================================================================
  library(FLICA)
  log.msg("PREPARING CONTROL OBJECTS...\n")
  #Setup FLICA control object
  NSH.ctrl <- FLICA.control(sep.nyr=5, sep.age=4, sep.sel=1.0, sr.age=1, sr=TRUE,
                            lambda.age=c(0.1, 0.1, 3.67, 2.87, 2.23, 1.74, 1.37, 1.04, 0.94, 0.91),
                            lambda.yr=c(1.0, 1.0, 1.0, 1.0, 1.0),
                            lambda.sr=0.1,
                            index.model=c("p","l","l","l"), #index model: MLAI, MIK, IBTS, Acoustic
                            index.cor=FALSE)

  ### ======================================================================================================
  ### Select the default indices
  ### ======================================================================================================

  NSH.tun   <- NSH.tun[-c("SCAI","IBTS-Q3")] #Remove SCAI and IBTS-Q3
  
  ### ======================================================================================================
  ### Make sure natural mortality equals the fixed version
  ### ======================================================================================================

  NSH@m[]   <- c(1,1,0.3,0.2,rep(0.1,6))
  
  ### ======================================================================================================
  ### Trim the timeseries to 1960-2010
  ### ======================================================================================================

  NSH       <- window(NSH,start=1960,end=2010)

  ### ======================================================================================================
  ### Perform the assessment
  ### ======================================================================================================
  NSH.ica                         <-  FLICA(NSH,NSH.tun,NSH.ctrl)
  NSH                             <-  NSH + NSH.ica
  range(NSH.ica)                  <-  range(NSH)[1:5]
  NSH@stock                       <-  computeStock(NSH)

  save(NSH,     file=paste(file.path(".","benchmark","resultsICA"),"/NSH.RData",sep=""))
  save(NSH.ica, file=paste(file.path(".","benchmark","resultsICA"),"/NSH.ica.RData",sep=""))
}
### ======================================================================================================
### SAM second to go
### ======================================================================================================
if(floor(an(R.Version()$minor))>=13){
  library(FLSAM)
  source(file.path("benchmark","Setup_objects.r"))

  ### ======================================================================================================
  ### Select the default indices
  ### ======================================================================================================

  NSH.tun   <- NSH.tun[-which(names(NSH.tun) %in% c("SCAI","IBTS-Q3"))] #remove the SCAI and IBTS-Q3

  ### ======================================================================================================
  ### Make sure natural mortality equals the fixed version
  ### ======================================================================================================

  NSH@m[]   <- c(1,1,0.3,0.2,rep(0.1,6))

  ### ======================================================================================================
  ### Trim the timeseries to 1960-2010
  ### ======================================================================================================

  NSH       <- window(NSH,start=1960,end=2010)

  ### ======================================================================================================
  ### Perform the assessment
  ### ======================================================================================================

  source(file.path(".","benchmark","attic","Setup_default_FLSAM_control.r"))
  NSH.sam   <- FLSAM(NSH,NSH.tun,NSH.ctrl)
  NSH       <- NSH + NSH.sam

  save(NSH,     file=paste(file.path(".","benchmark","resultsSAM"),"/NSH.RData",sep=""))
  save(NSH.sam, file=paste(file.path(".","benchmark","resultsSAM"),"/NSH.sam.RData",sep=""))
}
### ======================================================================================================
### Compare ICA and SAM
### ======================================================================================================

#Load ICA assessment
load(file.path(".","benchmark","resultsICA","NSH.RData"))
#Load SAM assessment
load(file.path(".","benchmark","resultsSAM","NSH.RData"))

#Store the two assessments together
NSH.stocks <- FLStocks(FLSAM=NSH.sam, FLICA=NSH.ica)

#Setup plots
pdf(file.path(".","benchmark","SAM_ICA_comparison",".pdf"))

#Plot result
NSH.sam@name <- "North Sea Herring FLSAM Assessment"
print(plot(NSH.sam))
print(plot(NSH.stocks,key=TRUE,main="Comparison of assessments"))

#Plot catchabilities values
catch <- catchabilities(NSH.sam)
print(xyplot(value+ubnd+lbnd ~ age | fleet,catch,
          scale=list(alternating=FALSE,y=list(relation="free")),as.table=TRUE,
          type="l",lwd=c(2,1,1),col=c("black","grey","grey"),
          subset=fleet %in% c("HERAS","IBTS-Q1"),
          main="Survey catchability parameters",ylab="Catchability",xlab="Age"))

#Plot obs_variance (weightings)
obv <- obs.var(NSH.sam)
print(barchart(value ~ sprintf("%s",age)| fleet,obv,
       col="grey",ylim=range(pretty(c(0,obv$value))),
       as.table=TRUE,scale=list(alternating=FALSE),horizontal=FALSE,
       main="Observation Variances",ylab="Observation Variances",xlab="Age"))

#Plot selectivity pattern over time
sel.pat <- merge(f(NSH.sam),fbar(NSH.sam),
             by="year",suffixes=c(".f",".fbar"))
sel.pat$sel <- sel.pat$value.f/sel.pat$value.fbar
sel.pat$age <- as.numeric(as.character(sel.pat$age))
print(xyplot(value.f ~ year,sel.pat,groups=sprintf("Age %02i",age),
         type="l",as.table=TRUE,auto.key=list(space="right"),
         main="Fishing pressure over time",xlab="Year",ylab="F",
         scale=list(alternating=FALSE)))
print(xyplot(sel ~ year,sel.pat,groups=sprintf("Age %02i",age),
         type="l",as.table=TRUE,auto.key=list(space="right"),
         main="Selectivity of the Fishery",xlab="Year",ylab="F/Fbar",
         scale=list(alternating=FALSE)))
print(xyplot(sel ~ age|sprintf("%i's",floor(year/5)*5),sel.pat,
         groups=year,type="l",as.table=TRUE,
         scale=list(alternating=FALSE),
         main="Selectivity of the Fishery by Pentad",xlab="Age",ylab="F/Fbar"))

#Survey fits
residual.diagnostics(NSH.sam)
