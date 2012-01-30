######################################################################################################
# NSH Assessment
#
# Author: Niels Hintzen
# IMARES, The Netherlands
#
#
####################################################################################################

rm(list=ls()); graphics.off(); start.time <- proc.time()[3]

if(substr(R.Version()$os,1,3)== "min")  path <- "D:/Repository/HAWG/HAWGrepository/NSAS/"
if(substr(R.Version()$os,1,3)== "lin")  path <- "/media/n/Projecten/ICES WG/Haring werkgroep HAWG/2012/tmpRepos/NSAS/"
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
  NSH.ica                         <-  FLICA(NSH,NSH.tun,NSH.ctrl)
  NSH                             <-  NSH + NSH.ica
  range(NSH.ica)                  <-  range(NSH)[1:5]
  NSH@stock                       <-  computeStock(NSH)

  save(NSH,     file=paste(file.path(".","benchmark","resultsICA"),"/NSH_step1.RData",sep=""))
  save(NSH.ica, file=paste(file.path(".","benchmark","resultsICA"),"/NSH_step1.ica.RData",sep=""))
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

  source(file.path(".","benchmark","01_Setup_SAMICA_comparison.r"))
  NSH.sam   <- FLSAM(NSH,NSH.tun,NSH.ctrl)
  NSH       <- NSH + NSH.sam

  save(NSH,     file=paste(file.path(".","benchmark","resultsSAM"),"/NSH_step1.RData",sep=""))
  save(NSH.sam, file=paste(file.path(".","benchmark","resultsSAM"),"/NSH_step1.sam.RData",sep=""))
}
### ======================================================================================================
### Compare ICA and SAM
### ======================================================================================================

#Load ICA assessment
load(file.path(".","benchmark","resultsICA","NSH_step1.RData")); NSH.ica  <- NSH
#Load SAM assessment
load(file.path(".","benchmark","resultsSAM","NSH_step1.RData")); NSH.sam  <- NSH

#Store the two assessments together
NSH.stocks <- FLStocks(FLSAM=NSH.sam, FLICA=NSH.ica)

#Setup plots
pdf(file.path(".","benchmark","01_SAM_ICA_comparison.pdf"))

#Plot result
NSH.sam@name <- "North Sea Herring FLSAM Assessment"
print(plot(NSH.sam))
print(plot(NSH.stocks,key=TRUE,main="Comparison of assessments"))
dev.off()

pdf(file.path(".","benchmark","01_ICA_comparison.pdf"))
load(file=paste(file.path(".","benchmark","resultsICA"),"/NSH_step1.ica.RData",sep=""))
#diagnostics of ICA
diagnostics(NSH.ica)
dev.off()

pdf(file.path(".","benchmark","01_SAM_comparison.pdf"))
load(file=paste(file.path(".","benchmark","resultsSAM"),"/NSH_step1.sam.RData",sep=""))
#diagnostics of SAM
residual.diagnostics(NSH.sam)

#Compare weightings with John's weighting
sim.wts   <- read.csv(file.path(".","data","simmonds_wts.csv"))
obv       <- obs.var(NSH.sam)
obv$str   <- sprintf("%s_%i",obv$fleet,obv$age)
obv       <- obv[order(obv$value),]
wts       <- merge(sim.wts,obv)
wts$fit.wts <- 1/wts$value
plot(wts$simmonds_wts,wts$fit.wts,xlab="HAWG 2011 Weightings",
  ylab="SAM Fitted Weights",type="n",log="xy",main="Comparison of weightings")
text(wts$simmonds_wts,wts$fit.wts,wts$str,xpd=NA)
dev.off()

