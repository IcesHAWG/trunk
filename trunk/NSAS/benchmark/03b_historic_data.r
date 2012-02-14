################################################################################
# NSH_SAM Historic data
#
# $Rev$
# $Date$
#
# Author: HAWG model devlopment group
#
# Examines the consequences of introducing the data from the historic period
# i.e. pre 1960
#
# Developed with:
#   - R version 2.13.0
#   - FLCore 2.4
#
# To be done:
#
# Notes: Have fun running this assessment!
#
################################################################################

### ============================================================================
### Initialise system, including convenience functions and title display
### ============================================================================
rm(list=ls()); graphics.off(); start.time <- proc.time()[3]
options(stringsAsFactors=FALSE)
log.msg     <-  function(string) {
	cat(string);flush.console()
}
log.msg("\nNSH SAM Historic period analysis\n================================\n")

### ============================================================================
### Setup assessment
### ============================================================================
#Somewhere to store results
resdir <- file.path("benchmark","resultsSAM")
respref <- "03b_historic_data" #Prefix for output files
resfile <- file.path(resdir,paste(respref,".RData",sep=""))

#Dependencies
full.history.file <- file.path(resdir,"03a_selected_surveys.RData")
if(!file.exists(full.history.file)) {
  stop(paste("Cannot find dependency",full.history.file,
      "Please run appropriate script to generate this file"))
} 

#Import externals
library(FLSAM)
source(file.path("benchmark","Setup_objects.r"))
source(file.path("benchmark","03_Setup_selected_surveys.r"))
full.history.sam <- local({load(full.history.file);return(NSH.sam)})
full.history.sam@name <- "All data"

### ============================================================================
### Run the assessment for a truncated period
### ============================================================================
#Only do the assessment if we are running in batch mode, or
#if the results file is missing
if(!file.exists(resfile) | !interactive()) {
   trunc.yrs <- c(1960,1991)
   
   #Loop over truncated years
   trunc.sams <- list()
   for(trunc.yr in sort(trunc.yrs)) {
     trunc.stck <- window(NSH,start=trunc.yr)
     trunc.tun <- window(NSH.tun,start=trunc.yr)
     trunc.ctrl <- NSH.ctrl
     trunc.ctrl@range["minyear"] <- trunc.yr
   
     #Perform assessment
     trunc.sam <- FLSAM(trunc.stck,trunc.tun,trunc.ctrl)
   
     #Store results
     trunc.sam@name <- sprintf("%i onwards",trunc.yr)
     trunc.sams[ac(trunc.yr)] <- trunc.sam
   }   
   variable.len.sams <- do.call(FLSAMs,c(trunc.sams,full.history.sam))
   
   #Save any results
   save(NSH,NSH.tun,NSH.ctrl,variable.len.sams,file=resfile)

} else {
  #Load the file
  load(resfile)
}

### ============================================================================
### Plots
### ============================================================================
#Setup plots
pdf(file.path(resdir,paste(respref,".pdf",sep="")),pointsize=16)

#Variability in catch residuals over time
catch.resids <- subset(residuals(full.history.sam),fleet=="catch")
catch.resids$pentad <- floor((catch.resids$year+2)/5)*5
catch.resids$decade <- sprintf("%02i",(floor(catch.resids$year/10)*10)%%100)
catch.resids$decade <- factor(catch.resids$decade,levels=unique(catch.resids$decade))
boxplot(std.res ~ pentad,catch.resids,xlab="Pentad",ylab="Standardised residuals",
  main="Variability of catch residuals by pentad",las=3)

print(bwplot(std.res ~ decade | sprintf("Age %02i",age),catch.resids,
        xlab="Decade",ylab="Standardised residuals",main="Catch-residual variability",
        as.table=TRUE,horizontal=FALSE,pch="|",lty=1,fill="grey",
        par.settings=list(box.rectangle=list(lty=1,col=1),
                          box.umbrella=list(lty=1,col=1),
                          plot.symbol=list(col=1)),
        scale=list(alternating=FALSE),
        panel=function(...) {
          panel.abline(h=-3:3,col="lightgrey")
          panel.bwplot(...)}))

#Comparison of assessments with and without historic data
print(plot(variable.len.sams,xlim=c(1990,2011),main="Variable data lengths"))

#Comparison of observation variances
obv <- obs.var(variable.len.sams)
obv$age[is.na(obv$age)] <- ""
levels(obv$fleet)[which(levels(obv$fleet)=="temp_MLAI")] <- "MLAI"
print(barchart(value ~ age| fleet,obv,groups=name,horiz=FALSE,
        as.table=TRUE,xlab="Age",ylab="Observation variance",
        main="Observation variances under differnt lengths of assessment",
        auto.key=list(space="right"),
        ylim=range(pretty(c(0,obv$value))),
        scale=list(alternating=FALSE)))

### ============================================================================
### Finish
### ============================================================================
dev.off()
log.msg(paste("COMPLETE IN",sprintf("%0.1f",round(proc.time()[3]-start.time,1)),"s.\n\n"))
