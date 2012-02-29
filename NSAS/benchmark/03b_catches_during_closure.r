################################################################################
# NSH_SAM catches during closure
#
# $Rev$
# $Date$
#
# Author: HAWG model devlopment group
#
# Examines the consequences of removing catch data during the fishery closure
# e.g. from 1976- 1980
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
log.msg("\nNSH SAM 1970s closure effects\n================================\n")

### ============================================================================
### Setup assessment
### ============================================================================
#Somewhere to store results
resdir <- file.path("benchmark","resultsSAM")
respref <- "03b_catches_during_closure" #Prefix for output files
resfile <- file.path(resdir,paste(respref,".RData",sep=""))

#Dependencies
step03.file <- file.path(resdir,"03a_selected_surveys.RData")
if(!file.exists(step03.file)) {
  stop(paste("Cannot find dependency",step03.file,
      "Please run appropriate script to generate this file"))
} 

#Import externals
library(FLSAM)
source(file.path("benchmark","Setup_objects.r"))
source(file.path("benchmark","03_Setup_selected_surveys.r"))
step03.sam <- local({load(step03.file);return(NSH.sam)})

### ============================================================================
### Run the assessment for data sets with closure removed
### ============================================================================
#Only do the assessment if we are running in batch mode, or
#if the results file is missing
if(!file.exists(resfile) | !interactive()) {
   closure.yrs <- list("78-79"=1978:1979,
                       "77-80"=1977:1980,
                       "77-82"=1977:1982)
   
   #Loop over masked years
   closure.sams <- list(step03=step03.sam)
   for(nme in names(closure.yrs)) {
     #Modify stock object
     mod.stck <- NSH
     yrs <- closure.yrs[[nme]]
     mod.stck@catch.n[,ac(yrs)] <- NA
   
     #Perform assessment
     closure.sam <- FLSAM(mod.stck,NSH.tun,NSH.ctrl)
   
     #Store results
     closure.sam@name <- nme
     closure.sams[[nme]] <- closure.sam
   }   
   closure.sams <- do.call(FLSAMs,closure.sams)
   
   #Save any results
   save(NSH,NSH.tun,NSH.ctrl,closure.sams,file=resfile)

} else {
  #Load the file
  load(resfile)
}

### ============================================================================
### Plots
### ============================================================================
#Setup plots
pdf(file.path(resdir,paste(respref,".pdf",sep="")))

#Comparison of assessments with and without historic data
print(plot(closure.sams,main="Effect of removing closure data"))

#Effect on observation variances
obv <- obs.var(closure.sams)
obv$age[is.na(obv$age)] <- ""
print(barchart(value ~ age| fleet,obv,groups=name,horiz=FALSE,
        as.table=TRUE,xlab="Age",ylab="Observation variance",
        main="Changes in survey weightings due to dropping catch data",
        auto.key=list(space="right"),
        ylim=range(pretty(c(0,obv$value))),
        scale=list(alternating=FALSE)))

#Variability in catch residuals over time
catch.resids <- subset(residuals(closure.sams[["78-79"]]),fleet=="catch")
catch.resids$pentad <- floor((catch.resids$year+2)/5)*5
catch.resids$decade <- sprintf("%02i",(floor(catch.resids$year/10)*10)%%100)
catch.resids$decade <- factor(catch.resids$decade,levels=unique(catch.resids$decade))
boxplot(std.res ~ pentad,catch.resids,xlab="Pentad",ylab="Standardised residuals",
  main="Variability of catch residuals by pentad",las=3)

#Diagnostics
residual.diagnostics(closure.sams[["78-79"]])

### ============================================================================
### Finish
### ============================================================================
dev.off()
log.msg(paste("COMPLETE IN",sprintf("%0.1f",round(proc.time()[3]-start.time,1)),"s.\n\n"))
