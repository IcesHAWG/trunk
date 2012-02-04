################################################################################
# NSH_SAM effect of dropping HERAS age 9
#
# $Rev$
# $Date$
#
# Author: HAWG model devlopment group
#
# Examines the consequences of dropping the HERAS age 9 time series from the
# assessment
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
log.msg("\nNSH SAM Effect of dropping HERAS 9\n================================\n")

### ============================================================================
### Setup assessment
### ============================================================================
#Somewhere to store results
resdir <- file.path("benchmark","resultsSAM")
respref <- "03b_drop_HERAS_9" #Prefix for output files
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
   #Modify HERAS survey
   mod.tun <- NSH.tun
   mod.tun[["HERAS"]] <- trim(mod.tun[["HERAS"]],age=1:8)

   #Modify control object
   mod.ctrl <- NSH.ctrl
   for(slt in slotNames(mod.ctrl)) {
      slt.obj <- slot(mod.ctrl,slt)
      if(is.matrix(slt.obj)) {slot(mod.ctrl,slt)["HERAS","9"] <- NA }
   } 
   mod.ctrl <- update(mod.ctrl)
   
   #Perform assessment
   dropped.HERAS9 <- FLSAM(NSH,mod.tun,mod.ctrl)
   
   #Store results
   dropped.HERAS9@name <- "Dropped HERAS 9"
   sams <- do.call(FLSAMs,list("Step03"=step03.sam, "Dropped HERAS 9"=dropped.HERAS9))
   
   #Save any results
   save(NSH,mod.tun,mod.ctrl,sams,file=resfile)

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
print(plot(sams,main="Effect of dropping HERAS age 9"))

#Effect on observation variances
obv <- obs.var(sams)
obv$age[is.na(obv$age)] <- ""
print(barchart(value ~ age| fleet,obv,groups=name,horiz=FALSE,
        as.table=TRUE,xlab="Age",ylab="Observation variance",
        main="Effect of dropping HERAS age 9",
        auto.key=list(space="right"),
        ylim=range(pretty(c(0,obv$value))),
        scale=list(alternating=FALSE)))

#Residuals
residual.diagnostics(sams[[2]])

### ============================================================================
### Finish
### ============================================================================
dev.off()
log.msg(paste("COMPLETE IN",sprintf("%0.1f",round(proc.time()[3]-start.time,1)),"s.\n\n"))
