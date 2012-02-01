################################################################################
# NSH_SAM Effect of dropping IBTS surveys from assessment
#
# $Rev$
# $Date$
#
# Author: HAWG model devlopment group
#
# Performs an assessment of the NSAS Herring stock using the SAM method but
# dropping all IBTS surveys except IBTS-Q1 age 1
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
log.msg("\nNSH SAM Drop IBTS Surveys  \n===========================\n")

### ============================================================================
### Setup assessment
### ============================================================================
#Somewhere to store results
resdir <- file.path("benchmark","resultsSAM")
respref <- "02b_drop_IBTS" #Prefix for output files

#Dependencies
all.in.file <- file.path(resdir,"02a_All_in_run.RData")
if(!file.exists(all.in.file)) {
  stop(paste("Cannot find dependency",all.in.file,
      "Please run appropriate script to generate this file"))
} 

#Import externals
library(FLSAM)
source(file.path("benchmark","Setup_objects.r"))
all.in.sam <- local({load(all.in.file);return(NSH.sam)})
all.in.sam@name <- "All-in"

#Drop (nearly) all IBTS surveys - still want to maintain IBTS-Q1 age 1
NSH.tun.all <- NSH.tun
NSH.tun <- NSH.tun.all[setdiff(names(NSH.tun),c("IBTS-Q1","IBTS-Q3"))]
NSH.tun[["IBTS-Q1"]] <- trim(NSH.tun.all[["IBTS-Q1"]],age=1)
source(file.path("benchmark","02_Setup_All_in_runs.r"))

### ============================================================================
### Run the assessment
### ============================================================================
#Perform assessment
drop.IBTS.sam <- FLSAM(NSH,NSH.tun,NSH.ctrl)
drop.IBTS.sam@name <- "Nearly no IBTS"

# Save results
IBTS.sams <- FLSAMs(all.in.sam,drop.IBTS.sam)
save(NSH,NSH.tun,NSH.ctrl,IBTS.sams,
   file=file.path(resdir,paste(respref,".RData",sep="")))

### ============================================================================
### Plots
### ============================================================================
#Setup plots
pdf(file.path(resdir,paste(respref,".pdf",sep="")))
plot(IBTS.sams,main="Effect of dropping nearly all IBTS surveys")

### ============================================================================
### Finish
### ============================================================================
dev.off()
log.msg(paste("COMPLETE IN",sprintf("%0.1f",round(proc.time()[3]-start.time,1)),"s.\n\n"))
