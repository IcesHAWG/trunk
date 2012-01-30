################################################################################
# NSH_SAM MLAI vs SCAI
#
# $Rev: 618 $
# $Date: 2011-11-11 16:42:31 +0100 (vr, 11 nov 2011) $
#
# Author: HAWG model devlopment group
#
# Examines the relationship between MLAI, SCAI and the assessment results
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
log.msg("\nNSH SAM MLAI vs SCAI\n==========================\n")

### ============================================================================
### Setup assessment
### ============================================================================
#Somewhere to store results
resdir <- file.path("benchmark","resultsSAM")
respref <- "02d_MLAI_vs_SCAI" #Prefix for output files

#Dependencies
all.in.file <- file.path(resdir,"02a_All_in.RData")
if(!file.exists(all.in.file)) {
  stop(paste("Cannot find dependency",all.in.file,
      "Please run appropriate script to generate this file"))
} 

#Import externals
library(FLSAM)
source(file.path("benchmark","Setup_objects.r"))
SCAI.sam <- local({load(all.in.file);return(NSH.sam)})
SCAI.sam@name <- "SCAI"

#Set surveys aside
NSH.tun.all <- NSH.tun

### ============================================================================
### Run the assessment with MLAI and without any IHLS data first
### ============================================================================
#Drop IHLS data, setup new objects, perform assessment
NSH.tun <- NSH.tun[setdiff(names(NSH.tun),c("SCAI","MLAI"))] 
source(file.path("benchmark","02_Setup_All_in_runs.r"))
noIHLS.sam <- FLSAM(NSH,NSH.tun,NSH.ctrl)
noIHLS.sam@name <- "no IHLS data"

#Now include the MLAI. The MLAI will only be dropped by "02_Setup_All_in_runs" if
#it is called "MLAI". We therefore rename it to "temp_MLAI" to avoid this problem
NSH.tun <- NSH.tun.all
NSH.tun[["temp_MLAI"]] <- NSH.tun[["MLAI"]]
NSH.tun <- NSH.tun[setdiff(names(NSH.tun),c("SCAI","MLAI"))]  #Drop SCAI,MLAI 
source(file.path("benchmark","02_Setup_All_in_runs.r"))
MLAI.sam <- FLSAM(NSH,NSH.tun,NSH.ctrl)
MLAI.sam@name <- "MLAI"

#Combine into one
IHLS.variations <- FLSAMs(SCAI.sam,MLAI.sam,noIHLS.sam)
#Save any results
save(NSH,NSH.tun,NSH.ctrl,IHLS.variations,
     file=file.path(resdir,paste(respref,".RData",sep="")))

### ============================================================================
### Plots
### ============================================================================
#Setup plots
pdf(file.path(resdir,paste(respref,".pdf",sep="")))

#Comparison of assessments with different plus groups
plot(IHLS.variations,main="Effects of IHLS survey")
plot(IHLS.variations,xlim=c(1990,2011),
    main="Effects of IHLS survey over recent time period")

### ============================================================================
### Finish
### ============================================================================
dev.off()
log.msg(paste("COMPLETE IN",sprintf("%0.1f",round(proc.time()[3]-start.time,1)),"s.\n\n"))
