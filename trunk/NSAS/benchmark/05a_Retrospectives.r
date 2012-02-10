################################################################################
# NSH_SAM "Final" Assessment Retrospectives
#
# $Rev$
# $Date$
#
# Author: HAWG model devlopment group
#
# Performs the retrospective analyses for the "Final" NSAS assessment model
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
rm(list=ls()); graphics.off(); start.time <- proc.time()[3]
options(stringsAsFactors=FALSE)
log.msg     <-  function(string) {cat(string);}
log.msg("\nNSH Final Assessment\n=====================\n")

### ============================================================================
### Setup assessment
### ============================================================================
#Somewhere to store results
resdir <- file.path("benchmark","resultsSAM")
respref <- "05a_Retrospectives" #Prefix for output files
resfile <- file.path(resdir,paste(respref,".RData",sep=""))

#Import externals
library(FLSAM)
source(file.path("benchmark","Setup_objects.r"))
source(file.path("benchmark","05_Setup_final_configuration.r"))

### ============================================================================
### Run the assessment
### ============================================================================
#Only do the assessment if we are running in batch mode, or
#if the results file is missing
if(!file.exists(resfile) | !interactive()) {
  #- Run default retrospective 
  NSH.retro1 <- retro(NSH,NSH.tun,NSH.ctrl,10)

  #- Run retrospective analyses on catch states binding
  NSH.ctrl@states["catch",ac(3:9)] <- 101
  NSH.ctrl <- update(NSH.ctrl)
  NSH.retro2 <- retro(NSH,NSH.tun,NSH.ctrl,10)
  NSH.retro2 <- FLSAMs(NSH.retro2[-6])

  # Save results
  save(NSH,NSH.tun,NSH.retro1,NSH.retro2,file=resfile)

} else {
  #Load the file
  load(resfile)
}

### ============================================================================
### Outputs
### ============================================================================
pdf(file.path(resdir,paste(respref,".pdf",sep="")))
plot(NSH.retro1)
plot(NSH.retro2)
dev.off()




