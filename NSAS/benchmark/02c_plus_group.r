################################################################################
# NSH_SAM Effect of plus group
#
# $Rev: 618 $
# $Date: 2011-11-11 16:42:31 +0100 (vr, 11 nov 2011) $
#
# Author: HAWG model devlopment group
#
# Examines the effect of shifting the plus group to age 8
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
log.msg("\nNSH SAM Plus group analyis\n==========================\n")

### ============================================================================
### Setup assessment
### ============================================================================
#Somewhere to store results
resdir <- file.path("benchmark","resultsSAM")
respref <- "02c_plus_group" #Prefix for output files

#Dependencies
default.pg.file <- file.path(resdir,"02a_All_in.RData")
if(!file.exists(default.pg.file)) {
  stop(paste("Cannot find dependency",default.pg.file,
      "Please run appropriate script to generate this file"))
} 

#Import externals
library(FLSAM)
source(file.path("benchmark","Setup_objects.r"))
source(file.path("benchmark","02_Setup_All_in_runs.r"))
default.pg.sam <- local({load(default.pg.file);return(NSH.sam)})
default.pg.sam@name <- sprintf("Age %i PG",default.pg.sam@range["plusgroup"])

### ============================================================================
### Run the assessment for a different plus group
### ============================================================================
pgs <- c(7,8)

#Loop over truncated years
pg.sams <- list()
for(pg in sort(pgs)) {
  pg.stck <- setPlusGroup(NSH,pg)
  pg.tun <- NSH.tun
  pg.tun[["HERAS"]] <- trim(pg.tun[["HERAS"]],age=1:pg)
  pg.ctrl <- NSH.ctrl
  pg.ctrl@range[c("max","plusgroup")] <- pg
  for(slt.name in slotNames(pg.ctrl)) {
    slt <- slot(pg.ctrl,slt.name)
    if(is.matrix(slt)) {
      slot(pg.ctrl,slt.name) <- slt[,ac(0:pg)]
    }
  }
  pg.ctrl@logN.vars <- pg.ctrl@logN.vars[ac(0:pg)]
  pg.ctrl <- update(pg.ctrl)
  
  #Perform assessment
  pg.sam <- FLSAM(pg.stck,pg.tun,pg.ctrl)

  #Store results
  pg.sam@name <- sprintf("Age %i PG",pg)
  pg.sams[ac(pg)] <- pg.sam
}   
variable.pg.sams <- do.call(FLSAMs,c(pg.sams,default.pg.sam))

#Save any results
save(NSH,NSH.tun,NSH.ctrl,variable.pg.sams,
     file=file.path(resdir,paste(respref,".RData",sep="")))

### ============================================================================
### Plots
### ============================================================================
#Setup plots
pdf(file.path(resdir,paste(respref,".pdf",sep="")))

#Comparison of assessments with different plus groups
plot(variable.pg.sams,main="Comparison of Plus groups over full time period")
plot(variable.pg.sams,xlim=c(1990,2011),
    main="Comparison of Plus groups over recent time period")

### ============================================================================
### Finish
### ============================================================================
dev.off()
log.msg(paste("COMPLETE IN",sprintf("%0.1f",round(proc.time()[3]-start.time,1)),"s.\n\n"))
