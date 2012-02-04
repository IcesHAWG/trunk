################################################################################
# NSH_SAM Effect of plus group
#
# $Rev$
# $Date$
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
respref <- "03b_plus_group" #Prefix for output files
resfile <- file.path(resdir,paste(respref,".RData",sep=""))

#Dependencies
default.pg.file <- file.path(resdir,"03a_selected_surveys.RData")
if(!file.exists(default.pg.file)) {
  stop(paste("Cannot find dependency",default.pg.file,
      "Please run appropriate script to generate this file"))
} 

#Import externals
library(FLSAM)
source(file.path("benchmark","Setup_objects.r"))
source(file.path("benchmark","03_Setup_selected_surveys.r"))
default.pg.sam <- local({load(default.pg.file);return(NSH.sam)})
default.pg.sam@name <- sprintf("%i+",default.pg.sam@range["plusgroup"])

### ============================================================================
### Run the assessment for a different plus group
### ============================================================================
#Only do the assessment if we are running in batch mode, or
#if the results file is missing
if(!file.exists(resfile) | !interactive()) {
   pgs <- c(7,8)
   
   #Loop over truncated years
   pg.sams <- list()
   for(pg in sort(pgs)) {
     log.msg(sprintf("\n\nPlus group %i....\n",pg))
     pg.stck <- setPlusGroup(NSH,pg)
     pg.tun <- NSH.tun
     pg.tun[["HERAS"]]@index[ac(pg),] <- quantSums(pg.tun[["HERAS"]]@index[ac(pg:9),])
     pg.tun[["HERAS"]] <- trim(pg.tun[["HERAS"]],age=1:pg)
     pg.tun[["HERAS"]]@range["plusgroup"] <- pg
     pg.ctrl <- drop.from.control(NSH.ctrl,ages=(pg+1):NSH.ctrl@range["max"])
     pg.ctrl@states["catch",ac((pg-1):pg)] <- 101
     pg.ctrl@obs.vars["catch",ac(pg)] <- 201
     pg.ctrl@range[c("max","plusgroup")] <- pg
     pg.ctrl <- update(pg.ctrl)
     
     #Perform assessment
     pg.sam <- FLSAM(pg.stck,pg.tun,pg.ctrl)
   
     #Store results
     pg.sam@name <- sprintf("%i+",pg)
     pg.sams[pg.sam@name] <- pg.sam
   }   
   variable.pg.sams <- do.call(FLSAMs,c(pg.sams,default.pg.sam))
   names(variable.pg.sams) <- lapply(variable.pg.sams,name)
   
   #Save any results
   save(NSH,NSH.tun,NSH.ctrl,variable.pg.sams,file=resfile)
} else {
  #Load the file
  load(resfile)
}

### ============================================================================
### Plots
### ============================================================================
#Setup plots
pdf(file.path(resdir,paste(respref,".pdf",sep="")))

#Comparison of assessments with different plus groups
print(plot(variable.pg.sams,main="Comparison of Plus groups over full time period"))
print(plot(variable.pg.sams,xlim=c(1990,2011),
    main="Comparison of Plus groups over recent time period"))

#Comparison of observation variances
obv <- obs.var(variable.pg.sams)
obv$age[is.na(obv$age)] <- ""
print(barchart(value ~ age| fleet,obv,groups=name,horiz=FALSE,
        as.table=TRUE,xlab="Age",ylab="Observation variance",
        main="Observation variances under differnt plus group scenarios",
        auto.key=list(space="right"),
        ylim=range(pretty(c(0,obv$value))),
        scale=list(alternating=FALSE)))

#Adding diagnostics for Age 8 Plus Group
residual.diagnostics(variable.pg.sams[["8+"]])

### ============================================================================
### Finish
### ============================================================================
dev.off()
log.msg(paste("COMPLETE IN",sprintf("%0.1f",round(proc.time()[3]-start.time,1)),"s.\n\n"))
