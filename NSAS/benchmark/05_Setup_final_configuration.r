################################################################################
# NSH_SAM Control for "All-in" Assessment
#
# $Rev$
# $Date$
#
# Author: HAWG model devlopment group
#
# Sets up a control object for use by Step 02 assessments i.e. the "All_in" run
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

if(substr(R.Version()$os,1,3)== "min")  path <- "D:/Repository/HAWG/HAWGrepository/NSAS/"
if(substr(R.Version()$os,1,3)== "lin")  path <- "/media/n/Projecten/ICES WG/Haring werkgroep HAWG/2012/tmpRepos/NSAS/"
try(setwd(path))

options(stringsAsFactors=FALSE)
log.msg     <-  function(string) {cat(string);}
log.msg("\nNSH Benchmark Assessment\n=====================\n")
an <- function(x){return(as.numeric(x))}


library(FLSAM)
### ============================================================================
### Setup assessment
### ============================================================================
source(file.path("benchmark","Setup_objects.r"))
source(file.path("benchmark","03_Setup_selected_surveys.r"))

#Modifications for Step05
NSH.ctrl@name <- "Step05"


#- Run retrospective analyses on catch states binding
NSH.retro1 <- retro(NSH,NSH.tun,NSH.ctrl,10)

  NSH.ctrl@states["catch",ac(3:9)] <- 101
  NSH.ctrl <- update(NSH.ctrl)
NSH.retro2 <- retro(NSH,NSH.tun,NSH.ctrl,10)
  NSH.retro2 <- FLSAMs(NSH.retro2[-6])
save(NSH,NSH.tun,NSH.retro1,file=file.path("benchmark","resultsSAM","05_Step5_Retro1.RData"))
save(NSH,NSH.tun,NSH.retro2,file=file.path("benchmark","resultsSAM","05_Step5_Retro2.RData"))
### ============================================================================
### Outputs
### ============================================================================
pdf(file.path("benchmark","resultsSAM","05_Step5_Retro.pdf"))
plot(NSH.retro1)
plot(NSH.retro2)
dev.off()




