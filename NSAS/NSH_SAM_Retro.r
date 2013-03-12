################################################################################
# NSH_SAM Assessment
#
# $Rev: 697 $
# $Date: 2012-02-10 09:52:28 +0100 (vr, 10 feb 2012) $
#
# Author: HAWG model devlopment group
#
# Performs the "Final" assessment for NSAS assessment
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
path <- "~/HAWG/trunk/NSAS/"
path <- "D:/Repository/HAWG/HAWGrepository/NSAS/"
try(setwd(path),silent=TRUE)

### ======================================================================================================
### Define parameters for use in the assessment code here
### ======================================================================================================
output.dir          <-  file.path(".","results")                #Output directory
output.base         <-  file.path(output.dir,"NSH Assessment")  #Output base filename, including directory. Other output filenames are built by appending onto this one
n.retro.years       <-  5                                       #Number of years for which to run the retrospective

### ============================================================================
### Setup assessment
### ============================================================================
#Import externals
library(FLSAM);
source(file.path("retroResidual.r"))

#Load the output data of the assessment
res <- try(load(file=file.path(output.dir,"North Sea Herring.RData")),silent=TRUE)
if(class(res)=="try-error") stop("Run the assessment first before you can run the retrospective")

#Perform retrospective
NSH.retro <- retro(NSH,NSH.tun,NSH.ctrl,n.retro.years)
for(i in ac(2006:2011))
  NSH.retro[[i]]@name <- i


# Save results
save(NSH.retro,file=file.path(output.dir,paste(name(NSH),"retro.RData",sep="")))

#Setup plots
pdf(file.path(output.dir,paste(name(NSH),"retro.pdf",sep="")))
png(file.path(output.dir,"figuresRetro - %02d.png"),units = "px", height=800,width=672, bg = "white")

#Plot retro
plot(NSH.retro,futureYrs=F)

print(retroResiduals(   NSH.retro,"HERAS",1990:2011))
print(retroResiduals(   NSH.retro,"catch",1990:2011))
print(retroSelectivity( NSH.retro,2003:2011))

dev.off()



