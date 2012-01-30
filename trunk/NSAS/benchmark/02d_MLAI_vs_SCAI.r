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
all.in.file <- file.path(resdir,"02a_All_in_run.RData")
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
noIHLS.sam@name <- "no IHLS"

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
names(IHLS.variations) <- sapply(IHLS.variations,name)

#Save any results
save(NSH,NSH.tun.all,NSH.ctrl,IHLS.variations,
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

#Prepare datasets for plotting
indep.ssb <- ssb(IHLS.variations[["no IHLS"]])
indep.ssb$ssb <- indep.ssb$value
SCAI <- as.data.frame(NSH.tun.all[["SCAI"]]@index )
SCAI$SCAI <- SCAI$data
MLAI <- as.data.frame(NSH.tun.all[["MLAI"]]@index )
MLAI$MLAI <- MLAI$data
dat <- merge(indep.ssb[,c("year","ssb")],MLAI[,c("year","MLAI")],all=TRUE)
dat <- merge(dat,SCAI[,c("year","SCAI")])

#Plot the time series showing MLAI,SCAI
par(mar=c(5,4,2,4))
yr.lims <- range(pretty(dat$year))
plot(dat$year,dat$SCAI,xlab="Year",ylab="SCAI",pch=19,type="p",
    xaxs="i",yaxs="i",xlim=yr.lims,ylim=range(pretty(dat$SCAI)))
lines(dat$year,dat$SCAI,lty=1)
par(new=TRUE)
plot(dat$year,dat$MLAI,pch=1,type="p",axes=FALSE,xlim=yr.lims,
   ylim=range(pretty(dat$MLAI)),xlab="",ylab="",xaxs="i",yaxs="i")
lines(dat$year,dat$MLAI,lty=3)
axis(4)
mtext("MLAI",4,line=2)
legend("topleft",legend=c("SCAI","MLAI"),pch=c(19,1),lty=c(1,3))
title(main="Comparison of SCAI and MLAI")

#Plot the comparison between MLAI and SCAI
plot(log(dat$MLAI),log(dat$SCAI),xlab="log MLAI",ylab="log SCAI",pch=19)
text(log(dat$MLAI),log(dat$SCAI),sprintf("%02i",round(dat$year%%100)),pos=4,xpd=NA)
mdl <- lm(log(SCAI) ~ log(MLAI),dat)
abline(mdl)
title(main="MLAI vs SCAI")

#Plot the comparison between SSB and SCAI
plot(log(dat$ssb),log(dat$SCAI),xlab="log SSB",ylab="log SCAI",pch=19)
text(log(dat$ssb),log(dat$SCAI),sprintf("%02i",round(dat$year%%100)),pos=4,xpd=NA)
mdl <- lm(log(SCAI) ~ log(ssb),dat)
abline(mdl)
legend("topleft",bty="n",legend=c(sprintf("r2 = %4.2f",summary(mdl)$r.squared),
       sprintf("Slope = %4.2f ± %4.2f",coef(mdl)[2],1.96*summary(mdl)$coef[2,2])))
title(main="SSB vs SCAI")

#Plot the comparison between SSB and MLAI
plot(log(dat$ssb),log(dat$MLAI),xlab="log SSB",ylab="log MLAI",pch=19)
text(log(dat$ssb),log(dat$MLAI),sprintf("%02i",round(dat$year%%100)),pos=4,xpd=NA)
mdl <- lm(log(MLAI) ~ log(ssb),dat)
abline(mdl)
legend("topleft",bty="n",legend=c(sprintf("r2 = %4.2f",summary(mdl)$r.squared),
       sprintf("Slope = %4.2f ± %4.2f",coef(mdl)[2],1.96*summary(mdl)$coef[2,2])))
title(main="SSB vs MLAI")

### ============================================================================
### Finish
### ============================================================================
dev.off()
log.msg(paste("COMPLETE IN",sprintf("%0.1f",round(proc.time()[3]-start.time,1)),"s.\n\n"))
