################################################################################
# NSH_SAM MLAI vs SCAI
#
# $Rev$
# $Date$
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
respref <- "03b_MLAI_vs_SCAI" #Prefix for output files
resfile <- file.path(resdir,paste(respref,".RData",sep=""))

#Dependencies
all.in.file <- file.path(resdir,"03a_selected_surveys.RData")
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
#Only do the assessment if we are running in batch mode, or
#if the results file is missing
if(!file.exists(resfile) | !interactive()) {
   #Drop IHLS data, setup new objects, perform assessment
   NSH.tun <- NSH.tun[setdiff(names(NSH.tun),c("SCAI","MLAI"))] 
   source(file.path("benchmark","03_Setup_selected_surveys.r"))
   noIHLS.sam <- FLSAM(NSH,NSH.tun,NSH.ctrl)
   noIHLS.sam@name <- "no IHLS"
   
   #Now include the MLAI. The MLAI will only be dropped by "03_Setup_selected_surveys" if
   #it is called "MLAI". We therefore rename it to "temp_MLAI" to avoid this problem
   NSH.tun <- NSH.tun.all
   NSH.tun[["temp_MLAI"]] <- NSH.tun[["MLAI"]]
   NSH.tun <- NSH.tun[setdiff(names(NSH.tun),c("SCAI","MLAI"))]  #Drop SCAI,MLAI 
   source(file.path("benchmark","03_Setup_selected_surveys.r"))
   MLAI.sam <- FLSAM(NSH,NSH.tun,NSH.ctrl)
   MLAI.sam@name <- "MLAI"
   
   #Combine into one
   IHLS.variations <- FLSAMs(SCAI.sam,MLAI.sam,noIHLS.sam)
   names(IHLS.variations) <- sapply(IHLS.variations,name)
   
   #Save any results
   save(NSH,NSH.tun.all,NSH.ctrl,IHLS.variations,file=resfile)

} else {
  #Load the file
  load(resfile)
}

### ============================================================================
### Plots
### ============================================================================
#Setup plots
pdf(file.path(resdir,paste(respref,".pdf",sep="")),pointsize=16)

#Comparison of assessments with different plus groups
print(plot(IHLS.variations,main="Effects of IHLS survey"))
print(plot(IHLS.variations,xlim=c(1990,2011),
    main="Effects of IHLS survey over recent time period"))

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
plot(log10(dat$ssb),log10(dat$SCAI),xlab="log10 SSB",ylab="log10 SCAI",pch=19)
text(log10(dat$ssb),log10(dat$SCAI),sprintf("%02i",round(dat$year%%100)),pos=4,xpd=NA)
mdl <- lm(log10(SCAI) ~ log10(ssb),dat)
abline(mdl)
legend("topleft",bty="n",legend=c(sprintf("r2 = %4.2f",summary(mdl)$r.squared),
       sprintf("Slope = %4.2f ± %4.2f",coef(mdl)[2],1.96*summary(mdl)$coef[2,2])))
title(main="SSB vs SCAI")

#Plot the comparison between SSB and MLAI
plot(log10(dat$ssb),log10(dat$MLAI),xlab="log10 SSB",ylab="log10 MLAI",pch=19)
text(log10(dat$ssb),log10(dat$MLAI),sprintf("%02i",round(dat$year%%100)),pos=4,xpd=NA)
mdl <- lm(log10(MLAI) ~ log10(ssb),dat)
abline(mdl)
legend("topleft",bty="n",legend=c(sprintf("r2 = %4.2f",summary(mdl)$r.squared),
       sprintf("Slope = %4.2f ± %4.2f",coef(mdl)[2],1.96*summary(mdl)$coef[2,2])))
title(main="SSB vs MLAI")

#Plot comparison of residuals
MLAI.resids <- subset(residuals(IHLS.variations[["MLAI"]]),fleet=="temp_MLAI")
SCAI.resids <- subset(residuals(IHLS.variations[["SCAI"]]),fleet=="SCAI")
ylims <- range(pretty(c(MLAI.resids$std.res,SCAI.resids$std.res)))
par(mfcol=c(1,2))
plot(MLAI.resids$year,MLAI.resids$std.res,ylim=ylims,
  xlab="Year",ylab="MLAI",type="h")
points(MLAI.resids$year,MLAI.resids$std.res,pch=16)
abline(h=0)
legend("topleft",legend="a)",bty="n")
plot(SCAI.resids$year,SCAI.resids$std.res,ylim=ylims,
  xlab="Year",ylab="SCAI",type="h",pch=16)
points(SCAI.resids$year,SCAI.resids$std.res,pch=16)
abline(h=0)
legend("topleft",legend="b)",bty="n")
title(main="MLAI and SCAI residuals",outer=TRUE,line=-1)

#Comparison of observation variances
obv <- obs.var(IHLS.variations)
obv$age[is.na(obv$age)] <- ""
levels(obv$fleet)[which(levels(obv$fleet)=="temp_MLAI")] <- "MLAI"
print(barchart(value ~ age| fleet,obv,groups=name,horiz=FALSE,
        as.table=TRUE,xlab="Age",ylab="Observation variance",
        main="Observation variances under differnt IHLS scenarios",
        auto.key=list(space="right"),
        ylim=range(pretty(c(0,obv$value))),
        scale=list(alternating=FALSE)))

### ============================================================================
### Finish
### ============================================================================
dev.off()
log.msg(paste("COMPLETE IN",sprintf("%0.1f",round(proc.time()[3]-start.time,1)),"s.\n\n"))
