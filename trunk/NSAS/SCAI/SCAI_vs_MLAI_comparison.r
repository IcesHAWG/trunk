######################################################################################################
# Comparison of SCAI and MLAI indices
#
# $Rev: 406 $
# $Date: 2011-03-04 15:36:01 +0100 (Fri, 04 Mar 2011) $
#
# Author: Mark Payne, mpa@aqua.dtu.dk
# DTU-Aqua, Charlottenlund, DK
# 
# Compares the SCAI and MLAI indices both directly, and then by correlating them against
# the SSB estimated from an assessment performed without the MLAI
#
# Developed with:
#   - R version 2.9.1
#
# Changes:
#   V 1.00 - Initial version
#
# To be done:
#
# Notes:
#   This work is described in more detail in the publication
#   Payne, M. R. 2010. Mind the gaps: a state-space model for analysing the dynamics
#   of North Sea herring spawning components. – ICES Journal of Marine Science, 67: 1939–1947.
#
# ----------------------------------------------------------------------------
# "THE BEER-WARE LICENSE" (Revision 42):
# Mark Payne wrote this file. As long as you retain this notice, you can do
# whatever you want with this stuff - just don't hold me responsible for it.
# If we meet some day, and you think still this stuff is worth it, you
# can buy me a beer in return. Mark Payne (with kudos to Poul-Henning Kamp)
# ----------------------------------------------------------------------------
####################################################################################################

### ======================================================================================================
### Initialise system
### ======================================================================================================
# Start with house cleaning
rm(list = ls()); gc(); graphics.off()
ver <- "SCAI vs MLAI comparison $Rev: 406 $"
ver.datetime   <- "$Date: 2011-03-04 15:36:01 +0100 (Fri, 04 Mar 2011) $"
cat(paste("\n",ver,"\n",sep=""));cat(paste(ver.datetime,"\n\n",sep=""))
start.time <- proc.time()[3]
options(stringsAsFactors=FALSE)

library(FLICA)  #Need for assessment data
### ======================================================================================================
### Parameters
### ======================================================================================================
#Load data
output.dir <- file.path(".","SCAIoutputs")
SCAI.dat <- read.csv(file.path(output.dir,"SCAI_indices.csv"))
ica.dat  <- load(file.path("..","results","NSH Assessment Assessment.RData"))

#output device
pdf(file.path(output.dir,"SCAI_vs_MLAI.pdf"),width=200/25.4,height=200/25.4,pointsize=18,onefile=TRUE)

### ======================================================================================================
### Reperform assessment after removing MLAI
### ======================================================================================================
MLAI.idx.no <- which(names(NSH.tun)=="MLAI")
#Remove the MLAI indices from the tuning
MLAI.idx <- NSH.tun[[MLAI.idx.no]]
NSH.tun <- NSH.tun[-MLAI.idx.no]

#Remove the MLAI from the ctrl object
NSH.ctrl@index.model <- NSH.ctrl@index.model[-MLAI.idx.no]

#Perform the assessment
NSH.ica <- FLICA(NSH,NSH.tun,NSH.ctrl)
NSH     <- NSH + NSH.ica

#Setup dataframes for further analysis
NSH.ssb <- as.data.frame(ssb(NSH))
NSH.ssb$ssb <- NSH.ssb$data
MLAI    <- subset(as.data.frame(MLAI.idx),slot=="index")
MLAI$MLAI <- MLAI$data

### ======================================================================================================
### Merge data sets
### ======================================================================================================
#Get sumSCAI
SCAI.cols <- grep("^SCAI",colnames(SCAI.dat))
sumSCAI   <- data.frame(year=SCAI.dat$Year,sumSCAI=rowSums(SCAI.dat[SCAI.cols]))

#Modify FLR data frames
dat <- merge(sumSCAI,MLAI[,c("year","MLAI")],all=TRUE)
dat <- merge(dat,NSH.ssb[,c("year","ssb")])

### ======================================================================================================
### Plot the data
### ======================================================================================================
par(mar=c(5,4,2,4))

#Plot the time series showing MLAI,SCAI
yr.lims <- range(pretty(dat$year))
plot(dat$year,dat$sumSCAI,xlab="Year",ylab="sum SCAI",pch=19,type="p",
    xaxs="i",yaxs="i",xlim=yr.lims,ylim=range(pretty(dat$sumSCAI)))
lines(dat$year,dat$sumSCAI,lty=1)
par(new=TRUE)
plot(dat$year,dat$MLAI,pch=1,type="p",axes=FALSE,xlim=yr.lims,ylim=range(pretty(dat$MLAI)),xlab="",ylab="",xaxs="i",yaxs="i")
lines(dat$year,dat$MLAI,lty=3)
axis(4)
mtext("MLAI",4,line=2)
legend("topleft",legend=c("sum SCAI","MLAI"),pch=c(19,1),lty=c(1,3))

#Plot the comparison between MLAI and SCAI
plot(log(dat$MLAI),log(dat$sumSCAI),xlab="log MLAI",ylab="log sum SCAI",pch=19)
text(log(dat$MLAI),log(dat$sumSCAI),sprintf("%02i",round(dat$year%%100)),pos=4,xpd=NA)
mdl <- lm(log(sumSCAI) ~ log(MLAI),dat)
abline(mdl)

#Plot the comparison between SSB and SCAI
plot(log(dat$ssb),log(dat$sumSCAI),xlab="log SSB",ylab="log sum SCAI",pch=19)
text(log(dat$ssb),log(dat$sumSCAI),sprintf("%02i",round(dat$year%%100)),pos=4,xpd=NA)
mdl <- lm(log(sumSCAI) ~ log(ssb),dat)
abline(mdl)
legend("topleft",bty="n",legend=c(sprintf("r2 = %4.2f",summary(mdl)$r.squared),
       sprintf("Slope = %4.2f ± %4.2f",coef(mdl)[2],1.96*summary(mdl)$coef[2,2])))


#Plot the comparison between SSB and MLAI
plot(log(dat$ssb),log(dat$MLAI),xlab="log SSB",ylab="log MLAI",pch=19)
text(log(dat$ssb),log(dat$MLAI),sprintf("%02i",round(dat$year%%100)),pos=4,xpd=NA)
mdl <- lm(log(MLAI) ~ log(ssb),dat)
abline(mdl)
legend("topleft",bty="n",legend=c(sprintf("r2 = %4.2f",summary(mdl)$r.squared),
       sprintf("Slope = %4.2f ± %4.2f",coef(mdl)[2],1.96*summary(mdl)$coef[2,2])))

#Close output
if(length(grep("pdf|png|wmf",names(dev.cur())))!=0) dev.off()
