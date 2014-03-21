######################################################################################################
# Comparison of SCAI and SSB indices
#
# $Rev: 406 $
# $Date: 2011-03-04 15:36:01 +0100 (Fri, 04 Mar 2011) $
#
# Author: Mark Payne, mpa@aqua.dtu.dk
# DTU-Aqua, Charlottenlund, DK
# 
# Estimates the SSB from the most recent SCAI
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
#   of North Sea herring spawning components. ? ICES Journal of Marine Science, 67: 1939?1947.
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
ver <- "SCAI vs SSB comparison $Rev: 406 $"
ver.datetime   <- "$Date: 2011-03-04 15:36:01 +0100 (Fri, 04 Mar 2011) $"
cat(paste("\n",ver,"\n",sep=""));cat(paste(ver.datetime,"\n\n",sep=""))
start.time <- proc.time()[3]
options(stringsAsFactors=FALSE)

library(FLSAM)
library(reshape)

### ======================================================================================================
### Parameters
### ======================================================================================================
#Load data
output.dir <- file.path(".","SCAIoutputs")
SCAI.dat <- read.csv(file.path(output.dir,"SCAI_indices.csv"))
load(file.path("..","..","results","North Sea Herring.RData"))

#output device
pdf(file.path(output.dir,"SCAI_vs_SSB.pdf"),width=297/25.4,
    height=210/25.4,pointsize=24,onefile=TRUE)

### ======================================================================================================
### Merge data sets
### ======================================================================================================
#Get sumSCAI
SCAI.cols <- grep("^SCAI",colnames(SCAI.dat))
sumSCAI   <- melt(tapply(SCAI.dat$SCAI,SCAI.dat[,"Year"],sum))
colnames(sumSCAI) <- c("year","sumSCAI")

#Get SSB, remove the most recent estimate
sam.ssb <- ssb(NSH.sam)
sam.ssb <- sam.ssb[-nrow(sam.ssb),]

#Merge data
dat <- merge(sumSCAI,sam.ssb[,c("year","value")],all=TRUE)

#Predict the SSB based on the most recent estimate
dat$logSSB <- log(dat$value)
dat$logSCAI <- log(dat$sumSCAI)
mdl <- lm(logSSB ~ logSCAI,dat)
most.rec.SCAI <- rev(sumSCAI$sumSCAI)[1]
pred.ssb <- exp(predict(mdl,
                    newdata=data.frame(logSCAI=log(most.rec.SCAI))))

### ======================================================================================================
### Plot the data
### ======================================================================================================
#Setup plotting data
fit.dat <- subset(dat,!is.na(dat$sumSCAI)&!is.na(dat$value),
                  c("year","sumSCAI","logSCAI"))
fit.dat <- cbind(fit.dat,predict(mdl,fit.dat,se.fit=TRUE))
fit.dat <- transform(fit.dat,
                     logSSB.ub=fit + 1.96*se.fit,
                     logSSB.lb=fit - 1.96*se.fit)
fit.dat <- transform(fit.dat,SSB=exp(fit),
                     SSB.ub=exp(logSSB.ub),SSB.lb=exp(logSSB.lb))
fit.dat <- fit.dat[order(fit.dat$sumSCAI),]

#Plot the comparison between SSB and SCAI
plot(dat$sumSCAI,dat$value/1e6,xlab="SCAI",ylab="SSB (Mt)",pch=NA,log="xy")
polygon(c(fit.dat$sumSCAI,rev(fit.dat$sumSCAI)),
        c(fit.dat$SSB.ub,rev(fit.dat$SSB.lb))/1e6,col="grey")
text(dat$sumSCAI,dat$value/1e6,sprintf("%02i",round(dat$year%%100)),xpd=NA)
lines(fit.dat$sumSCAI,fit.dat$SSB/1e6,lwd=4,col="red")
abline(h=pred.ssb/1e6,col="red",lty=3)
text(most.rec.SCAI,0.3,round(most.rec.SCAI),
     pos=2,srt=90,adj=c(0.5,0.5),col="red",cex=1.25)
abline(v=most.rec.SCAI,col="red",lty=3)
text(2000,round(pred.ssb/1e6,2),sprintf("%.2f",round(pred.ssb/1e6,2)),
     pos=1,col="red",cex=1.25)
abline(v=most.rec.SCAI,col="red",lty=3)

#Close output
if(length(grep("pdf|png|wmf",names(dev.cur())))!=0) dev.off()
