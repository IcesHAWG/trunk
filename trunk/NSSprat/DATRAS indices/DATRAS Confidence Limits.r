######################################################################################################
# DATRAS IBTS Confidence Limits
#
# Version 1.00 24/03/2009 15:03:32
#
# Author: Mark Payne, mpa@aqua.dtu.dk
# DTU-Aqua, Charlottenlund, DK
# 
# Plots the IBTS time series, with confidence intervals from bootstap analysis
#
# Developed with:
#   - R version 2.8.0
#
# Changes:
#
# To be done:
#
# Notes:
#
# This script is subject to Mark's version of the "BEER-WARE" LICENSE.
# Mark Payne wrote this file. It is not intended for use by other authors.
# If it breaks, it is not my fault. Support is not provided. But if you are determined
# to use it anyway, and we meet some day, then, if you think this stuff is worth it,
# you can buy me a beer in return. Mark.
####################################################################################################

### ======================================================================================================
### Initialise system
### ======================================================================================================
# Start with house cleaning
rm(list = ls(all.names=TRUE)); gc(); graphics.off()
ver <- "\nDATRAS IBTS Confidence Limits v1.00\n"; cat(ver)
ver.datetime   <- "24/03/2009 15:03:32\n\n";
cat(ver.datetime); start.time <- proc.time()[3]
options(stringsAsFactors=FALSE)

### ======================================================================================================
### Load and prepare data files & Determine the quarter for the analyze 
### ======================================================================================================
#Load general data files
dat <- read.csv(file.path("..","Data","DATRAS Sprat bootstrap data.csv"))
idxs.all <- read.csv(file.path("..","Data","DATRAS Sprat total indices by age.csv"))

# Reads in data based on the quarter. Quarter is changed in the first line below:
#       Q1: First quarter
#       Q3: Third quarter 

for(Q in c("Q1","Q3")) {

switch(Q,
      Q1={idxs.dat <- subset(idxs.all,Year>=1984 & IndexArea=="NS_SpratIV" & Quarter==1)
        Q.dat <- subset(dat,Quarter==1 & Year>=1984)
        file.name <- file.path(".","output","DATRAS IBTS Q1 conf interval - %02d.png")
        ylabel <- "IBTS Q1 Index"}, 
      Q3 = {idxs.dat <- subset(idxs.all,Year>=1991 & IndexArea=="NS_SpratIV" & Quarter==3)
        Q.dat <- subset(dat,Quarter==3 & Year>=1991)
        file.name <- file.path(".","output","DATRAS IBTS Q3 conf interval - %02d.png")
        ylabel <- "IBTS Q3 Index"}) 


### ======================================================================================================
### Process IBTS Q1 index
### ======================================================================================================
#Filter Q1 data
Q.dat$CPUE <- rowSums(Q.dat[,paste("Age_",0:6,sep="")],na.rm=TRUE)
#Extract quantiles
Q.CIs <- tapply(Q.dat$CPUE,Q.dat$Year,quantile,c(0.025,0.25,0.5,0.75,0.975),simplify=TRUE)
Q.plot.dat <- cbind(Year=as.numeric(names(Q.CIs)),do.call(rbind,Q.CIs))
#Calculate total CPUE
idxs.dat$CPUE <- rowSums(idxs.dat[,paste("Age_",0:6,sep="")],na.rm=TRUE)

### ======================================================================================================
### Plot
### ======================================================================================================
png(file.name,units = "px", height=720,width=720,pointsize = 16, bg = "white")
par(mar=c(5,4,2,1))
plot(0,0,pch=NA,xlab="Year",ylab=ylabel,
      xlim=range(pretty(Q.plot.dat[,"Year"])), ylim=c(0,20000),yaxs="i")
x.dat <- c(Q.plot.dat[,"Year"],rev(Q.plot.dat[,"Year"]))
grid()
box()
polygon(x.dat,c(Q.plot.dat[,"2.5%"],rev(Q.plot.dat[,"97.5%"])),col="lightgrey")
polygon(x.dat,c(Q.plot.dat[,"25%"],rev(Q.plot.dat[,"75%"])),col="darkgrey")
lines(Q.plot.dat[,"Year"],Q.plot.dat[,"50%"],lwd=2,lty=2,col="black")
lines(idxs.dat$Year,idxs.dat$CPUE,lwd=3,col="red")
legend("topleft",legend=c("Index","Median","50% Conf. Int.","95% Conf. Int."),lty=c(1,2,0,0),lwd=c(3,2,0,0),
      pch=c(NA,NA,15,15),col=c("red","black","darkgrey","lightgrey"),bg="white")
dev.off()

}