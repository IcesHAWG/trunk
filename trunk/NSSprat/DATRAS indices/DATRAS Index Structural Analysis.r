######################################################################################################
# DATRAS Sprat Index Structural Analysis
#
# Version 1.00 18/03/2009 11:40:40
#
# Author: Mark Payne, mpa@aqua.dtu.dk
# DTU-Aqua, Charlottenlund, DK
# 
# Analysises the relative contribution of individual hauls to the IBTS sprat Index, as
# calculated using DATRAS.
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
ver <- "\nDATRAS Sprat Index Structural Analysis V1.00\n"; cat(ver)
ver.datetime   <- "18/03/2009 11:40:40\n\n";
cat(ver.datetime); start.time <- proc.time()[3]
options(stringsAsFactors=FALSE)

### ======================================================================================================
### Determine the quarter for the analyze 
### ======================================================================================================
# Reads in data based on the quarter. Quarter is changed in the first line below:
#       Q1: First quarter
#       Q3: Third quarter 

for(Q in c("Q1","Q3")) {

switch(Q,
      Q1 = {qrt <- 1
            start.year <- 1984},
      Q3 = {qrt <- 3
            start.year <- 1991})

dat.file  <-  file.path("..","Data","IBTS Sprat CPUE per age per haul.csv")
ices.file <-  file.path("..","Data","DATRAS Sprat total indices by age.csv")
file.name <- file.path(".","output",paste("DATRAS IBTS Q",qrt," structure - %02d.png",sep=""))
png(file.name,units = "px", height=720,width=720,pointsize = 16, bg = "white")
par(mar=c(5,4,0,1))


### ======================================================================================================
### Options
### ======================================================================================================
area.wts.file  <-  file.path("..","Data","DATRAS Sprat statsquare weights.csv")

### ======================================================================================================
### Load, prepare data
### ======================================================================================================
dat.raw <- read.csv(dat.file)
ices.raw <- read.csv(ices.file)
area.wts <- read.csv(area.wts.file)
#Filter out years prior to 1984 and stuff that is not GOV
dat     <- subset(dat.raw,Year>=start.year & Quarter==qrt & Gear=="GOV")
ices    <- subset(ices.raw,Year>=start.year & Quarter==qrt & IndexArea=="NS_SpratIV")
#Generate a total CPUE
dat$CPUE  <-  rowSums(dat[,paste("Age_",0:10,sep="")],na.rm=TRUE)
ices$CPUE <-  rowSums(ices[,paste("Age_",0:10,sep="")],na.rm=TRUE)
#Split haul data into a list of values by year
dat.yrs  <- split(dat,dat$Year)
#Strip white space from area weighting
area.wts$Subarea <- gsub(" ","",area.wts$Subarea)

### ======================================================================================================
### Calculate indices
### ======================================================================================================
#Mean values by stat square and year
statsq.means <- lapply(dat.yrs,function(x) tapply(x$CPUE,x$Subarea,mean))
#Weight the stat-squares according to the weighting function
weighted.statsq.means <- lapply(statsq.means,function(x) {
                              weighted <- merge(data.frame(Subarea=names(x),mean.CPUE=x),area.wts)
                              return(weighted$mean.CPUE*weighted$area.wt)
                            })
#(Weighted) Mean of the stat-square values
ns.means     <- sapply(weighted.statsq.means,sum)/sum(area.wts$area.wt)
#This calculation is wrong, however, as it includes the weighting for squares that are not sampled.
#Below we correct for this effect.

### ======================================================================================================
### Calculate weight of each haul
### ======================================================================================================
weighted.hauls <- lapply(dat.yrs,function(x) {
                    #Hauls per square
                    n.hi <- table(x$Subarea)
                    #Weights by ss
                    num.wts <- 1/n.hi
                    #Account for those areas that are not sampled - they are not included
                    #in the calculation and weighting
                    sum.area.wts <- sum(subset(area.wts,area.wts$Subarea%in%unique(x$Subarea),select=area.wt))
                    #Now merge in sampling weights and area weighs
                    y <- merge(x,area.wts)
                    z <- merge(y,data.frame(Subarea=names(num.wts),num.wt=data.frame(num.wts)$Freq))
                    z$weighted.CPUE <- z$CPUE * z$num.wt *z$area.wt / sum.area.wts
                    return(z)
                })
#Now calculate the index by simply adding the weighted hauls together
weighted.idxs  <- sapply(weighted.hauls,function(x) sum(x$weighted.CPUE))

### ======================================================================================================
### Cross index calculation
### ======================================================================================================

ylims <- range(pretty(c(0,ns.means,ices$CPUE)))
plot(ices$Year,ices$CPUE,xlim=c(1980,2010),ylim=ylims,xlab="Year",ylab="Index",type="l",col="black",lwd=3)
grid()
points(as.numeric(names(weighted.idxs)),weighted.idxs,col="red",pch=2)
legend("topleft",legend=c("DATRAS calculation","Calculated Here"),pch=c(NA,2),
          col=c("black","red"),lwd=c(3,1),bg="white")

### ======================================================================================================
### Now calculate and make cumulative plot
### ======================================================================================================
#Calculate cumulative distribution
cum.dists  <-  lapply(weighted.hauls,function(x) {
                  sorted.hauls <- sort(x$weighted.CPUE,decreasing=TRUE)
                  cdf    <- cumsum(c(sorted.hauls))/sum(sorted.hauls)
               })


#Make the plot
plot(0,0,pch=NA,xlim=c(0,30),ylim=c(0,1),xlab="Haul Rank",
    ylab=sprintf("Cumulative Contribution - IBTS Q%i Index",qrt))
grid()
lapply(cum.dists,function(x) {
    lines(0:length(x),c(0,x))
    points(1:length(x),x,pch=19,col="white",cex=1.5)
})
lapply(names(cum.dists),function(yr) {
    x <- cum.dists[[yr]]
    text(1:length(x),x,sprintf("%02i",as.numeric(yr)%%100),cex=0.5)
})

#Plot of number of histogram of number of hauls to 90% of index
plot(0,0,pch=NA,xlab="Number of hauls",
    ylab=sprintf("Cumulative Distribution over Years - IBTS Q%i Index",qrt),
    xlim=c(0,40),ylim=c(0,1))
grid()
abline(h=c(0,1),lty=2)
pcts <- c(0.5,0.75)
cols <- c("red","black","blue")
for(i in 1:length(pcts)){
  hauls.to.set.pct <- sapply(cum.dists,function(x,lim){sum(x<lim)}, pcts[i])
  lines(ecdf(hauls.to.set.pct),do.points=FALSE,verticals=TRUE,lwd=3,col.vert=cols[i],col.hor=cols[i])
}
legend("bottomright",legend=sprintf("%i%% of Index",pcts*100),col=cols,pch=NA,lwd=3,bg="white")


dev.off()
}