######################################################################################################
# Direct Comparison of NS Sprat IBTS Q1 and Q3 indices
#
# Version 1.00 31/08/2009 19:54:38
#
# Author: Mark Payne, mpa@aqua.dtu.dk
# DTU-Aqua, Charlottenlund, DK
# 
# <description>
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
rm(list = ls(all.names=TRUE)); gc(); graphics.off()
ver <- "Direct Comparison of IBTS Q1 and Q3 indices"; cat(ver)
ver.datetime   <- "\n31/08/2009 19:54:38\n\n";
cat(ver.datetime); start.time <- proc.time()[3]
options(stringsAsFactors=FALSE)

### ======================================================================================================
### Parameters
### ======================================================================================================
dat <- read.csv(file.path("..","Data","DATRAS Sprat total indices by age.csv"))

### ======================================================================================================
### Process data
### ======================================================================================================
#Calculate total CPUE
age.cols <- grep("Age_",names(dat))
dat$CPUE <- rowSums(dat[,age.cols],na.rm=TRUE)

#Split data.sets
sel.cols <- c("Year","CPUE")
IBTSQ1 <- subset(dat,Quarter==1 & IndexArea=="NS_SpratIV")[,sel.cols]
IBTSQ3 <- subset(dat,Quarter==3 & IndexArea=="NS_SpratIV")[,sel.cols]

#Merge into a single data set
plot.dat <- merge(IBTSQ1,IBTSQ3,by="Year",suffix=c(".Q1",".Q3"))
plot.dat <- transform(plot.dat, log.Q1=log(CPUE.Q1),log.Q3=log(CPUE.Q3))

### ======================================================================================================
### Plot data
### ======================================================================================================

mdl <- lm(log.Q3 ~log.Q1,plot.dat)
new.dat <- data.frame(log.Q1=seq(min(plot.dat$log.Q1),max(plot.dat$log.Q1),length.out=101))
pred <- predict(mdl,new.dat,interval="prediction")
matplot(new.dat,pred,xlab="Log (IBTS Q1)",ylab="Log (IBTS Q3)",type="l",col="black",lty=c(1,2,2),lwd=c(2,1,1))
points(plot.dat$log.Q1,plot.dat$log.Q3,pch=19)
  rsq <-  sprintf("%4.3f",round(summary(mdl)$r.squared,3))
  if(summary(mdl)$coef[2,4]<0.05) rsq <- paste(rsq,"*")
  text(grconvertX(0.8,"npc"),grconvertY(0.1,"npc"),bquote(r^2 == .(rsq)),
    cex=ifelse(summary(mdl)$coef[2,4]<0.05,1.2,1))
