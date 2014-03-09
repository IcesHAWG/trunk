##############################################################################
# IBTS0 Laval length distributions
#
# $Rev: 619 $
# $Date: 2011-11-25 14:59:55 +0100 (Fri, 25 Nov 2011) $
#
# Author: Mark Payne, mpa@aqua.dtu.dk
# DTU-Aqua, Charlottenlund, DK
# 
# Examines the length distributions of the IBTS0 survey
#
# Developed with:
#   - R version 2.13.0
#   - sp package version 0.9-84
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
##############################################################################

### ============================================================================
### Initialise system, including convenience functions and title display
### ============================================================================
rm(list=ls()); graphics.off(); start.time <- proc.time()[3]
options(stringsAsFactors=FALSE)
log.msg     <-  function(string) {
	cat(string);flush.console()
}

#Source externals
source(file.path(".","IBTS0_common_elements.r"))
library(sp);library(mgcv);library(maps);library(mapdata);library(geoR)
library(PBSmapping)

log.msg("\nMIK Index Calculation and Check\n===============================\n")

### ==========================================================================
### Parameters
### ==========================================================================
#Output file
pdf(file.path(out.dir,"Length distribution.pdf"),width=210/25.4,height=210/25.4)

### ==========================================================================
### Retrieve data and setup object
### ==========================================================================
#load data
db <- read.IBTS0()

#Filter out hauls where there are no-larvae observed - its
#obviously a bit hard to define a median length when 
#you don't observe anything!
dat <- subset(db,no.per.m2>0)
dat <- subset(dat,!(is.na(LatDec) | is.na(LongDec) | is.na(median.length)))

#Convert to UTM using PBSMapping 
UTM.df <- data.frame(X=dat$LongDec,Y=dat$LatDec)
UTM.df$EID <- seq(nrow(UTM.df))
UTM.df <- as.EventData(UTM.df,projection="LL")
UTM.df <- convUL(UTM.df)
dat <- data.frame(lat=dat$LatDec,lon=dat$LongDec,UTM.df[,c("X","Y")],
          dat[,c("Year","POSIX","median.length")])

### ==========================================================================
### Fit spatial GAM models
### ==========================================================================
#Split data into individual years
dat.yrs <- split(dat,dat$Year) 

#Fit GAMs
mdls <- lapply(dat.yrs, function(d) {
         log.msg(sprintf("FITTING SPATIAL GAM FOR %i...\n",unique(d$Year)))
         mdl <- gamm(median.length ~ te(lon,lat),data=d,
                   correlation=corExp(form=~X+Y,nugget=TRUE))
         mdl$data <- d
         return(mdl) 
        })

### ==========================================================================
### Make Plots
### ==========================================================================
#Setup to Loop in reverse order
yrs <- rev(sort(names(mdls)))
latlims <- range(pretty(dat$lat))
lonlims <- range(pretty(dat$lon))
map.poly <- map("worldHires",xlim=lonlims,ylim=latlims,plot=FALSE,fill=TRUE)

#First generate spatial plots
for(yr in yrs) {
   log.msg(sprintf("Plotting %s...\n",yr))
   #Visualise spatial plot
   mdl <- mdls[[yr]]$gam 
   filled.contour(matrix(NA,nrow=2,ncol=2),
     xlim=lonlims,ylim=latlims,zlim=c(5,40),
     main=sprintf("Median Length Distribution %s",yr),
     color.palette=heat.colors,nlevels=100,
     plot.axes= {
        vis.gam(mdl,view=c("lon","lat"),plot.type="contour",
          contour.col="black",zlim=c(5,40),
          n.grid=256, xlab="Longitude",ylab="Latitude",add=TRUE,
          too.far=0.05,type="response")
        polygon(map.poly,col="grey")
        points(dat.yrs[[yr]]$lon,dat.yrs[[yr]]$lat,pch=".")
        box() })
}

#Now generate plots of model diagnostic plots  
for(yr in yrs) {
   log.msg(sprintf("Plotting model diagnostics for %s...\n",yr))
   mdl <- mdls[[yr]]$gam 
   par(mfrow=c(2,2),oma=c(0,0,2,0))
   #Residual plot
   stdres <- residuals(mdl,type="pearson")
   plot(NA,NA,xlim=lonlims,ylim=latlims,
     xlab="Longitude",ylab="Longitude",main="Spatial Residuals")
   polygon(map.poly,col="grey")
   points(mdl$data$lon,mdl$data$lat,pch=16,
     cex=sqrt(1*abs(stdres)/2)+0.01,col=ifelse(stdres<0,"red","blue"))
   legend("topright",c("Positive","Negative"),
    col=c("blue","red"),pch=16,bg="white")
   box()

   #QQ plot
   qqnorm(stdres)

   #Tukey-Anscombe
   plot(mdl$fitted.values,stdres,
    xlab="Fitted value",ylab="Residuals",main="Tukey-Anscombe")
   
   #Spatial variogram
   vg<-variog(data=residuals(mdl),pairs.min=5,
          max.dist=100, coords=cbind(mdl$data$X,mdl$data$Y))
   plot(vg,xlab="Distance (km)",main="Spatial Correlogram")

   #Title
   title(main=sprintf("%s Model Diagnostics",yr),outer=TRUE) 
}

#


### ==========================================================================
### Fit spatial GAM models
### ==========================================================================



### ==========================================================================
### Finish
### ==========================================================================
#Finish
dev.off()
log.msg(paste("COMPLETE IN",sprintf("%0.1f",round(proc.time()[3]-start.time,1)),"s.\n\n"))

