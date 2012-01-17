######################################################################################################
# MIK Index Calculation
#
# Version v1.00 6/10/2009 00:23:01
#
# Author: Mark Payne, mpa@aqua.dtu.dk
# DTU-Aqua, Charlottenlund, DK
# 
# Calculates the MIK index from the MIK database and also the partial contributions
# of each individual haul to the index
#
# Developed with:
#   - R version 2.9.1
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
ver <- "MIK Index Calculation v1.00"
ver.datetime   <- "6/10/2009 00:23:01";
cat(paste("\n",ver,"\n",sep=""));cat(paste(ver.datetime,"\n\n",sep=""))
start.time <- proc.time()[3]
options(stringsAsFactors=FALSE)
library(maps);library(mapdata);library(sp)

### ======================================================================================================
### Parameters
### ======================================================================================================
src.file <- file.path("..","MIK database Herring larvae 201009 - MPA mods.csv")

MIK.areas <- data.frame(MIK.area=c("nw","ne","cw","ce",
                                   "sw","se","ka","ch"),
                        MIK.area.surface.area=c(27,11,28,33,12,30,10,10)*3086913600/10^9)

#MIK data source
actual.MIK.index <- read.csv(file.path("MIK index.csv"))

### ======================================================================================================
### StatSquare to Lat,Long Function
### ======================================================================================================
ss2ll  <- function(statSquare) {
  #Calculates the Latitude and Longitude based on a 4 letter/number statsquare code

  #First, the lat
  ss.num    <-  as.integer(substring(statSquare,1,2))
  lat      <-  ss.num*0.5 + 35.75      #Relationship derived by correlating the two factors

  #Then the lon
  ss.letter <-  toupper(substring(statSquare,3,3))
  ss.num2   <-  as.integer(substring(statSquare,4,4))
  ss.letter <-  factor(ss.letter,levels=c("E","F","G","Q"))
  levels(ss.letter) <-  c(-10,0,10,-10)
  long       <-  ss.num2+as.numeric(as.character(ss.letter))+0.5

  #Check that the results of lat are reasonable
  if (prod(!is.na(lat)) == FALSE) {
    stop("Invalid statSquare input")
  }

  #Return results
  longLat     <-  cbind(long,lat)
  return(longLat)
}

### ======================================================================================================
### Load and Polish data
### ======================================================================================================
#load data
d.raw <- read.csv(src.file)

#Store rownames, so that they propigate properly
dat <- transform(d.raw,spreadsheet.row=1:nrow(d.raw)+1)       #So that they correspond to the row numbers in spreadsheet

#Filter out data where the mean length is too short
#dat <- subset(d.raw, !(is.na(d.raw$meanlength) | is.na(d.raw$rectangle) | is.na(d.raw$stat2) | d.raw$stat1==0))
dat <- subset(dat,!(meanlength<20 & lat < 54))    #Exclude small downs
#dat <- subset(dat,no!=0)

#Define subareas after SAS code
dat$stat4 <- NA
dat$stat4[dat$stat2=="E"] <- dat$stat3[dat$stat2=="E"]+10;
dat$stat4[dat$stat2=="F"] <- dat$stat3[dat$stat2=="F"]+20;
dat$stat4[dat$stat2=="G"] <- dat$stat3[dat$stat2=="G"]+30;

#Define sections following SAS code
dat$MIK.area <- NA
dat$MIK.area[dat$stat4<22 & dat$stat1>39 & dat$stat1<46] <- "cw"
dat$MIK.area[dat$stat4>21 & dat$stat1>39 & dat$stat1<46] <- "ce"
dat$MIK.area[dat$stat4<22 & dat$stat1<40 & dat$stat1>34] <- "sw"
dat$MIK.area[dat$stat4>21 & dat$stat1<40 & dat$stat1>34] <- "se"
dat$MIK.area[dat$stat4<22 & dat$stat1>45] <- "nw"
dat$MIK.area[dat$stat4>21 & dat$stat1>45] <- "ne"
dat$MIK.area[dat$stat4>28] <- "ka"
dat$MIK.area[dat$stat1<35] <- "ch"

#Plot rectangles with one colour for each area
#dat$MIK.area <- factor(dat$MIK.area)
ss.latlons <- as.data.frame(ss2ll(dat$rectangle))
xlims <- range(pretty(ss.latlons$long))
ylims <- range(pretty(ss.latlons$lat))
map("worldHires",xlim=xlims,ylim=ylims,fill=TRUE,col="grey")
map.axes()
abline(v=seq(min(xlims),max(ylims),by=1),h=seq(min(ylims),max(ylims),by=0.5),col="grey")
points(ss.latlons$long,ss.latlons$lat,pch=19,col=factor(dat$MIK.area))

#Misc mods
dat$no.per.m2 <- as.numeric(dat$no.per.m2)
dat$area     <- as.numeric(dat$area)
dat <- merge(dat,MIK.areas,all=FALSE,by="MIK.area",sort=FALSE)

### ======================================================================================================
### First calculation in the standard manner
### ======================================================================================================
#Split the data into MIK areas and years
yr.area.l <- split(dat,list(year=dat$year,area=dat$MIK.area),drop=TRUE)

#Calculate the mean number density per stat square
dens.by.ss <- lapply(yr.area.l,function(x) {
    means <- tapply(x$no.per.m2,factor(x$rectangle,levels=unique(x$rectangle)),mean)
    unique.rects  <- !duplicated(x$rectangle)

    opt   <- data.frame(x[unique.rects,c("year","MIK.area","rectangle","area")],means,names(means))
    return(opt)
    })

#Mean density per MIK area
dens.by.MIK.area.l <- lapply(dens.by.ss,function(x) {
             data.frame(x[1,c("year","MIK.area")],MIK.area.dens=mean(x$means))
              })
dens.by.MIK.area   <- do.call(rbind,dens.by.MIK.area.l)

#Abundance of larvae in each MIK area
abund.by.area   <- merge(dens.by.MIK.area,MIK.areas,all=FALSE,sort=FALSE)
abund.by.area$area.abund <- abund.by.area$MIK.area.surface.area*abund.by.area$MIK.area.dens
abund.by.area$MIK.area <- factor(abund.by.area$MIK.area,levels=MIK.areas$MIK.area)

#Mean density across the North Sea
MIK.table <- xtabs(MIK.area.dens ~ year + MIK.area,abund.by.area)
MIK.index <- tapply(abund.by.area$area.abund,abund.by.area$year,sum)
MIK.table <- cbind(MIK.table,index=MIK.index)
print(round(MIK.table,3))

### ======================================================================================================
### Then using partial contributions
### ======================================================================================================
#First split the data into individual years and calculate the weighting by rectangle and by MIK.area
d.yr <- split(dat,dat$year)
d.wt.l <- lapply(d.yr,function(x) {
            #Weights per individual rectangle
            rect.wts <- as.data.frame(1/table(rectangle=x$rectangle),responseName="rect.wt")
            x2 <- merge(x,rect.wts,all=TRUE,by="rectangle",sort=FALSE)
            #Then weights per MIK.area
            unique.rect  <- !duplicated(x$rectangle)
            areas.table   <- table(MIK.area=x$MIK.area[unique.rect])
            MIK.area.wts <- as.data.frame(1/areas.table,responseName="MIK.area.wt")
            x3 <- merge(x2,MIK.area.wts,all=TRUE,by="MIK.area",sort=FALSE)
            return(x3)
        })

#Combine back into a single data frame
d.wt <- do.call(rbind,d.wt.l)

#And then calculate individual weighting factors
d.wt$wt <- d.wt$MIK.area.surface.area*d.wt$rect.wt*d.wt$MIK.area.wt
d.wt$weighted.dens <- d.wt$wt*d.wt$no.per.m2

#MIK index by wts
MIK.index.wt <- tapply(d.wt$weighted.dens,d.wt$year,sum)

### ======================================================================================================
### Compare the old and the new calculations
### ======================================================================================================
#Relationship between as published MIK and recalculated
lims <- range(pretty(actual.MIK.index$HAWG.Mik,actual.MIK.index$Recalculated.MIK))
plot(actual.MIK.index$HAWG.Mik,actual.MIK.index$Recalculated.MIK,xlim=lims,ylim=lims,
    xlab="As Published HAWG MIK",ylab="Recalculated MIK")
text(actual.MIK.index$HAWG.Mik,actual.MIK.index$Recalculated.MIK,sprintf("%02i",actual.MIK.index$Year%%100),pos=4)
abline(a=0,b=1)

#Deviations between as published MIK and recalculated
devs <- (actual.MIK.index$Recalculated.MIK-actual.MIK.index$HAWG.Mik)/actual.MIK.index$HAWG.Mik*100
plot(actual.MIK.index$Year,devs,type="h",xlab="Year",ylab="Change in MIK upon recalculation (%)",lwd=2)
abline(h=0,lwd=2 )

### ======================================================================================================
### Compare the two by plotting
### ======================================================================================================
par(mfrow=c(2,1),oma=c(5,4,4,2),mar=c(0,0,0,0),las=1)
plot(0,0,type="n",xlim=range(pretty(dat$year)),xaxt="n",panel.first=grid(),
      ylim=range(pretty(c(MIK.index.wt,MIK.index))),xlab="",ylab="MIK index",xpd=NA)
axis(1,labels=NA)
lines(as.numeric(names(MIK.index)),MIK.index,lwd=2,col="black")
points(as.numeric(names(MIK.index.wt)),MIK.index.wt,col="red",pch=2)
points(actual.MIK.index$Year,actual.MIK.index$Recalculated.MIK,col="blue",pch=19)
legend("topleft",legend=c("Normal Calculation","Partial wts calculation","Recalc. MIK Index"),
      pch=c(NA,2,19),col=c("black","red","blue"),lwd=c(2,NA,NA))

#Summarise the comparison
cat(sprintf("Max relative error between calculation methods         : %4.2E\n",max(abs(MIK.index.wt-MIK.index)/MIK.index.wt)))
devs <- (MIK.index-actual.MIK.index$Recalculated.MIK)/actual.MIK.index$Recalculated.MIK
cat(sprintf("Max relative error between calculated and actual Index : %9.5f%%\n",
        max(abs(devs*100),na.rm=TRUE)))

plot(actual.MIK.index$Year,devs*100,type="h",xlim=range(pretty(dat$year)),
      ylim=range(pretty(devs*100)),xlab="Year",ylab="Deviation (%)",xpd=NA,lwd=2)
abline(h=0,lwd=4)

### ======================================================================================================
### write output
### ======================================================================================================
write.csv(d.wt,file=gsub(".csv"," with weights.csv",src.file),row.names=FALSE,na="")