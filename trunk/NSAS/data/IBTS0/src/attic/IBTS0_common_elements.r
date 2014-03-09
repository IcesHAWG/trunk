##############################################################################
# IBTS0 Index Common Elements
#
# $Rev: 619 $
# $Date: 2011-11-25 14:59:55 +0100 (Fri, 25 Nov 2011) $
#
# Author: Mark Payne, mpa@aqua.dtu.dk
# DTU-Aqua, Charlottenlund, DK
# 
# Common elements used in the calculation of the IBTS0 script
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

### ==========================================================================
### Parameters
### ==========================================================================
#Directories and files
out.dir <- file.path(".","outputs")
dat.dir <- file.path(".","data")
src.file <- file.path(dat.dir,"IBTS0_database.csv")

#Config 
MIK.areas <- data.frame(MIK.area=c("nw","ne","cw","ce","sw","se","ka","ch"),
                        MIK.area.surface.area=c(27,11,28,33,12,30,10,10)*3086913600/10^9)

### ==========================================================================
### StatSquare to Lat,Long Function
### ==========================================================================
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

log.msg     <-  function(string) {
	cat(string);flush.console()
}

### ==========================================================================
### Load and prepare data
### ==========================================================================
read.IBTS0 <- function(src=src.file) {
   #Display src
   log.msg(sprintf("Loading data from %s...",src.file))   

   #load data
   d.raw <- read.csv(src)
   
   #The default database is missing a HaulID setup. Here we store 
   #the row numbers in the spreadsheet in their place
   dat <- transform(d.raw,spreadsheet.row=1:nrow(d.raw)+1)  
   
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
   
   #Misc mods
   dat$no.per.m2 <- as.numeric(dat$no.per.m2)
   dat$area     <- as.numeric(dat$area)
   dat <- merge(dat,MIK.areas,all=FALSE,by="MIK.area",sort=FALSE)
   dat$POSIX <- as.POSIXct(strptime(dat$Datetime, "%d/%m/%y %H:%M") )

   #Calculate the median length
   len.bins <- 7:60
   length.cols <- paste("X",len.bins,sep="")
   dat$median.length <- apply(dat[,length.cols],1,function(x) {
             x[is.na(x)] <- 0
             median(rep(len.bins,times=x)) 
           })
  
   log.msg("OK.\n")
   
   return(dat)
}

### ==========================================================================
### Calculate in the standard manne
### i.e. Following the SAS code of P. Munk 
### ==========================================================================
calc.MIK.index <- function(dat) { 
   #Display details
   log.msg("Calculating MIK index...")

   #Split the data into MIK areas and years
   yr.area.l <- split(dat,list(year=dat$Year,area=dat$MIK.area),drop=TRUE)
 
   #Calculate the mean number density per stat square
   dens.by.ss <- lapply(yr.area.l,function(x) {
       means <- tapply(x$no.per.m2,factor(x$Rectangle,levels=unique(x$Rectangle)),mean)
       unique.rects  <- !duplicated(x$Rectangle)
       opt   <- data.frame(x[unique.rects,c("Year","MIK.area","Rectangle","area")],
                             means,names(means))
       return(opt)
       })
   
   #Mean density per MIK area
   dens.by.MIK.area.l <- lapply(dens.by.ss,function(x) {
                data.frame(x[1,c("Year","MIK.area")],MIK.area.dens=mean(x$means))
                 })
   dens.by.MIK.area   <- do.call(rbind,dens.by.MIK.area.l)
   
   #Abundance of larvae in each MIK area
   abund.by.area   <- merge(dens.by.MIK.area,MIK.areas,all=FALSE,sort=FALSE)
   abund.by.area$area.abund <- abund.by.area$MIK.area.surface.area*abund.by.area$MIK.area.dens
   abund.by.area$MIK.area <- factor(abund.by.area$MIK.area,levels=MIK.areas$MIK.area)
   
   #Mean density across the North Sea
   MIK.table <- xtabs(MIK.area.dens ~ Year + MIK.area,abund.by.area)
   MIK.index <- tapply(abund.by.area$area.abund,abund.by.area$Year,sum)
   MIK.out   <- as.data.frame(cbind(year=as.numeric(rownames(MIK.table)),
                     MIK.table,index=MIK.index))
   log.msg("OK.\n")
   return(MIK.out)
}

