#/*##########################################################################*/
#' MIK Index calculation
#' =====================================================================
#'
#' by Mark R Payne
#' DTU-Aqua, Charlottenlund, Denmark
#' mpa@aqua.dtu.dk
#'
#  $Rev$
#  $Date$
#'
#' Calculation of the MIK Index for use in the stock assessment and analysis
#' of the various contributions of each haul
#' 
#  Copyright and license details are provided at the bottom of this script
#
#  To do:
#
#  Notes:
#   - This script contains RMarkdown. Generate HTML with the following commands.
#           library(knitr);library(markdown)
#           opts_knit$set(root.dir=getwd(),width=120)
#           opts_chunk$set(echo=FALSE,results="hide",fig.width=10,
#                          message=FALSE,error=FALSE,fig.path="mdfigures/")
#           spin("src/Index_calculation.r")
#           file.rename("Index_calculation.html","outputs/Index_calculation.html")
#           file.remove("Index_calculation.md")
#/*##########################################################################*/

# ========================================================================
# Initialise system
# ========================================================================
cat(sprintf("\n%s\n","MIK Index calculation"))
cat(sprintf("Analysis performed %s\n\n",date()))

#Configure markdown style, do house cleaning
rm(list = ls(all.names=TRUE));  graphics.off()
start.time <- proc.time()[3]; options(stringsAsFactors=FALSE)
require(knitr)

#Helper functions, externals and libraries
log.msg <- function(fmt,...) {cat(sprintf(fmt,...));flush.console();return(invisible(NULL))}
library(reshape)
library(sp)
library(mapdata);library(maptools)

#Start recording from here
opts_chunk$set(results="markup")

#/* ========================================================================*/
#   Setup and prepare data
#/* ========================================================================*/
# Load QA data
load("objects//MIK_data_QA.RData")

#Load supporting data
#The area of each ICES stat-square is taken directly from Peter's spreadsheet
MIK.areas <- data.frame(area.code=c("nw","ne","cw","ce","sw","se","ka","ch"),
                        surface.area=c(27,11,28,33,12,30,10,10)*3086913600/10^9)

#/* ========================================================================*/
#   Prepare data
#/* ========================================================================*/
#Calculate mean length
lendist.colnames <- sprintf("X%i",7:60)
lendist <- as.matrix(dat[,colnames(dat) %in% lendist.colnames])
lens <- as.numeric(gsub("^X([[:digit:]]+)$","\\1",colnames(lendist)))
dat$mean.len <- apply(lendist,1,weighted.mean,x=lens)

#Define subareas after SAS code
#First recreate the stat1, stat2 and stat3 structures
dat$stat1 <- as.numeric(substr(dat$Rectangle,1,2))
dat$stat2 <- substr(dat$Rectangle,3,3)
dat$stat3 <- as.numeric(substr(dat$Rectangle,4,4))

#Now follow the SAS code to define areas
dat$stat4 <- NA
dat$stat4[dat$stat2=="E"] <- dat$stat3[dat$stat2=="E"]+10;
dat$stat4[dat$stat2=="F"] <- dat$stat3[dat$stat2=="F"]+20;
dat$stat4[dat$stat2=="G"] <- dat$stat3[dat$stat2=="G"]+30;

#Define sections following SAS code
dat$area.code <- NA
dat$area.code[dat$stat4<22 & dat$stat1>39 & dat$stat1<46] <- "cw"
dat$area.code[dat$stat4>21 & dat$stat1>39 & dat$stat1<46] <- "ce"
dat$area.code[dat$stat4<22 & dat$stat1<40 & dat$stat1>34] <- "sw"
dat$area.code[dat$stat4>21 & dat$stat1<40 & dat$stat1>34] <- "se"
dat$area.code[dat$stat4<22 & dat$stat1>45] <- "nw"
dat$area.code[dat$stat4>21 & dat$stat1>45] <- "ne"
dat$area.code[dat$stat4>28] <- "ka"
dat$area.code[dat$stat1<35] <- "ch"

#Now calculate larval densities
dat$depth <- with(dat,ifelse(is.na(Hauldepth),Waterdepth-5,Hauldepth))
dat$volume <- with(dat,ifelse(sel=="f",volFlowmeter,volFromGeometry))
dat$no.per.m2 <- with(dat,Numberlarvae/volume*depth)

#Set inclusion/ exclusion criteria
#  - Exclude if meanlength is too short and in the downs region
#  - Include if not possible to calculate mean length
dat$exclude <- !is.na(dat$mean.len) & dat$mean.len<20 & dat$LatDec < 54    #Exclude small downs
dat.all <- dat
dat <- subset(dat.all,!dat.all$exclude)

#/* ========================================================================*/
#'## Perform calculations in the standard manner
#/* ========================================================================*/
#Calculate the mean number density per stat square per area per campaign (phew!)
ss.dens <- melt(tapply(dat$no.per.m2,FUN=mean,
                       dat[,c("Campaign","area.code","Rectangle")]))

#Mean density per MIK area
MIKa.dens <- melt(tapply(ss.dens$value,FUN=mean,
                         ss.dens[,c("Campaign","area.code")],
                         na.rm=TRUE))

#Merge in area of each MIK area
MIKa.abund   <- merge(MIKa.dens,MIK.areas,all=TRUE,sort=FALSE)
MIKa.abund$area.abund <- with(MIKa.abund, surface.area*value)

#Output table of areas
MIKa.abund$area.code <- factor(MIKa.abund$area.code,levels=MIK.areas$area.code)
area.table <- xtabs(area.abund ~ Campaign + area.code,MIKa.abund)

#Add Indices per year
MIK.table <- cbind(area.table,index=rowSums(area.table))
print(round(MIK.table,3))

#/* ========================================================================*/
#'## Contribution per haul
#/* ========================================================================*/
#Get number of hauls per ss per campaign
ss.n <- table(dat[,c("Campaign","area.code","Rectangle")])
ss.n <- subset(as.data.frame(ss.n,responseName="hauls.per.ss"),hauls.per.ss!=0)

#Get ss's per area per campaign
MIKa.n <- as.data.frame(table(ss.n[,c("Campaign","area.code")]),
                        responseName="ss.per.MIKa")

#Merge data and calculate weights
MIKa.n <- merge(MIKa.n,MIK.areas)
haul.wts <- merge(ss.n,MIKa.n)
haul.wts$wt <- with(haul.wts,1/hauls.per.ss/ss.per.MIKa*surface.area)

#Now incorporate the weights into the *full* data matrix
dat.wt <- merge(dat.all,haul.wts[,c("Campaign","area.code","Rectangle","wt")],all=TRUE)
dat.wt$index.contrib <- ifelse(dat.wt$exclude,0,dat.wt$no.per.m2*dat.wt$wt)

#And finally calculate the index
dat.wt$area.code <- factor(dat.wt$area.code,levels=MIK.areas$area.code)
MIK.wt.index <- tapply(dat.wt$index.contrib,dat.wt[,c("Campaign","area.code")],sum)
MIK.wt.table <- cbind(MIK.wt.index,index=rowSums(MIK.wt.index))
print(round(MIK.wt.table,3))

# ========================================================================
#'## Index composition
# ========================================================================
#'### Contribution by latitude
dat.wt$lat.bin <- round(dat.wt$LatDec*4,)/4
plt.dat <- melt(tapply(dat.wt$index.contrib,list(lat=dat.wt$lat.bin),sum))
plot(lat ~value, plt.dat,type="b",
     ylab="Latitude",xlab="Index contribution")

#'### Cumulative contribution to the index 
plot(cumsum(sort(dat.wt$index.contrib,decreasing=TRUE))/sum(dat.wt$index.contrib),
    xlab="Haul rank",ylab="Cumulative Contribution",
     ylim=c(0,1),yaxs="i",xaxs="i")

#'###  Spatial distribution of index contributions
coordinates(dat.wt) <- ~ LongDec + LatDec
proj4string(dat.wt) <- CRS("+proj=longlat")
NS.poly <- map("worldHires",
               xlim=range(pretty(dat.wt$LongDec)),ylim=range(pretty(dat.wt$LatDec)),
               plot=FALSE,fill=TRUE)
NS.sp <- map2SpatialPolygons(NS.poly,NS.poly$names,proj4string=CRS("+proj=longlat +datum=WGS84"))
sp.theme(set=TRUE)
spplot(dat.wt,"index.contrib",
       sp.layout=list(list("sp.polygons",NS.sp,fill="lightgrey")),
       colorkey=TRUE,scales=list(draw=TRUE),
       panel=function(...) {
          dots <- list(...)
          dots$pch <- ifelse(dat.wt$exclude,1,16)
          dots$cex <- ifelse(dat.wt$exclude,0.3,1)
          do.call(panel.pointsplot,dots)
       })
#'Small hollow circles are hauls that have been excluded from the index calculation

#/* ========================================================================*/
#   Complete
#/* ========================================================================*/
#Write output table
write.table(round(MIK.table,2),file="outputs/MIK_indices.txt",
            na="",quote=FALSE,sep="\t")

#+ echo=FALSE,results='asis'
#Close files
if(grepl("pdf|png|wmf",names(dev.cur()))) {dmp <- dev.off()}
log.msg("\nAnalysis complete in %.1fs at %s.\n",proc.time()[3]-start.time,date())

#' -----------
#' <small>*This work by Mark R Payne is licensed under a  Creative Commons
#' Attribution-NonCommercial-ShareAlike 3.0 Unported License. 
#' For details, see http://creativecommons.org/licenses/by-nc-sa/3.0/deed.en_US
#' Basically, this means that you are free to "share" and "remix" for 
#' non-commerical purposes as you see fit, so long as you "attribute" me for my
#' contribution. Derivatives can be distributed under the same or 
#' similar license.*</small>
#'
#' <small>*This work comes with ABSOLUTELY NO WARRANTY or support.*</small>
#'
#' <small>*This work is also subject to the BEER-WARE License. For details, see
#' http://en.wikipedia.org/wiki/Beerware*</small>
#' 
#' -----------
#' 
#' <small> Script version:
#'$Rev$ $Date$ </small>
#'
#' -----------

# End
