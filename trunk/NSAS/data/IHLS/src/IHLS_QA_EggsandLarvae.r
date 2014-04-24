#/*##########################################################################*/
#' IHLS Quality Assurance Report
#' ==========================================================================
#'
#' by Mark R Payne
#' DTU-Aqua, Charlottenlund, Denmark
#' mpa@aqua.dtu.dk
#'
#  $Rev$
#  $Date$
#'
#' Performs quality assurance checks on the IHLS data contained in 
#' the ICES Eggs and Larvae Database
#
#  This work is subject to a Creative Commons "Attribution" "ShareALike" License.
#  You are largely free to do what you like with it, so long as you "attribute" 
#  me for my contribution. See the fine print at the end for exact details.
#
#  To do:
#
#  Notes:
#   - This script contains RMarkdown. Generate HTML with the following commands.
#           this.script <- "src/IHLS_QA_EggsandLarvae.r"
#           library(knitr);library(markdown)
#           opts_knit$set(root.dir=getwd(),width=120,unnamed.chunk.label="unnamed")
#           opts_chunk$set(echo=FALSE,results="hide",fig.width=10,
#                          message=FALSE,error=FALSE,fig.path="plots/")
#           this.script.HTML <- spin(this.script)
#           options("markdown.HTML.options"=c(markdownHTMLOptions(TRUE),"toc"))
#           markdownToHTML(gsub("html$","md",this.script.HTML),
#                sprintf("outputs/%s_QA.html",basename(rownames(f.details))))
#           file.remove(this.script.HTML,gsub("html$","md",this.script.HTML))
#/*##########################################################################*/

# ========================================================================
# Initialise system
# ========================================================================
cat(sprintf("\n%s\n","IHLS Quality Assurance Checks"))

#Configure markdown style, do house cleaning
rm(list = ls(all.names=TRUE));  graphics.off();
start.time <- proc.time()[3]; options(stringsAsFactors=FALSE)

#Helper functions, externals and libraries
log.msg <- function(fmt,...) {cat(sprintf(fmt,...));
                              flush.console();return(invisible(NULL))}
library(knitr)
library(reshape)
library(sp)
library(maps); library(mapdata);library(maptools)
library(lattice)

#Start recording output from here
opts_chunk$set(results="markup")
#+ results="asis"
cat(sprintf("Analysis performed %s\n\n",date()))

# ========================================================================
#  Helper functions
# ========================================================================
#First, setup a display function
disp.err <- function(test,colnames=NULL,from=haul.meta,n.max=250) {
  idxs <- which(test)
  if(length(idxs) ==0) {
    cat("No errors detected\n") 
  } else if(length(idxs)>n.max) {
    cat(sprintf("Errors detected in %i rows. \n",length(idxs)))
    d <- subset(from,test)
    print(table(Year=d$Year))
  } else {
    print(from[idxs,c("Haul.Num.","DateTime","Country",colnames)],
          row.names=TRUE)
    cat(sprintf("%i rows in total\n",length(idxs)))
  }
}

disp.range <- function(x,n=10) {
  rbind(smallest=sort(x)[1:n],
        largest=sort(x,decreasing=TRUE)[1:n])
}

disp.table <- function(...){
  tbl <- table(...)
  print(tbl,zero.print=".")
  return(invisible(tbl))
}

#/* ========================================================================*/
#'# Introduction
#'  This work documents the results of quality assurance checks on the the
#'  IHLS contents of the Eggs and Larvae database. The details of the analysed
#'  file are as follows:
#/* ========================================================================*/
#Load data file
load("objects//IHLS_EaL.RData")

#Some preparations
lat.layout <- c(3,3)

# File details
f.details <- attr(haul.meta,"source.details")
print(t(f.details))

#'<small>The md5 checksum is used as an indicator of the contents of the file. 
#'Files that have identical contents will have identical md5 checksums, 
#'irrespective of file name, modification date etc. Files are different even
#'by a single bit will have different checksums. </small>
#'
#' ### Haul Metadata table size
#' Number of rows, number of columns
dim(haul.meta)

#' ### Data fields available
colnames(haul.meta)

# ========================================================================
#'## Data Summaries
#' The following tables explore the contributions to the database
#' grouped by various key values - the numbers in the matrices represent
#' the number of samples in the database. These tables can be used as
#' a quick overview and to check the quality of data entry, particularly for
#' the fields concerned. 
# ========================================================================
#'### Data by Sampling Unit
#'This table can be used to check quickly for years that are clearly wrong or
#'mislabelled e.g. years in which the survey did not take place
tbl <- disp.table(Sampling.Period=haul.meta$Sampling.Period)
barplot(tbl,space=0,las=3,ylab="Number of samples")

#'### Data by Year
#'This table can be used to check quickly for years that are clearly wrong or
#'mislabelled e.g. years in which a survey did not take place. Note also that
#' in the following tables, Year is the year in which the haul was conducted, 
#' and not the "Campaign" year - this may be particularly relevant for the Downs
#' surveys 
tbl <- disp.table(Year=haul.meta$Year)
barplot(tbl,space=0,las=3,ylab="Number of samples")

#'### Data by Sampling Unit and Year
tbl <- disp.table(Year=haul.meta$Year,Sampling.Period=haul.meta$Sampling.Period)
tbl <- subset(melt(tbl), value!=0)
levelplot(value ~ as.numeric(Year) * Sampling.Period,data=tbl,
          xlab="Year",ylab="Sampling Unit",
          col.regions=bpy.colors)

#'### Data by Country 
tbl <- disp.table(Country=haul.meta$Country)
barplot(tbl,space=0,las=3,ylab="Number of samples")

#'### Data by Country and Year
tbl <- disp.table(Year=haul.meta$Year,Country=haul.meta$Country)
tbl <- subset(melt(tbl), value!=0)
levelplot(value ~ as.numeric(Year) * Country,data=tbl,
          xlab="Year",ylab="Country",
          col.regions=bpy.colors)

#'### Data by Gear
tbl <- disp.table(Gear=haul.meta$Gear.Type)
barplot(tbl,space=0,las=3,ylab="Number of samples")

# ========================================================================
#'## Spatial integrity of the data
#' The following tests check for errors in the spatial coordinates. 
# ========================================================================
#' ### Missing spatial coordinates
sp.missing <-  is.na(haul.meta$Longitude) | is.na(haul.meta$Latitude)
disp.err(sp.missing,c("Longitude","Longitude"))

#Create spatial object
dat.sp <- subset(haul.meta,!sp.missing)
coordinates(dat.sp) <- ~ Longitude + Latitude
proj4string(dat.sp) <- CRS("+proj=longlat")

#'### Points on land

#Extract coastlines
map.dat <- map("worldHires",
               xlim=bbox(dat.sp)[1,],
               ylim=bbox(dat.sp)[2,],
               plot=FALSE,fill=TRUE,res=0)
map.sp <- map2SpatialPolygons(map.dat,map.dat$names)
proj4string(map.sp) <- proj4string(dat.sp)

#Test for points on land
onland <- !is.na(over(dat.sp,map.sp))

#If any points found on land, print them out them
if(any(onland)) {
  disp.err(onland,c("Sampling.Period"),from=dat.sp)
} else {
  cat("No points found on land\n")
}

#'### Plot spatial distribution
#'Check here that all of the points appear within the expected domain. Points
#'identified as being on land above are plotted in red - all other wet points
#'are plotted in blue.
plot(dat.sp,pch=16,cex=0.5,col="blue")
map("worldHires",add=TRUE,col="grey",fill=TRUE)
box()
plot(dat.sp,add=TRUE,pch=16,col=ifelse(onland,"red",NA))

#'### Plot spatial distribution by Sampling Unit
#'There should also be consistency between the spatial domain and the sampling 
#'unit (as the sampling unit is defined in terms of space anyway)
#Get map polygon
map.poly <- map("worldHires",xlim=bbox(dat.sp)[1,],ylim=bbox(dat.sp)[2,],
                plot=FALSE,fill=TRUE)
map.sp <- map2SpatialPolygons(map.poly,map.poly$names)
xyplot(Latitude ~ Longitude | Sampling.Period,
       data=as.data.frame(dat.sp),
       scales=list(relation="free",draw=FALSE),
       pch=16,col="red",as.table=TRUE,cex=0.5,
       xlab="",ylab="",
       asp=mapasp(dat.sp),
       layout=c(2,2),
       panel=function(...){
         sp.polygons(map.sp,fill="black")
         panel.xyplot(...)
       })

# ========================================================================
#'## Temporal Data
#' These algorithms parse and look for specific errors in the temporal data. 
# ========================================================================
#'#### Rows where it has not been possible to fully construct a date-time
disp.err(is.na(haul.meta$POSIX))

#'#### Check sample date by country
#'An easy way to check for date-time typos stems from the fact that that the data is 
#'collected in bursts by vessels in predefined sampling periods, and should
#'therefore occur in clusters. 
#'Isolated data points therefore suggest the presence of a typo. Note that 
#'this analysis only includes those samples where it has been possible to 
#'parse the date-time fields.
haul.meta$doy <- as.POSIXlt(haul.meta$POSIX)$yday+1
xyplot(as.numeric(Year)~ doy| Sampling.Period ,data=haul.meta,
       as.table=TRUE,
       groups=Country,auto.key=list(space="right"),
       xlab="Day of Year",ylab="Year",
       scales=list(x=list(relation="free")),
       layout=lat.layout)

#/* ========================================================================*/
#'## Sampling details
#' Details of the sampling process.
#/* ========================================================================*/
#'#### Both Sample depth and Water depth are missing or did not parse
disp.err(is.na(haul.meta$Haul.Depth) & is.na(haul.meta$Water.Depth),
         c("Haul.Depth","Water.Depth"))
 
#'#### Insufficient Information Present to Calculate filtered volume
#haul.dat$VolFilt <- with(haul.dat,Flow/Cal*Eff*(Aper/1000/2)^2*pi)
 
# #'#### Distribution of Flowmeter values
# bwplot(VolFilt ~ Nation|Typ ,data=dat,
#        scales=list(relation="free"),
#        xlab="Sampling Unit",ylab="Volume filtered from flowmeter (m³)",
#        as.table=TRUE)
# 
#'#### Haul duration is missing
disp.err(is.na(haul.meta$Haul.Duration))

#'#### Distribution of Haul Speeds through the water by Country
#'Haul speed is not reported directly in the database. However, it can
#'be inferred from the combination of Volume Filtered, gear area,
#'and haul duration. Target speed is 5 kts. Deviations from this variable
#'can be indicative of typos or misperforming flowmeters
# dat$haul.speed <- with(dat,VolFilt/(pi*(Aper/2000)^2)/haul.duration*1.94384449 )
# xyplot(haul.speed ~ seq(nrow(dat)),data=dat,groups=Nation,
#        scales=list(x=list(relation="free")),
#        xlab="Row number",ylab="Haul Speed (kts)",
#        auto.key=list(space="right"),
#        as.table=TRUE,
#        layout=lat.layout,
#        panel=function(...) {
#          panel.abline(h=5,col="grey")
#          panel.superpose(...)
#        })
# 
# #'Haul speeds more than 2 kt from the target are suspicious.
# disp.err(abs(dat$haul.speed-5)>2,
#          c("latDec","lonDec","Dura","haul.speed","POSIX"),from=dat)
# 
# #/* ========================================================================*/
# #'## Larval Details
# #' Details of the larval distribution data
# #/* ========================================================================*/
# #'###Total Caught does not tally with Total Measured
# disp.err(dat$TotCaug!=dat$TotMeas,
#          c("TotCaug","TotMeas"))
# 
# #'### Total Measured does not tally with length distribution
# len.cols <- grep("X[.]*[[:digit:]]+.*mm$",colnames(dat),value=TRUE)
# dat$len.sums <- rowSums(dat[,len.cols])
# dat$diff <- dat$TotCaug-dat$len.sums
# xyplot(diff ~ seq(nrow(dat)),data=dat,groups=Nation,
#        xlab="Row number",ylab="Len distr discrepancy",
#        auto.key=list(space="right"))
# disp.err(dat$diff!=0,
#          c("TotCaug","TotMeas","len.sums","diff"),from=dat)
# 
# #'### Number per square metre does not tally with rest
# dat$No.per.m2 <- dat$TotCaug/dat$VolFilt*dat$Sam
# xyplot(No.per.m2/TotPerm2 ~ seq(nrow(dat)),data=dat,
#        group=Nation,auto.key=list(space="right"))
# 
# ========================================================================
# Complete
# ========================================================================
#+ results='asis'
#save(dat,file="objects//IHLS_data_QA.RData")

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
#' <small>*This work should also be considered as BEER-WARE. For details, see
#' http://en.wikipedia.org/wiki/Beerware*</small>
#' 
#' -----------
#' 
#' <small> Script version:
#' $Rev$ $Date$ </small>
#'
#' -----------
#
# Fin
