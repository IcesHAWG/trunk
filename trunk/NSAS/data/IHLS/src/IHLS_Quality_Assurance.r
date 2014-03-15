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
#' Performs quality assurance checks on the IHLS Database
#
#  This work is subject to a Creative Commons "Attribution" "ShareALike" License.
#  You are largely free to do what you like with it, so long as you "attribute" 
#  me for my contribution. See the fine print at the end for exact details.
#
#  To do:
#
#  Notes:
#   - This script contains RMarkdown. Generate HTML with the following commands.
#           library(knitr);library(markdown)
#           opts_knit$set(root.dir=getwd(),width=120,unnamed.chunk.label="unnamed")
#           opts_chunk$set(echo=FALSE,results="hide",fig.width=10,
#                          message=FALSE,error=FALSE,fig.path="plots/")
#           spin("src//IHLS_Quality_Assurance.r")
#           options("markdown.HTML.options"=c(markdownHTMLOptions(TRUE)))#,"toc"))
#           markdownToHTML("IHLS_Quality_Assurance.md",
#                     sprintf("outputs/%s_QA.html",basename(rownames(f.details))))
#           file.remove("IHLS_Quality_Assurance.md","IHLS_Quality_Assurance.html")
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
disp.err <- function(test,colnames=NULL,from=dat.raw,n.max=250) {
  idxs <- which(test)
  if(length(idxs) ==0) {
    cat("No errors detected\n") 
  } else if(length(idxs)>n.max) {
    cat(sprintf("Errors detected in %i rows. \n",length(idxs)))
    d <- subset(from,test)
    print(table(Nation=d$Nation))
  } else {
    #Create a DateTime field for convenient formatting
    print(from[idxs,c("Nation",colnames)],
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
#'  IHLS database. The details of the analysed file are as follows:
#/* ========================================================================*/
#Load data file
load("objects//IHLS_data_raw.RData")

#Some preparations
lat.layout <- c(1,1)

# File details
f.details <- attr(dat,"source.details")
print(t(f.details))

#'<small>The md5 checksum is used as an indicator of the contents of the file. 
#'Files that have identical contents will have identical md5 checksums, 
#'irrespective of file name, modification date etc. Files are different even
#'by a single bit will have different checksums. </small>
#'
#' ### Data table size
#' Number of rows, number of columns
dim(dat)

#' ### Data fields available
colnames(dat)

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
tbl <- disp.table(Survey.Time=dat$Survey.Time)
barplot(tbl,space=0,las=3,ylab="Number of samples")

# ========================================================================
# ## Data Parsing and Missing Values
#  The first check is of the ability of R to "parse" the data - parsing here
#  means that the data is converted from its native storage format (e.g. as
#  characters, integers, real numbers) to an internal representation in R.
#  R requires that all elements in a "column" have the same data type - Excel
#  does not and allows characters and numbers to mixed together in the same
#  column. A failure to parse properly therefore indicates a value 
#  that is not in agreement with the expected format. we also distinguish 
#  between values that are already missing (`missing.in.src`) and those that
#  failed to parse (`parse.errors`).
# ========================================================================
#First, expect that we have all the column names that we expect to retain
#as characters
allowed.char.cols <- c("Nation","Typ","E.W","Date","UTC","Area","Dez..E.W","Periode",
                       "Survey.Time","ICES.Code","ICES.Code.1")
if(any(!allowed.char.cols %in% colnames(dat))) {
  miss.cols.idxs <- which(!allowed.char.cols %in% colnames(dat))
  miss.cols <- allowed.char.cols[miss.cols.idxs]
  dat[,miss.cols] <- NA
  warning(sprintf("Expected character columns are missing: %s",
                  paste(miss.cols,collapse=",")))
}

#Now convert the other columns to numerics
other.cols <- colnames(dat)[!colnames(dat) %in% allowed.char.cols]
parsed.cols <- lapply(dat[other.cols],function(x) {
  x.clean <- gsub(",",".",x)
  return( suppressWarnings(as.numeric(x.clean)))})

#Estimate the parsing failures vs the missing values
dat.missing <- melt(colSums(sapply(dat,is.na)))
parsed.failures <- melt(colSums(sapply(parsed.cols,is.na)))
parsing.sum <- merge(dat.missing,parsed.failures,
                     by="row.names",sort=FALSE,all=TRUE)
colnames(parsing.sum) <- c("col.name","missing.in.src",
                           "missing.after.parsing")
parsing.sum$parse.errors <- parsing.sum$missing.after.parsing - 
  parsing.sum$missing.in.src
rownames(parsing.sum) <- parsing.sum$col.name
parsing.sum <- parsing.sum[,c(-1,-3)][colnames(dat),]

#Print
#print(parsing.sum)

#Add parsed values back into the data
dat.raw <- dat
dat[names(parsed.cols)] <- parsed.cols

# ========================================================================
#'## Spatial integrity of the data
#' The following tests check for errors in the spatial coordinates (`N.surf`, 
#' `E.W.surf`). 
# ========================================================================
#' ### Missing spatial coordinates
sp.missing <-  is.na(dat$Longitude) | is.na(dat$Latitude)
disp.err(sp.missing,c("Longitude","Longitude"))

#Parse long-lat
dat$latDec <- as.numeric(substr(dat.raw$Latitude,1,2))+
                  as.numeric(substr(dat.raw$Latitude,3,4))/60
dat$lonDec <- (as.numeric(substr(dat.raw$Longitude,1,2))+
              as.numeric(substr(dat.raw$Longitude,3,4))/60) * 
              ifelse(dat.raw$E.W=="E",1,-1)

#Create spatial object
dat.sp <- subset(dat,!sp.missing)
coordinates(dat.sp) <- ~ lonDec + latDec
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
  onland.pts <- as.data.frame(subset(dat.sp,onland))
  disp.err(rownames(dat) %in% rownames(onland.pts),c("LongDec","LatDec"))
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

# ========================================================================
#'## Temporal Data
#' These algorithms parse and look for specific errors in the temporal data. 
# ========================================================================
#'#### Rows where it has not been possible to fully construct a date-time
dat$POSIX <- as.POSIXct(strptime(sprintf("%s %s",dat$Date,dat$UTC),
                                 "%d-%m-%y %H%M",tz="GMT"))
disp.err(is.na(dat$POSIX),c("Date","UTC"))

#'#### Check sample date by country
#'An easy way to check for date-time typos stems from the fact that that the data is 
#'collected in bursts by vessels, and should therefore occur in clusters. 
#'Isolated data points therefore suggest the presence of a typo. Note that 
#'this analysis only includes those samples where it has been possible to 
#'parse the date-time fields.
xyplot(factor(Nation)~ POSIX ,data=dat,
       as.table=TRUE,groups=Nation,
       xlab="Day of Year",ylab="Nation",
       scales=list(x=list(relation="free")),
       layout=lat.layout)

#/* ========================================================================*/
#'## Sampling details
#' Details of the sampling process.
#/* ========================================================================*/
#'#### Both Sample depth and Water depth are missing or did not parse
disp.err(is.na(dat$Sam) & is.na(dat$Bot),c("Sam","Bot"))

#Calculate filtered volume
dat$VolFilt <- dat$Flow/dat$Cal*dat$Eff*(dat$Aper/1000/2)^2*pi

#'#### Distribution of Flowmeter values
bwplot(VolFilt ~ Nation|Typ ,data=dat,
       scales=list(relation="free"),
       xlab="Sampling Unit",ylab="Volume filtered from flowmeter (m³)",
       as.table=TRUE)

#'#### Haul duration is missing
disp.err(is.na(dat$Dura))

#'#### Distribution of Haul durations 
if(any(nchar(dat.raw$Dura)!=4)) stop("Duration in unexpected format")
dat$haul.duration <- 60*as.numeric(substr(dat.raw$Dura,1,2)) +
                        as.numeric(substr(dat.raw$Dura,3,4))
bwplot(haul.duration ~ Nation ,data=dat,
       scales=list("free"),
       xlab="Sampling Unit",ylab="Haul duration (s)",
       as.table=TRUE,
       layout=lat.layout)

#'#### Distribution of Haul Speeds through the water by Country
#'Haul speed is not reported directly in the database. However, it can
#'be inferred from the combination of Volume Filtered, gear area,
#'and haul duration. Target speed is 5 kts. Deviations from this variable
#'can be indicative of typos or misperforming flowmeters
dat$haul.speed <- with(dat,VolFilt/(pi*(Aper/2000)^2)/haul.duration*1.94384449 )
xyplot(haul.speed ~ seq(nrow(dat)),data=dat,groups=Nation,
       scales=list(x=list(relation="free")),
       xlab="Row number",ylab="Haul Speed (kts)",
       auto.key=list(space="right"),
       as.table=TRUE,
       layout=lat.layout,
       panel=function(...) {
         panel.abline(h=5,col="grey")
         panel.superpose(...)
       })

#'Haul speeds more than 2 kt from the target are suspicious.
disp.err(abs(dat$haul.speed-5)>2,
         c("latDec","lonDec","Dura","haul.speed","POSIX"),from=dat)

#/* ========================================================================*/
#'## Larval Details
#' Details of the larval distribution data
#/* ========================================================================*/
#'###Total Caught does not tally with Total Measured
disp.err(dat$TotCaug!=dat$TotMeas,
         c("TotCaug","TotMeas"))

#'### Total Measured does not tally with length distribution
len.cols <- grep("X[.]*[[:digit:]]+.*mm$",colnames(dat),value=TRUE)
dat$len.sums <- rowSums(dat[,len.cols])
dat$diff <- dat$TotCaug-dat$len.sums
xyplot(diff ~ seq(nrow(dat)),data=dat,groups=Nation,
       xlab="Row number",ylab="Len distr discrepancy",
       auto.key=list(space="right"))
disp.err(dat$diff!=0,
         c("TotCaug","TotMeas","len.sums","diff"),from=dat)

#'### Number per square metre does not tally with rest
dat$No.per.m2 <- dat$TotCaug/dat$VolFilt*dat$Sam
xyplot(No.per.m2/TotPerm2 ~ seq(nrow(dat)),data=dat,
       group=Nation,auto.key=list(space="right"))

# ========================================================================
# Complete
# ========================================================================
#+ results='asis'
save(dat,file="objects//IHLS_data_QA.RData")

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
