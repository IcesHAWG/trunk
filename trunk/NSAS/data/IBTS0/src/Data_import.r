#/*##########################################################################*/
#' MIK Data Import
#' =====================================================================
#'
#' by Mark R Payne
#' DTU-Aqua, Charlottenlund, Denmark
#' mpa@aqua.dtu.dk
#'
#' $Rev$
#' $Date$
#'
#' Imports the MIK data directly from the source csv into
#' an R data object, which we can run further with
#
#  Copyright and license details are provided at the bottom of this script
#
#  To do:
#
#  Notes:
#   - This script contains RMarkdown. Generate HTML with the following commands.
#           library(knitr);library(markdown)
#           opts_knit$set(root.dir=getwd(),width=120)
#           opts_chunk$set(echo=FALSE,results="hide",fig.width=10,
#                          message=FALSE,error=FALSE,fig.path="plots/")
#           spin("src/Data_import.r")
#/*##########################################################################*/

# ========================================================================
# Initialise system
# ========================================================================
cat(sprintf("\n%s\n","MIK Data Import"))
cat(sprintf("Analysis performed %s\n\n",date()))

#Configure markdown style, do house cleaning
rm(list = ls(all.names=TRUE));  graphics.off()
start.time <- proc.time()[3]; options(stringsAsFactors=FALSE)
require(knitr)

#Helper functions, externals and libraries
log.msg <- function(fmt,...) {cat(sprintf(fmt,...));flush.console();return(invisible(NULL))}
library(tools)
library(sp)

#Start recording from here
opts_chunk$set(results="markup")

#/* ========================================================================*/
#  Identify data file
#/* ========================================================================*/
#Identify input data source
fname <- dir("data",pattern=".*csv$",full.names=TRUE)
if(length(fname)!=1) {
  stop("Problem with data source definition. Please ensure that the data directory ",
       "contains one and only one .csv data file")
}

# File details
log.msg("Calculating MD5 Checksum...\n")
f.details <- data.frame(filesize=file.info(fname)$size,
             "last modification time"=file.info(fname)$mtime,
             "md5 Checksum"=md5sum(fname))

#/* ========================================================================*/
#  Load data
#/* ========================================================================*/
#Reading the data is a bit tricky, as there are two potential formats - one
#that is comma separated with "." for a decimal point, and one that is a
#semi-colon seperated with a comma for a decimal point. The type that is generated
#is dependent on the Locale of the machine that is used to cover it.
#We need to identify the format, and then load the data accordingly
#Read in the first line and count number of dots / periods
hdr <- readLines(fname,n=1)
n.comma <- nchar(gsub("[^;]","",hdr))
n.dot <- nchar(gsub("[^,]","",hdr))

#Load data accordingly
log.msg("Loading data...")
if(n.dot > n.comma) {
  dat <- read.csv(fname,colClasses="character",
                  na.strings=c("NA","")) 
} else {
  dat <- read.csv2(fname,colClasses="character",
                  na.strings=c("NA","")) 
}
#Set rownames. We use the rownumbers from the csv, assuming the header to
#be row 1 and the data starting on row 2 - hopefully these
#should help find the problem quickly
rownames(dat) <- seq(nrow(dat))+1

#Strip out padding rows, defined here as all elements
#being NA
NA.mat <- sapply(dat,is.na)
n.NAs <- rowSums(NA.mat)
mt.row <- n.NAs==ncol(NA.mat)
dat <- subset(dat,!mt.row)

#Strip out padding columns similarly
mt.col <- colSums(NA.mat)==nrow(NA.mat)
dat <- dat[,!mt.col]

#Attach metadata to object
attr(dat,"source.details") <- f.details

#Save data
save(dat,file="objects/MIK_data_raw.RData")

#/* ========================================================================*/
#   Complete
#/* ========================================================================*/
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

