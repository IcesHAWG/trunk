#/*##########################################################################*/
#' IHLS Data Import
#' ==========================================================================
#'
#' by Mark R Payne
#' DTU-Aqua, Charlottenlund, Denmark
#' mpa@aqua.dtu.dk
#'
#  $Rev$
#  $Date$
#'
#' Imports IHLS data from the data directory into R for preparation in
#' quality assurance checks and modelling. The code is intended to work with
#' two different formats - either the "extended format" from the Eggs and Larvae
#' database, or the internal format used by Norbert
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
#           spin("~/Templates//R_template.r")
#   - To add a table of contents, recompile the .md again
#           options("markdown.HTML.options"=c(markdownHTMLOptions(TRUE),"toc"))
#           markdownToHTML("R_template.md","R_template.html")
#/*##########################################################################*/

# ========================================================================
# Initialise system
# ========================================================================
cat(sprintf("\n%s\n","Import IHLS Data"))
cat(sprintf("Analysis performed %s\n\n",date()))

#Configure markdown style, do house cleaning
rm(list = ls(all.names=TRUE));  graphics.off();
start.time <- proc.time()[3]; options(stringsAsFactors=FALSE)

#Helper functions, externals and libraries
log.msg <- function(fmt,...) {cat(sprintf(fmt,...));
                              flush.console();return(invisible(NULL))}
library(knitr)
library(tools)

#Start recording output from here
opts_chunk$set(results="markup")

# ========================================================================
### Get input filenames
# ========================================================================
#Identify input data source
fname <- dir("data/EggsandLarvae/",pattern=".*csv$",full.names=TRUE)
if(length(fname)!=1) {
  stop("Problem with data source definition. Please ensure that the data directory ",
       "contains one and only one .csv data file")
}

# File details
log.msg("Calculating MD5 Checksum...\n")
f.details <- data.frame(filesize=file.info(fname)$size,
                        "last modification time"=file.info(fname)$mtime,
                        "md5 Checksum"=md5sum(fname))

# ========================================================================
### Load data
# ========================================================================
#Load data accordingly
log.msg("Loading data...")
dat <- read.csv(fname,na.strings=c("NA","")) 

#Strip out the haul meta data
haul.meta <- dat[!duplicated(dat$Haul.Num.),]
haul.meta <- haul.meta[,-which(colnames(haul.meta) %in% c("Length"))]

#Strip out the length distribution
len.tbl <- dat[,c("Haul.Num.","Length","Num..counted")]

# ========================================================================
### Parse data further
# ========================================================================
#Parse date-time
haul.meta$POSIX <- as.POSIXct(strptime(haul.meta$DateTime,
                                       "%d/%m/%Y %H:%M:%s",tz="GMT"))
haul.meta$Year <- as.numeric(format(haul.meta$POSIX,"%Y"))

# ========================================================================
# Outputs
# ========================================================================
#Attach metadata to object
attr(haul.meta,"source.details") <- f.details

#Save data
save(haul.meta,len.tbl,file="objects/IHLS_EaL.RData")

#+ results='asis'
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
#
# Fin
