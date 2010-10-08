######################################################################################################
# Exploratory use of Taylor Diagrams in Stock Assessments
#
# Author: Mark Payne, mpa@aqua.dtu.dk
# DTU-Aqua, Charlottenlund, DK
# 
# Explores the use of Taylor Diagrams in Fish Stock assessment, to see if they
# may be useful or not. Work is based on the plotrix package to start with.
#
# Reference:
# Taylor, K.E. (2001) Summarizing multiple aspects of model performance in 
#         a single diagram. Journal of Geophysical Research, 106: 7183-7192. 
#
# Developed with:
#   - R version 2.8.1
#
# Changes:
#
# To be done:
#
# Notes:
#  - Taylor diagram here is referenced against the *modelled* results, rather than
#    the observations, as is more usual. This puts things on a more natural footing
#    when comparing data sources
#  - Stock assessment model of choice must be run prior to Taylor analysis
#  - Analysis is done on log-transformed values, as this is the natural basis on
#    which we do all of our modelling
#
# ----------------------------------------------------------------------------
# "THE BEER-WARE LICENSE" (Revision 42):
# Mark Payne wrote this file. As long as you retain this notice, you can do
# whatever you want with this stuff - just don't hold me responsible for it.
# If we meet some day, and you think still this stuff is worth it, you
# can buy me a beer in return. Mark Payne (with kudos to Poul-Henning Kamp)
# ----------------------------------------------------------------------------
####################################################################################################

library(plotrix)
### ======================================================================================================
### Initialise system
### ======================================================================================================
#Rename target object to allow generalisation
ica.obj <- NSH.ica

### ======================================================================================================
### Loop through index slots to produce Taylor Diagram
### ======================================================================================================
#Setup Taylor diagram
first.plot <- TRUE
leg.str <- list()
for(i in 1:length(ica.obj@index)) {
  dat <- ica.obj@index[[i]]
  mdl <- ica.obj@index.hat[[i]]
  for(a in 1:(dim(dat)[1])) {
    symb <- substr(dimnames(dat)$age[a],1,1)
    clr <- i
    leg.str <- c(leg.str,list(data.frame(str=sprintf("%s, Age %s",names(ica.obj@index)[i],symb),pch=symb,clr=clr)))
    taylor.diagram(log(as.vector(mdl[a,]@.Data)),log(as.vector(dat[a,]@.Data)),add=!first.plot,
        normalize=TRUE,ref.sd=TRUE,show.gamma=FALSE,grad.corr.lines=FALSE,pch=symb,col=clr,main=NULL)
    first.plot <- FALSE
  }
}
leg.str <- do.call(rbind,leg.str)
legend("topright",legend=sapply(strsplit(names(ica.obj@index)," "),function(x) x[1]),pch=15,col=1:length(ica.obj@index),bty="n")


