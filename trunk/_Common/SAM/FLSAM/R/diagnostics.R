### Methods #############################################################
# diagnostics {{{
# prepare diagnostic plots from a FLSAM object
#if (!isGeneric("diagnostics")) {
#    setGeneric("diagnostics", function(x,...) standardGeneric("diagnostics"))
#}

#setMethod("diagnostics", signature(x="FLSAM",...),
#  function(x, ...)
#  {

#Load external data sources etc
library(FLCore)
source("FLSAM.class.r")
source("FLSAM.control.r")
load(file.path("..","data","NSH.RData"))
x <- NSH.sam.out

#Your plots, start here!
#Note that the FLSAM object should be called "x"
