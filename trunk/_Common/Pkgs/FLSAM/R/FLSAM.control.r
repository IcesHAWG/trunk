setClass("FLSAM.control",
	representation(
    range	          ="numeric",   ## minimum and miximum age represented internally in the assessment
    plus.group      ="logical",   ## should we model the maximum age as a plus group or not?
    state.coupling  ="matrix",   ## matrix describing the coupling of the states
    param.coupling  ="matrix",   ## matrix characterising the coupling of the parameters
    survey.qs       ="matrix",   ## matrix of survey catchabilities
    f.var.coupling  ="matrix",   ## matrix of fishing mortality couplings
    logN.coupling   ="vector",   ## vector of coupling of the logN variables
    obs.var.coupling="matrix",   ## matrix coupling the observation variances
    srr             ="integer",    ## stock recruitment relationship
    catch.data.scaling="integer"),  ##N
	prototype=prototype(
    range	          =as.numeric(1),   ## minimum age represented internally in the assessment
    plus.group      =as.logical(TRUE),   ## should we model the maximum age as a plus group or not?
		state.coupling  =as.matrix(0),   ## matrix describing the coupling of the states
    param.coupling  =as.matrix(0),   ## matrix characterising the coupling of the parameters
    survey.qs       =as.matrix(0),   ## matrix of survey catchabilities
    f.var.coupling  =as.matrix(0),   ## matrix of fishing mortality couplings
    logN.coupling   =as.vector(0),   ## vector of coupling of the logN variables
    obs.var.coupling=as.matrix(0),   ## matrix coupling the observation variances
    srr             =as.integer(0),    ## stock recruitment relationship
    catch.data.scaling=as.integer(0)),  ##N
	validity=function(object){
                	if (any(object@lambda.age < 0))
                		return("weights must be > 0")
                	if (any(object@lambda.yr < 0))
                		return("value of rage must be >= -1")
                	if (object@lambda.sr < 0)
                		return("SR weight must be >= 0")
                	if (!is.na(object@sr.age))
                  	if (object@sr.age < 0)
                	   	return("sr.age must be >= 0")

                	# Everything is fine
                	return(TRUE)}
)



FLSAM.control <- function(stck,tun) {
  #Create object
  ctrl <- new("FLSAM.control")

  #Populate metadata
  ctrl@range <- stck@range[c("min","max","minfbar","maxfbar")]
  ctrl@plus.group <- stck@range["plusgroup"]==stck@range["max"]
  
  #Setup coupling structures
  default.coupling <- matrix(as.integer(0),nrow=1+length(tun),ncol=dims(stck)$age,
                        dimnames=list(c("catch",names(tun)),dimnames(stck@catch.n)$age))
  ctrl@state.coupling  <- default.coupling
  ctrl@param.coupling  <- default.coupling
  ctrl@survey.qs       <- default.coupling
  ctrl@f.var.coupling  <- default.coupling
  ctrl@obs.var.coupling<- default.coupling

  #Other variables
  ctrl@logN.coupling   <- default.coupling[1,]
  ctrl@srr <- as.integer(0)
  ctrl@catch.data.scaling <- as.integer(0)

  #Finished!
  return(ctrl)
}