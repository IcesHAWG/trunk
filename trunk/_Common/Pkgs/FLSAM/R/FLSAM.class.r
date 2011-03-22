  setClass("FLSAM",
    contains="FLAssess",
  	representation(
      nopar    = "integer",
      nlogl    = "numeric",
      maxgrad  = "numeric",
      res      = "data.frame"),
  	prototype=prototype(
      nopar    = as.integer(NA),
      nlogl    = as.numeric(NA),
      maxgrad  = as.numeric(NA),
      res      = as.data.frame(NA)),
  	validity=function(object){
                	# Everything is fine
                	return(TRUE)}
)
