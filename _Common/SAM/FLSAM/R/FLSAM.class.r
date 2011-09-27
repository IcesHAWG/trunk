  setClass("FLSAM",
  	representation(
      nopar    = "integer",
      nlogl    = "numeric",
      maxgrad  = "numeric",
      logDetHess = "numeric",
      ssb      = "data.frame",
      fbar     = "data.frame",
      tsb      = "data.frame",
      logssb   = "data.frame",
      logfbar  = "data.frame",
      logtsb   = "data.frame",
      logscale = "data.frame",
      logFpar  = "data.frame",
      logCatch = "data.frame",
      recruitment = "data.frame",
      stock.n  = "FLQuant",
      harvest  ="FLQuant",
      residuals = "data.frame"),
  	prototype=prototype(),
  	validity=function(object){
                	# Everything is fine
                	return(TRUE)}
)

### Methods #############################################################
# merge {{{
# merge results from FLAssess into FLStock
if (!isGeneric("merge")) {
    setGeneric("merge", useAsDefault = merge)
}
setMethod("merge", signature(x="FLStock", y="FLSAM"),
  function(x, y, ...)
  {
    quant <- quant(stock.n(x))
    dnx <- dimnames(stock.n(x))
    dny <- dimnames(y@stock.n)

    # check dimensions match
    if(!all.equal(dnx[c(-2,-6)], dny[c(-2,-6)]))
      stop("Mismatch in dimensions: only year can differ between stock and assess")

    # same plusgroup
    if(x@range['plusgroup'] != x@range['plusgroup'])
      stop("Mismatch in plusgroup: x and y differ")

    # year ranges match?
    if(!all(dny[['year']] %in% dnx[['year']]))
    {
      years <- as.numeric(unique(c(dnx$year, dny$year)))
      x <- window(x, start=min(years), end=max(years))
      y <- window(y, start=min(years), end=max(years))
    }
    #x@desc <- paste(x@desc, "+ FLAssess:", y@name)  Commented out. Is this really necessary?
    x@stock.n <- y@stock.n
    x@harvest <- y@harvest
    x@range=c(unlist(dims(x)[c('min', 'max', 'plusgroup','minyear', 'maxyear')]),
      x@range[c('minfbar', 'maxfbar')])

    return(x)
  }
)   # }}}

# "+"      {{{
setMethod("+", signature(e1="FLStock", e2="FLSAM"),
	function(e1, e2) {
    if(validObject(e1) & validObject(e2))
      return(merge(e1, e2))
    else
      stop("Input objects are not valid: validObject == FALSE")
    }
)
setMethod("+", signature(e1="FLSAM", e2="FLStock"),
	function(e1, e2) {
    if(validObject(e1) & validObject(e2))
      return(merge(e2, e1))
    else
      stop("Input objects are not valid: validObject == FALSE")
    }
)   # }}}
