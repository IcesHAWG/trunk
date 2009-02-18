######################################################################################################
# A custom version of quantMeans, until it is  fixed in the SVN
#
####################################################################################################

setMethod('quantMeans', signature(x='FLQuant'), function(x, na.rm=TRUE) {
	res <- colMeans(x,na.rm=na.rm)             #This is the change - added na.rm=na.rm
	dim(res) <- c(1, dim(res))
	return(FLQuant(res, dimnames= c(list(quant='all'),dimnames(x)[2:6]), quant=quant(x),
    units=units(x)))
})