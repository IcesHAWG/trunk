######################################################################################################
# Retrospective analysis function
#
# Version 2.99 20/01/2009 15:09:08
#
# Niels Hintzen (IMARES) and Mark Payne (DTU-AQUA)
#
# Modification of FLAssess retro() function to account for "hangover years" - the years
# where years further into the future than the assessment year are incorporated into an assessment
#
# Developed with:
#   - R version 2.8.0
#   - FLCore 1.99-107
#   - FLAssess 1.99-102
#
# Changes:
# V 2.99  - Automatic identification of hangover years
# V 2.00  - Booting version number of achieve synchronisation with other code
# V 1.00  - First full version

# To be done:
#
# Notes:
#
####################################################################################################

setMethod('retro', signature(stock='FLStock', indices='FLIndices', control='ANY',retro='numeric'),
function(stock, indices, control="missing", retro=0, year.range="missing",return.FLStocks=TRUE){
  # ---------- Checks ----------------------
    if (!inherits(stock, "FLStock"))
      stop("stock must be an 'FLStock' object!")
    if (!validObject(stock))
      stop("stock is not a valid object!")
    if (!inherits(indices, "FLIndices"))
      stop("indices must be an 'FLIndices' object!")
    if (!validObject(indices))
      stop("indices is not a valid object!")
    # Check we have at least a usable retro or years.range.
    if ((missing(year.range) || !is.numeric(year.range)) && (!is.numeric(retro) || retro < 0 || is.na(retro) || length(retro) > 1))
      stop("Either 'retro' argument must be a non-negative integer or 'year.range' must be a numeric vector.")
    # ------------ Sort out retrospective years over which to perform assessment ------------
    stck.min.yr <- stock@range["minyear"]
    stck.max.yr <- stock@range["maxyear"]
    if(missing(year.range))
      year.range <- (stck.max.yr-retro):stck.max.yr
    # Calculate hangover years - but only consider positive hangovers
    hangover.yrs <- sapply(indices,function(x) dims(x)$maxyear) - dims(stock)$maxyear
    hangover.yrs <- ifelse(hangover.yrs < 0,0,hangover.yrs)
    # Check year.range is sensible
    if(min(year.range) < stck.min.yr || max(year.range) > stck.max.yr)
      stop("Year range outside stock object range")
    # ---------- Run that retrospective -------------
    cat("Running retrospective...\n")
    Indices.temp<-indices
    res <- new("FLStocks")
    counter<-0
    assess.l  <-  list()        #List of assessment objects
    for (yr in year.range)  #yr is the year in which the assessment is being simulated
    {
      counter <- counter+1
      Stock <- trim(stock, year=stck.min.yr:yr)
      for (j in 1:length(indices))
      {
        idx.min.yr <- dims(indices[[j]])$minyear
        if (yr < idx.min.yr) stop(paste("Year requested (",yr,") is earlier than the first year (",idx.min.yr,") of the ",indices[[j]]@name," index.",sep=""))
        idx.max.yr <- min(dims(indices[[j]])$maxyear, yr+hangover.yrs[j])
        Indices.temp[[j]] <- trim(indices[[j]],year=idx.min.yr:idx.max.yr)
      }

      assess.l[[counter]]        <- assess(control, Stock, Indices.temp)  #assess does not thave the ability to pass desc option
      assess.l[[counter]]@desc   <-  paste(as.character(yr), "Retrospective")
      Stock <- Stock + assess.l[[counter]]
      Stock@name <- paste(Stock@name, " Retrospective analysis for ", yr,sep="")
      res[[as.character(yr)]] <- Stock
    }
    names(assess.l)   <-  as.character(year.range)
    res@desc   <- paste("Retrospective analysis from object", stock@desc)
    if(return.FLStocks) {return(res) } else { return(assess.l)}
  }
)   #End setMethod

