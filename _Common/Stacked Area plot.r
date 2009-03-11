######################################################################################################
# Stacked Area plot
#
# Version 1.00 11/03/2009 00:31:04
#
# Mark Payne (DTU-AQUA)
#
# A generic lattice function to make stacked area plots
#
# Developed with:
#   - R version 2.8.0
#
# Changes:
# V 1.00  - First full version

# To be done:
#
# Notes:
#
####################################################################################################

stacked.area.plot <- function(x,data,...) {
        #Input arguments
        in.args <-  list(...)

        #Panel function
        pfun <- function(x,y,z,subscripts,col,...) {
                    #Set NA's to zero
                    z[is.na(z)] <- 0
                    #Calculate the cumulative totals and convert to a matrix
                    props.l <- tapply(z[subscripts],x[subscripts],cumsum)
                    props   <- do.call(cbind,props.l)
                    #This basically mimics the structure of the quant, just with everything converted to
                    #cumulative values. Now plot it!
                    x.lbls <- as.numeric(colnames(props))
                    for(i in nrow(props):1) {        #For loops aren't sexy, but they allow us to move through the colours as well
                        panel.polygon(c(x.lbls,rev(x.lbls)),c(props[i,],rep(0,length(x.lbls))),col=rep(col,length.out=i)[i],...)
                    }
        }


        #Key default definition
        bands    <-  unique(latticeParseFormula(x,data,dim=3)$right.y)
        n.bands  <-  length(bands)
        cols     <-  if(is.null(in.args$col)) {rainbow(n.bands)} else { in.args$col}
        key.default   <- list(col=rep(cols,length.out=n.bands),
                              at=(1:(n.bands+1))-0.5,
                              labels=list(labels=as.character(bands),at=1:n.bands))

        #Setup the other defaults
        default.args <- list(x,
                            data=data,
                            col=cols,
                            prepanel=function(x,y,z,subscripts) {
                                list(ylim= range(pretty(c(0,tapply(z[subscripts],x[subscripts],sum))))) },
                            colorkey=key.default,
                            ylab="",         #The meaning of the y-axis is different from that in the formula, so its easiest just to discard the label
                            scale=list(alternating=1),
                            panel=pfun)

        #Combine input args with the defaults
        plot.args <- default.args
        plot.args[names(in.args)] <- in.args

        #Make the plot
        do.call(levelplot,plot.args)
}


#Now, to try it out
#library(FLCore)
#library(FLEDA)
#data(ple4sex)
#data(ple4)
#print(stacked.area.plot(data~year*age| unit, as.data.frame(ple4sex@catch.n),ylab="Catch in numbers"))
#print(stacked.area.plot(data~year*age| unit, as.data.frame(pay(ple4sex@catch.n)),ylab="Proportion of Catch at age"))
#print(stacked.area.plot(data~year*age,as.data.frame(pay(ple4@stock.n*ple4@stock.wt)),col=c("black","white"),ylab="Proportion by weight in the stock"))
#print(stacked.area.plot(data ~age*year| unit, as.data.frame(ple4sex@catch.n),ylab="Total Historic Catches from an age group"))
#print(stacked.area.plot(data ~ year*unit| sprintf("Age %02i",age), as.data.frame(ple4sex@catch.n),ylab="Catches from an age group by sex"))
#