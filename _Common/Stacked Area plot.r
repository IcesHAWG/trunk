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

stacked.area.plot <- function(x,data,groups,...) {
        #Input arguments
        in.args <-  list(...)

        #Stack function
        stck <- function(x,y,subscripts,groups) {
                    #Build data frame for splitting with NAs to zero
                    dat <- data.frame(x,groups=groups[subscripts],y)
                    dat$y[is.na(dat$y)] <- 0
                    #Cumsums of y for a given x
                    cumsums.l <- lapply(split(dat,dat$x),function(tmp) {
                        tmp$cumsums <- cumsum(tmp$y)
                        return(tmp)})
                    dat   <- do.call(rbind,cumsums.l)
                    return(dat)
        }

        #Panel function
        pfun <- function(x,y,subscripts,groups,...) {
                    panel.grid(h=-1,v=-1)
                    #Generate cumsums
                    dat <- stck(x,y,subscripts,groups)
                    #Prepare the colour vector
                    grps   <- unique(dat$groups)
                    pfun.args <- list(...)
                    cols    <- rep(pfun.args$cols,length.out=length(grps))
                    pfun.args["col"] <- NULL
                    pfun.args["cols"] <- NULL
                    #Now plot each group!
                    for(i in 1:length(grps)) {        #For loops aren't sexy, but they allow us to move through the colours as well
                        poly.dat  <- subset(dat,dat$groups==grps[i])
                        poly.plot <- data.frame(x=c(poly.dat$x,rev(poly.dat$x)),
                                        y=c(poly.dat$cumsums,rev(poly.dat$cumsums-poly.dat$y)))
                        do.call(panel.polygon,c(list(x=poly.plot$x,y=poly.plot$y,col=cols[i]),pfun.args))
                    }
        }

        #Key default definition, from the "lattice" book, Figure 5.6
        grps     <-  subset(data,select=groups)[,1]
        n.grps   <-  length(unique(grps))
        cols     <-  if(is.null(in.args$col)) {rainbow(n.grps)} else { in.args$col}
        key.default <- list(right = list(fun = draw.colorkey,
                        args = list(key = list(col = rep(cols,length.out=n.grps),
                                                at = (1:(n.grps+1))-0.5,
                                                labels=list(labels=as.character(unique(grps)),at=1:n.grps)),
                                    draw = FALSE)))

        #Setup default arguments
        default.args <- list(x,data,groups=as.formula(paste("~",groups)),
                              panel=pfun,
                              prepanel=function(x,y,subscripts,groups,...) {
                                        dat <- stck(x,y,subscripts,groups)
                                        return(list(ylim=range(pretty(c(0,dat$cumsums)))))
                                        },
                              cols=cols,
                              scales=list(alternating=1),
                              legend=key.default)

        #Add in optional input args, and do plot
        plot.args <- default.args
        plot.args[names(in.args)] <- in.args
        do.call(xyplot,plot.args)
}


#Now, to try it out
#library(FLCore)
#library(FLEDA)
#data(ple4sex)
#data(ple4)
#print(stacked.area.plot(data~year| unit, as.data.frame(ple4sex@catch.n),groups="age",ylab="Catch in numbers"))
##print(stacked.area.plot(data~year| unit, as.data.frame(pay(ple4sex@catch.n)),ylab="Proportion of Catch at age"))
##print(stacked.area.plot(data~year|sprintf("Age %02i",age),as.data.frame(ple4sex@catch.n),groups="unit",scale=list(y=list(relation="free"))))
#ple4<-WBSS
#ssb.dat             <- ple4@stock.wt*ple4@stock.n*exp(-ple4@harvest*ple4@harvest.spwn - ple4@m*ple4@m.spwn)*ple4@mat
#ssb.cohorts         <- as.data.frame(FLCohort((ssb.dat)))
#ssb.cohorts$year    <- ssb.cohorts$cohort+ssb.cohorts$age
#ssb.cohorts         <- subset(ssb.cohorts,!is.na(ssb.cohorts$data))
#ssb.cohorts         <- rev(ssb.cohorts)
#ssb.by.cohort.plot  <- stacked.area.plot(data~year,ssb.cohorts,groups="cohort")
#print(ssb.by.cohort.plot)
