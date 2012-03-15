######################################################################################################
# HAWG Herring Generic Stock Assessment Script
#
# $Rev: 727 $
# $Date: 2012-03-14 17:20:52 +0100 (wo, 14 mrt 2012) $
#
# Generic Stock Assessment Script for use with any FLR assessment model,
# producing the standard set of outputs employed by the HAWG working group.
#
#
# Notes:
#
####################################################################################################

### ======================================================================================================
### Display version numbering, etc
### ======================================================================================================
cat(gsub("$","","HAWG GENERIC STOCK ASSESSMENT MODULE\n$Revision: 727 $\n$Date: 2012-03-14 17:20:52 +0100 (wo, 14 mrt 2012) $\n\n",fixed=TRUE))
flush.console()

### ======================================================================================================
### Source other external code (seperate for readiblity)
### ======================================================================================================

source(file.path("..","_Common","writeStandardOutput.r"))
an <- function(x){ return(as.numeric(x))}

### ======================================================================================================
### Data exploration plots
### ======================================================================================================

#Ratio of mature and immature biomass              
mat.immat.ratio <- function(stk,...){
                    default.args <- list(x=data~year, data=as.data.frame(bmass(stk)),
                                      groups=expression(qname),
                                      type="l",
                                      main="Mature - Immature biomass ratio",
                                      key=simpleKey(text=c("Mature", "Immature"), points=F, lines=T),
                                      ylab="Relative biomass")
                    arg.ins      <- list(...)
                    call.args    <- default.args
                    call.args[names(arg.ins)]  <- arg.ins
                    do.call(xyplot,call.args)
                  }


# Plot time series of any slot in an FLR object (with class FLQuant) (added 18-03-2010 by NTH)
timeseries <- function(stck.,slot.){
                assign("stck.",stck.,envir=.GlobalEnv);assign("slot.",slot.,envir=.GlobalEnv);
                print(xyplot(data~year,data=slot(stck.,slot.),
                groups=age,
                auto.key=list(space="right",points=FALSE,lines=TRUE,type="b"),
                type="b",
                xlab="Year",ylab=paste("Time series of",slot.,ifelse(units(slot(stck.,slot.))=="NA","",paste("(",units(slot(stck.,slot.)),")",sep=""))),
                main=paste(stck.@name,"timeseries of",slot.),
                par.settings=list(superpose.symbol=list(pch=as.character(0:8),cex=1.25))))}

#Anomaly plots - primarily oriented towards weight at age anomalies, but should work for other things too
anom.plot <- function(x,...) {
              #Calculate anomalies
              means <- rowMeans(x)
              sds   <- apply(x,1,sd)
              anoms <- (x-means)/drop(sds@.Data)
              weight.anoms <- as.data.frame(t(drop(anoms@.Data)))
              #Generate plot
              yrs <- as.numeric(colnames(anoms))
              matplot(yrs,weight.anoms,pch=rownames(x),...)
              grid()
              smoother <- loess(unlist(weight.anoms) ~ rep(yrs,ncol(weight.anoms)),span=0.2)
              predict.yrs <- seq(min(yrs),max(yrs),length.out=100)
              smoothed <- predict(smoother,predict.yrs,se=TRUE)
              polygon(c(predict.yrs,rev(predict.yrs)),
                  c(smoothed$fit+smoothed$se.fit*1.96,rev(smoothed$fit-smoothed$se.fit*1.96)),
                  col="lightgrey")
              matpoints(yrs,weight.anoms,type="p",pch=rownames(x))
              lines(predict.yrs,smoothed$fit,lwd=2,col="black")
}

#Growth anomaly plots
growth.anom.plot <- function(y,...) {
              #Calculate growth
              flc <- FLCohort(y)
              growth <- apply(flc,2,diff)
              #Coerce back to an FLQuant
              x <- flc[1:(dims(flc)$age-1),]
              x@.Data[1:dims(x)$age,1:dims(x)$cohort,1,1,1,1] <-growth
              flq   <- flc2flq(x)
              flq <- flq[,2:(dims(flq)$year-1)]    #Drop the first and last year, where we don't have any information
              #Plot anomaly
              anom.plot(flq,...)
}

#Overlay time series by cohort and align by cohort
overlayTimeseries <- function(x,nyrs,ages){
                      require(doBy)
                      validObject(x)
                      if(class(x)!="FLQuants") stop("Object is not an FLQuants")
                      if(any(is.na(names(x))==T)) stop("Each FLQuant must have a name")
                      require(reshape)
                      lng   <- length(x)
                      dmns  <- list()
                      for(i in 1:lng)
                        dmns[[i]] <- dimnames(x[[i]])

                      ags   <- unique(unlist(lapply(dmns,function(x){return(x$age)})))
                      if("all" %in% ags){ x <- x[-which(ags == "all")]; dmns <- dmns[-which(ags == "all")]}
                      lng   <- length(x);
                      idx   <- lapply(dmns,function(x){any(x$age %in% ages)})
                      if(length(idx)>0){
                        x   <- x[unlist(idx)]; dmns <- dmns[unlist(idx)]}
                      lng   <- length(x)
                      yrs   <- range(unlist(lapply(x,function(y){dims(y)[c("minyear","maxyear")]})))

                      stk   <- data.frame()
                      for(i in 1:lng)
                        stk   <- rbind(stk,cbind(as.data.frame(rescaler(trim(window(x[[i]],start=(max(an(yrs))-(nyrs-1)),end=max(an(yrs))),age=c(max(ages[1],dims(x[[i]])$min):min(rev(ages)[1],dims(x[[i]])$max))))),qname=names(x)[i]))
                      stk$track <- stk$year - stk$age

                      stk <- orderBy(~age+qname+track,data=stk)
                      xyplot(data ~ track,data=stk,groups=qname,type="l",
                             prepanel=function(...) {list(ylim=range(pretty(c(0,list(...)$y))))},xlab="Cohort",ylab="Standardized residuals",
                             auto.key=list(space="right",points=FALSE,lines=TRUE,type="l"),
                             panel = panel.superpose,
                             panel.groups = function(...) {
                              res <- list(...)
                              lng <- length(res$x)/nyrs
                              for(i in 1:lng){
                                panel.grid(v=-1,h=-1,lty=3)
                                panel.xyplot(res$x[(nyrs*i-nyrs+1):(nyrs*i)],res$y[(nyrs*i-nyrs+1):(nyrs*i)],lty=i,type="l",col=res$col.line)
                                panel.text(res$x[(nyrs*i-nyrs+1):(nyrs*i)],res$y[(nyrs*i-nyrs+1):(nyrs*i)],labels=stk$age[res$subscript[(nyrs*i-nyrs+1):(nyrs*i)]],col=res$col.line,cex=0.8)
                              }
                            },
                            scales=list(alternating=1,y=list(relation="free",rot=0)))

}
#Plot all survey timeseries by age
surveyTimeseries <- function(x){
  if(class(x) != "FLIndices") stop("Object must be of class 'FLIndices'")
  validObject(x)

  lst <- lapply(x,index)
  names(lst) <- names(x)

  inds <- mcf(lapply(NSH.tun,index))
  # scale
  indsN01 <- lapply(inds, function(x){
                  arr <- apply(x@.Data, c(1,3,4,5,6), scale)
                  arr <- aperm(arr, c(2,1,3,4,5,6))
                  # small trick to fix an apply "feature"
                  dimnames(arr) <- dimnames(x)
                  x <- FLQuant(arr)
                  #x <- x[ac(2:10)]
                })

  indsN01 <- FLQuants(indsN01)
  # fine tune
  pfun <- function(x,y,...){
            panel.grid(v=-1,h=-1,col="grey",lty=3)
            panel.xyplot(x,y, ...)
          }
  assign("pfun", pfun, envir = .GlobalEnv)

  # plot
  print(xyplot(data~year|factor(age), data=indsN01, type="l",
  xlab="", ylab="", auto.key=list(space="right",columns=1,type="l",lines=T,points=F),panel=pfun,
  groups=qname,as.table=TRUE))
}

#Time series scaled between 0 and 1 and stacked
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

#Set penality function so that we don't get any scientific notation
options("warn.FPU"=FALSE)