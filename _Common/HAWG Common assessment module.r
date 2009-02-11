######################################################################################################
# HAWG Herring Generic Stock Assessment Script
#
# $Rev$
# $Date$
#
# Author: Mark Payne
# DIFRES, Charlottenlund, DK
#
# Generic Stock Assessment Script for use with the FLICA method, producing the standard set of outputs
# employed by the HAWG working group.
#
# Developed with:
#   - R version 2.8.0
#   - FLCore 2.0
#   - FLICA, version 1.4-3
#   - FLAssess, version 1.99-102
#   - FLSTF, version 1.99-1
#   - FLEDA, version 2.0
#
# Changes:
# V 2.00 - Simplified everything down into functions
# V 1.10 - Added error checking and default options
# V 1.00 - Creation of common assessment module
#
# To be done:
#    Add in retrospective
#
# Notes:
#
####################################################################################################

### ======================================================================================================
### Display version numbering, etc
### ======================================================================================================
cat(gsub("$","","HAWG GENERIC STOCK ASSESSMENT MODULE\n$Revision$\n$Date$\n\n",fixed=TRUE))
flush.console()

### ======================================================================================================
### Summary Plots
### ======================================================================================================
do.summary.plots <- function(stck,ica.obj) {
    cat("GENERATING SUMMARY PLOTS ...\n");flush.console()

    #Make stock summary plots (ie SSB, Fbar, recs)
    summary.data <- FLQuants(SSB=ssb(stck),"Mean F"=fbar(stck),Recruits=rec(stck))
    summary.plot <-xyplot(data~year|qname,data=as.data.frame(summary.data),
                      prepanel=function(...) {list(ylim=range(pretty(c(0,list(...)$y))))},
                      main=list(paste(stck@name,"Stock Summary Plot"),cex=0.9),
                      col="black",
                      ylab="",
                      layout=c(1,3),
                      type="l",
                      panel=function(...) {
                        panel.grid(h=-1,v=-1)
                        panel.xyplot(...)
                      },
                      scales=list(alternating=1,abbreviate=TRUE,y=list(relation="free",rot=0)))
    print(summary.plot)

    #Now generate the diagnostic plots
    cat("GENERATING DIAGNOSTICS ...\n");flush.console()
    diagnostics(ica.obj)

    #New diagnostics! Contribution of each age class to the SSQ
    ssq.age.dat  <- lapply(ica.obj@weighted.resids,function(x) sqrt(yearMeans(x^2,na.rm=TRUE)))
    ssq.age.breakdown <- xyplot(data~age|qname,data=ssq.age.dat,
                      ylab="RMS Contribution to Objective Function",xlab="Age",
                      prepanel=function(...) {list(ylim=range(pretty(c(0,list(...)$y))))},
                      as.table=TRUE,
                      horizontal=FALSE,origin=0,box.width=1,col="grey",   #Barchart options
                      panel=function(...) {
                        panel.grid(h=-1,v=-1)
                        panel.barchart(...)
                        panel.abline(h=0,col="black",lwd=1)
                      },
                      scales=list(alternating=1))
    ssq.age.breakdown <- update(ssq.age.breakdown,main=list(paste(stck@name,"SSQ Breakdown by Age"),cex=0.9))
    print(ssq.age.breakdown)

    #New diagnostics! Contribution of each year to the SSQ
    ssq.yr.dat  <- lapply(ica.obj@weighted.resids,function(x) sqrt(quantMeans(x^2,na.rm=TRUE)))
    ssq.yr.breakdown <- xyplot(data~year|qname,data=ssq.yr.dat,
                      ylab="RMS Contribution to Objective Function",xlab="Year",
                      prepanel=function(...) {list(ylim=range(pretty(c(0,list(...)$y))))},
                      as.table=TRUE,
                      horizontal=FALSE,origin=0,box.width=1,col="grey",   #Barchart options
                      panel=function(...) {
                        panel.grid(h=-1,v=-1)
                        panel.barchart(...)
                        panel.abline(h=0,col="black",lwd=1)
                      },
                      scales=list(alternating=1))
    ssq.yr.breakdown <- update(ssq.yr.breakdown,main=list(paste(stck@name,"SSQ Breakdown by Year"),cex=0.9))
    print(ssq.yr.breakdown)

    #New diagnostics! Contribution of each cohort to the SSQ
    ssq.cohort.dat  <- lapply(ica.obj@weighted.resids,function(x) {
                            if(dims(x)$age>1) {     #FLCohort breaks down for a single age quant
                                means <- sqrt(apply(FLCohort(x)^2,c(2:6),mean,na.rm=TRUE))
                                FLQuant(means,dimnames=c(list(year="all"),dimnames(means)))
                            } else {
                                dat.names <- dimnames(x)
                                new.names <- c(list(cohort=dat.names$year,year="all"),dat.names[3:6])
                                FLQuant(as.vector(abs(x)),dimnames=new.names)}
                        })
    ssq.cohort.breakdown <- xyplot(data~as.numeric(as.character(cohort))|qname,data=ssq.cohort.dat,
                      ylab="RMS Contribution to Objective Function",xlab="Cohort",
                      prepanel=function(...) {list(ylim=range(pretty(c(0,list(...)$y))))},
                      as.table=TRUE,
                      horizontal=FALSE,origin=0,box.width=1,col="grey",   #Barchart options
                      panel=function(...) {
                        panel.grid(h=-1,v=-1)
                        panel.barchart(...)
                        panel.abline(h=0,col="black",lwd=1)
                      },
                      scales=list(alternating=1))
    ssq.cohort.breakdown <- update(ssq.cohort.breakdown,main=list(paste(stck@name,"SSQ Breakdown by Cohort"),cex=0.9))
    print(ssq.cohort.breakdown)

    #Setup plotting data for bubble plots
    n.ages   <- sapply(ica.obj@index.res,nrow)    #number of ages in each quant
    n.quants <- length(n.ages)
    res.dat  <- as.data.frame(ica.obj@index.res)
    res.dat$age <- factor(as.character(res.dat$age))  #Sorted automatically
    res.dat$qname <- factor(res.dat$qname,levels=unique(res.dat$qname)) #Not sorted - natural order

    #Bubble plots of the index residuals - this is how the code *should* be
#    bubble.plot <- bubbles(age~year|qname,data=res.dat,
#                      layout=c(1,n.quants),
#                      main=list(paste(stck@name,"Index Residuals Bubble Plot"),cex=0.9),
#                      prepanel=function(...){ #Only show ages for which we have data
#                         arg <- list(...)
#                         list(yat=unique(as.numeric(arg$y)))},
#                      ylab="age",
#                      as.table=TRUE,
#                      plot.args=list(panel.height=list(x=n.ages,units="null")), #Set relative heights of each panel
#                      scale=list(alternating=1,rot=0,y=list(relation="free")))
#    print(bubble.plot)

    #Bubble plots of the index residuals - this is a hack to deal with the fact that
    #there are currently some inconsistencies in the way that the FLR bubbles function is setup
    #this should be solved, and the previous code used instead, by around about FLCore 2.1 or so
    bubble.plot <- bubbles(factor(as.character(age))~year|qname,data=ica.obj@index.res,
                      layout=c(1,n.quants),
                      main=list(paste(stck@name,"Index Residuals Bubble Plot"),cex=0.9),
                      prepanel=function(...){ #Only show ages for which we have data
                         arg <- list(...)
                         list(yat=unique(as.numeric(arg$y)))},
                      ylab="age",
                      as.table=TRUE,
                      index.cond=list(rank(names(ica.obj@index.res))),
                      plot.args=list(panel.height=list(x=n.ages,units="null")),
                      scale=list(alternating=1,rot=0,y=list(relation="free")))
    print(bubble.plot)

    #Shade plot of index residuals
    shade.plot <- levelplot(data~year*age|qname,data=res.dat,
                      main=list(paste(stck@name,"Index Residuals Shade Plot"),cex=0.9),
                      layout=c(1,n.quants),
                      at=seq(-2,2,length.out=101),
                      col.regions=colorRampPalette(c("Green","White","Blue"))(100),
                      prepanel=function(...){ #Only show ages for which we have data
                         arg <- list(...)
                         list(yat=unique(as.numeric(arg$y[arg$subscripts])))},
                      pretty=TRUE,
                      ylab="age",
                      as.table=TRUE,
                      plot.args=list(panel.height=list(x=n.ages,units="null")),
                      scale=list(alternating=1,rot=0,y=list(relation="free")))
    print(shade.plot)

    #Generate an "otolith" plot showing the uncertainty distribution
    oldpar <- par() #Otolith plots tends to mess with par a bit much and not clean up!
    plot.otolith(stck,ica.obj)
    par(oldpar[-which(names(oldpar)%in%c("cin","cra","csi","cxy","din"))])   #Some parameters cannot be set
    invisible(NULL)
}

### ======================================================================================================
### Retrospective analysises
### ======================================================================================================
do.retrospective.plots<- function(stck,idxs,ctrl,n.retro.yrs) {
    cat("GENERATING RETROSPECTIVE ANALYSES...\n");flush.console()

    #Generate a retrospective analysis
    retro.icas <- retro(stck,idxs,ctrl,retro=n.retro.yrs,return.FLStocks=FALSE)
    retro.stck <- do.call(FLStocks,lapply(retro.icas,function(ica) {ica + trim(stck, year=dims(ica@stock.n)$minyear:dims(ica@stock.n)$maxyear)}))
    most.recent <- max(as.numeric(names(retro.stck)))

    #Standard retrospective plot
    cat("RETROSPECTIVE PLOT...\n");flush.console()
    retro.ssbs  <- do.call(FLQuants,lapply(retro.stck,ssb))
    retro.fbar  <- do.call(FLQuants,lapply(retro.stck,fbar))
    retro.recs  <- do.call(FLQuants,lapply(retro.stck,rec))
    retro.dat   <- rbind(cbind(value="SSB",as.data.frame(retro.ssbs)),
                          cbind(value="Recruits",as.data.frame(retro.recs)),
                          cbind(value="Mean F",as.data.frame(retro.fbar)))
    retro.dat$value <-  factor(retro.dat$value,levels=unique(retro.dat$value))  #Need to force the factoring to get the correct plotting order
    retro.plot<-xyplot(data~year|value,data=retro.dat,
                    main=list(paste(stck@name,"Retrospective Summary Plot"),cex=0.9),
                    groups=qname,
                    prepanel=function(...) {list(ylim=range(pretty(c(0,list(...)$y))))},
                    layout=c(1,3),
                    ylab="",
                    type="l",
                    as.table=TRUE,
                    lwd=c(rep(1,n.retro.yrs),3),
                    col="black",
                    panel=function(...) {
                        panel.grid(h=-1,v=-1)
                        panel.xyplot(...)
                    },
                    scales=list(alternating=1,y=list(relation="free",rot=0)))
    plot(retro.plot)

    #Lattice log10 y.scale fanciness
    log10.lbls <- c(seq(0.1,1.3,by=0.1),1.5,1.7,2,3,4,5,7,10)
    yscale.components.log10 <- function(lim, ...) {
        ans <- yscale.components.default(lim = lim, ...)
        tick.at <- log10.lbls
        ans$left$ticks$at <- log(tick.at, 10)
        ans$left$labels$at <- log(tick.at, 10)
        ans$left$labels$labels <- as.character(tick.at)
        ans
        }

#Calculate the biases by year
    cat("RETROSPECTIVE BIASES BY YEAR...\n");flush.console()
    most.recent.results <- subset(retro.dat,qname==most.recent,select=-qname)
    colnames(most.recent.results)[8] <- "most.recent"
    bias.dat           <-  merge(retro.dat,most.recent.results)
    bias.dat$bias      <-  bias.dat$data/bias.dat$most.recent
    bias.dat$TY.offset <-  bias.dat$year-as.numeric(bias.dat$qname)
    bias.plot<-xyplot(bias~year|value,data=bias.dat,
                    main=list(paste(stck@name,"Retrospective Bias Plot by Year"),cex=0.9),
                    groups=qname,
                    yscale.components=yscale.components.log10,
                    layout=c(1,3),
                    ylab="Bias",
                    xlab="Year",
                    type="l",
                    as.table=TRUE,
                    lwd=1,
                    col="black",
                    panel=function(...) {
                        panel.grid(h=0,v=-1)
                        panel.abline(h=log10(log10.lbls),col="lightgrey")
                        panel.abline(h=0,lwd=3)
                        panel.xyplot(...)
                    },
                    scales=list(alternating=1,y=list(relation="free",rot=0,log=TRUE)))
    plot(bias.plot)

    #Calculate the biases by offset from terminal year
    cat("RETROSPECTIVE BIASES BY OFFSET...\n");flush.console()
    bias.offset.plot<-xyplot(bias~TY.offset|value,data=bias.dat,
                    main=list(paste(stck@name,"Retrospective Bias Plot by Offset"),cex=0.9),
                    groups=qname,
                    yscale.components=yscale.components.log10,
                    layout=c(1,3),
                    ylab="Bias",
                    xlab="Offset from Terminal Year of Assessment",
                    type="l",
                    as.table=TRUE,
                    lwd=1,
                    col="black",
                    panel=function(...) {
                        panel.grid(h=0,v=-1)
                        panel.abline(h=log10(log10.lbls),col="lightgrey")
                        panel.abline(h=0,lwd=3)
                        panel.xyplot(...)
                    },
                    scales=list(alternating=1,y=list(relation="free",rot=0,log=TRUE)))
    plot(bias.offset.plot)

    #Retrospective selectivity
    cat("RETROSPECTIVE SELECTIVITY...\n");flush.console()
    sels <- sapply(rev(retro.icas),function(ica) drop(yearMeans(ica@sel)@.Data))
    most.recent.sel <- subset(retro.icas[[as.character(most.recent)]]@param,Param=="Sel",select=c("Age","Lower.95.pct.CL","Upper.95.pct.CL"))   #For the selectivity from the most recent assessment
    most.recent.sel <- rbind(most.recent.sel,c(ctrl@sep.age,1,1),
                            c(stck@range["plusgroup"]-1,rep(ctrl@sep.sel,2)),c(stck@range["plusgroup"],rep(ctrl@sep.sel,2)))   #Add CI's for sep.age, last true age, plus.group
    most.recent.sel <- most.recent.sel[order(most.recent.sel$Age),]
    plot(0,0,pch=NA,xlab="Age",ylab="Catch Selectivity",xlim=range(pretty(as.numeric(rownames(sels)))),
        ylim=range(pretty(c(0,most.recent.sel$Upper.95.pct.CL))),main=paste(stck@name,"Retrospective selectivity pattern"))
    polygon(c(most.recent.sel$Age,rev(most.recent.sel$Age)),c(most.recent.sel$Lower.95.pct.CL,rev(most.recent.sel$Upper.95.pct.CL)),col="grey")
    grid()
    matlines(as.numeric(rownames(sels)),sels,type="b",lwd=c(3,rep(1,n.retro.yrs)),
        pch=as.character(as.numeric(colnames(sels))%%10),col=1:6)
    legend("bottomright",legend=colnames(sels),lwd=c(3,rep(1,n.retro.yrs)),pch=as.character(as.numeric(colnames(sels))%%10),
        col=1:6,lty=1:5,bg="white")

    #Return retrospective object
    return(retro.stck)
}

### ======================================================================================================
### Stock Recruitment Plot
### ======================================================================================================
do.SRR.plot<- function(stck) {
    plot(ssb(stck),rec(stck),xlab="Spawning Stock Biomass",ylab="Recruits",type="b",
        xlim=range(pretty(c(0,ssb(stck)))),ylim=range(pretty(c(0,rec(stck)))))
    text(ssb(stck),rec(stck),labels=sprintf("%02i",as.numeric(dimnames(ssb(stck))$year)%%100),pos=4)
    title(main=paste(stck@name,"Stock-Recruitment Relationship"))
    warning("WARNING: do.SRR.plot() does not yet account for age offsets properly. Please check your results carefully!")
}

### ======================================================================================================
### Check FLR Package version numbers
### ======================================================================================================
#Load packages - strict, active enforcement of version numbers.
check.versions <-  function(lib,ver){
  available.ver <-  do.call(packageDescription,list(pkg=lib, fields = "Version"))
  if(compareVersion(available.ver,ver)==-1) {stop(paste("ERROR:",lib,"package availabe is version",available.ver,"but requires at least version",ver))}
  do.call(require,list(package=lib))
  invisible(NULL)
}
check.versions("FLCore","2.0")
check.versions("FLAssess","1.99-102")
check.versions("FLICA","1.4-4")
check.versions("FLSTF","1.99-1")
check.versions("FLEDA","2.0")
#Check R version too!
required.version <- "2.8.0"
if(compareVersion(paste(version$major,version$minor,sep="."),required.version)==-1) {
 stop(paste("ERROR: Current R version is",paste(version$major,version$minor,sep="."),"This code requires at least R",required.version))
}

#Add in other functions
source(file.path("..","_Common","HAWG Retro func.r"))

