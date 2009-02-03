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
#   - FLCore 1.99-111
#   - FLICA, version 1.4-3
#   - FLAssess, version 1.99-102
#   - FLSTF, version 1.99-1
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
    diagnostics(ica.obj)

    #Bubble plots of the index residuals
    bubble.plot <- bubbles(factor(as.character(age))~year|qname,data=ica.obj@index.res,
                      layout=c(1,length(ica.obj@index.res)),
                      main=list(paste(stck@name,"Index Residuals Bubble Plot"),cex=0.9),
                      prepanel=function(...){
                                arg <- list(...)
                                ylims  <- levels(arg$y)
                                yats   <- unique(as.numeric(arg$y))
                                list(ylim=ylims,yat=yats)
                      },
                      ylab="age",
                      as.table=TRUE,
                      scale=list(alternating=1,rot=0,y=list(relation="free")))
    print(bubble.plot)

    #Shade plot of index residuals
    shade.plot <- levelplot(data~year*factor(as.character(age))|qname,data=as.data.frame(ica.obj@index.res),
                      main=list(paste(stck@name,"Index Residuals Shade Plot"),cex=0.9),
                      layout=c(1,length(ica.obj@index.res)),
                      at=seq(-2,2,length.out=101),
                      col.regions=colorRampPalette(c("Green","White","Blue"))(100),
                      prepanel=function(...){
                                arg <- list(...)
                                ylims  <- levels(arg$y)
                                yats   <- unique(as.numeric(arg$y[arg$subscripts]))
                                list(ylim=ylims,yat=yats)
                      },
                      pretty=TRUE,
                      ylab="age",
                      as.table=TRUE,
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

    #Calculate the biases by year
    cat("RETROSPECTIVE BIASES BY YEAR...\n");flush.console()
    most.recent.results <- subset(retro.dat,qname==most.recent,select=-qname)
    colnames(most.recent.results)[8] <- "most.recent"
    bias.dat           <-  merge(retro.dat,most.recent.results)
    bias.dat$bias      <-  bias.dat$data/bias.dat$most.recent -1
    bias.dat$TY.offset <-  bias.dat$year-as.numeric(bias.dat$qname)
    bias.plot<-xyplot(bias~year|value,data=bias.dat,
                    main=list(paste(stck@name,"Retrospective Bias Plot by Year"),cex=0.9),
                    groups=qname,
                    prepanel=function(...) {list(ylim=range(pretty(c(0,list(...)$y))))},
                    layout=c(1,3),
                    ylab="Bias",
                    xlab="Year",
                    type="l",
                    as.table=TRUE,
                    lwd=1,
                    col="black",
                    panel=function(...) {
                        panel.grid(h=-1,v=-1)
                        panel.abline(h=0,lwd=3)
                        panel.xyplot(...)
                    },
                    scales=list(alternating=1,y=list(relation="free",rot=0)))
    plot(bias.plot)

    #Calculate the biases by offset from terminal year
    cat("RETROSPECTIVE BIASES BY OFFSET...\n");flush.console()
    bias.offset.plot<-xyplot(bias~TY.offset|value,data=bias.dat,
                    main=list(paste(stck@name,"Retrospective Bias Plot by Offset"),cex=0.9),
                    groups=qname,
                    prepanel=function(...) {list(ylim=range(pretty(c(0,list(...)$y))))},
                    layout=c(1,3),
                    ylab="Bias",
                    xlab="Offset from Terminal Year of Assessment",
                    type="l",
                    as.table=TRUE,
                    lwd=1,
                    col="black",
                    panel=function(...) {
                        panel.grid(h=-1,v=-1)
                        panel.abline(h=0,lwd=3)
                        panel.xyplot(...)
                    },
                    scales=list(alternating=1,y=list(relation="free",rot=0)))
    plot(bias.offset.plot)

    #Retrospective selectivity
    cat("RETROSPECTIVE SELECTIVITY...\n");flush.console()
    sels <- sapply(rev(retro.icas),function(ica) drop(yearMeans(ica@sel)@.Data))
    most.recent.sel <- subset(retro.icas[[as.character(most.recent)]]@param,Param=="Sel")   #For the selectivity from the most recent assessment
    plot(0,0,pch=NA,xlab="Age",ylab="Catch Selectivity",xlim=range(pretty(as.numeric(rownames(sels)))),
        ylim=range(pretty(c(0,most.recent.sel$Upper.95.pct.CL))),main=paste(stck@name,"Retrospective selectivity pattern"))
    polygon(c(most.recent.sel$Age,rev(most.recent.sel$Age)),c(most.recent.sel$Lower.95.pct.CL,rev(most.recent.sel$Upper.95.pct.CL)),col="grey")
    grid()
    matlines(as.numeric(rownames(sels)),sels,type="b",lwd=c(3,rep(1,n.retro.yrs)),pch=as.character(as.numeric(colnames(sels))%%10))
    legend("bottomright",legend=colnames(sels),lwd=c(3,rep(1,n.retro.yrs)),pch=as.character(as.numeric(colnames(sels))%%10),
        col=1:(n.retro.yrs+1),lty=1:(n.retro.yrs+1),bg="white")

    #Return retrospective object
    return(retro.stck)
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
check.versions("FLICA","1.4-3")
check.versions("FLSTF","1.99-1")
#Check R version too!
required.version <- "2.8.0"
if(compareVersion(paste(version$major,version$minor,sep="."),required.version)==-1) {
 stop(paste("ERROR: Current R version is",paste(version$major,version$minor,sep="."),"This code requires at least R",required.version))
}

#Add in other functions
source(file.path(".","Common","HAWG Retro func.r"))