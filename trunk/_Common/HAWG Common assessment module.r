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
#   - FLCore 2.2
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
    summary.data <- as.data.frame(FLQuants(SSB=ssb(stck),"Mean F"=fbar(stck),Recruits=rec(stck)))
    scaling.factors <- tapply(summary.data$data,summary.data$qname,function(x) trunc(log10(max(pretty(c(0,x))))/3)*3)
    summary.data$data <- summary.data$data/10^scaling.factors[summary.data$qname]
    ylabels <- apply(cbind(lbl=names(scaling.factors),fctr=scaling.factors),1,function(x) {
        if(x[2]=="0"){x[1]}else {bquote(expression(paste(.(x[1])," [",10^.(x[2]),"]"))) }})
    summary.plot <-xyplot(data~year|qname,data=summary.data,
                      prepanel=function(...) {list(ylim=range(pretty(c(0,list(...)$y))))},
                      main=list(paste(stck@name,"Stock Summary Plot"),cex=0.9),
                      ylab=do.call(c,ylabels),
                      xlab="Year",
                      layout=c(1,3),
                      type="l",
                      panel=function(...) {
                        panel.grid(h=-1,v=-1)
                        if(panel.number()==2) { #Do recruits as bar plot
                            panel.barchart(...,horizontal=FALSE,origin=0,box.width=1,col="grey")
                        } else {
                            panel.xyplot(...,col="black")
                        }
                      },
                      scales=list(alternating=1,y=list(relation="free",rot=0)))
    print(summary.plot)

    #Now generate the diagnostic plots
    cat("GENERATING DIAGNOSTICS ...\n");flush.console()
    diagnostics(ica.obj)

    #New diagnostics! Contribution of each age class to the SSQ
    ssq.age.dat  <- lapply(ica.obj@weighted.resids,function(x) yearMeans(x^2,na.rm=TRUE))
    ssq.age.breakdown <- xyplot(data~age|qname,data=ssq.age.dat,
                      ylab="Mean Contribution to Objective Function per point",xlab="Age",
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
    ssq.yr.dat  <- lapply(ica.obj@weighted.resids,function(x) quantMeans(x^2,na.rm=TRUE))
    ssq.yr.breakdown <- xyplot(data~year|qname,data=ssq.yr.dat,
                      ylab="Mean Contribution to Objective Function per point",xlab="Year",
                      prepanel=function(...) {list(ylim=range(pretty(c(0,list(...)$y))))},
                      as.table=TRUE,
                      horizontal=FALSE,origin=0,box.width=1,col="grey",   #Barchart options
                      panel=function(...) {
                        panel.grid(h=-1,v=-1)
                        panel.barchart(...)
                        panel.abline(h=0,col="black",lwd=1)
                      },
                      scales=list(alternating=1,y=list(relation="free")))
    ssq.yr.breakdown <- update(ssq.yr.breakdown,main=list(paste(stck@name,"SSQ Breakdown by Year"),cex=0.9))
    print(ssq.yr.breakdown)

    #New diagnostics! Contribution of each cohort to the SSQ
    ssq.cohort.dat  <- lapply(ica.obj@weighted.resids,function(x) {
                            if(is.na(dims(x)$min)) {
                                dat.names <- dimnames(x)
                                new.names <- c(list(cohort=dat.names$year,year="all"),dat.names[3:6])
                                FLQuant(as.vector(x^2),dimnames=new.names)
                            } else {  #FLCohort breaks down for a single age quant
                                means <- apply(FLCohort(x)^2,c(2:6),mean,na.rm=TRUE)
                                FLQuant(means,dimnames=c(list(year="all"),dimnames(means)))}
                        })
    ssq.cohort.breakdown <- xyplot(data~as.numeric(as.character(cohort))|qname,data=ssq.cohort.dat,
                      ylab="Mean Contribution to Objective Function per point",xlab="Cohort",
                      prepanel=function(...) {list(ylim=range(pretty(c(0,list(...)$y))))},
                      as.table=TRUE,
                      horizontal=FALSE,origin=0,box.width=1,col="grey",   #Barchart options
                      panel=function(...) {
                        panel.grid(h=-1,v=-1)
                        panel.barchart(...)
                        panel.abline(h=0,col="black",lwd=1)
                      },
                      scales=list(alternating=1,y=list(relation="free")))
    ssq.cohort.breakdown <- update(ssq.cohort.breakdown,main=list(paste(stck@name,"SSQ Breakdown by Cohort"),cex=0.9))
    print(ssq.cohort.breakdown)

    #Filter any infinite values from the weighted residuals
    ica.obj@weighted.resids <- lapply(ica.obj@weighted.resids,function(x) FLQuant(ifelse(is.finite(x),1,NA)*x))

   #Bubble plots of the weighted residuals - this is the same hack as below
    n.ages   <- sapply(ica.obj@weighted.resids,nrow)    #number of ages in each quant
    n.quants <- length(n.ages)
    wt.bubble.plot <- bubbles(factor(as.character(age))~year|qname,data=ica.obj@weighted.resids,
                      layout=c(1,n.quants),
                      main=list(paste(stck@name,"Weighted Residuals Bubble Plot"),cex=0.9),
                      prepanel=function(...){ #Only show ages for which we have data
                         arg <- list(...)
                         list(yat=unique(as.numeric(arg$y)))},
                      ylab="Age",
                      as.table=TRUE,
                      index.cond=list(rank(names(ica.obj@weighted.resids))),
                      plot.args=list(panel.height=list(x=n.ages,units="null")),
                      scale=list(alternating=1,rot=0,y=list(relation="free")))
    print(wt.bubble.plot)

    #Setup plotting data for bubble plots
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
    n.ages   <- sapply(ica.obj@index.res,nrow)    #number of ages in each quant
    n.quants <- length(n.ages)
    bubble.plot <- bubbles(factor(as.character(age))~year|qname,data=ica.obj@index.res,
                      layout=c(1,n.quants),
                      main=list(paste(stck@name,"Unweighted Index Residuals Bubble Plot"),cex=0.9),
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
    title(main=paste(stck@name,"Otolith Plot"),outer=TRUE)
    par(oldpar[-which(names(oldpar)%in%c("cin","cra","csi","cxy","din"))])   #Some parameters cannot be set
    invisible(NULL)
}

### ======================================================================================================
### Retrospective analysises
### ======================================================================================================
do.retrospective.plots<- function(stck,idxs,ctrl,n.retro.yrs) {
    cat("GENERATING RETROSPECTIVE ANALYSES...\n");flush.console()

    #Generate a retrospective analysis
    icas <- retro(stck,idxs,ctrl,retro=n.retro.yrs,return.FLStocks=FALSE)
    stcks <- do.call(FLStocks,lapply(icas,function(ica) {ica + trim(stck, year=dims(ica@stock.n)$minyear:dims(ica@stock.n)$maxyear)}))

    #Now call the retro plotting functions
    retro.plots(stcks,icas,ctrl)

    #Return retrospective object
    return(stcks)
}

retro.plots<- function(retro.stck,retro.icas,ctrl) {
    #Setup some prelims
    stock.yrs <- sapply(retro.stck,function(x) {dims(x)$maxyear})
    most.recent <- max(stock.yrs)
    most.recent.stck <- retro.stck[[which.max(stock.yrs)]]
    n.retros    <- length(retro.stck)
    stck.name   <- most.recent.stck@name

    #Standard retrospective plot
    cat("RETROSPECTIVE PLOT...\n");flush.console()
    retro.ssbs  <- lapply(retro.stck,ssb)
    retro.fbar  <- lapply(retro.stck,fbar)
    retro.recs  <- lapply(retro.stck,rec)
    retro.dat   <- rbind(cbind(value="SSB",as.data.frame(retro.ssbs)),
                          cbind(value="Recruits",as.data.frame(retro.recs)),
                          cbind(value="Mean F",as.data.frame(retro.fbar)))
    scaling.factors <- tapply(retro.dat$data,retro.dat$value,function(x) trunc(log10(max(pretty(c(0,x))))/3)*3)
    ylabels <- apply(cbind(lbl=names(scaling.factors),fctr=scaling.factors),1,function(x) {
        if(x[2]=="0"){x[1]}else {bquote(expression(paste(.(x[1])," [",10^.(x[2]),"]"))) }})
    retro.dat$data <- retro.dat$data/10^scaling.factors[retro.dat$value]
    retro.dat$value <-  factor(retro.dat$value,levels=unique(retro.dat$value))  #Need to force the factoring to get the correct plotting order
    retro.plot<-xyplot(data~year|value,data=retro.dat,
                    main=list(paste(stck.name,"Retrospective Summary Plot"),cex=0.9),
                    groups=qname,
                    prepanel=function(...) {list(ylim=range(pretty(c(0,list(...)$y))))},
                    layout=c(1,3),
#                    ylab=list(paste(c("Mean F","Recruits","SSB"),ifelse(scaling.factors!=0,paste(" [10^",scaling.factors,"]",sep=""),""),sep="")),
                    ylab=do.call(c,ylabels),
                    type="l",
                    as.table=TRUE,
                    lwd=c(rep(1,n.retros-1),3),
                    col="black",
                    panel=function(...) {
                        panel.grid(h=-1,v=-1)
                        panel.xyplot(...)
                    },
                    scales=list(alternating=1,y=list(relation="free",rot=0)))
    plot(retro.plot)

    #Lattice log10 y.scale fanciness for use in subsequent functions
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
                    main=list(paste(stck.name,"Retrospective Bias Plot by Year"),cex=0.9),
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
                    main=list(paste(stck.name,"Retrospective Bias Plot by Offset"),cex=0.9),
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

    #Retrospective cohort plot
    cat("RETROSPECTIVE BY COHORT...\n");flush.console()
    flc.dat.all     <- as.data.frame(lapply(retro.stck,function(x) FLCohort(x@stock.n)))
    current.cohorts <- dimnames(FLCohort(most.recent.stck@stock.n[,as.character(dims(most.recent.stck)$maxyear)]))$cohort
    flc.dat         <- subset(flc.dat.all,cohort%in%as.numeric(current.cohorts) & !is.na(data))  #Drop the NAs and the non-current cohorts
    cohort.retro <- xyplot(data~age|factor(cohort),data=flc.dat,
                    main=list(paste(stck.name,"Retrospective Plot by Cohort"),cex=0.9),
                    ylim=10^c(floor(min(log10(flc.dat$data))),ceiling(max(log10(flc.dat$data)))),
                    as.table=TRUE,
                    groups=cname,
                    type="l",
                    ylab="Cohort Numbers",
                    xlab="Age",
                    col="black",
                    scales=list(alternating=1,y=list(log=TRUE)),
                    panel=function(...) {
                        panel.grid(h=-1,v=-1)
                        panel.xyplot(...)
                        dat <- list(...)
                        panel.xyplot(dat$x[1],dat$y[1],pch=19,cex=0.5,col="black")  #The first estimate of the cohort strength is never plotted
                    })
    print(cohort.retro)

    #Retrospective selectivity
    cat("RETROSPECTIVE SELECTIVITY...\n");flush.console()
    sels <- sapply(rev(retro.icas),function(ica) {drop(yearMeans(ica@sel)@.Data)})
    most.recent.sel <- subset(retro.icas[[as.character(most.recent)]]@param,Param=="Sel",select=c("Age","Lower.95.pct.CL","Upper.95.pct.CL"))   #For the selectivity from the most recent assessment
    most.recent.sel <- rbind(most.recent.sel,c(ctrl@sep.age,1,1),
                            c(most.recent.stck@range["plusgroup"]-1,rep(ctrl@sep.sel,2)),c(most.recent.stck@range["plusgroup"],rep(ctrl@sep.sel,2)))   #Add CI's for sep.age, last true age, plus.group
    most.recent.sel$Age <- as.numeric(most.recent.sel$Age)
    most.recent.sel <- most.recent.sel[order(most.recent.sel$Age),]
    plot(0,0,pch=NA,xlab="Age",ylab="Catch Selectivity",xlim=range(pretty(as.numeric(rownames(sels)))),
        ylim=range(pretty(c(0,most.recent.sel$Upper.95.pct.CL))),main=paste(stck.name,"Retrospective selectivity pattern"))
    polygon(c(most.recent.sel$Age,rev(most.recent.sel$Age)),c(most.recent.sel$Lower.95.pct.CL,rev(most.recent.sel$Upper.95.pct.CL)),col="grey")
    grid()
    matlines(as.numeric(rownames(sels)),sels,type="b",lwd=c(3,rep(1,n.retros-1)),
        pch=as.character(as.numeric(colnames(sels))%%10),col=1:6)
    legend("bottomright",legend=colnames(sels),lwd=c(3,rep(1,n.retros-1)),pch=as.character(as.numeric(colnames(sels))%%10),
        col=1:6,lty=1:5,bg="white")

}

### ======================================================================================================
### Stock Recruitment Plot
### ======================================================================================================
do.SRR.plot<- function(stck) {
    ssb.dat <- as.data.frame(ssb(stck))
    rec.dat <- as.data.frame(FLCohort(rec(stck)))
    dat <- merge(ssb.dat,rec.dat,by.x="year",by.y="cohort",suffixes=c(".ssb",".rec"))
    plot(dat$data.ssb,dat$data.rec,xlab="Spawning Stock Biomass",ylab="Recruits",type="b",
        xlim=range(pretty(c(0,ssb(stck)))),ylim=range(pretty(c(0,rec(stck)))))
    text(dat$data.ssb,dat$data.rec,labels=sprintf("%02i",as.numeric(dat$year)%%100),pos=4)
    title(main=paste(stck@name,"Stock-Recruitment Relationship"))
    if(mean(stck@m.spwn)>0.5) warning("WARNING: SRR code does not properly account for autumn spawning stocks. You'll have to make your own plot!")
}

### ======================================================================================================
### Data exploration plots
### ======================================================================================================
#MPA: This is a candidate for deletion, as this is one line of code that could be done by the user
cor.tun <- function(stk.tun){ for(i in names(stk.tun)) if(dim(stk.tun[[i]]@index)[1]>1) plot(stk.tun[[i]],type="internal",main=name(stk.tun[[i]]))}              

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


#CPUE plot of the surveys per age class                  
#MPA: I think this may be covered by plot FLIndices but am not sure
cpue.survey <- function(stk.tun,slot.){
            lst <- lapply(stk.tun, function(x){return(slot(x,slot.))})
            # now a nice FLQuants
            stk.inds <- mcf(lst)
            # scale
            stk.indsN01 <- lapply(stk.inds, function(x){
                              arr <- apply(x@.Data, c(1,3,4,5,6), scale)
                              arr <- aperm(arr, c(2,1,3,4,5,6))
                              # small trick to fix an apply "feature"
                              dimnames(arr) <- dimnames(x)
                              x <- FLQuant(arr)
                            })
            
            stk.indsN01 <- FLQuants(stk.indsN01)
            # stupid hack to correct names (fixed in version 2)
            names(stk.indsN01) <- names(lst)
#            akey$lines$lty<-c(0,1,1,1)    #1=mlai, #2=MIK #3=IBTS #4=Acoust
            # plot                                                                   #1=Acoust, #2=IBTS #3=MIK #4=MLAI
            print(xyplot(data~year|factor(age), data=as.data.frame(stk.indsN01), type="l",col=c(2,3,6,0),
                main=list("Surveys CPUE", cex=1),
                xlab=list(cex=0.8),
                ylab=list("Standardized CPUE", cex=0.8),
                key=simpleKey(text=names(stk.indsN01), points=F, lines=T, columns=2, cex=0.8,col=c(0,6,3,2)),
                striptext=list(cex=0.8),
                scales=list(cex=0.7),
                groups=qname,as.table=TRUE, layout=c(5,2,1)))
            }

#Three ways of looking at the catch curves
catch.curves <- function(stk,start.,end.){
                  stk.cc.l      <- logcc(window(stk@catch.n,start=start.,end=end.))
                  stk.cc.coh    <- FLCohort(window(stk@catch.n,start=start.,end=end.))
                  color         <- c("black","green","red","blue","orange","grey","pink","lightblue","darkviolet","darkblue")
                  
                  stk.age       <- apply(stk.cc.l,1:2,mean)
                  years         <- as.numeric(dimnames(stk.age)$cohort); ages        <- as.numeric(dimnames(stk.age)$age)
                  
                  
                  akey <- simpleKey(text=paste(seq(start.,end.,1)), points=F, lines=T, columns=3, cex=0.8)
                  print(ccplot(data~age, data=stk.cc.l, type="l",lwd=2,main="Log catch ratios",key=akey))
                  akey <- simpleKey(text=paste("yearclass",dimnames(stk.cc.coh)$cohort), points=F, lines=T, columns=3, cex=0.8)
                  print(ccplot(data~age, data=stk.cc.coh, type="l",main="Cohort absolute catch ratios",lwd=2,key=akey))
                  
                  plot(stk.age[1,ac(years)]~years,type="l",ylim=c(-3,3),col=color[1],lwd=2,main="Selectivity at age",ylab="data")
                  for(i in 2:length(ages)) lines(stk.age[i,ac(years)]~years,col=color[i],lwd=2)
                  legend("bottomleft",c(ac(ages)),lwd=2,col=color,box.lty=0,ncol=4)
                  }

#Fitting SR and plotting reference points. Returnes SR too                  
ref.pts <- function(stk,model.,factor.){
                bevholtfactor   <- factor.
                stk.sr  <- fmle(as.FLSR(transform(stk, stock.n = stock.n/bevholtfactor),model=model.)); 
                if(model.=="bevholt"){ stk.sr@params<-stk.sr@params * bevholtfactor   
                } else {
                  stk.sr@params[2]<-stk.sr@params[2] * bevholtfactor; }
                rpts<-refpts()[4:5,]
                dimnames(rpts)[[1]][2]<-"crash"
                stk.brp    <- brp(FLBRP(stk,sr=stk.sr,fbar=seq(0,1,length.out=100),nyrs=3,refpts=rpts))
                print(refpts(stk.brp)[,1:5])
                plot(stk.brp)
                return(stk.sr)
            }
            
an <- function(x){ return(as.numeric(x))}            

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



### ======================================================================================================
### Check FLR Package version numbers
### ======================================================================================================
#Load packages - strict, active enforcement of version numbers.
check.versions <-  function(lib,ver,required.date="missing"){
  available.ver <-  do.call(packageDescription,list(pkg=lib, fields = "Version"))
  if(compareVersion(available.ver,ver)==-1) {stop(paste("ERROR:",lib,"package available is version",available.ver,"but requires at least version",ver))}
  package.date <- as.POSIXct(strptime(strsplit(packageDescription(lib)$Built,";")[[1]][3],format="%Y-%m-%d %H:%M:%S"))
  if(!missing(required.date)) {
    if(required.date - package.date > 0)
        {stop(paste("ERROR:",lib,"package date/time is",package.date,"but at least",required.date,"is required. Try updating the package from /_Common/pkgs."))}
  }
  do.call(require,list(package=lib))
  invisible(NULL)
}
check.versions("FLCore","2.2",ISOdatetime(2009,05,19,19,23,00))
check.versions("FLAssess","1.99-102",ISOdatetime(2009,03,23,08,18,00))
check.versions("FLICA","1.4-12")
check.versions("FLSTF","1.99-1")
check.versions("FLEDA","2.0")
check.versions("FLBRP","2.0")
check.versions("FLash","2.0",ISOdatetime(2009,03,24,09,11,00))
#Check R version too!
required.version <- "2.8.1"
if(compareVersion(paste(version$major,version$minor,sep="."),required.version)!=0) {
 stop(paste("ERROR: Current R version is",paste(version$major,version$minor,sep="."),"The HAWG repository currently only supports R",required.version))
}

#Add in other functions
source(file.path("..","_Common","HAWG Retro func.r"))
source(file.path("..","_Common","Stacked Area plot.r"))
source(file.path("..","_Common","WriteIcaSum.r"))

#Set penality function so that we don't get any scientific notation
options("warn.FPU"=FALSE)