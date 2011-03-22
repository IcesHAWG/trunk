######################################################################################################
# Exploratory use of Taylor Diagrams in Stock Assessments
#
# Author: Mark Payne, mpa@aqua.dtu.dk
# DTU-Aqua, Charlottenlund, DK
#
# Code for applying Taylor diagrams in Fish Stock assessment
#
# References:
# Taylor, K.E. (2001) Summarizing multiple aspects of model performance in
#         a single diagram. Journal of Geophysical Research, 106: 7183-7192.
# Payne, M.R. (2011) Taylor diagrams can aid in the interpretation of fisheries
#         models. CJFAS (submitted)
#
# Developed with:
#   - R version 2.8.1
#
# Changes:
#
# To be done:
#
# Notes:
#  - Taylor diagram here is referenced against the *modelled* results, rather than
#    the observations, as is more usual. This puts things on a more natural footing
#    when comparing data sources
#  - Stock assessment model of choice must be run prior to Taylor analysis
#  - Analysis is done on log-transformed values, as this is the natural basis on
#    which we do all of our modelling
#
# ----------------------------------------------------------------------------
# "THE BEER-WARE LICENSE" (Revision 42):
# Mark Payne wrote this file. As long as you retain this notice, you can do
# whatever you want with this stuff - just don't hold me responsible for it.
# If we meet some day, and you think still this stuff is worth it, you
# can buy me a beer in return. Mark Payne (with kudos to Poul-Henning Kamp)
# ----------------------------------------------------------------------------
####################################################################################################

setGeneric("taylor.diagram", function(object, object2,...){
	standardGeneric("taylor.diagram")
	})

### ======================================================================================================
### Plot the Taylor Diagram
### ======================================================================================================
.setup.taylor <- function(sd.dat,r.dat) {
  #Calculate limits based on supplied data
  sd.lims <- range(pretty(c(0,sd.dat)))
  radius <- max(sd.lims)   #Radius of circle
  #Setup plot
  label <- "Normalised std dev (obs/model)"
  oldpar <- par(pty="s")
  plot(NA,NA,xlim=sd.lims,ylim=sd.lims,xaxs="i",yaxs="i",bty="n",xlab=label,ylab=label,asp=1,xaxt="n",yaxt="n")
  axis.ats <- pretty(sd.lims)
  axis.lbls <- ifelse(axis.ats==1,"MDL",axis.ats)
  axis(1,at=axis.ats,axis.lbls)
  axis(2,at=axis.ats,axis.lbls)
  #Setup and plot arcs
  rarc <- seq(0,pi/2,length.out=200)
  lines(radius*cos(rarc),radius*sin(rarc),lwd=1.5)
  for(r in unique(abs(axis.ats-1))){
    lines(1+r*cos(rarc),r*sin(rarc),lty="dashed",col="grey")
    lines(1-r*cos(rarc),r*sin(rarc),lty="dashed",col="grey")
  }
  #angular axis
  lines(cos(rarc),sin(rarc),lwd=1.5)
  #labelled ticks
  labelled.ticks <- c(seq(0,1,by=0.1),0.95,0.99)   #Defined as r (not r2)
  ang.axis.angle <- acos(labelled.ticks)
  tick.len <- 1-0.025
  segments(radius*cos(ang.axis.angle),radius*sin(ang.axis.angle),0,0,lty="dotted",col="grey")
  segments(0,0,0,radius)   #Redraw axes for tidyness
  segments(0,0,radius,0)
  segments(radius*cos(ang.axis.angle),radius*sin(ang.axis.angle),
      radius*tick.len*cos(ang.axis.angle),radius*tick.len*sin(ang.axis.angle))
  text.offset <- 1.05
  for(a in ang.axis.angle) {
    text(radius*text.offset*cos(a),radius*text.offset*sin(a),
      labels=sprintf("%5.2f",cos(a)),xpd=NA,srt=a/pi*180)}
  #unlabelled ticks
  unlabelled.ticks <- c(seq(0.05,0.85,by=0.1),seq(0.91,0.98,by=0.01))   #Defined as r (not r2)
  ang.axis.angle <- acos(unlabelled.ticks)
  tick.len <- 1-0.0125
  segments(radius*cos(ang.axis.angle),radius*sin(ang.axis.angle),
      radius*tick.len*cos(ang.axis.angle),radius*tick.len*sin(ang.axis.angle))
  #Axis label - the tricky one
  ang.lab <- "Correlation coefficient (r)"
  axis.lab.offset <- 1.125
  text(radius*axis.lab.offset*cos(pi/4),radius*axis.lab.offset*sin(pi/4),ang.lab,srt=-45)
  #done
  par(oldpar)
  invisible(NULL)
}

### ======================================================================================================
### Prepare taylor data
### ======================================================================================================
.prepare.taylor.data <- function(obj,common.basis=TRUE) {
  #Extract data
  mdl.dat <- as.data.frame(obj@index.hat)
  obs.dat <- as.data.frame(obj@index)

  #Combine data sets into a single frame
  dat <- merge(obs.dat,mdl.dat,by=names(mdl.dat)[-which(names(mdl.dat)=="data")],suffixes=c(".obs",".mdl"))
  dat <- dat[!(is.na(dat$data.obs) | is.na(dat$data.mdl)),]

  #Apply common basis if requested
  yrs <- unique(dat$year)
  if(common.basis) {
    tbl <- table(dat$year,paste(dat$qname,dat$age))
    all.pts <- apply(tbl!=0,1,all)
    yrs <- as.numeric(names(which(all.pts)))
    dat <- subset(dat,dat$year %in% yrs)
  }

  #Calculate r and sd statistics
  sum.dat <- lapply(split(dat,dat[,c("qname","age")],drop=TRUE),function(x) {
               obs <- log(x$data.obs)
               mdl <- log(x$data.mdl)
               sd.obs <- sd(obs)
               sd.mdl <- sd(mdl)
               r <- cor(obs,mdl)
               return(data.frame(src=factor(unique(x$qname)),age=unique(x$age),sd.ratio=sd.obs/sd.mdl,r))})
  sum.dat <- do.call(rbind,sum.dat)

  #Sort for convenience
  sum.dat <- sum.dat[order(sum.dat$src,sum.dat$age),]

  #return
  attr(sum.dat,"yrs") <- yrs
  return(sum.dat)
}

### ======================================================================================================
### Plot single plot
### ======================================================================================================
setMethod("taylor.diagram", signature(object="FLAssess",object2="missing"), function(object, object2="missing", pchs=21:25,cols=1:6,common.basis=TRUE){
    #Extract data
    plt.dat <- .prepare.taylor.data(object,common.basis=common.basis)
    #For data sources where there is only one age, set this to zero
    src.freq <- as.data.frame(table(plt.dat$src))
    loners <- subset(src.freq$Var1,src.freq$Freq==1)
    if(length(loners)>0) {plt.dat[plt.dat$src %in% loners,]$age <- NA}
    #Assign pchs, col
    plt.dat$src <- factor(plt.dat$src,levels=sort(unique(as.character(plt.dat$src))))
    pchs <- rep(pchs,length.out=nlevels(plt.dat$src))
    cols <- rep(cols,length.out=nlevels(plt.dat$src))
    plt.dat$pchs <- pchs[as.numeric(plt.dat$src)]
    plt.dat$cols <- cols[as.numeric(plt.dat$src)]
    #Plot points
    sd.lims <- range(pretty(c(0,plt.dat$sd.ratio)))
    .setup.taylor(sd.lims)
    with(plt.dat,{
      points(sd.ratio*cos(acos(r)),sd.ratio*sin(acos(r)),pch=pchs,bg=cols,col=cols,cex=1.25,xpd=NA)
      text(sd.ratio*cos(acos(r)),sd.ratio*sin(acos(r)),age,pos=1,cex=0.75,xpd=NA)
    })
    #Finally, the legend
    yr.rng <- range(attr(plt.dat,"yrs"))
    legend("topright",pch=pchs,pt.bg=cols,col=cols,legend=levels(plt.dat$src),cex=0.75,pt.cex=1,
        title=ifelse(common.basis,sprintf("(%i-%i)",yr.rng[1],yr.rng[2]),"All years"))
})

### ======================================================================================================
### Two object difference plot
### ======================================================================================================
setMethod("taylor.diagram", signature(object="FLAssess",object2="FLAssess"), function(object, object2, pchs=21:25,cols=1:6,common.basis=TRUE){
  #Prepare data
  sum.dat1 <- .prepare.taylor.data(object,common.basis=common.basis)
  sum.dat2 <- .prepare.taylor.data(object2,common.basis=common.basis)

  #Combine the two data sets into one table
  plt.dat <- merge(sum.dat1,sum.dat2,suffixes=c(".1",".2"),by=c("src","age"))
  plt.dat$src <- factor(plt.dat$src,levels=sort(unique(as.character(plt.dat$src))))
  #For data sources where there is only one age, set this to zero
  src.freq <- as.data.frame(table(plt.dat$src))
  loners <- subset(src.freq$Var1,src.freq$Freq==1)
  if(length(loners)>0) {plt.dat[plt.dat$src %in% loners,]$age <- NA}
  #Assign pchs, col
  pchs <- seq_len(nlevels(plt.dat$src))
  pchs <- c(15,16,17,18)
  cols <- seq_len(nlevels(plt.dat$src))
  plt.dat$pchs <- pchs[as.numeric(plt.dat$src)]
  plt.dat$cols <- cols[as.numeric(plt.dat$src)]
  #Plot points
  sd.lims <- range(pretty(c(0,plt.dat$sd.ratio.1,plt.dat$sd.ratio.2)))
  .setup.taylor(sd.lims)
  with(plt.dat,{
    arrows(sd.ratio.1*cos(acos(r.1)),sd.ratio.1*sin(acos(r.1)),
      sd.ratio.2*cos(acos(r.2)),sd.ratio.2*sin(acos(r.2)),length=0.05)
    points(sd.ratio.1*cos(acos(r.1)),sd.ratio.1*sin(acos(r.1)),pch=pchs,col=cols,cex=1.25,xpd=NA)
  #  points(sd.ratio.2*cos(acos(r.2)),sd.ratio.2*sin(acos(r.2)),pch=pchs,col=cols,cex=1.25)
    text(sd.ratio.1*cos(acos(r.1)),sd.ratio.1*sin(acos(r.1)),age,pos=1,cex=0.75,xpd=NA)
  })
  points(1,0,pch=8,col="black",xpd=TRUE)
  #Finally, the legend
  yr.rng <- range(attr(sum.dat1,"yrs"))
  legend("topright",pch=pchs,col=cols,legend=c(levels(plt.dat$src)),cex=0.75,pt.cex=1,
        title=ifelse(common.basis,sprintf("(%i-%i)",yr.rng[1],yr.rng[2]),"All years"))
})

