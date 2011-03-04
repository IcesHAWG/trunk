######################################################################################################
# SCAI model
#
# $Rev$
# $Date$
#
# Author: Mark Payne, mpa@aqua.dtu.dk
# DTU-Aqua, Charlottenlund, DK
# 
# Sets up and plots the results of the SCAI Random Effects Model of the IHLS larval abundance indices
#
# Developed with:
#   - R version 2.9.1
#   - ADMB v 9.0
#
# Changes:
#   r404   - Added retrospective analysis
#   V 2.01 - Tweaks to output figures
#   V 2.00 - Simplifed for inclusion in HAWG repository
#   V 1.42 - Tweaks to figures for paper
#   V 1.41 - Fitting process figures for presentation
#   V 1.40 - Fixed bug with incorrect application of sum proportions = 1 constraint
#   V 1.30 - Now with SCAI_hat, estimated internally within the model
#   V 1.20 - Renamed to SCAI, and internal modifications to fit with manuscript
#   V 1.10 - Estimates the total abundance in each area, allowing intercomparison of components
#   V 1.00 - Initial version
#
# To be done:
#
# Notes:
#   This work is described in more detail in the publication
#   Payne, M. R. 2010. Mind the gaps: a state-space model for analysing the dynamics
#   of North Sea herring spawning components. – ICES Journal of Marine Science, 67: 1939–1947.
#
# ----------------------------------------------------------------------------
# "THE BEER-WARE LICENSE" (Revision 42):
# Mark Payne wrote this file. As long as you retain this notice, you can do
# whatever you want with this stuff - just don't hold me responsible for it.
# If we meet some day, and you think still this stuff is worth it, you
# can buy me a beer in return. Mark Payne (with kudos to Poul-Henning Kamp)
# ----------------------------------------------------------------------------
####################################################################################################

### ======================================================================================================
### Initialise system
### ======================================================================================================
# Start with house cleaning
rm(list = ls(all.names=TRUE)); gc(); graphics.off()
ver <- "SCAI Model $Rev$"
ver.datetime   <- "$Date$"
cat(paste("\n",ver,"\n",sep=""));cat(paste(ver.datetime,"\n\n",sep=""))
start.time <- proc.time()[3]
options(stringsAsFactors=FALSE)

### ======================================================================================================
### Parameters
### ======================================================================================================
#Load data
LAI.dat <- read.csv(file.path(".","LAI Time Series.csv"),header=TRUE)

#Abbreviations
areas <- c("OrkShe"="OS",Buchan="B",Banks="CNS",Downs="SNS")
samp.unit.names <- colnames(LAI.dat)[-1]

#Retrospective analysis
retro.yrs <- 1  #Set to null to switch off retro

#Working dirs
wkdir <- file.path(".","ADMBwkdir")
output.dir <- file.path(".","SCAIoutputs")

#output device
pdf(file.path(output.dir,"SCAI_outputs.pdf"),width=200/25.4,height=200/25.4,pointsize=16,onefile=TRUE)


### ======================================================================================================
### Model fitting function
### ======================================================================================================
fit.SCAI <- function(area.code,dat) {
    ### Write input to ADMB
    ### ====================
    #setup list object containing data
    opt <- list()
    opt$n.years <- nrow(dat)
    opt$start.yr <- min(dat$year)
    opt$end.yr   <- max(dat$year)

    #Extract data and reshape
    cols <- grep(area.code,colnames(dat))
    obs <- data.frame(stack(dat[,cols]),Year=dat$year)
    obs <- obs[!is.na(obs$values),]
    obs$ind <-factor(obs$ind)

    #Add to output list
    opt$n.surv <- nlevels(obs$ind)
    opt$n.obs <- nrow(obs)
    opt$obs   <- obs
    opt$obs$ind <- as.numeric(opt$obs$ind)

    #Write .dat file
    opt.file <- file.path(wkdir,"SCAI.dat")
    cat("",file=opt.file)
    lapply(names(opt), function(x) {
         cat(paste("#",x,"\n"),file=opt.file,append=TRUE)
         write.table(opt[[x]],file=opt.file,append=TRUE,col.names=FALSE,row.names=FALSE,quote=FALSE)
    })

    ### Run ADMB
    ### ========
    olddir <- setwd(wkdir)
    shell("scai.exe",mustWork=TRUE)
    setwd(olddir)


    ### Load results
    ### ============
    #Load results
    res <- read.table(file.path(wkdir,"scai.std"),skip=1,sep="",strip.white=TRUE,
              col.names=c("index","name","value","std.dev"))
    logSCAI <- subset(res,res$name=="logSCAI")
    logSCAI$year <- dat$year
    logSCAI$SCAI   <- exp(logSCAI$value)
    logSCAI$lb    <- exp(logSCAI$value-1.96*logSCAI$std.dev)
    logSCAI$ub    <- exp(logSCAI$value+1.96*logSCAI$std.dev)

    #Setup SCAI_hat values
    logSCAI_hat <- subset(res,res$name=="logSCAI_hat")
    obs$SCAI_hat <- exp(logSCAI_hat$value)
    obs$SCAI_hat.ub <- exp(logSCAI_hat$value+logSCAI_hat$std.dev*1.96)
    obs$SCAI_hat.lb <- exp(logSCAI_hat$value-logSCAI_hat$std.dev*1.96)
    obs$SCAI  <- logSCAI[as.numeric(obs$Year)-1971,c("SCAI","lb","ub")]
    obs$resid <- log(obs$SCAI_hat)-log(obs$SCAI$SCAI)       #Observation (SCAI_hat) - model (SCAI$SCAI)

    res2 <- readLines(file.path(wkdir,"scai.par"),n=1)
    jnll.line <- strsplit(res2,"  ")
    jnll <-  as.numeric(gsub("^.*=","",jnll.line[[1]][2]))

    #Return results
    opt <- list(fit=logSCAI,res=res,obs=obs,jnll=jnll)
    return(opt)
}

### ======================================================================================================
### Fit model
### ======================================================================================================
fit <- lapply(as.list(areas),fit.SCAI,LAI.dat)

### ======================================================================================================
### Analysis of residuals
### ======================================================================================================
#Test residuals for randomness
cat("\nTests for normally distributed residuals by sampling unit\n")
for(a in names(areas)) {
  d <- split(fit[[a]]$obs$resid,fit[[a]]$obs$ind)
  fit.mean  <- 0
  fit.sd  <- exp(subset(fit[[a]]$res,name=="logSdObsErr")$value)
#  print(ks.test(re.diffs)$p.value)
  sapply(d, function(x) {
    cat(sprintf("Area = %s, \tKS Test p.value = %5.3f, \tSW Test p.value = %5.3f\n",a,
      ks.test(x,"pnorm",mean=fit.mean,sd=fit.sd)$p.value,shapiro.test(x)$p.value))
  })
}

#Test random effects for Normal distribution
cat("\nKolmogorov-Smirnoff test for Normally Distributed Steps\n")
for(a in names(areas)) {
  re.diffs  <- diff(fit[[a]]$fit$value)
  fit.mean  <- subset(fit[[a]]$res,name=="mulogSCAI")$value
  fit.sd  <- exp(subset(fit[[a]]$res,name=="logSdSCAI")$value)
#  print(ks.test(re.diffs)$p.value)
  cat(sprintf("KS Test, area = %s, p.value = %5.3f\n",a,
      ks.test(re.diffs,"pnorm",mean=fit.mean,sd=fit.sd)$p.value))
}

#Likelihoods for each component
print(sapply(fit,function(d) d$jnll))

### ======================================================================================================
### Retrospective analysis
### ======================================================================================================
#Strip results of a fit down to bare basis for use in retro
strip.results <- function(ft) {
  res<- lapply(ft,function(b) {b$fit[,c("year","value")]})
  for(r in names(res)) {
    res[[r]]$comp <- r
  }
  res <- do.call(rbind,res)
  return(res)
}

#Setup baseline
base.res <- strip.results(fit)
base.res$retro <- 0   #Indicates that it is the base

#Now perform retros
retro.res <- list()
for(i in retro.yrs) {
  retro.dat <- LAI.dat[seq(1,nrow(LAI.dat)-i),]
  retro.fit <- lapply(as.list(areas),fit.SCAI,retro.dat)
  retro.res[[i]] <- strip.results(retro.fit)
  retro.res[[i]]$retro <- i
}
retro.res <- do.call(rbind,c(list(base.res),retro.res))


### ======================================================================================================
### Plot results
### ======================================================================================================
xlims <- range(pretty(LAI.dat$year))
n.areas <- length(areas)

#First plot available data
image(seq(colnames(LAI.dat)[-1]),LAI.dat$year,is.na(t(as.matrix(LAI.dat[,-1]))),
    col=c("black","grey90"),xaxt="n",xlab="",ylab="",las=1)
axis(1,at=seq(colnames(LAI.dat)[-1]),labels=colnames(LAI.dat)[-1],las=3)
abline(v=c(2.5,4.5,8.5),lwd=4,col="red")
box()

#Plot comparison of SNS1 vs SNS3 to show systematic biases
plot(LAI.dat$SNS1,LAI.dat$SNS3,log="xy",pch=19,xlab="SNS1 Survey",ylab="SNS3 Survey")
abline(a=0,b=1,lwd=2)

#First plot the time series for each component, with observations
par(mfrow=c(4,1),mar=c(0,0,0,0),oma=c(3.5,5,4,0.5),las=1,mgp=c(4,1,0))
pchs <- c(19,2,3,8)
for(i in 1:n.areas){
  a <- names(areas)[i]
  d <- fit[[a]]
  ylims <- range(pretty(c(0,d$obs$SCAI_hat.ub,d$fit$ub)))
  plot(1,1,xlim=xlims,ylim=c(1,1e5),type="n",
      xlab="",ylab=paste("SCAI(",a,")",sep=""),xaxt="n",xpd=NA,log="y")
  leg.width <- strwidth("Orkney-Shetland")
  polygon(c(d$fit$year,rev(d$fit$year)),c(d$fit$lb,rev(d$fit$ub)),col="grey")
  lines(d$fit$year,d$fit$SCAI,lwd=2)
  #If a point is outside confidence region, plot its error bars -otherwise just the points
  pts.outside <- d$obs[d$obs$SCAI_hat< d$obs$SCAI$lb | d$obs$SCAI_hat >d$obs$SCAI$ub,]
  arrows(pts.outside$Year,pts.outside$SCAI_hat.lb,pts.outside$Year,pts.outside$SCAI_hat.ub,angle=90,code=3,length=0.1)
  points(d$obs$Year,d$obs$SCAI_hat,pch=pchs[as.numeric(d$obs$ind)],col="black")
  lvls <- 1:nlevels(d$obs$ind)
  legend("bottomright",col="black",pch=pchs[lvls],legend=levels(d$obs$ind),bg="white",horiz=TRUE)
  legend("topleft",legend=NA,title=sprintf("%s) %s",letters[i],a),bty="n")

}
#Add axis at the bottom
axis(1)
title(main="SCAI indices by area",outer=TRUE)
title(xlab="Year",outer=TRUE,mgp=c(2.5,1,0))


#Now plot residuals as bubbles
par(mfrow=c(4,1),mar=c(0,0,0,0),oma=c(5,5,4,2),las=1,mgp=c(4,1,0))
lapply(names(fit),function(a) {
  d <- fit[[a]]
  n.units <- nlevels(d$obs$ind)
  plot(0,0,xlim=xlims,ylim=rev(c(0.5,n.units+0.5)),
      xlab="",ylab=a,xaxt="n",yaxt="n",xpd=NA)
  axis(2,labels=levels(d$obs$ind),at=1:nlevels(d$obs$ind))
  points(d$obs$Year,as.numeric(d$obs$ind),pch=21,col=as.numeric(d$obs$ind),
      bg=ifelse(d$obs$resid<0,"black","white"),cex=2*sqrt(abs(d$obs$resid)))
  lvls <- 1:nlevels(d$obs$ind)
  box(lwd=2)
})
axis(1)
title(main="Residuals",xlab="Year",outer=TRUE)


#Now plot residuals as lines
par(mfrow=c(4,1),mar=c(0,0,0,0),oma=c(5,5,4,2),las=1,mgp=c(4,1,0))
lapply(names(fit),function(a) {
  d <- fit[[a]]
  n.units <- nlevels(d$obs$ind)
  plot(0,0,xlim=xlims,ylim=rev(c(0.5,n.units+0.5)),
      xlab="",ylab=a,xaxt="n",yaxt="n",xpd=NA)
  axis(2,labels=levels(d$obs$ind),at=1:nlevels(d$obs$ind))
  rect(as.numeric(d$obs$Year)-0.5,as.numeric(d$obs$ind),as.numeric(d$obs$Year)+0.5,
      as.numeric(d$obs$ind)-d$obs$resid*0.2,col="black")   #A positive residual should be up
  box(lwd=2)
  abline(h=1:n.units)
})
axis(1)
title(main="Residuals",xlab="Year",outer=TRUE)


#Plot model parameters for each component
par(mfrow=c(3,1),mar=c(0,0,0,0),oma=c(5,4,4,2),las=1,mgp=c(3,1,0))
rw.mu <- lapply(names(fit),function(a) {
                data.frame(component=a,subset(fit[[a]]$res,name==("mulogSCAI")))
              })
rw.mu <- transform(do.call(rbind,rw.mu),ub=value+1.96*std.dev,lb=value-1.96*std.dev)
plot(NA,NA,xlim=c(0.5,0.5+n.areas),ylim=range(pretty(c(rw.mu$lb,rw.mu$ub))),
    xlab="",ylab="RW Mean",xaxt="n",panel.first=abline(h=0,lty=2,col="grey"),xpd=NA)
points(1:n.areas,rw.mu$value,pch=19,col="black")
arrows(1:n.areas,rw.mu$lb,1:n.areas,rw.mu$ub,angle=90,code=3)
axis(1,at=1:n.areas,labels=FALSE)

rw.sd <- lapply(names(fit),function(a) {
                data.frame(component=a,subset(fit[[a]]$res,name==("logSdSCAI")))
              })
rw.sd <- transform(do.call(rbind,rw.sd),sd=exp(value),ub=exp(value+1.96*std.dev),lb=exp(value-1.96*std.dev))
plot(NA,NA,xlim=c(0.5,0.5+n.areas),ylim=range(pretty(c(0,rw.sd$ub))),
    xlab="",ylab="RW Std Dev",xaxt="n",panel.first=abline(h=0,lty=2,col="grey"),xpd=NA)
points(1:n.areas,rw.sd$sd,pch=19,col="black")
arrows(1:n.areas,rw.sd$lb,1:n.areas,rw.sd$ub,angle=90,code=3)
axis(1,at=1:n.areas,labels=FALSE)

obs.err <- lapply(names(fit),function(a) {
                data.frame(component=a,subset(fit[[a]]$res,name==("logSdObsErr")))
              })
obs.err  <- transform(do.call(rbind,obs.err),sd=exp(value),ub=exp(value+1.96*std.dev),lb=exp(value-1.96*std.dev))
plot(NA,NA,xlim=c(0.5,0.5+n.areas),ylim=range(pretty(c(0,obs.err$ub))),
    xlab="",ylab="Obs error Std Dev",xaxt="n",panel.first=abline(h=0,lty=2,col="grey"),xpd=NA)
points(1:n.areas,obs.err$sd,pch=19,col="black")
arrows(1:n.areas,obs.err$lb,1:n.areas,obs.err$ub,angle=90,code=3)
axis(1,at=1:n.areas,labels=names(areas))
title(main="SCAI model parameters",xlab="Spawning Component",outer=TRUE)


#plot the time series of standard deviations in the SCAI
par(mfrow=c(1,1),mar=c(5,4,4,2),oma=c(0,0,0,0),mgp=c(3,1,0),las=0)
dat.to.plot <- sapply(names(fit),function(a) fit[[a]]$fit$std.dev)
matplot(LAI.dat$year,dat.to.plot,xlim=xlims,ylim=range(pretty(c(0,unlist(dat.to.plot)))),yaxs="i",
      xlab="Year",ylab="Standard Deviation",type="b",
      main="Standard Deviation of SCAI indices by area and year")
legend("topleft",legend=names(areas),pch=as.character(1:n.areas),col=1:6,lty=1,bg="white")


#Plot proportions of SCAI for each sampling unit
props <- lapply(names(fit),function(a) {
                data.frame(component=a,subset(fit[[a]]$res,name==("props")))
              })
props  <- transform(do.call(rbind,props),ub=value+1.96*std.dev,lb=value-1.96*std.dev)
par(mfrow=c(1,1),mar=c(5,4,4,2),oma=c(0,0,0,0),mgp=c(3,1,0),las=0)
plot(NA,NA,xlim=c(0.5,0.5+nrow(props)),ylim=c(0,1),yaxs="i",xaxs="i",
    xlab="Sampling Units",ylab="Proportion",main="Proportion of component covered by each unit",xaxt="n")
points(1:nrow(props),props$value,pch=19,col="black")
arrows(1:nrow(props),props$lb,1:nrow(props),props$ub,angle=90,code=3,length=0.1)
axis(1,at=1:nrow(props),labels=samp.unit.names, las=2)
abline(v= cumsum(table(factor(props$component,levels=unique(props$component)))[-n.areas])+0.5,lwd=4)


#SCAIs plotted on one figure
par(mfrow=c(1,1),mar=c(5,4,4,2),oma=c(0,0,0,0),mgp=c(3,1,0),las=0)
SCAIs <- sapply(fit,function(d) d$fit$SCAI)
SCAIs <- data.frame(Year=LAI.dat$year,Total=rowSums(SCAIs),SCAIs)
dat.to.plot <- SCAIs[,-c(1:2)]
plot(NA,NA,xlim=xlims,ylim=range(pretty(c(0,unlist(dat.to.plot)))),yaxs="i",
  xlab="Year",ylab="SCAI",main="SCAI indices for each component")
matlines(SCAIs$Year,dat.to.plot,lwd=2,lty=1)
legend("topleft",col=1:6,lty=1,legend=colnames(dat.to.plot),bg="white")

#Retrospective analysis - individual values
retro.res$SCAI <- exp(retro.res$value)
retro.res$comp <- factor(retro.res$comp,names(areas))
retro.mat <- xtabs(SCAI ~ year + comp + retro, retro.res)
retro.mat[retro.mat==0] <- NA
plot(NA,NA,xlim=xlims,ylim=range(pretty(c(0,retro.res$SCAI))),yaxs="i",
  xlab="Year",ylab="SCAI",main="SCAI retrospective analysis")
for(i in 1:dim(retro.mat)[3]) {
  matlines(LAI.dat$year,retro.mat[,,i],lwd=0.5,lty=1)
}
matlines(LAI.dat$year,retro.mat[,,"0"],lwd=2,lty=1)
legend("topleft",col=1:6,lty=1,legend=dimnames(retro.mat)$comp,bg="white")


#And  on a log scale
matplot(SCAIs$Year,log10(dat.to.plot),lwd=2,lty=1,type="b",xlab="Year",ylab="log10(SCAI)",main="SCAI indices for each component")
legend("topleft",col=1:6,lty=1,legend=colnames(dat.to.plot),pch=as.character(1:4),bg="white")

#Interannual changes per component
diffs <- apply(log(SCAIs[,-1]),2,diff)
mid.yrs   <- rev(rev(SCAIs$Year+0.5)[-1])
matplot(mid.yrs,diffs[,-1],lwd=2,lty=1,type="b",xlab="Year",ylab="Growth rate (.yr-1)",
  main="Annual growth rates for each component")
lines(mid.yrs,diffs[,1],lwd=4,lty=1,col="black")
abline(h=0,lty=3,col="grey")
legend("topright",col=c(1,1:6),lty=1,lwd=c(4,rep(2,4)),legend=colnames(diffs),pch=c("",as.character(1:4)),bg="white")

#Proportion of total by each area - area plot
par(mfrow=c(1,1),mar=c(5,4,4,2),oma=c(0,0,0,0),mgp=c(3,1,0),las=0)
plot(0,0,type="n",xlim=xlims,ylim=c(0,1),yaxs="i",xaxs="i",xlab="Year",ylab="Fraction",
  main="Proportion of North Sea stock by component")
area.plot.dat <- t(apply(SCAIs[,-c(1,2)],1,function(x) 1-cumsum(c(0,x))/sum(x)))
for(i in 1:length(areas)) {
  x.to.plot <- c(SCAIs$Year,rev(SCAIs$Year))
  y.to.plot <- c(area.plot.dat[,i],rev(area.plot.dat[,i+1]))
  polygon(x.to.plot,y.to.plot,col=i)
}
legend("topleft",bg="white",legend=colnames(area.plot.dat)[-1],pch=22,pt.bg=1:4,pt.cex=2)

#Proportion of total by each area - line plot
dat.to.plot <- SCAIs[,-c(1:2)]
plot(0,0,type="n",xlim=xlims,ylim=c(0,1),yaxs="i",xaxs="i",xlab="Year",ylab="Fraction",
  main="Proportion of North Sea stock by component")
matlines(SCAIs$Year,dat.to.plot/rowSums(dat.to.plot),lwd=2,lty=1)
legend("topright",bg="white",legend=colnames(dat.to.plot),lwd=2,lty=1,col=1:6)

#Retrospective analysis - by proportion
retro.fracs <- apply(retro.mat,c(1,3),function(x) x/sum(x))
retro.fracs <- aperm(retro.fracs,c(2,1,3))
plot(0,0,type="n",xlim=xlims,ylim=c(0,1),yaxs="i",xaxs="i",xlab="Year",ylab="Fraction",
  main="Proportion of stock by component - retrospective")
for(i in 1:dim(retro.fracs)[3]) {
  matlines(LAI.dat$year,retro.fracs[,,i],lwd=0.5,lty=1)
}
matlines(LAI.dat$year,retro.fracs[,,"0"],lwd=2,lty=1)
legend("topleft",col=1:6,lty=1,legend=dimnames(retro.fracs)[[2]],bg="white")


#Close output
if(length(grep("pdf|png|wmf",names(dev.cur())))!=0) dev.off()

### ======================================================================================================
### Write out results
### ======================================================================================================
write.out <- cbind(Year=fit[[1]]$fit$year,sapply(fit,function(d) d$fit$SCAI),sapply(fit,function(d) d$fit$std.dev))
write.csv(write.out,file=file.path(output.dir,"SCAI indices.csv"),row.names=FALSE)


