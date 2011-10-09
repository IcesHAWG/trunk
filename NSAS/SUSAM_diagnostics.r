################################################################################
# NSH_SAM Diagnostics
#
# $Rev: 522 $
# $Date: 2011-10-03 10:19:53 +0200 (Mon, 03 Oct 2011) $
#
# Author: HAWG model devlopment group
#
# Development routine for the FLSAM diagnostics
#
# Developed with:
#   - R version 2.13.1
#   - FLCore 2.4
#
# To be done:
#
# Notes:
#
################################################################################

### ============================================================================
### Initialise system, including convenience functions and title display
### ============================================================================
rm(list=ls()); gc(); graphics.off(); start.time <- proc.time()[3]
options(stringsAsFactors=FALSE)
log.msg     <-  function(string) {
	cat(string);
 flush.console()
}
log.msg("\nNSH SAM Diagnostics developemtn\n==========================\n")

### ============================================================================
### Import externals
### ============================================================================
library(FLSAM)
data(NSH.sam)   #Load result
x <- NSH.sam

  #######################################################################
  ## Need to do:
  ##
  ## Find out how to use expression() to superscript in the labels
  ## Find nicer way to do x axis in plot 1,3,5
  ## Work on working title for plots (ttl)
  ## In legend for plot 1, how to fill in symbol?
  ## Legend intrudes on several plot 1's
  ## Need to get rid of windows() in line 83- DONE - MPA
  #######################################################################



# extracts residuals dataframe from x and drops all rows where fleet is catch
  index.res <- x@residuals[x@residuals$fleet!="catch",]


# Back transform log transformed observed and modelled to normal space
  index.res$obs <- exp(index.res$log.obs)
  index.res$mdl <- exp(index.res$log.mdl)


# split dataframe by age and survey (fleet)
  index.res.l   <-  split(index.res,list(index.res$age,index.res$fleet),drop=TRUE)


# Run through each combination of survey and age

  for (i in 1:length(index.res.l)){

# create working titel to identify plots with age and survey information
  ttl <- names(index.res.l[i])


#Scale index axes
  idx.rng       <-  range(c(index.res.l[[i]]$obs,index.res.l[[i]]$mdl),na.rm=TRUE)
  idx.lim       <-  range(c(0,idx.rng))
  idx.exp       <-  floor(log10(max(pretty(idx.lim)))/3)*3
  idx.div       <-  10^idx.exp
  idx.label     <-  paste("Index ","[","10^",idx.exp,"]",sep="")
  index.res.l[[i]]$obs <-  index.res.l[[i]]$obs/idx.div
  index.res.l[[i]]$mdl <-  index.res.l[[i]]$mdl/idx.div
  idx.rng       <-  idx.rng/idx.div
  idx.lim       <-  idx.lim/idx.div
  idx.min       <- min(c(index.res.l[[i]]$obs,index.res.l[[i]]$mdl))
  idx.max       <- max(c(index.res.l[[i]]$obs,index.res.l[[i]]$mdl))
  idx.lim       <- c(idx.min/1.1,ceiling(idx.max))


# scale for residuals axes
  res.range     <- abs(range(index.res.l[[i]]$std.res))
  res.lim       <- c(-max(res.range),max(res.range))


#Setup plots
  par(mfrow=c(3,2),las=0,oma=c(0,0,3,0),mgp=c(1.75,0.5,0),
                          mar=c(3,3,2.5,1),cex.main=1,tck=-0.01)


#plot 1 obs and mdl index time series plotted on a log scale

  plot(obs ~ year, index.res.l[[i]], log="y",ylim=idx.lim,xlab="Year", ylab=idx.label,pch=21,bg="black")
  points(mdl ~ year, index.res.l[[i]],pch=4)
  points(mdl ~ year, index.res.l[[i]],type="l")
  legend("topleft",c("Observed","Fitted"),pch=c(21,4),lty=c(NA,1),horiz=TRUE)
  title("a) Observed and fitted index time series")


# plot 2 observed against modelled directly, both axes log scale.

  plot(obs~mdl,index.res.l[[i]],log="xy",ylim=idx.lim,xlim=idx.lim,pch=21,
                      bg="black",ylab=paste("Observed ",idx.label,sep=""),
                      xlab=paste("Fitted ",idx.label,sep=""))
  title("b) Observed vs fitted values")


# Plot 3 Index residuals over time

  plot(std.res ~year, index.res.l[[i]],ylim=res.lim, ylab="Standardised Residuals", xlab="Year")
          points(std.res ~year, index.res.l[[i]], type="h")
          points(std.res ~year, index.res.l[[i]], pch=19,cex=0.75)
          abline(h=0)
  title("c) Standardised residuals over time")


# Plot 4   Standardised residuals vs Fitted index values, x-axis is log scaled

  plot(std.res ~mdl, index.res.l[[i]], ylab="Standardised Residuals", ylim=res.lim, xlim=idx.lim,
        xlab=paste("Fitted ",idx.label,sep=""),log="x")
        points(std.res ~mdl, index.res.l[[i]], pch=19,cex=0.75)
        abline(h=0)

  title("d) Index residuals vs fitted index values")


#plot 5 Normal Q-Q plot

  qqnorm(index.res.l[[i]]$std.res,ylim=res.lim,xlab="Normal Quantiles",ylab="Standardised Residuals",pch=19,main="")
  title("e) Normal Q-Q plot")

  # Add a main titel to the plots with Survey and age information
  title(main=ttl,outer=TRUE)

  }

