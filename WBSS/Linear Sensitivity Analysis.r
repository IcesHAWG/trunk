######################################################################################################
# WBSS Linear Sensitivity Analysis
#
# $Rev: 136 $
# $Date: 2009-03-13 16:52:29 +0100 (Fri, 13 Mar 2009) $
#
# Author: Mark Payne
# DTU-AQUA, Denmark
#
# Tests the sensitivity of a stock assessment to the most recent set of data points by
# performing a linear perturbation (sensitivity) analysis. Each input data point in the terminal year
# is modified by +/- a small relative amount, dev, and the assessment performed again.
# The sensitivity of key values in the assessment to this small change is then calculated
# and the results plotted. Sensitivity is defined as the ratio of relative changes ie a
# sensitivity of -0.25 of SSB to for catch in numbers at age 6 implies that a 50% increase
# in the catch in numbers at age 6 would decrease the SSB by 50% * 0.25 = 12.5%.
#
# Developed with:
#   - R version 2.8.1
#
# Changes:
# V 1.00 - Initial version
#
# To be done:
#
# Notes:
#
####################################################################################################

### ======================================================================================================
### Initialise system, including convenience functions and title display
### ======================================================================================================
rm(list=ls()); gc(); graphics.off(); start.time <- proc.time()[3]
options(stringsAsFactors=FALSE)
FnPrint     <-  function(string) {
	cat(string)
	flush.console()
}
FnPrint("\nWBSS Linear Sensitivity Analysis\n=======================================\n")

### ======================================================================================================
### Incorporate Common modules
### Uses the common HAWG FLICA Assessment module to do the graphing, diagnostics and output
### ======================================================================================================
source(file.path("..","_Common","HAWG Common assessment module.r"))
### ======================================================================================================

### ======================================================================================================
### Define parameters for use in the assessment code here
### ======================================================================================================
workspace           <-  file.path(".","res","WBSS Assessment .RData")      #Workspace containing all data
output.dir          <-  file.path(".","res")       #Output directory
devs                <-  c(-0.001,0.001,-0.002,0.002)               #Relative deviations about input value, used to estimate derivatives
linear.check        <-  TRUE    #Whether the linear approximation should be checked - execution is interrrupted if
                                #linearity is violated and the sensitivity is greater than sens.alarm
sens.alarm          <-  2e-3
sens.YR             <-  2008    #Year in which to make CANUM sensitivity analysis
TY                  <-  2008    #Terminal year of the assessment
do.canum.sens       <-  TRUE
do.idx.sens         <-  TRUE

### ======================================================================================================
### Load data, setup values
### ======================================================================================================
load(workspace)
stck <- WBSS
tun  <- WBSS.tun
ica  <- WBSS.ica
ctrl <- WBSS.ctrl
#ctrl@sep.nyr  <-  as.integer(6)
#
##Update assessment
#ica  <- FLICA(stck,tun,ctrl)
#stck <- stck +ica

### ======================================================================================================
### Generic function to calculate summary states
### ======================================================================================================
sum.stats <- function(stck.in,ica.in) {
              c(SSQ=sum(sapply(ica.in@weighted.resids,function(x) sum(x@.Data^2,na.rm=TRUE))),
                         SSB=ssb(stck.in)[,ac(c(TY-1,TY)),drop=TRUE]@.Data,
                         Fbar=fbar(stck.in)[,ac(c(TY-1,TY)),drop=TRUE]@.Data)
}

zero.pt   <- sum.stats(stck,ica)

### ======================================================================================================
### Initialise figure
### ======================================================================================================
#Initialise Figure
png(file.path(output.dir,"WBSS sensitivity analysis - %02d.png"),units = "px", height=1200,width=800,pointsize = 24, bg = "white")
trellis.par.set(fontsize=list(text=24,points=20),superpose.symbol=list(pch=16:19))

### ======================================================================================================
### Start with looping through catch
### ======================================================================================================
if(do.canum.sens) {
catch.derivs <- lapply(as.list(as.character(dims(stck)$min:dims(stck)$max)), function(age) {
                  cat(paste("CATCH SENSITIVITY:\tAge :",age,"\n"));flush.console()
                  #Perform new assessments based on deviations about the input data
                  stats.l <- lapply(as.list(devs), function(dx) {
                      tmp.stck <- stck
                      x.new  <- tmp.stck@catch.n[age,ac(sens.YR)]*(1+dx)   #Change given point by dx
                      tmp.stck@catch.n[age,ac(sens.YR)] <- x.new
                      tmp.ica  <- FLICA(tmp.stck,tun,ctrl)
                      tmp.stck <- tmp.stck + tmp.ica      #Update stock object to contain new assessment
                      #Now return summary stats
                      return(c(dx=dx,x.new=x.new,sum.stats(tmp.stck,tmp.ica)))
                  })
                  #Add the zero points
                  stats <- rbind(c(dx=0,x.new=stck@catch.n[age,ac(sens.YR)],zero.pt),
                              do.call(rbind,stats.l))
                  #Calculate the derivatives - but not of the first two columns
                  derivs <- apply(stats[,-(1:2)],2,x=stats[,"x.new"],FUN=function(y,x){
                                    #Slope of ln(y) ~ ln(x) is d(y/yo) / d(x/xo)
                                    mdl <- lm(log(y)~log(x))
                                    slope <- coef(mdl)[2]
                                    if(linear.check) {
                                      rsq <- summary(mdl)$r.squared
                                      if(rsq< 0.95 & abs(slope) > sens.alarm ) browser()
                                      cat(sprintf("R.squared : %7.5f\n",round(rsq,5)))
                                    }
                                    return(slope)
                            })
                  #And return the results
                  return(data.frame(dat="Catch.n",age=age,sum.stat=names(derivs),derivs))
                })

#Now plot the results
catch.derivs <- do.call(rbind,catch.derivs)
catch.plot <- xyplot(derivs ~ as.numeric(age) | sum.stat, data=catch.derivs,
                type="b",pch=19,
                ylab="Sensitivity",
                xlab=paste("Catch at Age",sens.YR),
                prepanel=function(...) list(ylim=range(pretty(c(0,list(...)$y)))),
                main="WBSS Sensitivity Analysis - Catch at Age in Term. Yr.",
                as.table=TRUE,
                scale=list(alternating=1,y=list(relation="free")),
                panel=function(...) {
                      panel.grid(h=-1,v=-1)
                      panel.abline(h=0,col="black",lwd=2)
                      panel.xyplot(...)
                      })
print(catch.plot)

}
### ======================================================================================================
### Now do the same with the index data
### ======================================================================================================
if(do.idx.sens) {

idx.derivs <- list()
for(idx in names(tun)) {
  #First update the TY, as it can vary from index to index
  idx.TY <- as.character(dims(tun[[idx]])$maxyear)
  #Now calculate the derivatives
  idx.derivs[[idx]] <- lapply(as.list(as.character(dims(tun[[idx]])$min:dims(tun[[idx]])$max)), function(age) {
                        cat(paste("INDEX SENSITIVITY:",idx,"\tAge :",age,"\n"));flush.console()
                        #Perform new assessments based on deviations about the input data
                        stats.l <- lapply(as.list(devs), function(dx) {
                            tmp.tun <- tun
                            x.new <- tmp.tun[[idx]]@index[age,idx.TY]*(1+dx)   #Change given point by dx
                            tmp.tun[[idx]]@index[age,idx.TY] <- x.new
                            tmp.ica  <- FLICA(stck,tmp.tun,ctrl)
                            tmp.stck <- stck + tmp.ica      #Update stock object to contain new assessment
                            #Now return summary stats
                            return(c(dx=dx,x.new=x.new,sum.stats(tmp.stck,tmp.ica)))
                        })
                        #Add the zero points
                        stats <- rbind(c(dx=0,x.new=tun[[idx]]@index[age,idx.TY],zero.pt),
                                      do.call(rbind,stats.l))
                        #Calculate the derivatives - but not of the first two columns
                        derivs <- apply(stats[,-(1:2)],2,x=stats[,"x.new"],FUN=function(y,x){
                                      #Slope of ln(y) ~ ln(x) is d(y/yo) / d(x/xo)
                                      mdl <- lm(log(y)~log(x))
                                      slope <- coef(mdl)[2]
                                      if(linear.check) {
                                        rsq <- summary(mdl)$r.squared
                                        if(rsq< 0.95 & abs(slope) > sens.alarm ) browser()
                                        cat(sprintf("R.squared : %7.5f\n",round(rsq,5)))
                                      }
                                      return(slope)
                                  })
                        #And return the results
                        return(data.frame(dat=idx,age=age,sum.stat=names(derivs),derivs))
                      })
}

#Now make the index plots
idx.dat <- do.call(rbind,do.call(c,idx.derivs))
idx.plot <- xyplot(derivs ~ as.numeric(age) | sum.stat, data=idx.dat,
                groups=dat,
                type="b",
                main="WBSS Sensitivity Analysis - Index Values in Term. Yr.",
                ylab="Sensitivity",
                xlab=paste("Index Age",sens.YR),
                prepanel=function(...) list(ylim=range(pretty(c(0,list(...)$y)))),
                auto.key=list(space="bottom",type="b",points=FALSE,lines=TRUE,columns=length(tun)),
                as.table=TRUE,
                scale=list(alternating=1,y=list(relation="free")),
                panel=function(...) {
                      panel.grid(h=-1,v=-1)
                      panel.abline(h=0,col="black",lwd=2)
                      panel.xyplot(...)
                      })
print(idx.plot)
}


### ======================================================================================================
### Finish Up
### ======================================================================================================
dev.off()
FnPrint(paste("COMPLETE IN",sprintf("%0.1f",round(proc.time()[3]-start.time,1)),"s.\n\n"))