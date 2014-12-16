#--------------------------------------------------------------
#
# Code to run the NSAS assessment using the SCAI data
#
#
# By: Niels Hintzen & Mark Payne
# Developed: Nov-Dec 2014
#
#--------------------------------------------------------------
#setwd("D:/Repository/HAWG/HAWGrepository/NSAS/samIHLS/TMB/")

library(TMB)

#- Load the NSAS data including SCAI observations
source("NSAS_ssamDAT.r")

compile("ssam.cpp")
dyn.load(dynlib("ssam"))

#- Compile the TMB code and load the dll
parameters <- list(
  logFpar=c(-0.13092145, 0.02071598, 0.11170518, -8.86860036, -12.78393203),
  logQpow=numeric(0),
  logSdLogFsta=c(-0.58193222, -1.28597707, -1.19805909, -0.61124060),
  logSdLogN=c(-0.63652721, -1.74489832),
  logSdLogObs=c(-0.68165532, -1.76252445, -1.57001726, -0.98154029, -1.66667440, -1.35423683, -1.19664715, -0.97958921),
  rec_loga=1,
  rec_logb=-12,
  rho=0.5,
  logScale=numeric(data$noScaledYears),
  logScaleSSB=-4.43797750,
  logPowSSB=numeric(0),
  logSdSSB=-0.80761185,
  U=matrix(0, nrow=max(data$keyLogFsta)+1 + data$maxAge-data$minAge+1,ncol=data$noYears)
)
#- Provide starting point for states
Us                              <- unlist(read.csv("../Us.csv",header=F))
Us                              <- matrix(Us,nrow=17,ncol=68)
parameters$U[1:17,]             <- Us
#- Add dimensions of states to data fill
data$nlogF=max(data$keyLogFsta)+1
data$nlogN=data$maxAge-data$minAge+1
data$nlogP=4

#- Create optimization code and gradients
# Estimation without correlation between random walks
obj                             <- MakeADFun(data,parameters,random=c("U"),REPORT=1,DLL="sam_NTH",
                                             map=list(rec_loga=as.factor(NA),rec_logb=as.factor(NA),rho=as.factor(NA)))
#- Set upper and lower limits of parameters to estimate
lower                           <- obj$par*0-Inf
upper                           <- obj$par*0+Inf
lower["rho"]                    <- 0.01
upper["rho"]                    <- 0.99

#- Perform optimization
system.time(opt<-nlminb(obj$par,obj$fn,obj$gr,lower=lower,upper=upper))
rep<-sdreport(obj)
rep
