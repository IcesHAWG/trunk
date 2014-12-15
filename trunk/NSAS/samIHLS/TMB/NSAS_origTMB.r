setwd("D:/Repository/HAWG/HAWGrepository/NSAS/samIHLS/TMB/")
source("NSAS_origDat.r")

library(TMB)
compile("sam_NTH.cpp")
dyn.load(dynlib("sam_NTH"))

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
Us  <- unlist(read.csv("../Us.csv",header=F))
Us <- matrix(Us,nrow=17,ncol=68)
parameters$U[1:17,] <- Us
data$nlogF=max(data$keyLogFsta)+1
data$nlogN=data$maxAge-data$minAge+1
data$nlogP=4

obj <- MakeADFun(data,parameters,random=c("U"),REPORT=1,DLL="sam_NTH",
                 map=list(rec_loga=as.factor(NA),rec_logb=as.factor(NA),rho=as.factor(NA)))
lower <- obj$par*0-Inf
upper <- obj$par*0+Inf
lower["rho"] <- 0.01
upper["rho"] <- 0.99

system.time(opt<-nlminb(obj$par,obj$fn,obj$gr,lower=lower,upper=upper))
rep<-sdreport(obj)
rep

plot(exp(opt$par),ylim=c(0,2),pch=19,col=2,xlab="Parameters estimated",ylab="Value")
text(x=c(1:(length(opt$par))),y=c(exp(opt$par)),pos=3,offset=2,labels=c(names(opt$par)),srt=90,cex=0.8)
