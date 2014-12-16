#--------------------------------------------------------------
#
# Code to run the NSAS assessment using the 'raw' LAI data
#  rather than the SCAI or MLAI
#
# By: Niels Hintzen & Mark Payne
# Developed: Nov-Dec 2014
#
#--------------------------------------------------------------

#setwd("D:/Repository/HAWG/HAWGrepository/NSAS/samIHLS/TMB/")
library(TMB)

#- Load the NSAS data including LAI observations
source("NSAS_slam.r")

#- Compile the TMB code and load the dll
compile("slam.cpp")
dyn.load(dynlib("slam"))

#- Setup the parameters to estimate
parameters <- list(
  logFpar=c(-0.13092145, 0.02071598, 0.11170518, -8.86860036, -12.78393203),    # Catchabilities of surveys (not LAI survey)
  logQpow=numeric(0),
  logSdLogFsta=c(-0.58193222, -1.28597707, -1.19805909, -0.61124060),           # Variances of F random walks
  logSdLogN=c(-0.63652721, -1.74489832),                                        # Variances of N survival analyses
  logSdLogP=rep(-1,4),                                                          # Variances of P random walk
  logSdLogObs=c(-0.68165532, -1.76252445, -1.57001726, -0.98154029,             # Variances of observations
                -1.66667440, -1.35423683, -1.19664715, -0.97958921),
  rec_loga=1,                                                                   # Rec parameter to estimate
  rec_logb=-12,                                                                 # Rec parameter to estimate
  rho=0.5,                                                                      # Correlation parameter of F random walks
  rhop=0.5,                                                                     # Correlation parameter of P random walks
  logScale=numeric(data$noScaledYears),                                         # Years to scale
  logScaleSSB=-4.43797750,                                                      # Catchability of SSB survey (LAI)
  logPowSSB=numeric(0),                                                         # Power function in SSB survey (not used)
  logSdSSB=rep(-1,4),#-0.80761185,                                              # Variances of LAI observations
  logAlphaSCB=log(rep(1,7)),                                                    # Survey contributions per component
  U=matrix(0, nrow=max(data$keyLogFsta)+1 + data$maxAge-data$minAge+1 + 3,      # States to estimate
              ncol=data$noYears)
)
#- Provide starting point for states
Us                                    <- unlist(read.csv("../Us.csv",header=F))
Us                                    <- matrix(Us,nrow=17,ncol=68)
parameters$U[1:17,]                   <- Us
Ps                                    <- unlist(read.csv("../props.csv",header=T)[,2])
parameters$U[18:20,26:65]             <- log(matrix(Ps,nrow=40,ncol=4,byrow=T)[,2:4])
parameters$U[18:20,1:25]              <- log(0.25)
parameters$U[18:20,66:68]             <- log(0.25)
parameters$U                          <- c(parameters$U[1:17,1:25],parameters$U[,26:68])
#- Add dimensions of states to data fil
data$nlogF=max(data$keyLogFsta)+1
data$nlogN=data$maxAge-data$minAge+1
data$nlogP=4

#- Create optimization code and gradients
# Estimation without correlation between random walks
obj                                   <- MakeADFun(data,parameters,random=c("U"),REPORT=1,DLL="slam",
                                                   map=list(rec_loga=as.factor(NA),rec_logb=as.factor(NA),rho=as.factor(NA)))
# Estimation with correlation betweeen random walks
obj                                   <- MakeADFun(data,parameters,random=c("U"),REPORT=1,DLL="slam",
                                                   map=list(rec_loga=as.factor(NA),rec_logb=as.factor(NA),rho=as.factor(NA)))

#- Set upper and lower limits of parameters to estimate
lower <- obj$par*0-Inf
upper <- obj$par*0+Inf
lower["rho"]                          <- 0.01
upper["rho"]                          <- 0.99
lower["rhop"]                         <- 0.01
upper["rhop"]                         <- 0.99

#- Perform optimization
system.time(opt<-nlminb(obj$par,obj$fn,obj$gr,lower=lower,upper=upper))

#- Estimate states
rep<-sdreport(obj)
rep

#-------------------------------------------------------------------------------
# Figures and diagnostics (very quick and dirty)
#-------------------------------------------------------------------------------

#- Retreive Ns, Fs and Props
NFs             <- matrix(rep$par.random[c(1:(17*25),c(mapply(seq,seq(426,1285,20),seq(426+16,1285,20))))],nrow=17,ncol=68)
Ps              <- matrix(rep$par.random[c(mapply(seq,seq(426+17,1285,20),seq(426+19,1285,20)))],nrow=3,ncol=43)
Props           <- matrix(NA,nrow=4,ncol=43)
for(i in 1:43){
  totprop       <- sum(exp(Ps[,i]))
  Props[2:4,i]  <- exp(Ps[,i]) / (1+totprop)
  Props[1,i]    <- 1- totprop / (1+totprop)
}
#- Retreive Alphas
AlphaPars       <- rep$par.fixed[which(names(rep$par.fixed)=="logAlphaSCB")]
Alpha           <- numeric(11)
Alpha[2]        <- exp(AlphaPars[1])/(1+exp(AlphaPars[1]))
Alpha[1]        <- 1- exp(AlphaPars[1])/(1+exp(AlphaPars[1]))
Alpha[4]        <- exp(AlphaPars[2])/(1+exp(AlphaPars[2]))
Alpha[3]        <- 1- exp(AlphaPars[2])/(1+exp(AlphaPars[2]))
totAlpha        <- sum(exp(AlphaPars[3:5]))
Alpha[6:8]      <- exp(AlphaPars[3:5])/(1+totAlpha)
Alpha[5]        <- 1 - totAlpha/(1+totAlpha)
totAlpha        <- sum(exp(AlphaPars[6:7]))
Alpha[10:11]    <- exp(AlphaPars[6:7])/(1+totAlpha)
Alpha[9]        <- 1 - totAlpha/(1+totAlpha)


#- Plot all parameters
pars            <- exp(opt$par); idx <- which(names(opt$par) %in% c("rho","rhop","logAlphaSCB")); pars[idx] <- opt$par[idx]
pars            <- pars[-which(names(opt$par) %in% "logAlphaSCB")]; pars <- c(pars,"logAlphaSCB"=Alpha)
plot(pars,ylim=c(0,2),pch=19,col=2,xlab="Parameters estimated",ylab="Value")
text(x=1:length(pars),y=pars,pos=3,offset=2,labels=c(names(pars)),srt=90,cex=0.8)

#- Plot the proportions over time
matplot(y=t(Props),x=1972:2014,type="l",lwd=2,lty=1,ylim=c(0,1),ylab="Proportions",xlab="Years")
legend("topleft",legend=c("OrkShe","Buchan","Banks","Downs"),col=1:4,lty=1,lwd=2)

Props           <- Props[4:1,]
PropsCum        <- apply(Props,2,cumsum)
plot(y=t(PropsCum[1,]),x=1972:2014,col="white",ylim=c(0,1),xlab="Years",ylab="Fraction")
polygon(x=c(1972:2014,2014:1972),y=c(rep(0,43),   rev(PropsCum[1,])),col=4)
polygon(x=c(1972:2014,2014:1972),y=c(PropsCum[1,],rev(PropsCum[2,])),col=3)
polygon(x=c(1972:2014,2014:1972),y=c(PropsCum[2,],rev(PropsCum[3,])),col=2)
polygon(x=c(1972:2014,2014:1972),y=c(PropsCum[3,],rev(PropsCum[4,])),col=1)
legend("topleft",legend=c("OrkShe","Buchan","Banks","Downs"),col=1:4,lty=1,lwd=2,bg="white")

#- Compare original NSAS run with LAI run
load("D:/Repository/HAWG/HAWGrepository/NSAS/results/North Sea Herring.RData")
NSH.LAI                   <- NSH
NSH.LAI@stock.n[]         <- exp(NFs[1:9,-68])
NSH.LAI@harvest[ac(0:7),] <- exp(NFs[10:17,-68])
NSH.LAI@harvest[ac(8),]   <- exp(NFs[17,-68])
plot(FLStocks("SCAI" = NSH,"LAI"=NSH.LAI))

#- Predicted ~ Observed
predObs           <- sweep(sweep(Props[c(1,1,2,2,3,3,3,3,4,4,4),-43],1,Alpha,"*"),2,
                                (ssb(NSH.LAI[,ac(1972:2013)]) * exp(rep$par.fixed[which(names(rep$par.fixed)=="logScaleSSB")]))[,drop=T],"*")
predObs           <- as.data.frame.table(predObs)
predObs$Var1      <- rep(1:11,42)
predObs$Var2      <- rep(1972:2013,each=11)
colnames(predObs) <- c("age","year","pred")
obs               <- subset(as.data.frame(data$obs),fleet==2)
obs$age           <- obs$age +1
tot               <- merge(obs,predObs,by=c("year","age"),all=T)

library(lattice)
tot$age[which(tot$age == "1")] <- "B1"
tot$age[which(tot$age == "2")] <- "B2"
tot$age[which(tot$age == "3")] <- "C1"
tot$age[which(tot$age == "4")] <- "C2"
tot$age[which(tot$age == "5")] <- "D1"
tot$age[which(tot$age == "6")] <- "D2"
tot$age[which(tot$age == "7")] <- "D3"
tot$age[which(tot$age == "8")] <- "D4"
tot$age[which(tot$age == "9")] <- "E1"
tot$age[which(tot$age == "10")]<- "E2"
tot$age[which(tot$age == "11")]<- "E3"

xyplot(pred+obs ~ year | age,data=tot,
  ylab="Value",
  panel=function(...){
    res <- list(...)
    idx  <- which(res$x == 1972)
    idxp <- 1:(idx[2]-1)
    idxo <- idx[2]:length(res$x)
    panel.grid(h=-1,v=-1)
    panel.xyplot(x=res$x[idxo],y=res$y[idxo],pch=19,type="p",col=2)
    panel.lines(x=res$x[idxp],y=res$y[idxp],col=1,lwd=2)
   })

