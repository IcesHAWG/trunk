#setwd("D:/Repository/HAWG/HAWGrepository/NSAS/samIHLS/TMB/")

#- Estimating only alpha's, no props

source("NSAS_LAIDat_V2.r")

library(TMB)
compile("sam_NTH_alpha.cpp")
dyn.load(dynlib("sam_NTH_alpha"))

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
  logAlphaSCB=log(c(0.5,0.5,0.25,0.25,0.25,0.33,0.33)),
  U=matrix(0, nrow=max(data$keyLogFsta)+1 + data$maxAge-data$minAge+1,ncol=data$noYears)
)
Us  <- unlist(read.csv("../Us.csv",header=F))
Us <- matrix(Us,nrow=17,ncol=68)
parameters$U[1:17,] <- Us
data$nlogF=max(data$keyLogFsta)+1
data$nlogN=data$maxAge-data$minAge+1
data$nlogP=4

obj <- MakeADFun(data,parameters,random=c("U"),REPORT=1,DLL="sam_NTH_alpha",
                 map=list(rec_loga=as.factor(NA),rec_logb=as.factor(NA),rho=as.factor(NA)))
lower <- obj$par*0-Inf
upper <- obj$par*0+Inf
lower["rho"] <- 0.01
upper["rho"] <- 0.99

system.time(opt<-nlminb(obj$par,obj$fn,obj$gr,lower=lower,upper=upper))
rep<-sdreport(obj)
rep

#---------------

source("NSAS_LAIDat_V2.r")

library(TMB)
#compile("sam_NTH_tLAI.cpp")
compile("sam_NTH_LAI_V3.cpp")
dyn.load(dynlib("sam_NTH_LAI_V3"))

parameters <- list(
  logFpar=c(-0.13092145, 0.02071598, 0.11170518, -8.86860036, -12.78393203),
  logQpow=numeric(0),
  logSdLogFsta=c(-0.58193222, -1.28597707, -1.19805909, -0.61124060),
  logSdLogN=c(-0.63652721, -1.74489832),
  logSdLogP=c(-1),
  logSdLogObs=c(-0.68165532, -1.76252445, -1.57001726, -0.98154029, -1.66667440, -1.35423683, -1.19664715, -0.97958921),
  rec_loga=1,
  rec_logb=-12,
  rho=0.5,
  rhop=0.5,
  logScale=numeric(data$noScaledYears),
  logScaleSSB=-4.43797750,
  logPowSSB=numeric(0),
  logSdSSB=-0.80761185,
  logAlphaSCB=log(rep(1,7)),
  U=matrix(0, nrow=max(data$keyLogFsta)+1 + data$maxAge-data$minAge+1 + 3,ncol=data$noYears)
  #U=matrix(0, nrow=max(data$keyLogFsta)+1 + data$maxAge-data$minAge+1,ncol=data$noYears),
  #P=matrix(0,nrow=3,ncol=length(1972:2014))
)
Us  <- unlist(read.csv("../Us.csv",header=F))
Us <- matrix(Us,nrow=17,ncol=68)
parameters$U[1:17,] <- Us
Ps  <- unlist(read.csv("../props.csv",header=T)[,2])
parameters$U[18:20,26:65] <- log(matrix(Ps,nrow=40,ncol=4,byrow=T)[,2:4])
parameters$U[18:20,1:25]  <- log(0.25)
parameters$U[18:20,66:68]  <- log(0.25)
parameters$U              <- c(parameters$U[1:17,1:25],parameters$U[,26:68])
data$nlogF=max(data$keyLogFsta)+1
data$nlogN=data$maxAge-data$minAge+1
data$nlogP=4

#obj <- MakeADFun(data,parameters,random=c("U","P"),REPORT=1,DLL="sam_NTH_tLAI",
#                 map=list(rec_loga=as.factor(NA),rec_logb=as.factor(NA),rho=as.factor(NA),rhop=as.factor(NA)))
obj <- MakeADFun(data,parameters,random=c("U"),REPORT=1,DLL="sam_NTH_LAI_V2",
                 map=list(rec_loga=as.factor(NA),rec_logb=as.factor(NA),rho=as.factor(NA),rhop=as.factor(NA)))

lower <- obj$par*0-Inf
upper <- obj$par*0+Inf
lower["rho"] <- 0.01
upper["rho"] <- 0.99
lower["rhop"] <- 0.01
upper["rhop"] <- 0.99

system.time(opt<-nlminb(obj$par,obj$fn,obj$gr,lower=lower,upper=upper))
rep<-sdreport(obj)
rep


load("D:/Repository/HAWG/HAWGrepository/NSAS/results/North Sea Herring.RData")

NFs  <- matrix(rep$par.random[c(1:(17*25),c(mapply(seq,seq(426,1285,20),seq(426+16,1285,20))))],nrow=17,ncol=68)
Ps   <- matrix(rep$par.random[c(mapply(seq,seq(426+17,1285,20),seq(426+19,1285,20)))],nrow=3,ncol=43)
Props<- matrix(NA,nrow=4,ncol=43)
for(i in 1:43){
  totprop <- sum(exp(Ps[,i]))
  Props[2:4,i] <- exp(Ps[,i]) / (1+totprop)
  Props[1,i] <- 1- totprop / (1+totprop)
}
AlphaPars <- rep$par.fixed[which(names(rep$par.fixed)=="logAlphaSCB")]
Alpha<- numeric(11)
Alpha[2] <- exp(AlphaPars[1])/(1+exp(AlphaPars[1]))
Alpha[1] <- 1- exp(AlphaPars[1])/(1+exp(AlphaPars[1]))
Alpha[4] <- exp(AlphaPars[2])/(1+exp(AlphaPars[2]))
Alpha[3] <- 1- exp(AlphaPars[2])/(1+exp(AlphaPars[2]))
totAlpha <- sum(exp(AlphaPars[3:5]))
Alpha[6:8] <- exp(AlphaPars[3:5])/(1+totAlpha)
Alpha[5] <- 1 - totAlpha/(1+totAlpha)
totAlpha <- sum(exp(AlphaPars[6:7]))
Alpha[10:11] <- exp(AlphaPars[6:7])/(1+totAlpha)
Alpha[9] <- 1 - totAlpha/(1+totAlpha)



plot(c(exp(opt$par)[1:22],Alpha),ylim=c(0,2),pch=19,col=2,xlab="Parameters estimated",ylab="Value")
text(x=c(1:(length(opt$par[1:22])+11)),y=c(exp(opt$par[1:22]),Alpha),pos=3,offset=2,labels=c(names(opt$par[1:22]),rep("alphaSCB",11)),srt=90,cex=0.8)




matplot(t(Props),type="l",lwd=2,lty=1,ylim=c(0,1))
legend("topleft",legend=c("OrkShe","Buchan","Banks","Downs"),col=1:4,lty=1,lwd=2)

Props    <- Props[4:1,]
PropsCum <- apply(Props,2,cumsum)
plot(y=t(PropsCum[1,]),x=1972:2014,col="white",ylim=c(0,1),xlab="Years",ylab="Fraction")
polygon(x=c(1972:2014,2014:1972),y=c(rep(0,43),   rev(PropsCum[1,])),col=4,fill=T)
polygon(x=c(1972:2014,2014:1972),y=c(PropsCum[1,],rev(PropsCum[2,])),col=3,fill=T)
polygon(x=c(1972:2014,2014:1972),y=c(PropsCum[2,],rev(PropsCum[3,])),col=2,fill=T)
polygon(x=c(1972:2014,2014:1972),y=c(PropsCum[3,],rev(PropsCum[4,])),col=1,fill=T)
legend("topleft",legend=c("OrkShe","Buchan","Banks","Downs"),col=1:4,lty=1,lwd=2,bg="white")



NSH.LAI                   <- NSH
NSH.LAI@stock.n[]         <- exp(NFs[1:9,-68])
NSH.LAI@harvest[ac(0:7),] <- exp(NFs[10:17,-68])
NSH.LAI@harvest[ac(8),]   <- exp(NFs[17,-68])

plot(FLStocks("SCAI" = NSH,"LAI"=NSH.LAI))

predObs <- sweep(sweep(Props[c(1,1,2,2,3,3,3,3,4,4,4),-43],1,Alpha,"*"),2,
                 (ssb(NSH.LAI[,ac(1972:2013)]) * exp(rep$par.fixed[which(names(rep$par.fixed)=="logScaleSSB")]))[,drop=T],"*")
predObs <- as.data.frame.table(predObs)
predObs$Var1 <- rep(1:11,42)
predObs$Var2 <- rep(1972:2013,each=11)
colnames(predObs) <- c("age","year","pred")
                 
obs     <- subset(as.data.frame(data$obs),fleet==2)
obs$age <- obs$age +1
tot <- merge(obs,predObs,by=c("year","age"),all=T)

par(mfrow=c(3,4),mar=rep(0.5,4),oma=c(4,4,2,2))
for(i in 1:11){
  subdat <- subset(tot,age==i)
  plot(y=log(subdat$obs),x=subdat$year,pch=19,xaxt="n",yaxt="n")
  lines(y=log(subdat$pred),x=subdat$year,lty=2)
}
mtext(side=2,at=0.66+0.33/2,outer=T,line=1,text="OrkShe & Bunchan")
mtext(side=2,at=0.33+0.33/2,outer=T,line=1,text="Banks")
mtext(side=2,at=0.33/2,outer=T,line=1,text="Downs")
