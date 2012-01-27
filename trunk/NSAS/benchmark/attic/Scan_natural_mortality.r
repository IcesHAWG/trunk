################################################################################
# Scan natural mortality
#
# $Rev: 630 $
# $Date: 2012-01-18 10:22:41 +0100 (wo, 18 jan 2012) $
#
# Author: HAWG model devlopment group
#
# Embed new natural mortality estimates into the assessment and perform a run.
#
# Developed with:
#   - R version 2.13.2
#   - FLCore 2.4
#
# To be done:
#
# Notes: Have fun running this assessment!
#
################################################################################

### ============================================================================
### Read in the new M's
### ============================================================================

  M2            <- read.csv(file.path(".","data","SMoothedM_NotExtrapolated_NSAS.csv"),header=T)
  colnames(M2)  <- sub("X","",colnames(M2))
  rownames(M2)  <- M2[,1]
  M2            <- M2[,-1] #Trim off first column as it contains 'ages'
  M2            <- M2[,apply(M2,2,function(x){all(is.na(x))==F})]

### ============================================================================
### Modify the default assessment
### ============================================================================

  NSHM2       <- NSH
  NSHM2@m[]   <- NA
  yrs         <- dimnames(NSHM2@m)$year
  yrs         <- yrs[which(yrs %in% colnames(M2))]
  NSHM2@m[,yrs][] <- as.matrix(M2)
  
  #- Apply 5 year running average
  extryrs <- dimnames(NSHM2@m)$year[which(!dimnames(NSHM2@m)$year %in% yrs)]
  ages    <- dimnames(NSHM2@m)$age
  extrags <- names(which(apply(M2,1,function(x){all(is.na(x))==T})==T))
  yrAver  <- 5
  for(iYr in as.numeric(rev(extryrs))){
    for(iAge in ages[!ages%in%extrags]){
      NSHM2@m[ac(iAge),ac(iYr)] <- yearMeans(NSHM2@m[ac(iAge),ac((iYr+1):(iYr+yrAver)),],na.rm=T)
    }
  }
  for(iAge in extrags)
    NSHM2@m[ac(iAge),]          <- NSHM2@m[ac(as.numeric(min(sort(extrags)))-1),]

  #Plot
  source(file.path(".","benchmark","vectorise.r"))
  pdf(file.path(".","benchmark","resultsSAM","Scan_natural_mortality.pdf"))
  MSmooth           <- vectorise(NSHM2@m)[,1:3]
  colnames(MSmooth) <- c("Value","Age","Year")
  MSmooth$Age       <- as.factor(MSmooth$Age)
  MSmooth$Year      <- as.numeric(MSmooth$Year)
  print(xyplot(Value~Year|Age,data=MSmooth,type="l",xlab="Years",ylab="Total Natural Mortality",
         prepanel=function(...) {list(ylim=range(c(0,list(...)$y*1.05)))},main="North Sea herring smoothed M",
         panel=function(...){
          dat <- list(...)
          panel.grid(h=-1, v= -1)
          panel.xyplot(...)
         },
         scales=list(alternating=1,y=list(relation="free",rot=0))))



### ============================================================================
### Modify the default assessment
### ============================================================================

  NSHM2.sam   <- FLSAM(NSHM2,NSH.tun,NSH.ctrl)
  NSHM2       <- NSHM2 + NSHM2.sam

  save(NSHM2,NSHM2.sam,NSH,NSH.sam,NSH.tun,NSH.ctrl, file=file.path(".","benchmark","resultsSAM","Scan_natural_mortality.RData"))

### ============================================================================
### Analyse and plot the results
### ============================================================================
NSHM.sams         <- FLSAMs(NSH.sam,NSHM2.sam)
names(NSHM.sams)  <- c("fixed","variable")
NSHM.stcks        <- FLStocks(NSH,NSHM2)
names(NSHM.stcks) <- paste(names(NSHM.stcks),c("fixed","variable"))

#Extract AICs
M.AICs        <- AIC(NSHM.sams)
M.nlogl       <- lapply(NSHM.sams,nlogl)

#Plot
par(mfrow=c(1,2))
plot(M.AICs,main="Natural mortality scan",ylab="AIC",xaxt="n",xlab="Model",pch=16)
axis(1,labels=names(M.AICs),at=seq(M.AICs))
plot(unlist(M.nlogl),ylab="LogLikelihood",xaxt="n",xlab="Model",pch=16,col="red")
axis(1,labels=names(M.nlogl),at=seq(M.nlogl))
print(plot(NSHM.stcks,main="Natural mortality scan",key=TRUE))
dev.off()

### ============================================================================
### Compare results
### ============================================================================
log.msg(paste("COMPLETE IN",sprintf("%0.1f",round(proc.time()[3]-start.time,1)),"s.\n\n"))
