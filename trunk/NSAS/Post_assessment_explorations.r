################################################################################
# NSH Post-assessment explorations
#
# $Rev: 697 $
# $Date: 2012-02-10 09:52:28 +0100 (vr, 10 feb 2012) $
#
# Author: HAWG model devlopment group
#
# Performs additional supplementary data explorations
# post assessment
#
# Developed with:
#   - R version 2.13.0
#   - FLCore 2.4
#
# To be done:
#
# Notes: Have fun running this assessment!
#
################################################################################
rm(list=ls()); graphics.off(); start.time <- proc.time()[3]
options(stringsAsFactors=FALSE)
log.msg     <-  function(string) {cat(string);}
log.msg("\nNSH Post-assessment explorations\n=====================\n")

### ============================================================================
### Setup 
### ============================================================================
#Import externals
library(FLSAM); library(reshape); library(mgcv)
load(file.path(".","results","North Sea Herring.RData"))

### ============================================================================
### Recruitment exploration
### ============================================================================
pdf(file.path("results","Post_assessment_exploration.pdf"),
    pointsize=16)
par(mar=c(5,4,2,1),mgp=c(2.5,1,0))
#Load component data
SCAI.dat <- read.csv("data/SCAI/SCAIoutputs/SCAI_indices.csv")
SCAI.dat$norSCAI <- rowSums(SCAI.dat[,c("SCAI.OrkShe","SCAI.Buchan","SCAI.Banks")])

#Extract data
xcols <- c("data","cohort")
SCAI.df <- as.data.frame(NSH.tun[["SCAI"]]@index) 
SCAI.df$cohort <- SCAI.df$year
MIK.df <- as.data.frame(NSH.tun[["IBTS0"]]@index) 
MIK.df$cohort <- MIK.df$year - 1
surv.dat <- merge(SCAI.df[,xcols],MIK.df[,xcols],by="cohort",
                  suffixes=c(".SCAI",".MIK"),all=TRUE)
n.spawners <- NSH@stock.n * NSH@mat * 
  exp(-(NSH@harvest*NSH@harvest.spwn + NSH@m*NSH@m.spwn))
spawners.ts <- as.data.frame(quantSums(n.spawners))
spawners.ts$cohort <- spawners.ts$year
spawners.ts$ssb <- as.data.frame(ssb(NSH))$data
recs.ts  <- as.data.frame(stock.n(NSH.sam)["0",])
recs.ts$cohort <- recs.ts$year -1
ass.dat <- merge(spawners.ts[,c(xcols,"ssb")],recs.ts[,xcols],by="cohort",
                 suffixes=c(".spwn",".rec"),all=TRUE)
plt.dat <- merge(ass.dat,surv.dat,all=TRUE,by="cohort") 
plt.dat$surv.ratio <- plt.dat$data.MIK / plt.dat$data.SCAI
plt.dat$rps <- plt.dat$data.rec/plt.dat$data.spwn
plt.dat$yr <- sprintf("%02i",plt.dat$cohort%%100)
plt.dat <- merge(plt.dat,SCAI.dat[,c("Year","norSCAI")],
                 by.x="cohort",by.y="Year",all=TRUE)
plt.dat$norSCAI.surv <- plt.dat$data.MIK / plt.dat$norSCAI
plt.dat$downs.frac <- 1-plt.dat$norSCAI/plt.dat$data.SCAI

#Time series of recruits-per-spawner
plot(plt.dat$cohort,plt.dat$rps,log="y",type="n",
     xlab="Spawning Year",ylab="Recruits per spawner")
#rug(plt.dat$cohort[plt.dat$ssb < 800000],col="black",lwd=2)
#rug(plt.dat$cohort[plt.dat$ssb < 800000],col="black",side=3,lwd=2)
points(plt.dat$cohort,plt.dat$rps,pch=16)
#lines(smooth.spline(plt.dat$cohort[-1],plt.dat$rps[-1],penalty=1.4),
#      col="red",lwd=2)
text(grconvertX(0,"npc"),grconvertY(0.9,"npc"),"a)",pos=4)

#SRR plot
plot(plt.dat$ssb/1e6,plt.dat$data.rec/1e6,pch=1,
     xlab="Spawning Stock Biomass (Mt)",
     ylab=expression(paste("Recruits (",10^9," individuals)")),
     log="xy") 
points(plt.dat$ssb/1e6,plt.dat$data.rec/1e6,
       pch=ifelse(plt.dat$cohort>2001,16,NA),col="red") 
points(plt.dat$ssb/1e6,plt.dat$data.rec/1e6,
       pch=ifelse(plt.dat$cohort==2012,16,NA),col="black") 
legend("bottomright",c("Post 2001","2012","Other"),
       pch=c(16,16,1),col=c("red","black","black"))

#Residuals in IBTS0
res <- subset(residuals(NSH.sam),fleet=="IBTS0")
res$cohort <- res$year -1
xlims <- range(pretty(res$cohort))
oldpar <- par(mfrow=c(2,1),mar=c(0,0,0,0),
              oma=c(5,4,4,2))
plot(res$cohort,res$std.res,type="h",xlim=xlims,
     xaxt="n",xpd=NA,xlab="",ylab="IBTS0 residual")
abline(h=0,col="grey")
points(res$cohort,res$std.res,pch=16)
abline(lm(std.res ~ cohort,res),col="red")

plot(NA,NA,ylim=c(0,1),xlim=xlims,yaxs="i",
     xlab="Cohort",ylab="Downs Fractions",xpd=NA)
with(subset(plt.dat,!is.na(plt.dat$downs.frac)),
     polygon(c(cohort,rev(cohort)),
             c(downs.frac,downs.frac*0),
             col="blue"))

#IBTSO / SCAI time series
par(oldpar)
plot(plt.dat$cohort,plt.dat$surv,log="y",pch=16,
     xlim=range(pretty(plt.dat$cohort[!is.na(plt.dat$surv.ratio)],na.rm=TRUE)),
     xlab="Spawning Year",ylab="Larval Survival (IBTS0/SCAI)")
text(grconvertX(0,"npc"),grconvertY(0.9,"npc"),"b)",pos=4)

#IBTS0 / norSCAI time series
plot(plt.dat$cohort,plt.dat$norSCAI.surv,log="y",pch=16,
     xlim=range(pretty(plt.dat$cohort[!is.na(plt.dat$surv.ratio)],na.rm=TRUE)),
     xlab="Spawning Year",ylab="Northern Component Larval Survival")

dev.off()  


### ============================================================================
### Weights at age exploration
### ============================================================================
pdf(file.path("results","Weight_anomalies.pdf"),
    pointsize=16)

#Load data
stck <- readFLStock(file.path("data", "index.txt"),no.discards=TRUE)
west <- subset(as.data.frame(stck@stock.wt),year>=1984)

#Calculate anomalies
west.stats <- melt(tapply(west$data,list(age=west$age),mean))
west.stats$mean <- west.stats$value
west.stats$value <- NULL
west.stats$sd <- tapply(west$data,west$age,sd)
west<- merge(west,west.stats,by="age")
west$anom <- (west$data-west$mean)/west$sd

#Clustering of ages
west.mat <- cast(west,age ~ year,value="anom")
rownames(west.mat) <- west.mat[,1]
west.mat <- west.mat[,-1]
d.mat <- dist(west.mat)
#plot(hclust(d.mat,method="complete"))
age.groups <- list(juveniles=0:1,adults=2:9)

#Plot
par(mfrow=c(2,1),mar=c(0,0,0,0),
    oma=c(5,4,2,1))
pred.yr <- data.frame(year=seq(min(west$year),max(west$year),len=100))
xlims <- range(pretty(pred.yr$year))
ymax <- max(pretty(abs(west$anom)))
ylims <- c(-ymax,ymax)
for(i in seq(age.groups) ) {
  plt.mat <- west.mat[as.character(age.groups[[i]]),,drop=FALSE]
  
  #Fit a gam
  mdl.dat <- melt(as.data.frame(plt.mat))
  mdl.dat$year <- as.numeric(as.character(mdl.dat$variable))
  mdl <- loess(value ~ year,data=mdl.dat,span=0.25)
  pred.dat <- predict(mdl,newdata=pred.yr)
  
  #Make plot
  plot(NA,NA,xaxt="n",xlim=xlims,ylim=ylims,bty="n",
       xlab="",ylab="")
  abline(h=0,col="grey")
  matpoints( as.numeric(colnames(plt.mat)),t(plt.mat),
      pch=rownames(plt.mat))
  lines(pred.yr$year,pred.dat,lwd=4)
  text(grconvertX(0,"npc"),grconvertY(0.9,"npc"),pos=4,
       sprintf("%s) %s",letters[i],names(age.groups)[i]))
  
}
box("inner")
axis(1)
title(xlab="Year",ylab="Standardised anomaly",xpd=TRUE,outer=TRUE)

dev.off()

### ============================================================================
### Independent recruitment estimation
### ============================================================================
# #Setup control
# freeR.ctrl <- NSH.ctrl
# freeR.ctrl@logN.vars[] <- 1
# freeR.ctrl@sam.binary <- "model/freer"
# 
# #Run freeR assessment
# freeR.sam <- FLSAM(NSH,NSH.tun,freeR.ctrl)
# 
# #Plot the two
# freeR.sams <- FLSAMs("RW rec"=NSH.sam,"Indep. rec"=freeR.sam)
# rec.ests <- rec(freeR.sams)
# xyplot(value ~ year,data=rec.ests,groups=name,
#        type="l",auto.key=TRUE)

### ============================================================================
### Survey catchability trend exploration
### ============================================================================
HERAS.q <- catchabilities(NSH.retro)
col  <- rainbow(11)
print(xyplot(value ~ age | fleet,HERAS.q,groups=name,
          auto.key=list(space="right",points=FALSE,lines=FALSE,type="l",col=col),
          scale=list(alternating=FALSE,y=list(relation="free")),as.table=TRUE,
          type="l",lwd=2,col=col,
          subset=fleet %in% c("HERAS"),
          main="Survey catchability parameters",ylab="Catchability",xlab="Age"))

### ============================================================================
### Finish
### ============================================================================
log.msg(paste("COMPLETE IN",sprintf("%0.1f",round(proc.time()[3]-start.time,1)),"s.\n\n"))

