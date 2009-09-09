######################################################################################################
# Internal consistency
#
# Version 1.00 23/07/2009
#
# Author: Christina Frisk, cfr@aqua.dtu.dk
# DTU-Aqua, Charlottenlund, DK
#
# Plots a double interannual consistency for both DATRAS and LGCP data.
#
# Developed with:
#   - R version 2.8.1
#
# Changes:
#
# To be done:
#
# Notes:
#
####################################################################################################

### ======================================================================================================
### Initialise system
### ======================================================================================================
# Start with house cleaning
rm(list = ls(all.names=TRUE)); gc(); graphics.off()
ver <- "\nDATRAS and LGCP Interannual consistency v1.00\n"; cat(ver)
ver.datetime   <- "23/07/2009 \n\n";
cat(ver.datetime); start.time <- proc.time()[3]
options(stringsAsFactors=FALSE)

### ======================================================================================================
### Load and prepare data files & Determine the quarter for the analyze
### ======================================================================================================

dat.file <- file.path("..","Data","HERAS Sprat Abundance.csv")
file.name <- file.path(".","output",paste("HERAS Internal Consistency - %02d.png",sep=""))


age.rng <- 1:2
  
# Reading in data:
dat.heras <- read.csv(dat.file)     
dat.heras <- subset(dat.heras,Year!=2003)
    
# Seperate DATRAS data to agegroups:
yr.heras <- dat.heras$Year
dat.heras <- dat.heras[,paste("Age_",1:3,sep="")]

# Data manipulation to allow for extraction of yearclasses
rcrd.heras <- data.frame(expand.grid(Year=yr.heras,Age=1:3),Idx=as.vector(as.matrix(dat.heras)))

# Calculation of the yearcrlass and adding to the data
rcrd.heras$Yearclass <- rcrd.heras$Year-rcrd.heras$Age

a.heras <- subset(rcrd.heras,rcrd.heras$Idx>0)
a.heras$LogIdx <- log(a.heras$Idx) 
  
### ======================================================================================================
### Plot
### ======================================================================================================
            
png(file.name,units = "px", height=720,width=720,pointsize = 16, bg = "white")

par(mfrow=c(1,2))
par(mar=c(3,2,1,1))
for(i in age.rng) 
{
  x.heras<-subset(a.heras,Age==i)
  y.heras<-subset(a.heras,Age==i+1)
  xy.heras<-merge(x.heras,y.heras,by="Yearclass")
  
  mdl.heras <- lm(LogIdx.y ~ LogIdx.x, data=xy.heras)
  
  
  x.rng <- c(range(xy.heras$LogIdx.x))  
  x.new <- data.frame(LogIdx.x=seq(min(x.rng),max(x.rng),length.out=100))
  pred.w.plim <- predict(mdl.heras, newdata = x.new, interval="prediction")
  linetype <- c( 1,2,2)
  matplot(x.new,pred.w.plim,type="l",col='red',lw='2',lty=linetype,xlab="",ylab="")
  
  
  points(xy.heras$LogIdx.x,xy.heras$LogIdx.y,xlab="",ylab="",pch=19,cex=1.5)
  
  subtitle <- paste("Age",i,"vs. Age",i+1)
  text(grconvertX(0.5,"npc"),grconvertY(0.95,"npc"),subtitle, cex = 1.4)          
  rsq <-  sprintf("%4.3f",round(summary(mdl.heras)$r.squared,3))
  if(summary(mdl.heras)$coef[2,4]<0.05) rsq <- paste(rsq,"*")
  text(grconvertX(0.8,"npc"),grconvertY(0.1,"npc"),bquote(r^2 == .(rsq)),
    cex=ifelse(summary(mdl.heras)$coef[2,4]<0.05,1.2,1))
  print(summary(mdl.heras))
  }                              

dev.off()



