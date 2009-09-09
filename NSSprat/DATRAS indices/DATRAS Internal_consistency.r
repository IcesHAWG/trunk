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
# Reads in data based on the quarter. Quarter is changed in the first line below:
#       Q1: First quarter
#       Q3: Third quarter

for(Q in c("Q1","Q3")){
switch(Q,
      Q1 = {qrt <- 1
            start.year <- 1984
            age.rng <- 1:4},
      Q3 = {qrt <- 3
            start.year <- 1991
            age.rng <- 0:3})



dat.file <- file.path("..","Data","DATRAS Sprat total indices by age.csv")
file.name <- file.path(".","output",paste("IBTS Q",qrt," Internal Consistency - %02d.png",sep=""))
  
# Reading in DATRAS data:
dat.raw <- read.csv(dat.file)     
    
# Cleaning DATRAS data to only incl. NS sprat
dat.datras    <- subset(dat.raw,Year>=start.year & Quarter==qrt & IndexArea=="NS_SpratIV")

# Seperate DATRAS data to agegroups:
yr.datras <- dat.datras$Year
dat.datras <- dat.datras[,paste("Age_",0:6,sep="")]
#expand.grid(Year=yr,Age=0:6)

# Data manipulation to allow for extraction of yearclasses
rcrd.datras <- data.frame(expand.grid(Year=yr.datras,Age=0:6),Idx=as.vector(as.matrix(dat.datras)))

# Calculation of the yearcrlass and adding to the data
rcrd.datras$Yearclass <- rcrd.datras$Year-rcrd.datras$Age

a.datras <- subset(rcrd.datras,rcrd.datras$Idx>0)
a.datras$LogIdx <- log(a.datras$Idx) 
  
### ======================================================================================================
### Plot
### ======================================================================================================
png(file.name,units = "px", height=720,width=720,pointsize = 16, bg = "white")
par(mfrow=c(2,2))
par(mar=c(3,3,0.5,1))
for(i in age.rng) 
{
  x.datras<-subset(a.datras,Age==i)
  y.datras<-subset(a.datras,Age==i+1)
  xy.datras<-merge(x.datras,y.datras,by="Yearclass")
  
  mdl.datras <- lm(LogIdx.y ~ LogIdx.x, data=xy.datras)
  
  
  x.rng <- c(range(xy.datras$LogIdx.x))  
  x.new <- data.frame(LogIdx.x=seq(min(x.rng),max(x.rng),length.out=100))
  pred.w.plim <- predict(mdl.datras, newdata = x.new, interval="prediction")
  linetype <- c( 1,2,2)
  matplot(x.new,pred.w.plim,type="l",col='red',lw='2',lty=linetype,xlab = "",ylab="")#,lty=c(1,2,2,3,3))
  
  
  points(xy.datras$LogIdx.x,xy.datras$LogIdx.y,xlab="",ylab=" ",pch=19)
  
  subtitle <- paste("Age",i,"vs. Age",i+1)
  text(grconvertX(0.5,"npc"),grconvertY(0.95,"npc"),subtitle, cex = 1.4)          #("Age",i+1,"vs. Age",1)
  rsq <-  sprintf("%4.3f",round(summary(mdl.datras)$r.squared,3))
  if(summary(mdl.datras)$coef[2,4]<0.05) rsq <- paste(rsq,"*")
  text(grconvertX(0.8,"npc"),grconvertY(0.1,"npc"),bquote(r^2 == .(rsq)),
    cex=ifelse(summary(mdl.datras)$coef[2,4]<0.05,1.2,1))

  }

summary(mdl.datras)


dev.off()

}