################################################################################
# Code to perform Herring assessment calling function FLICA                    #
#                                                                              #
# Project leader: WOT - Frans van Beek / Mark Dickey-Collas                    #
# Author: Niels Hintzen & Hans Bogaards                                        #
# 27-02-2008                                                                   #
# first version: HAWG_assessment_1.1.r                                         #
################################################################################

rm(list=ls())
flush.console()

#install.packages(repos="http://www.flr-project.org/R")
library(FLCore)
library(FLAssess)
library(FLSTF)
library(FLICA)
#library(FLEDA)

path <- "N:\Projecten\ICES WG\Haring werkgroep HAWG\2009\assessment\rcode"
if (!exists("path")) { print("No path selected. Current directory will be used."); path <- getwd()}
setwd(path)
source("FLICA v 5.00.r") #updated version from the FLICA package
source("Write.tbl v 5.01.r") #code to write the tables for in the annex
source("FLStock plot v 2.10.r") #code to make plots from a FLStock object  (e.g. nsh)
source("FLStocks plot v 2.20.r") #code to make plots from a FLStocks object (e.g. nsh.retro)
source("FLICA diagnostics v 4.00.r") #code to run the diagnostics + plot from the ica output
source("Retro.func v 2.99.r") #updated retro function to be able to have extra years in the indices
source("SSB v 2.00.r") #updated quantSums function to deal with NA SSB's
source("private diagnostics.r")
source("Write.FLStock v 3.05.r")
source("cohort-retrospective v 1.03.r")
source("SSB-Yield_curves.r")

#Read all stock data
nsh <- readFLStock(file=paste(path,"input07/index.txt",sep=""),no.discards=T,quiet=T)
nsh@range["plusgroup"] <- nsh@range["max"] 

nsh@catch.n <- nsh@landings.n
nsh@catch.wt <- nsh@landings.wt 

#Read in tuning series
nsh.tun <- readFLIndices(paste(path,"input07/fleet.txt",sep=""),paste(path,"input07/ssb.txt",sep=""),type="ICA")
nsh.tun[[1]]@type <- "number"
nsh.tun[[2]]@type <- "number"
nsh.tun[[3]]@type <- "number"
nsh.tun[[3]]@range["plusgroup"] <- NA

### change order in such a way that the MLAI is first (needed for the ICA)
nsh.tun <- rev(nsh.tun)
if (nsh.tun[[1]]@name != "MLAI") print("Error - MLAI not as the first index")
### give 1/weighting factors as variance
nsh.tun[[4]]@index.var[] <- 1.0/FLQuant(c(0.63,0.62,0.17,0.10,0.09,0.08,0.07,0.07,0.05),dimnames=dimnames(nsh.tun[[4]]@index)) #Acoustic
nsh.tun[[3]]@index.var[] <- 1.0/FLQuant(c(0.47,0.28,0.01,0.01,0.01),dimnames=dimnames(nsh.tun[[3]]@index)) #IBTS
nsh.tun[[2]]@index.var[] <- 1.0/FLQuant(0.63,dimnames=dimnames(nsh.tun[[2]]@index)) #MIK
nsh.tun[[1]]@index.var[] <- 1.0/FLQuant(0.60,dimnames=dimnames(nsh.tun[[1]]@index)) #MLAI

#Setup FLICA control object
nsh.ctrl <- FLICA.control(sep.nyr=5, sep.age=4, sep.sel=1.0, sr.age=0, sr=TRUE,
                          lambda.age=c(0.1, 0.1, 3.67, 2.87, 2.23, 1.74, 1.37, 1.04, 0.94, 0.91),
                          lambda.yr=c(1.0, 1.0, 1.0, 1.0, 1.0),
                          lambda.sr=0.1,
                          index.model=c("l","l","l","p"), index.cor=FALSE)  #index model: Acoustic, IBTS, MIK, MLAI
### don't forget to reorder the index models too!!                          
nsh.ctrl@index.model <- rev(nsh.ctrl@index.model)
                  
#Do the assessment
nsh.ica  <- FLICA(nsh, nsh.tun, nsh.ctrl)
nsh      <- nsh+nsh.ica
nsh.orig <- nsh
nsh.ssb  <- ssb(nsh,na.rm=F)
nsh@stock[1,] <- colSums(nsh@stock.n*nsh@stock.wt)

#Run historical retrospective and add new recruitment in a year ahead
nsh.retro     <- retro2(nsh.orig,nsh.tun,nsh.ctrl,retro=0,year.range=seq(1998,nsh@range[["maxyear"]],1)) #extra.indices.yrs(MLAI,MIK,IBTS,Acoustic)
nsh.retro.ica <- retro2(nsh.orig,nsh.tun,nsh.ctrl,year.range=seq(1998,nsh@range[["maxyear"]],1),retro=0,return.FLStocks=FALSE) #extra.indices.yrs(MLAI,MIK,IBTS,Acoustic)
nsh.retro.orig      <- nsh.retro
nsh.retro.ica.orig  <- nsh.retro.ica
for (i in as.character(1998:nsh@range[["maxyear"]])) { nsh.retro[[i]] <- window(nsh.retro[[i]],1960,as.numeric(i)+1); nsh.retro[[i]]@stock.n[1,as.character(as.numeric(i)+1)] <- nsh.retro.ica[[i]]@param["Recruitment prediction","Value"]}

#Adding units to the stock objects
units(nsh)[1:17]  <- as.list(c(rep(c("tonnes","thousands","kg"),4), "NA", "NA", "f", "NA", "NA"))    
nsh.retro         <- lapply(nsh.retro, function(x) { units(x)[1:17] <- as.list(c(rep(c("tonnes","thousands","kg"),4), "NA", "NA", "f", "NA", "NA")); x})
units(nsh.ica)[1:11] <- as.list(c(rep(NA,3),"f","NA","NA","f","NA","thousands","thousands","f"))
nsh.retro.ica     <- lapply(nsh.retro.ica, function(x) { units(x)[1:11] <- as.list(c(rep(NA,3),"f","NA","NA","f","NA","thousands","thousands","f")); x})
nsh.retro         <- do.call(FLStocks,nsh.retro)
names(nsh.retro)  <- as.list(seq(1998,nsh@range[["maxyear"]],1))

#Compute recruitments in the year ahead by projecting forward
TAC         <- 200
REC         <- nsh.ica@param["Recruitment prediction","Value"]
nsh.stf     <- FLSTF.control(fbar.min=2,fbar.max=6,nyrs=1,catch.constraint=TAC,f.rescale=TRUE,rec=REC)
nsh.stock08 <- FLSTF(stock=nsh,control=nsh.stf,unit=1,season=1,area=1,survivors=NA,quiet=TRUE,sop.correct=FALSE)
nsh.stock.tot <- window(nsh,1960,2008)
nsh.stock.tot@stock.n[,"2008"] <- nsh.stock08@stock.n[,"2008"]
nsh <- nsh.stock.tot

#Generate output - but set directories first
dir.create(paste(getwd(),"/output07",sep=""))
setwd(paste(getwd(),"/output07",sep=""))

#Write interesting tables from the assessment
#write.tbl(nsh@catch,filename="catch.txt",append=FALSE,title="Stock numbers",round=0)
#write.tbl(nsh@catch.n,filename="catch.n.txt",append=FALSE,title="Stock numbers",round=0)
#write.tbl(nsh@landings.n,filename="landings.n.txt",append=FALSE,title="Stock numbers",round=0)
#write.tbl(nsh@landings,filename="landings.txt",append=FALSE,title="Stock numbers",round=0)
#write.tbl(nsh@stock.n,filename="stock.n.txt",append=FALSE,title="Stock numbers",round=0)
#write.tbl(nsh@stock,filename="stock.txt",append=FALSE,title="Stock numbers",round=0)
#write.tbl(nsh.tun,filename="indices.txt",append=FALSE,title="Indices")

write.ica.out(nsh,nsh.tun,nsh.ica,filename="ica.out.txt",f.ages=list(c(0:1),c(2:6)), tsb=TRUE,width=75,table.number="Table 2.6.2.",digits=5)
write.FLStock(nsh.orig,filename="NSH.txt")
write.FLStock(nsh,filename="NSH.txt",slots="stock.n")


#Make interesting graphs from the assessment
write.to.pdf          <-  TRUE
pause.between.graphs  <-  TRUE
file.output           <- c("wmf")
 
if (file.output == "pdf") {
graphics.off()
if(write.to.pdf) {
  pdf("diagnostics.pdf",height=297/25.4,width=210/25.4,pointsize=16)
  new.window  <- FALSE
} else {
  par(ask=pause.between.graphs)
  new.window  <-  !pause.between.graphs     #If not pausing, then open a new window for each graph
}
}

if (file.output == "wmf") {
graphics.off()
if(write.to.pdf) {
  win.metafile("diagnostics%02d.wmf",height=297/25.4,width=210/25.4,pointsize=16)
  new.window  <- FALSE
} else {
  par(ask=pause.between.graphs)
  new.window  <-  !pause.between.graphs     #If not pausing, then open a new window for each graph
}
}
#Stock summary - all options
if(new.window) dev.set(1)
plot(nsh,f.ages=list(c(2:6),c(0:1)),blim=8e5,bpa=13e5,fpa=c(0.25,0.12),show.grid=TRUE)
title(main="Stock summary for North Sea Herring")

#Retrospective - classical style
if(new.window) dev.set(1)                    
plot(nsh.retro,f.ages=2:6,retro.style=FALSE,blim=8e5,bpa=13e5,flim=0.25,col=c(rep("black",length(seq(1998,nsh.orig@range[["maxyear"]],1))-1),"red"),show.legend=F,lty=rep(1,length(seq(1998,nsh.orig@range[["maxyear"]],1))),lwd=rep(1,length(seq(1998,nsh.orig@range[["maxyear"]],1))),type=rep("l",length(seq(1998,nsh.orig@range[["maxyear"]],1))))                                                         
title("Analytical retrospective North Sea Herring")

if(new.window) dev.set(1)
plot(nsh@stock.n[1,as.character(1961:2008)]~ssb(nsh)[,as.character(1960:2007)],type="b",col="black",lwd=1,lty=2,pch=1,xlab="SSB [tonnes]",ylab="Recruits [thousands]",ylim=c(0,108e6))
text(ssb(nsh)[,as.character(1960:2007)],nsh@stock.n[1,as.character(1961:2008)],labels=seq(1960,2007,1),col="black",cex=0.5,adj=c(-0.5,0.75))
title(main="North Sea Herring stock to recruits")

#Now examine the diagnostics of the FLICA output - first the catch residuals
for (year in as.character(2004:2007)){
if(new.window) dev.set(1)
plot(nsh.retro.ica[[year]])
mtext(paste("Retro catch diagnostics for NSHerring ",year,sep=""), side = 3, line = 2,cex=1.5,font=1.8)
}

if(new.window) dev.set(1)
selectivity.patterns(nsh.retro.ica)

if(new.window) dev.set(1)
for (year in as.character(2006:2007)) {
par(mfrow=c(2,2))
  for (j in 1:4) {
    bubble.nh(nsh.retro.ica[[year]]@index.res[[j]]*(1/nsh.tun[[j]]@index.var[,as.character(as.numeric(dimnames(nsh.retro.ica[[year]]@index.res[[j]])$year[1]):as.numeric(dimnames(nsh.retro.ica[[year]]@index.res[[j]])$year[length(dimnames(nsh.retro.ica[[year]]@index.res[[j]])$year)]))]))
    title(main=names(nsh.retro.ica[[year]]@index.range[j]))
  }
  par(mfrow=c(1,1))
  mtext(text=paste("Retrospective survey residuals  ",year,sep=""),side = 3, line = 2.75,cex=1.5,font=1.8)
}

#And finally the more detailed diagnostics
if(new.window) dev.set(1)
FLICA.diagnostics(nsh.ica)

#plot banana plot on its own
if(new.window) dev.set(1)
plot.banana(nsh.orig,nsh.ica,f.ages=c(2:6),n=5000)

#plot otolith (bootstrap of parameters)
if(new.window) dev.set(1)
plot.otolith(nsh.orig,nsh.ica,f.ages=c(2:6),n=100000)

#plot cohort retrospective analyses
if(new.window) dev.set(1)
retro.cohort(nsh.retro.orig,nsh.retro.ica.orig,2007,yearclasses=c(0:9),f.ages=c(2,6))

if(new.window) dev.set(1)
par(mfrow=c(1,1))
SSB_Yield(stock=nsh.orig,year=2007,alpha=13.91,betha=1.55e6,sep.age=4,Btrigger=1.3e6,Blim=0.8e6)

if(write.to.pdf) {
  dev.off()                 #Close writing to pdf
  shell.exec(file.path(getwd(),"diagnostics.pdf"))    #Open file
}

