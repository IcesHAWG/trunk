################################################################################
# Code to do the short term forecast for WBSS herring
#
# By: Niels Hintzen
# IMARES
# 20 March 2011
#
################################################################################

rm(list=ls());

#-Load the libraries needed
library(MASS)#7.2-47
library(minpack.lm)

#- Perhaps, set a path here
path <- "N:/Projecten/ICES WG/Haring werkgroep HAWG/2011/assessment/WBSS/"
try(setwd(path))
source(file.path("..","_Common","HAWG Common assessment module.r"))
try(setwd("./stf/"))
path  <- getwd()

#- Load the assessment output objects
stk     <- WBSS
stk.ica <- WBSS.ica


#===============================================================================
# SETTINGS
#===============================================================================

#-Year objects
DtY   <- ac(range(WBSS)["maxyear"]) #Data year
ImY   <- ac(an(DtY)+1)              #Intermediate year
FcY   <- ac(an(DtY)+2)              #Forecast year
CtY   <- ac(an(DtY)+3)              #Continuation year

#-Source functions and input data
source(paste(path,"/functions.r",sep=""))
source(paste(path,"/readStfData.r",sep=""))


rec.years                           <- (range(WBSS)["maxyear"]-5):(range(WBSS)["maxyear"]-1)  #Years to take the geometric mean of the recruitment for
propTACinIV                         <- 0.5                                                    #Proportion of the IIIa TAC taken in IV
TACusage                            <- list("A"=c(1,1,1),"C"=c(1,NA,NA),"D"=c(0.4479259,NA,NA),"F"=c(1,NA,NA)) #TAC D fleet usage is based on 3-year average catches/TAC


#- TACS
#   A-fleet: take last years catch of WBSS in IVaE transfer area from the 'Wonderful Dankert' table
#   C-fleet: TAC in IIIa - outtake TAC in the IVa,b from EU-norway negotiations
#   D-fleet: Bycatch ceiling * 3-year average usage: From EU-norway negotiations
#   F-fleet: Quota for F fleet

TACS        <- list("A"=c(770*TACusage$A[1],770,770),   "C"=c(30000*propTACinIV*TACusage$C[1],NA,NA),
                    "D"=c(6659*TACusage$D[1],NA,NA),    "F"=c(15884*TACusage$F[1],NA,NA));
TACS.orig   <- list("A"=c(770,770,770),                 "C"=c(30000,NA,NA),
                    "D"=c(6659,NA,NA),                  "F"=c(15884,NA,NA))

RECS        <- list("DtY"=exp(apply(log(rec(WBSS)[,ac(rec.years)]),6,mean)),"ImY"=exp(apply(log(rec(WBSS)[,ac(rec.years)]),6,mean)),
                    "FcY"=exp(apply(log(rec(WBSS)[,ac(rec.years)]),6,mean)),"CtY"=exp(apply(log(rec(WBSS)[,ac(rec.years)]),6,mean)))

#- get the partial Fs and partial Weights by fleet and quarter (see readStfData.r)
FS          <- list("A"=FA,"C"=FC,"D"=FD,"F"=FF)
WS          <- list("A"=WA,"C"=WC,"D"=WD,"F"=WF)

#- Biology settings: Take last years or 3-year average estimate for the intermediate, forecasted and continuation year
yrs1        <- list("m","m.spwn","harvest.spwn","stock.n","mat")
yrs3        <- list("stock.wt")
#- These values come from the partial N's and partial F's solely based on the assessment year results
yrs0        <- list("catch","catch.n","catch.wt","landings.wt","landings","landings.n","stock","stock.n","harvest")

dsc         <- "Western Baltic Spring Spawning Herring"
nam         <- "WBSS"
dms         <- dimnames(stk@m)
dms$year    <- c(rev(rev(dms$year)[1:3]),ImY,FcY,CtY)
dms$unit    <- c("A","C","D","F")
dms$season  <- c("Q1","Q2","Q3","Q4")

f01         <- ac(0:1)
f36         <- ac(3:6)

stf.options <- c("mp","-15%","+15%","nf","bpa","tacro","fmsy") #mp=according to management plan, +/-% = TAC change, nf=no fishing, bpa=reach BPA in CtY or if ssb > bpa fish at fpa,
                                                        # tacro=same catch as last year

#===============================================================================
# Setup stock file
#===============================================================================

stf         <- FLStock(name=nam,desc=dsc,m=FLQuant(NA,dimnames=dms))
units(stf)  <- units(stk)
# Fill slots that are the same for all fleets
for(i in c(unlist(yrs1),unlist(yrs3))){
  if(i %in% unlist(yrs1)) slot(stf,i)[] <- slot(stk,i)[,DtY]
  if(i %in% unlist(yrs3)) slot(stf,i)[] <- apply(slot(stk,i)[,ac((an(DtY)-2):an(DtY))],1,mean,na.rm=T)
}
# Fill slots with historic data where appropriate
for(i in c(unlist(yrs0))){
  slot(stf,i)[,dms$year,] <- NA
  for(iYr in ac(dms$year[1]:DtY)){
    slot(stf,i)[,iYr,]      <- slot(stk,i)[,iYr]
  }
}

# Fill the historic data slots with the fleet based historic data
stf@harvest[,ac(an(DtY)-c(2,1,0))]          <- aperm(sweep(sweep(numbers,c(1,4),apply(numbers,c(1,4),sum,na.rm=T),"/"),c(1,4),stk@harvest[,ac(an(DtY)-c(2,1,0))],"*"),c(1,4,2,3))
stf@catch.n[,ac(an(DtY)-c(2,1,0)),,]        <- aperm(numbers,c(1,4,2,3))
stf@landings.n[,ac(an(DtY)-c(2,1,0)),,]     <- aperm(numbers,c(1,4,2,3))
stf@catch.wt[,ac(an(DtY)-c(2,1,0)),,]       <- aperm(weights,c(1,4,2,3))
stf@landings.wt[,ac(an(DtY)-c(2,1,0)),,]    <- aperm(weights,c(1,4,2,3))

# Fill the future data slots with the averaged data
for(i in dms$unit){
  for(j in dms$season){
    stf@harvest[,ac(c(ImY,FcY,CtY)),i,j]    <- FS[[i]][,which(j==dms$season)]  #Harvest pattern, not absolute harvest
    stf@catch.wt[,ac(c(ImY,FcY,CtY)),i,j]   <- WS[[i]][,which(j==dms$season)]
    stf@landings.wt[,ac(c(ImY,FcY,CtY)),i,j]<- WS[[i]][,which(j==dms$season)]
  }
}
stf@catch                                       <- computeCatch(stf)
stf@landings                                    <- computeLandings(stf)
# We don't use discards, so fill these slots
stf@discards.n[]  <- 0
stf@discards.wt[] <- 0

#===============================================================================
# From 4 quarters to 1 year setup
#===============================================================================

stfY                                    <- stf[,,,1]
stfY@catch.n[,ac(an(DtY)-c(2,1,0))]     <- seasonSums(stf@catch.n[,ac(an(DtY)-c(2,1,0))],na.rm=T)
stfY@landings.n[,ac(an(DtY)-c(2,1,0))]  <- seasonSums(stf@catch.n[,ac(an(DtY)-c(2,1,0))],na.rm=T)
for(iYr in ac(c(ImY,FcY,CtY))){
  stfY@harvest[,iYr]     <- seasonSums(stf@harvest[,ac(DtY)],na.rm=T)
  stfY@catch.wt[,iYr]    <- seasonSums(stf@catch.wt[,ac(c(DtY))] * yearSums(stf@catch.n[,ac(an(DtY)-c(2,1,0))]),na.rm=T) / seasonSums(yearSums(stf@catch.n[,ac(an(DtY)-c(2,1,0))]),na.rm=T)  #You could also take the weighted mean here
  stfY@landings.wt[,iYr] <- stfY@catch.wt[,iYr]
}
#===============================================================================
# Use the split ogive
#===============================================================================

#- Split ogive is read in 'readStfData' code

#===============================================================================
# Intermediate year
#===============================================================================

stk@stock.n[1,DtY]          <- RECS$DtY
stfY@stock.n[,ImY]          <- survivors(RECS$ImY,stk[,DtY])
stfY@stock[,ImY]            <- computeStock(stfY[,ImY])

#- We split the TAC into WBSS and NSAS catches, because the rescaling should only be based on WBSS, because partial F's are only based on WBSS
WBSSTAC                     <- apply(sweep(yearMeans(stfY@catch.wt[,ac(an(DtY)-c(2,1,0))] * stfY@catch.n[,ac(an(DtY)-c(2,1,0))] / splt),3,
                                    (unlist(TACS)[seq(1,12,3)] / apply(yearMeans(stfY@catch.wt[,ac(an(DtY)-c(2,1,0))] * stfY@catch.n[,ac(an(DtY)-c(2,1,0))] / splt),2:6,sum,na.rm=T)),"*")*
                                     splt,2:6,sum,na.rm=T)

stfY@harvest[,ImY]          <- fleet.harvest(stk=stfY,iYr=ImY,TACS=c(WBSSTAC))
for(i in dms$unit){
  stfY@catch.n[,ImY,i]      <- stfY@stock.n[,ImY,i]*(1-exp(-unitSums(stfY@harvest[,ImY])-stfY@m[,ImY,i]))*(stfY@harvest[,ImY,i]/(unitSums(stfY@harvest[,ImY])+stfY@m[,ImY,i]))
  stfY@catch[,ImY,i]        <- computeCatch(stfY[,ImY,i])
  stfY@landings.n[,ImY,i]   <- stfY@catch.n[,ImY,i]
  stfY@landings[,ImY,i]     <- computeLandings(stfY[,ImY,i])
}


#Intermediate year stf option table
stf.table         <- matrix(NA,nrow=2,ncol=16,dimnames=list("options"=c("intermediate year",
                            "fmsy"),
                            "values"=c("Fbar 3-6 A","Fbar 3-6 C","Fbar 0-1 D","Fbar 3-6 F","Fbar 3-6","Fbar 0-1",
                            "Catch A","Catch C","Catch D","Catch F","SSB","SSB",
                            "TAC A","TAC C","TAC D","TAC F")))
stf.table[1,1:11] <- c(round(c(apply(stfY@harvest[f36,ImY,c("A","C")],3,mean),mean(stfY@harvest[f01,ImY,c("D")]),apply(stfY@harvest[f36,ImY,c("F")],3,mean),mean(apply(stfY@harvest[f36,ImY],1,sum,na.rm=T)),mean(apply(stfY@harvest[f01,ImY],1,sum,na.rm=T))),3),
                       round(computeCatch(stfY[,ImY]),0),
                       round(sum(stfY@stock.n[,ImY,1]*stfY@stock.wt[,ImY,1]*exp(-apply(stfY@harvest[,ImY],1,sum)*stfY@harvest.spwn[,ImY,1]-stfY@m[,ImY,1]*stfY@m.spwn[,ImY,1])*stfY@mat[,ImY,1]),0))

ImYCatch          <- sum(stfY@catch[,ImY])
#===============================================================================
# Forecast year
#===============================================================================

stfY@stock.n[,FcY] <- c(RECS$FcY,(stfY@stock.n[,ImY,1]*exp(-apply(stfY@harvest[,ImY],1,sum)-stfY@m[,ImY,1]))[ac(range(stf)["min"]:(range(stf)["max"]-2)),],
                       sum((stfY@stock.n[,ImY,1]*exp(-apply(stfY@harvest[,ImY],1,sum)-stfY@m[,ImY,1]))[ac((range(stf)["max"]-1):range(stf)["max"]),]))


if("fmsy" %in% stf.options){
  #- Set F target based on Fmsy transition period
  Ftarget                     <- Fmsytrans(mean(apply(stfY@harvest[f36,ac(ImY)],1,sum)),Fmsy=0.25,Fpa=NA,SSB=stf.table[1,11],Bpa=110000,transPoints <- c(0.6,0.4))

  stfY@harvest[,FcY]          <- fleetCDF.harvest(stfY,FcY,c(WBSSTAC)[1],Ftarget)
  totalCatch                  <- unitSums(harvestCatch(stfY,FcY)[,,c("C","D","F")])

  CDfleetShare      <- yearMeans(stfY@catch[,ac(an(DtY)-c(2,1,0)),"C"]) / (yearMeans(stfY@catch[,ac(an(DtY)-c(2,1,0)),"D"]) + yearMeans(stfY@catch[,ac(an(DtY)-c(2,1,0)),"C"]))
  shareByFleet      <- list("A"=c(WBSSTAC)[1],"C"=0.5*totalCatch*CDfleetShare,"D"=0.5*totalCatch*(1-CDfleetShare),"F"=0.5*totalCatch)

  TACS$A[2]         <- shareByFleet$A
  TACS$C[2]         <- shareByFleet$C
  TACS$D[2]         <- shareByFleet$D
  TACS$F[2]         <- shareByFleet$F

  stfY@harvest[,FcY]         <- fleet.harvest(stfY,FcY,unlist(TACS)[seq(2,12,3)])
  for(i in dms$unit){
    stfY@catch.n[,FcY,i]     <- stfY@stock.n[,FcY,i]*(1-exp(-unitSums(stfY@harvest[,FcY])-stfY@m[,FcY,i]))*(stfY@harvest[,FcY,i]/(unitSums(stfY@harvest[,FcY])+stfY@m[,FcY,i]))
    stfY@catch[,FcY,i]       <- computeCatch(stfY[,FcY,i])
    stfY@landings.n[,FcY,i]  <- stfY@catch.n[,FcY,i]
    stfY@landings[,FcY,i]    <- computeLandings(stfY[,FcY,i])
  }

  ssb.CtY                   <- sum(c(RECS$CtY,(stfY@stock.n[,FcY,1]*exp(-apply(stfY@harvest[,FcY],1,sum)-stfY@m[,FcY,1]))[ac(range(stfY)["min"]:(range(stfY)["max"]-2)),],
                                   sum((stfY@stock.n[,FcY,1]*exp(-apply(stfY@harvest[,FcY],1,sum)-stfY@m[,FcY,1]))[ac((range(stfY)["max"]-1):range(stfY)["max"]),]))
                                   *stfY@stock.wt[,FcY,1]*stfY@mat[,FcY,1]*exp(-apply(stfY@harvest[,FcY],1,sum)*stfY@harvest.spwn[,FcY,1]-stfY@m[,FcY,1]*stfY@m.spwn[,FcY,1]))
  stf.table["fmsy",1:12]    <- c(round(c(mean(stfY@harvest[f36,FcY,"A"]),mean(stfY@harvest[f36,FcY,"C"]),mean(stfY@harvest[f01,FcY,"D"]),mean(stfY@harvest[f36,FcY,"F"]),
                                         mean(unitSums(stfY@harvest[f36,FcY])),mean(unitSums(stfY@harvest[f01,FcY]))),3),
                                 round(harvestCatch(stfY,FcY),0),
                                 round(c(sum(stfY@stock.n[,FcY,1]*stfY@stock.wt[,FcY,1]*exp(-unitSums(stfY@harvest[,FcY])*stfY@harvest.spwn[,FcY,1]-stfY@m[,FcY,1]*stfY@m.spwn[,FcY,1])*stfY@mat[,FcY,1]),
                                 ssb.CtY),0))
                                 
  stf.table["fmsy",13:16]   <-