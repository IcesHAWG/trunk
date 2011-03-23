################################################################################
# Code to do the multifleet short term forecast for North Sea Herring
#
# By: Niels Hintzen
# IMARES
# 20 March 2011
#
################################################################################
rm(list=ls());
library(minpack.lm)
#Read in data
path <- "N:/Projecten/ICES WG/Haring werkgroep HAWG/2011/assessment2/NSAS/"
try(setwd(path))
source(file.path("../","_Common","HAWG Common assessment module.r"))
try(setwd("./stf/"))


stk     <- NSH
stk.ica <- NSH.ica
#===============================================================================
# Setup control file
#===============================================================================

DtY   <- ac(range(stk)["maxyear"]) #Data year
ImY   <- ac(an(DtY)+1) #Intermediate year
FcY   <- ac(an(DtY)+2) #Forecast year
CtY   <- ac(an(DtY)+3) #Continuation year
FuY   <- c(ImY,FcY,CtY)#Future years


#- For the TAC's there is some special things going on. The A-fleet, you can get the TAC from the agreed management plan between Norway-EU. However, there might be catches from Norway from the C-fleet
#  which they are allowed to take in the North Sea. So, substract these catches (which are a percentage of their total C-fleet quota) from the total C-fleet quota and move that into the North Sea
#  Then split the C-fleet and D-fleet into parts from the WBSS and NSH
#  The B-fleet is the bycatch in the North Sea
#  Where to find the information: TAC A-Fleet: Agreement EU-Norway North Sea --> table
#                                 TAC B-Fleet: Agreement EU-Norway North Sea --> in text
#                                 TAC C-Fleet: Agreement EU-Norway Kattegat - Skaggerak --> table
#                                 TAC D-Fleet: Agreement EU-Norway Kattegat - Skaggerak --> in text
#                                 Transfer C-Fleet to A-fleet: Agreement EU-Norway Kattegat - Skaggerak --> in text, and combination with TAC of C-Fleet of Norway
#  Split the C-Fleet and D-Fleet into the WBSS and NSH fraction of the TAC (after taking the transfer from Norway from the C-Fleet)
#  Split information comes from the 'Wonderful Table'
#  Split is based on parts taken in the history
#  TAC's for the C and D fleet for the Forecast and Continuation year come from the Danish. Or:
#  Compute the fraction of NSH caught by the C fleet from the total C fleet catch (wonderful table), the same for the D-fleet
#  Then calculate the proportion of the C fleet that was catching WBSS. Then take the forecasted TAC and
#  multiply it by the C fleet share in WBSS, then divide it by 1-fraction NSH and then times the fraction of NSH. This gives you the TAC's for the C and D Fleet
#  However, this might be changed by the Danish

#- Source the code to read in the weights at age and numbers at age by fleet, to compute partial F's and partial weights
source("./data/readStfData.r")
source("./data/writeSTF.out.r")

#2009 retrospect: TACS      <- list("A"=c(168400,NA,NA),"B"=c(9800,NA,NA),"C"=c(5100,4300,4300),"D"=c(1500,980,980));
#                 TACS.orig <- list("A"=c(168400,NA,NA),"B"=c(9800,NA,NA),"C"=c(5100,4300,4300),"D"=c(1500,980,980))
# RECS <-  list("ImY"=32832169,"FcY"=32832169)
#2009: TACS  <- list("A"=c(194233,NA,NA),"B"=c(7310,NA,NA),"C"=c(6538,5400,5400),"D"=c(2701,2200,2200));
#      TACS.orig <- list("A"=c(171000,NA,NA),"B"=c(15985,NA,NA),"C"=c(37722,5100,5100),"D"=c(8373,2000,2000))
#2010:
#TACS      <- list("A"=c(164300+903,NA,NA),  "B"=c(8329,NA,NA),  "C"=c(4300,1680,1680),"D"=c(980,490,490));
#TACS.orig <- list("A"=c(164300,NA,NA),      "B"=c(13587,NA,NA), "C"=c(4300,1680,1680),"D"=c(980,490,490))
#2011:
TACS      <- list("A"=c(200000+15000,NA,NA),"B"=c(16539*0.668,NA,NA), "C"=c(3875,6810,6810),"D"=c(1723,1938,1938));
TACS.orig <- list("A"=c(200000,NA,NA),      "B"=c(16539,NA,NA),       "C"=c(3875,6810,6810),"D"=c(1723,1938,1938))

RECS  <- list("ImY"=NSH.ica@param["Recruitment prediction","Value"],"FcY"=exp(mean(log(rec(NSH)[,ac((range(NSH)["maxyear"]-8):(range(NSH)["maxyear"]))]))),
              "CtY"=exp(mean(log(rec(NSH)[,ac((range(NSH)["maxyear"]-8):(range(NSH)["maxyear"]))]))))

FS    <- list("A"=FA,"B"=FB,"C"=FC,"D"=FD)
WS    <- list("A"=WA,"B"=WB,"C"=WC,"D"=WD)

yrs1  <- list("m","m.spwn","harvest.spwn","stock.wt")
yrs3  <- list("mat")

dsc   <- "North Sea Herring"
nam   <- "NSH"
dms   <- dimnames(stk@m)
dms$year <- c(rev(rev(dms$year)[1:3]),ImY,FcY,CtY)
dms$unit <- c("A","B","C","D")

f01   <- ac(0:1)
f26   <- ac(2:6)

stf.options <- c("mp","-15%","+15%","nf","tacro") #mp=according to management plan, +/-% = TAC change, nf=no fishing, bpa=reach BPA in CtY or if ssb > bpa fish at fpa,
                                                        # tacro=same catch as last year
mp.options  <- c("i") #i=increase in B fleet is allowed, fro=B fleet takes fbar as year before
#===============================================================================
# Setup stock file
#===============================================================================

stf         <- FLStock(name=nam,desc=dsc,m=FLQuant(NA,dimnames=dms))
for(i in dms$unit) stf[,,i] <- window(NSH,start=an(dms$year)[1],end=rev(an(dms$year))[1])
units(stf)  <- units(stk)
# Fill slots that are the same for all fleets
for(i in c(unlist(yrs1),unlist(yrs3))){
  if(i %in% unlist(yrs1)) slot(stf,i)[,FuY] <- slot(stk,i)[,DtY]
  if(i %in% unlist(yrs3)) slot(stf,i)[,FuY] <- apply(slot(stk,i)[,ac((an(DtY)-2):an(DtY))],1,mean,na.rm=T)
}
# Fill slots that are unique for the fleets
for(i in dms$unit){
  stf@harvest[,FuY,i]    <- FS[[i]]
  stf@catch.wt[,FuY,i]   <- WS[[i]]
  stf@landings.wt[,FuY,i]<- WS[[i]]
}
# Fill slots that have no meaning for NSAS
stf@discards.n[]        <- 0
stf@discards[]          <- 0
stf@discards.wt[]       <- 0

#===============================================================================
# Intermediate year
#===============================================================================

stf@stock.n[,ImY]     <- stk.ica@survivors
stf@harvest[,ImY]     <- fleet.harvest(stk=stf,iYr=ImY,TACS=unlist(TACS)[seq(1,12,3)])
for(i in dms$unit){
  stf@catch.n[,ImY,i]     <- stf@stock.n[,ImY,i]*(1-exp(-unitSums(stf@harvest[,ImY])-stf@m[,ImY,i]))*(stf@harvest[,ImY,i]/(unitSums(stf@harvest[,ImY])+stf@m[,ImY,i]))
  stf@catch[,ImY,i]       <- computeCatch(stf[,ImY,i])
  stf@landings.n[,ImY,i]  <- stf@catch.n[,ImY,i]
  stf@landings[,ImY,i]    <- computeLandings(stf[,ImY,i])
}


#Intermediate year stf option table
stf.table <- matrix(NA,nrow=c(length(stf.options)+1),ncol=12,dimnames=list("options"=c("intermediate year",stf.options),
                    "values"=c("Fbar 2-6 A","Fbar 0-1 B","Fbar 0-1 C","Fbar 0-1 D","Fbar 2-6","Fbar 0-1","Catch A","Catch B","Catch C","Catch D","SSB","SSB")))
stf.table[1,1:11] <- c(round(c(mean(stf@harvest[f26,ImY,"A"]),apply(stf@harvest[f01,ImY,c("B","C","D")],3,mean),mean(apply(stf@harvest[f26,ImY],1,sum,na.rm=T)),mean(apply(stf@harvest[f01,ImY],1,sum,na.rm=T))),3),
                       round(computeCatch(stf[,ImY]),0),
                       round(sum(stf@stock.n[,ImY,1]*stf@stock.wt[,ImY,1]*exp(-apply(stf@harvest[,ImY],1,sum)*stf@harvest.spwn[,ImY,1]-stf@m[,ImY,1]*stf@m.spwn[,ImY,1])*stf@mat[,ImY,1]),0))

#===============================================================================
# Forecast year
#===============================================================================

stf@stock.n[,FcY] <- c(RECS$FcY,(stf@stock.n[,ImY,1]*exp(-apply(stf@harvest[,ImY],1,sum)-stf@m[,ImY,1]))[ac(range(stf)["min"]:(range(stf)["max"]-2)),],
                       sum((stf@stock.n[,ImY,1]*exp(-apply(stf@harvest[,ImY],1,sum)-stf@m[,ImY,1]))[ac((range(stf)["max"]-1):range(stf)["max"]),]))

###--- Management options ---###

### Following the management plan ###
if("mp" %in% stf.options){

  res                           <- nls.lm(par=rep(1,dims(stf)$unit),find.FAB,stk=stf[,FcY],f01=f01,f26=f26,mp.options=mp.options,TACS=unlist(TACS)[seq(2,12,3)],jac=NULL)$par
  stf@harvest[,FcY]             <- sweep(stf@harvest[,FcY],3,res,"*")
  for(i in dms$unit){
    stf@catch.n[,FcY,i]     <- stf@stock.n[,FcY,i]*(1-exp(-unitSums(stf@harvest[,FcY])-stf@m[,FcY,i]))*(stf@harvest[,FcY,i]/(unitSums(stf@harvest[,FcY])+stf@m[,FcY,i]))
    stf@catch[,FcY,i]       <- computeCatch(stf[,FcY,i])
    stf@landings.n[,FcY,i]  <- stf@catch.n[,FcY,i]
    stf@landings[,FcY,i]    <- computeLandings(stf[,FcY,i])
  }
  
  ssb.CtY           <- sum(c(RECS$CtY,(stf@stock.n[,FcY,1]*exp(-apply(stf@harvest[,FcY],1,sum)-stf@m[,FcY,1]))[ac(range(stf)["min"]:(range(stf)["max"]-2)),],sum((stf@stock.n[,FcY,1]*exp(-apply(stf@harvest[,FcY],1,sum)-stf@m[,FcY,1]))[ac((range(stf)["max"]-1):range(stf)["max"]),]))
                       *stf@stock.wt[,FcY,1]*stf@mat[,FcY,1]*exp(-apply(stf@harvest[,FcY],1,sum)*stf@harvest.spwn[,FcY,1]-stf@m[,FcY,1]*stf@m.spwn[,FcY,1]))
  stf.table["mp",]  <- c(round(c(mean(stf@harvest[f26,FcY,"A"]),apply(stf@harvest[f01,FcY,c("B","C","D")],3,mean),mean(apply(stf@harvest[f26,FcY],1,sum,na.rm=T)),mean(apply(stf@harvest[f01,FcY],1,sum,na.rm=T))),3),
                         round(harvestCatch(stf,FcY),0),
                         round(c(sum(stf@stock.n[,FcY,1]*stf@stock.wt[,FcY,1]*exp(-apply(stf@harvest[,FcY],1,sum)*stf@harvest.spwn[,FcY,1]-stf@m[,FcY,1]*stf@m.spwn[,FcY,1])*stf@mat[,FcY,1]),
                         ssb.CtY),0))
  #As this is most likely the agreed TAC for the B fleet, use this in all other scenario's too
  TACS[["B"]][2]    <-  computeCatch(stf[,FcY,"B"])
}
### No fishing ###
if("nf" %in% stf.options){
  stf@harvest[,FcY] <- 0
  ssb.CtY           <- sum(c(RECS$CtY,(stf@stock.n[,FcY,1]*exp(-apply(stf@harvest[,FcY],1,sum)-stf@m[,FcY,1]))[ac(range(stf)["min"]:(range(stf)["max"]-2)),],sum((stf@stock.n[,FcY,1]*exp(-apply(stf@harvest[,FcY],1,sum)-stf@m[,FcY,1]))[ac((range(stf)["max"]-1):range(stf)["max"]),]))
                     *stf@stock.wt[,FcY,1]*stf@mat[,FcY,1]*exp(-apply(stf@harvest[,FcY],1,sum)*stf@harvest.spwn[,FcY,1]-stf@m[,FcY,1]*stf@m.spwn[,FcY,1]))
  stf.table["nf",]  <- c(round(c(mean(stf@harvest[f26,FcY,1]),apply(stf@harvest[f01,FcY,2:4],3,mean),mean(apply(stf@harvest[f26,FcY],1,sum,na.rm=T)),mean(apply(stf@harvest[f01,FcY],1,sum,na.rm=T))),3),
                         round(harvestCatch(stf,FcY),0),
                         round(c(sum(stf@stock.n[,FcY,1]*stf@stock.wt[,FcY,1]*exp(-apply(stf@harvest[,FcY],1,sum)*stf@harvest.spwn[,FcY,1]-stf@m[,FcY,1]*stf@m.spwn[,FcY,1])*stf@mat[,FcY,1]),
                         ssb.CtY),0))
}
### 15% increase in TAC for the A-fleet ###
if("+15%" %in% stf.options){
  #reset harvest for all fleets
  stf@harvest[,FcY] <- stf@harvest[,ImY]

  #Take the original TAC, not the overshooted TAC
  TAC.A <- TACS.orig[["A"]][1]*1.15

  stf@harvest[,FcY]         <- fleet.harvest(stf,FcY,c(TAC.A,unlist(TACS)[seq(2,12,3)][-1]))
  for(i in dms$unit){
    stf@catch.n[,FcY,i]     <- stf@stock.n[,FcY,i]*(1-exp(-unitSums(stf@harvest[,FcY])-stf@m[,FcY,i]))*(stf@harvest[,FcY,i]/(unitSums(stf@harvest[,FcY])+stf@m[,FcY,i]))
    stf@catch[,FcY,i]       <- computeCatch(stf[,FcY,i])
    stf@landings.n[,FcY,i]  <- stf@catch.n[,FcY,i]
    stf@landings[,FcY,i]    <- computeLandings(stf[,FcY,i])
  }

  ssb.CtY           <- sum(c(RECS$CtY,(stf@stock.n[,FcY,1]*exp(-apply(stf@harvest[,FcY],1,sum)-stf@m[,FcY,1]))[ac(range(stf)["min"]:(range(stf)["max"]-2)),],
                           sum((stf@stock.n[,FcY,1]*exp(-apply(stf@harvest[,FcY],1,sum)-stf@m[,FcY,1]))[ac((range(stf)["max"]-1):range(stf)["max"]),]))
                     *stf@stock.wt[,FcY,1]*stf@mat[,FcY,1]*exp(-apply(stf@harvest[,FcY],1,sum)*stf@harvest.spwn[,FcY,1]-stf@m[,FcY,1]*stf@m.spwn[,FcY,1]))
  stf.table["+15%",]  <- c(round(c(mean(stf@harvest[f26,FcY,1]),apply(stf@harvest[f01,FcY,2:4],3,mean),mean(apply(stf@harvest[f26,FcY],1,sum,na.rm=T)),
                           mean(apply(stf@harvest[f01,FcY],1,sum,na.rm=T))),3),
                           round(harvestCatch(stf,FcY),0),
                           round(c(sum(stf@stock.n[,FcY,1]*stf@stock.wt[,FcY,1]*exp(-apply(stf@harvest[,FcY],1,sum)*stf@harvest.spwn[,FcY,1]-stf@m[,FcY,1]*stf@m.spwn[,FcY,1])*stf@mat[,FcY,1]),
                           ssb.CtY),0))
  TACS[["B"]][2]     <- computeCatch(stf[,FcY,"B"])
}
### 15% reduction in TAC for the A-fleet ###
if("-15%" %in% stf.options){

  #reset harvest for all fleets
  stf@harvest[,FcY] <- stf@harvest[,ImY]

  #Take the original TAC, not the overshooted TAC
  TAC.A <- TACS.orig[["A"]][1]*0.85

  stf@harvest[,FcY]         <- fleet.harvest(stf,FcY,c(TAC.A,unlist(TACS)[seq(2,12,3)][-1]))
  for(i in dms$unit){
    stf@catch.n[,FcY,i]     <- stf@stock.n[,FcY,i]*(1-exp(-unitSums(stf@harvest[,FcY])-stf@m[,FcY,i]))*(stf@harvest[,FcY,i]/(unitSums(stf@harvest[,FcY])+stf@m[,FcY,i]))
    stf@catch[,FcY,i]       <- computeCatch(stf[,FcY,i])
    stf@landings.n[,FcY,i]  <- stf@catch.n[,FcY,i]
    stf@landings[,FcY,i]    <- computeLandings(stf[,FcY,i])
  }
  ssb.CtY           <- sum(c(RECS$CtY,(stf@stock.n[,FcY,1]*exp(-apply(stf@harvest[,FcY],1,sum)-stf@m[,FcY,1]))[ac(range(stf)["min"]:(range(stf)["max"]-2)),],
                           sum((stf@stock.n[,FcY,1]*exp(-apply(stf@harvest[,FcY],1,sum)-stf@m[,FcY,1]))[ac((range(stf)["max"]-1):range(stf)["max"]),]))
                     *stf@stock.wt[,FcY,1]*stf@mat[,FcY,1]*exp(-apply(stf@harvest[,FcY],1,sum)*stf@harvest.spwn[,FcY,1]-stf@m[,FcY,1]*stf@m.spwn[,FcY,1]))
  stf.table["-15%",]  <- c(round(c(mean(stf@harvest[f26,FcY,1]),apply(stf@harvest[f01,FcY,2:4],3,mean),mean(apply(stf@harvest[f26,FcY],1,sum,na.rm=T)),
                           mean(apply(stf@harvest[f01,FcY],1,sum,na.rm=T))),3),
                           round(harvestCatch(stf,FcY),0),
                           round(c(sum(stf@stock.n[,FcY,1]*stf@stock.wt[,FcY,1]*exp(-apply(stf@harvest[,FcY],1,sum)*stf@harvest.spwn[,FcY,1]-stf@m[,FcY,1]*stf@m.spwn[,FcY,1])*stf@mat[,FcY,1]),
                           ssb.CtY),0))
}


### Same TAC for A-fleet as last year ###
if("tacro" %in% stf.options){
  #reset harvest for all fleets
  stf@harvest[,FcY] <- stf@harvest[,ImY]

  #Take the original TAC, not the overshooted TAC
  TAC.A <- TACS.orig[["A"]][1]

  stf@harvest[,FcY]         <- fleet.harvest(stf,FcY,c(TAC.A,unlist(TACS)[seq(2,12,3)][-1]))
  for(i in dms$unit){
    stf@catch.n[,FcY,i]     <- stf@stock.n[,FcY,i]*(1-exp(-unitSums(stf@harvest[,FcY])-stf@m[,FcY,i]))*(stf@harvest[,FcY,i]/(unitSums(stf@harvest[,FcY])+stf@m[,FcY,i]))
    stf@catch[,FcY,i]       <- computeCatch(stf[,FcY,i])
    stf@landings.n[,FcY,i]  <- stf@catch.n[,FcY,i]
    stf@landings[,FcY,i]    <- computeLandings(stf[,FcY,i])
  }

  ssb.CtY           <- sum(c(RECS$CtY,(stf@stock.n[,FcY,1]*exp(-apply(stf@harvest[,FcY],1,sum)-stf@m[,FcY,1]))[ac(range(stf)["min"]:(range(stf)["max"]-2)),],
                           sum((stf@stock.n[,FcY,1]*exp(-apply(stf@harvest[,FcY],1,sum)-stf@m[,FcY,1]))[ac((range(stf)["max"]-1):range(stf)["max"]),]))
                     *stf@stock.wt[,FcY,1]*stf@mat[,FcY,1]*exp(-apply(stf@harvest[,FcY],1,sum)*stf@harvest.spwn[,FcY,1]-stf@m[,FcY,1]*stf@m.spwn[,FcY,1]))
  stf.table["tacro",]  <- c(round(c(mean(stf@harvest[f26,FcY,1]),apply(stf@harvest[f01,FcY,2:4],3,mean),mean(apply(stf@harvest[f26,FcY],1,sum,na.rm=T)),mean(apply(stf@harvest[f01,FcY],1,sum,na.rm=T))),3),
                            round(harvestCatch(stf,FcY),0),
                            round(c(sum(stf@stock.n[,FcY,1]*stf@stock.wt[,FcY,1]*exp(-apply(stf@harvest[,FcY],1,sum)*stf@harvest.spwn[,FcY,1]-stf@m[,FcY,1]*stf@m.spwn[,FcY,1])*stf@mat[,FcY,1]),
                            ssb.CtY),0))
}

### Bpa in continuation year but obeying the management plan targets ###
if("bpa" %in% stf.options){
   #reset harvest for all fleets
  stf@harvest[,FcY] <- stf@harvest[,ImY]

  res                           <- nls.lm(par=rep(1,dims(stf)$unit),find.Bpa,stk=stf,rec=RECS$FcY,bpa=1.3e6,fpa=0.25,f01=f01,f26=f26,iYr=FcY,TACS=unlist(TACS)[seq(2,12,3)],jac=NULL)$par
  stf@harvest[,FcY]             <- sweep(stf@harvest[,FcY],3,res,"*")
  for(i in dms$unit){
    stf@catch.n[,FcY,i]     <- stf@stock.n[,FcY,i]*(1-exp(-unitSums(stf@harvest[,FcY])-stf@m[,FcY,i]))*(stf@harvest[,FcY,i]/(unitSums(stf@harvest[,FcY])+stf@m[,FcY,i]))
    stf@catch[,FcY,i]       <- computeCatch(stf[,FcY,i])
    stf@landings.n[,FcY,i]  <- stf@catch.n[,FcY,i]
    stf@landings[,FcY,i]    <- computeLandings(stf[,FcY,i])
  }

  ssb.CtY               <- sum(c(RECS$CtY,(stf@stock.n[,FcY,1]*exp(-apply(stf@harvest[,FcY],1,sum)-stf@m[,ImY,1]))[ac(range(stf)["min"]:(range(stf)["max"]-2)),],
                           sum((stf@stock.n[,FcY,1]*exp(-apply(stf@harvest[,FcY],1,sum)-stf@m[,FcY,1]))[ac((range(stf)["max"]-1):range(stf)["max"]),]))
                                *stf@stock.wt[,FcY,1]*stf@mat[,FcY,1]*exp(-apply(stf@harvest[,FcY],1,sum)*stf@harvest.spwn[,FcY,1]-stf@m[,FcY,1]*stf@m.spwn[,FcY,1]))
  stf.table["bpa",]   <- c(round(c(mean(stf@harvest[f26,FcY,1]),apply(stf@harvest[f01,FcY,2:4],3,mean),mean(apply(stf@harvest[f26,FcY],1,sum,na.rm=T)),
                          mean(apply(stf@harvest[f01,FcY],1,sum,na.rm=T))),3),
                          round(harvestCatch(stf,FcY),0),
                          round(c(sum(stf@stock.n[,FcY,1]*stf@stock.wt[,FcY,1]*exp(-apply(stf@harvest[,FcY],1,sum)*stf@harvest.spwn[,FcY,1]-stf@m[,FcY,1]*stf@m.spwn[,FcY,1])*stf@mat[,FcY,1]),
                          ssb.CtY),0))
}
if("fmsy" %in% stf.options){
  stf.table["fmsy",] <- stf.table["bpa",]
}


#- Writing the STF to file
for(i in c("catch","catch.n","stock.n","harvest")){
  slot(stf,i)[,FcY] <- ""
  slot(stf,i)[,CtY] <- ""
}

options("width"=80,"scipen"=1000)
stf.out.file <- stf.out(stf,RECS,format="TABLE 2.7.%i NORTH SEA HERRING.")
write(stf.out.file,file=paste(output.base,"stf.out",sep="."))

#- Write the stf.table to file
write.table(stf.table,file=paste(output.base,"stf.table",sep="."),append=F,col.names=T,row.names=T)

#- Save the output to an RData file
save.image(file=paste(output.base,"ShortTermForecast Workspace.RData"))
