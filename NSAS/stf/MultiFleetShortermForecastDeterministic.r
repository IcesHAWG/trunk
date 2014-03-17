#-------------------------------------------------------------------------------
# Code to do the multifleet short term forecast for North Sea Herring
#
# By: Niels Hintzen
# IMARES
# 2 January 2012
#
#-------------------------------------------------------------------------------

rm(list=ls());
library(FLCore)
library(FLSAM)
library(minpack.lm)
require(msm)
#Read in data
path <- "D:/Repository/HAWG/HAWGrepository/NSAS/"
try(setwd(path),silent=TRUE)
output.dir <- file.path(".","results")
load(file=file.path(output.dir,"North Sea Herring.RData"))
try(setwd(path))
try(setwd("./stf/"))


#-------------------------------------------------------------------------------
# Setup control file
#-------------------------------------------------------------------------------

DtY   <- ac(range(NSH)["maxyear"]) #Data year
ImY   <- ac(an(DtY)+1) #Intermediate year
FcY   <- ac(an(DtY)+2) #Forecast year
CtY   <- ac(an(DtY)+3) #Continuation year
FuY   <- c(ImY,FcY,CtY)#Future years

#- Source the code to read in the weights at age and numbers at age by fleet, to compute partial F's and partial weights
source("./data/readStfData.r")
source("./data/writeSTF.out.r")

#- Generate multi-iters opbject
  #Deterministic
stk     <- NSH
stk.sam <- NSH.sam

FA      <- Ns[,paste("A",DtY,sep="")]/apply(Ns,1,sum,na.rm=T) * stk@harvest[,DtY]
FB      <- Ns[,paste("B",DtY,sep="")]/apply(Ns,1,sum,na.rm=T) * stk@harvest[,DtY]
FC      <- Ns[,paste("C",DtY,sep="")]/apply(Ns,1,sum,na.rm=T) * stk@harvest[,DtY]
FD      <- Ns[,paste("D",DtY,sep="")]/apply(Ns,1,sum,na.rm=T) * stk@harvest[,DtY]

#-------------------------------------------------------------------------------
# TAC information
#
#   For the TAC's there is some special things going on. The A-fleet, you can get the TAC from the agreed management plan between Norway-EU. However, there might be catches from Norway from the C-fleet
#   which they are allowed to take in the North Sea. So, substract these catches (which are a percentage of their total C-fleet quota) from the total C-fleet quota and move that into the North Sea
#   Then split the C-fleet and D-fleet into parts from the WBSS and NSH
#   The B-fleet is the bycatch in the North Sea
#   Where to find the information: TAC A-Fleet: Agreement EU-Norway North Sea --> table
#                                  TAC B-Fleet: Agreement EU-Norway North Sea --> in text
#                                  TAC C-Fleet: Agreement EU-Norway Kattegat - Skaggerak --> table
#                                  TAC D-Fleet: Agreement EU-Norway Kattegat - Skaggerak --> in text
#                                  Transfer C-Fleet to A-fleet: Agreement EU-Norway Kattegat - Skaggerak --> in text, and combination with TAC of C-Fleet of Norway
#   In the A fleet, also WBSS are caught (under the NSAS TAC) which need to be substracted from the A-fleet intermediate year catch expectation
#   Split the C-Fleet and D-Fleet into the WBSS and NSH fraction of the TAC (after taking the transfer from Norway from the C-Fleet)
#   Split information comes from the 'Wonderful Table'
#   Split is based on parts taken in the history
#   TAC's for the C and D fleet for the Forecast and Continuation year come from the Danish. Or:
#   Compute the fraction of NSH caught by the C fleet from the total C fleet catch (wonderful table), the same for the D-fleet
#   Then calculate the proportion of the C fleet that was catching WBSS. Then take the forecasted TAC and
#   multiply it by the C fleet share in WBSS, then divide it by 1-fraction NSH and then times the fraction of NSH. This gives you the TAC's for the C and D Fleet
#   However, this might be changed by the Danish
#-------------------------------------------------------------------------------


#2011:
TACS        <- FLQuant(NA,dimnames=list(age="all",year=FuY,unit=c("A","B","C","D"),season="all",area=1,iter=1:dims(stk)$iter))
TACS.orig   <- TACS
#40% from IIIa TAC in IV, as suggested by PRAC in February 2013
TACS[,,"A"] <- c(470037+ 0.45 * 46750 - 452,NA,NA);     TACS.orig[,,"A"]  <- c(470037,NA,NA)
TACS[,,"B"] <- c(13085*0.56534722           ,NA,NA);    TACS.orig[,,"B"]  <- c(13085, NA,NA)
TACS[,,"C"] <- c(9777,12600,12600);                     TACS.orig[,,"C"]  <- c(9777,12600,12600)
TACS[,,"D"] <- c(2493, 3212, 3212);                     TACS.orig[,,"D"]  <- c(2493, 3212, 3212)

  recWeights<- subset(NSH.sam@params,name=="U"); recWeights <- (recWeights[seq(1,nrow(recWeights),dims(NSH.sam)$age+length(unique(NSH.sam@control@states["catch",]))),]$std.dev)^2
#RECS        <- FLQuants("ImY" =FLQuant(subset(rec(NSH.sam),year==ImY)$value,dimnames=list(age="0",year=ImY,unit="unique",season="all",area="unique",iter=1:dims(stk)$iter)),
#                        "FcY" =exp(apply(log(rec(stk)[,ac((an(DtY)-10):DtY)]),3:6,weighted.mean,w=1/rev(rev(recWeights)[2:12]),na.rm=T)),
#                        "CtY" =exp(apply(log(rec(stk)[,ac((an(DtY)-10):DtY)]),3:6,weighted.mean,w=1/rev(rev(recWeights)[2:12]),na.rm=T)))
RECS        <- FLQuants("ImY" =exp(apply(log(rec(stk)[,ac((an(DtY)-10):DtY)]),3:6,weighted.mean,w=1/rev(rev(recWeights)[2:12]),na.rm=T)),
                        "FcY" =exp(apply(log(rec(stk)[,ac((an(DtY)-10):DtY)]),3:6,weighted.mean,w=1/rev(rev(recWeights)[2:12]),na.rm=T)),
                        "CtY" =exp(apply(log(rec(stk)[,ac((an(DtY)-10):DtY)]),3:6,weighted.mean,w=1/rev(rev(recWeights)[2:12]),na.rm=T)))


FS          <- list("A"=FA,"B"=FB,"C"=FC,"D"=FD)
WS          <- list("A"=WA,"B"=WB,"C"=WC,"D"=WD)

yrs1        <- list("m.spwn","harvest.spwn","stock.wt")
yrs3        <- list("mat")
yrs5        <- list("m")

dsc         <- "North Sea Herring"
nam         <- "NSH"
dms         <- dimnames(stk@m)
dms$year    <- c(rev(rev(dms$year)[1:3]),ImY,FcY,CtY)
dms$unit    <- c("A","B","C","D")

f01         <- ac(0:1)
f26         <- ac(2:6)

stf.options <- c("mp","-15%","+15%","nf","tacro","fmsy","newmp") #mp=according to management plan, +/-% = TAC change, nf=no fishing, bpa=reach BPA in CtY or if ssb > bpa fish at fpa,
                                                        # tacro=same catch as last year
mp.options  <- c("i") #i=increase in B fleet is allowed, fro=B fleet takes fbar as year before
#===============================================================================
# Setup stock file
#===============================================================================

stf         <- FLStock(name=nam,desc=dsc,m=FLQuant(NA,dimnames=dms))
for(i in dms$unit)
  stf[,,i]  <- window(stk,start=an(dms$year)[1],end=rev(an(dms$year))[1])
units(stf)  <- units(stk)
# Fill slots that are the same for all fleets
for(i in c(unlist(yrs1),unlist(yrs3),unlist(yrs5))){
  if(i %in% unlist(yrs1)){ for(j in dms$unit){ slot(stf,i)[,FuY,j] <- slot(stk,i)[,DtY]}}
  if(i %in% unlist(yrs3)){ for(j in dms$unit){ slot(stf,i)[,FuY,j] <- yearMeans(slot(stk,i)[,ac((an(DtY)-2):an(DtY))])}}
  if(i %in% unlist(yrs5)){ for(j in dms$unit){ slot(stf,i)[,FuY,j] <- yearMeans(slot(stk,i)[,ac((an(DtY)-4):an(DtY))])}}
}

# Fill slots that are unique for the fleets
for(i in dms$unit){
  stf@harvest[,FuY,i]     <- FS[[i]]
  stf@catch.wt[,FuY,i]    <- WS[[i]]
  stf@landings.wt[,FuY,i] <- WS[[i]]
}
# Fill slots that have no meaning for NSAS
stf@discards.n[]          <- 0
stf@discards[]            <- 0
stf@discards.wt[]         <- 0

#===============================================================================
# Intermediate year
#===============================================================================

for(i in dms$unit)
  stf@stock.n[,ImY,i]     <- stk.sam@stock.n[,ImY]
#-Fix in 2014
for(i in dms$unit)
  stf@stock.n[1,ImY,i]    <- RECS[[1]]

stf@harvest[,ImY]         <- fleet.harvest(stk=stf,iYr=ImY,TACS=TACS[,ImY])
for(i in dms$unit){
  stf@catch.n[,ImY,i]     <- stf@stock.n[,ImY,i]*(1-exp(-unitSums(stf@harvest[,ImY])-stf@m[,ImY,i]))*(stf@harvest[,ImY,i]/(unitSums(stf@harvest[,ImY])+stf@m[,ImY,i]))
  stf@catch[,ImY,i]       <- computeCatch(stf[,ImY,i])
  stf@landings.n[,ImY,i]  <- stf@catch.n[,ImY,i]
  stf@landings[,ImY,i]    <- computeLandings(stf[,ImY,i])
}


#Intermediate year stf option table
stf.table <- array(NA,dim=c(c(length(stf.options)+1),12,3),
                   dimnames=list("options"=c("intermediate year",stf.options),
                                 "values" =c("Fbar 2-6 A","Fbar 0-1 B","Fbar 0-1 C","Fbar 0-1 D","Fbar 2-6","Fbar 0-1","Catch A","Catch B","Catch C","Catch D","SSB","SSB"),
                                 "stats"  =c("5%","50%","95%")))

stf.table[1,"Fbar 2-6 A",]                                  <- iterQuantile(quantMeans(stf@harvest[f26,ImY,"A"]))
stf.table[1,grep("Fbar 0-1 ",dimnames(stf.table)$values),]  <- aperm(iterQuantile(quantMeans(stf@harvest[f01,ImY,c("B","C","D")])),c(2:6,1))
stf.table[1,"Fbar 2-6",]                                    <- iterQuantile(quantMeans(unitSums(stf@harvest[f26,ImY,])))
stf.table[1,"Fbar 0-1",]                                    <- iterQuantile(quantMeans(unitSums(stf@harvest[f01,ImY,])))
stf.table[1,grep("Catch ",dimnames(stf.table)$values),]     <- aperm(iterQuantile(harvestCatch(stf,ImY)),c(2:6,1))
stf.table[1,grep("SSB",dimnames(stf.table)$values)[1],]     <- iterQuantile(quantSums(stf@stock.n[,ImY,1] * stf@stock.wt[,ImY,1] *
                                                                 exp(-unitSums(stf@harvest[,ImY])*stf@harvest.spwn[,ImY,1]-stf@m[,ImY,1]*stf@m.spwn[,ImY,1]) *
                                                                 stf@mat[,ImY,1]))

#===============================================================================
# Forecast year
#===============================================================================

for(i in dms$unit) stf@stock.n[1,FcY,i]                     <- RECS$FcY
for(i in dms$unit) stf@stock.n[2:(dims(stf)$age-1),FcY,i]   <- (stf@stock.n[,ImY,1]*exp(-unitSums(stf@harvest[,ImY])-stf@m[,ImY,1]))[ac(range(stf)["min"]:(range(stf)["max"]-2)),]
for(i in dms$unit) stf@stock.n[dims(stf)$age,FcY,i]         <- apply((stf@stock.n[,ImY,1]*exp(-unitSums(stf@harvest[,ImY])-stf@m[,ImY,1]))[ac((range(stf)["max"]-1):range(stf)["max"]),],2:6,sum,na.rm=T)

###--- Management options ---###

### Following the management plan ###
if("mp" %in% stf.options){

  res <- matrix(NA,nrow=dims(stf)$unit,ncol=dims(stf)$iter,dimnames=list(dimnames(stf@stock.n)$unit,dimnames(stf@stock.n)$iter))
  for(iTer in 1:dims(stf)$iter)
    res[,iTer]              <- nls.lm(par=rep(1,dims(stf)$unit),lower=NULL, upper=NULL,find.FAB,stk=iter(stf[,FcY],iTer),f01=f01,f26=f26,mp.options=mp.options,TACS=iter(TACS[,FcY],iTer),jac=NULL)$par
  stf@harvest[,FcY]         <- sweep(stf@harvest[,FcY],c(3,6),res,"*")
  for(i in dms$unit){
    stf@catch.n[,FcY,i]     <- stf@stock.n[,FcY,i]*(1-exp(-unitSums(stf@harvest[,FcY])-stf@m[,FcY,i]))*(stf@harvest[,FcY,i]/(unitSums(stf@harvest[,FcY])+stf@m[,FcY,i]))
    stf@catch[,FcY,i]       <- computeCatch(stf[,FcY,i])
    stf@landings.n[,FcY,i]  <- stf@catch.n[,FcY,i]
    stf@landings[,FcY,i]    <- computeLandings(stf[,FcY,i])
  }
  #Update to continuation year
  for(i in dms$unit) stf@stock.n[1,CtY,i]                     <- RECS$CtY
  for(i in dms$unit) stf@stock.n[2:(dims(stf)$age-1),CtY,i]   <- (stf@stock.n[,FcY,1]*exp(-unitSums(stf@harvest[,FcY])-stf@m[,FcY,1]))[ac(range(stf)["min"]:(range(stf)["max"]-2)),]
  for(i in dms$unit) stf@stock.n[dims(stf)$age,CtY,i]         <- apply((stf@stock.n[,FcY,1]*exp(-unitSums(stf@harvest[,FcY])-stf@m[,FcY,1]))[ac((range(stf)["max"]-1):range(stf)["max"]),],2:6,sum,na.rm=T)
  ssb.CtY                                                     <- quantSums(stf@stock.n[,CtY,1] * stf@stock.wt[,CtY,1]*stf@mat[,CtY,1]*exp(-unitSums(stf@harvest[,FcY])*stf@harvest.spwn[,CtY,1]-stf@m[,CtY,1]*stf@m.spwn[,CtY,1])) #assume same harvest as in FcY

  stf.table["mp","Fbar 2-6 A",]                                  <- iterQuantile(quantMeans(stf@harvest[f26,FcY,"A"]))
  stf.table["mp",grep("Fbar 0-1 ",dimnames(stf.table)$values),]  <- aperm(iterQuantile(quantMeans(stf@harvest[f01,FcY,c("B","C","D")])),c(2:6,1))
  stf.table["mp","Fbar 2-6",]                                    <- iterQuantile(quantMeans(unitSums(stf@harvest[f26,FcY,])))
  stf.table["mp","Fbar 0-1",]                                    <- iterQuantile(quantMeans(unitSums(stf@harvest[f01,FcY,])))
  stf.table["mp",grep("Catch ",dimnames(stf.table)$values),]     <- aperm(iterQuantile(harvestCatch(stf,FcY)),c(2:6,1))
  stf.table["mp",grep("SSB",dimnames(stf.table)$values)[1],]     <- iterQuantile(quantSums(stf@stock.n[,FcY,1] * stf@stock.wt[,FcY,1] *
                                                                 exp(-unitSums(stf@harvest[,FcY])*stf@harvest.spwn[,FcY,1]-stf@m[,FcY,1]*stf@m.spwn[,FcY,1]) *
                                                                 stf@mat[,FcY,1]))
  stf.table["mp",grep("SSB",dimnames(stf.table)$values)[2],]     <- iterQuantile(ssb.CtY)


  #As this is most likely the agreed TAC for the B fleet, use this in all other scenario's too
  TACS[,FcY,"B"]    <- computeCatch(stf[,FcY,"B"])
  #TACSmp            <- harvestCatch(stf,FcY)
}
### No fishing ###
if("nf" %in% stf.options){
  stf@harvest[,FcY] <- 0
  #Update to continuation year
  for(i in dms$unit) stf@stock.n[1,CtY,i]                     <- RECS$CtY
  for(i in dms$unit) stf@stock.n[2:(dims(stf)$age-1),CtY,i]   <- (stf@stock.n[,FcY,1]*exp(-unitSums(stf@harvest[,FcY])-stf@m[,FcY,1]))[ac(range(stf)["min"]:(range(stf)["max"]-2)),]
  for(i in dms$unit) stf@stock.n[dims(stf)$age,CtY,i]         <- apply((stf@stock.n[,FcY,1]*exp(-unitSums(stf@harvest[,FcY])-stf@m[,FcY,1]))[ac((range(stf)["max"]-1):range(stf)["max"]),],2:6,sum,na.rm=T)
  ssb.CtY                                                     <- quantSums(stf@stock.n[,CtY,1] * stf@stock.wt[,CtY,1]*stf@mat[,CtY,1]*exp(-unitSums(stf@harvest[,FcY])*stf@harvest.spwn[,CtY,1]-stf@m[,CtY,1]*stf@m.spwn[,CtY,1])) #assume same harvest as in FcY

  stf.table["nf","Fbar 2-6 A",]                                  <- iterQuantile(quantMeans(stf@harvest[f26,FcY,"A"]))
  stf.table["nf",grep("Fbar 0-1 ",dimnames(stf.table)$values),]  <- aperm(iterQuantile(quantMeans(stf@harvest[f01,FcY,c("B","C","D")])),c(2:6,1))
  stf.table["nf","Fbar 2-6",]                                    <- iterQuantile(quantMeans(unitSums(stf@harvest[f26,FcY,])))
  stf.table["nf","Fbar 0-1",]                                    <- iterQuantile(quantMeans(unitSums(stf@harvest[f01,FcY,])))
  stf.table["nf",grep("Catch ",dimnames(stf.table)$values),]     <- aperm(iterQuantile(harvestCatch(stf,FcY)),c(2:6,1))
  stf.table["nf",grep("SSB",dimnames(stf.table)$values)[1],]     <- iterQuantile(quantSums(stf@stock.n[,FcY,1] * stf@stock.wt[,FcY,1] *
                                                                 exp(-unitSums(stf@harvest[,FcY])*stf@harvest.spwn[,FcY,1]-stf@m[,FcY,1]*stf@m.spwn[,FcY,1]) *
                                                                 stf@mat[,FcY,1]))
  stf.table["nf",grep("SSB",dimnames(stf.table)$values)[2],]     <- iterQuantile(ssb.CtY)
}

### 15% increase in TAC for the A-fleet ###
if("+15%" %in% stf.options){
  #reset harvest for all fleets
  stf@harvest[,FcY] <- stf@harvest[,ImY]

  TACS[,FcY,"A"]            <- TACS.orig[,ImY,"A"]*1.15
  stf@harvest[,FcY]         <- fleet.harvest(stk=stf,iYr=FcY,TACS=TACS[,FcY])
  for(i in dms$unit){
    stf@catch.n[,FcY,i]     <- stf@stock.n[,FcY,i]*(1-exp(-unitSums(stf@harvest[,FcY])-stf@m[,FcY,i]))*(stf@harvest[,FcY,i]/(unitSums(stf@harvest[,FcY])+stf@m[,FcY,i]))
    stf@catch[,FcY,i]       <- computeCatch(stf[,FcY,i])
    stf@landings.n[,FcY,i]  <- stf@catch.n[,FcY,i]
    stf@landings[,FcY,i]    <- computeLandings(stf[,FcY,i])
  }
  #Update to continuation year
  for(i in dms$unit) stf@stock.n[1,CtY,i]                     <- RECS$CtY
  for(i in dms$unit) stf@stock.n[2:(dims(stf)$age-1),CtY,i]   <- (stf@stock.n[,FcY,1]*exp(-unitSums(stf@harvest[,FcY])-stf@m[,FcY,1]))[ac(range(stf)["min"]:(range(stf)["max"]-2)),]
  for(i in dms$unit) stf@stock.n[dims(stf)$age,CtY,i]         <- apply((stf@stock.n[,FcY,1]*exp(-unitSums(stf@harvest[,FcY])-stf@m[,FcY,1]))[ac((range(stf)["max"]-1):range(stf)["max"]),],2:6,sum,na.rm=T)
  ssb.CtY                                                     <- quantSums(stf@stock.n[,CtY,1] * stf@stock.wt[,CtY,1]*stf@mat[,CtY,1]*exp(-unitSums(stf@harvest[,FcY])*stf@harvest.spwn[,CtY,1]-stf@m[,CtY,1]*stf@m.spwn[,CtY,1])) #assume same harvest as in FcY

  stf.table["+15%","Fbar 2-6 A",]                                  <- iterQuantile(quantMeans(stf@harvest[f26,FcY,"A"]))
  stf.table["+15%",grep("Fbar 0-1 ",dimnames(stf.table)$values),]  <- aperm(iterQuantile(quantMeans(stf@harvest[f01,FcY,c("B","C","D")])),c(2:6,1))
  stf.table["+15%","Fbar 2-6",]                                    <- iterQuantile(quantMeans(unitSums(stf@harvest[f26,FcY,])))
  stf.table["+15%","Fbar 0-1",]                                    <- iterQuantile(quantMeans(unitSums(stf@harvest[f01,FcY,])))
  stf.table["+15%",grep("Catch ",dimnames(stf.table)$values),]     <- aperm(iterQuantile(harvestCatch(stf,FcY)),c(2:6,1))
  stf.table["+15%",grep("SSB",dimnames(stf.table)$values)[1],]     <- iterQuantile(quantSums(stf@stock.n[,FcY,1] * stf@stock.wt[,FcY,1] *
                                                                 exp(-unitSums(stf@harvest[,FcY])*stf@harvest.spwn[,FcY,1]-stf@m[,FcY,1]*stf@m.spwn[,FcY,1]) *
                                                                 stf@mat[,FcY,1]))
  stf.table["+15%",grep("SSB",dimnames(stf.table)$values)[2],]     <- iterQuantile(ssb.CtY)
  #TACSmp                                                           <- harvestCatch(stf,FcY)
}

### 15% reduction in TAC for the A-fleet ###
if("-15%" %in% stf.options){

  #reset harvest for all fleets
  stf@harvest[,FcY] <- stf@harvest[,ImY]

  TACS[,FcY,"A"]            <- TACS.orig[,ImY,"A"]*0.85
  stf@harvest[,FcY]         <- fleet.harvest(stk=stf,iYr=FcY,TACS=TACS[,FcY])
  for(i in dms$unit){
    stf@catch.n[,FcY,i]     <- stf@stock.n[,FcY,i]*(1-exp(-unitSums(stf@harvest[,FcY])-stf@m[,FcY,i]))*(stf@harvest[,FcY,i]/(unitSums(stf@harvest[,FcY])+stf@m[,FcY,i]))
    stf@catch[,FcY,i]       <- computeCatch(stf[,FcY,i])
    stf@landings.n[,FcY,i]  <- stf@catch.n[,FcY,i]
    stf@landings[,FcY,i]    <- computeLandings(stf[,FcY,i])
  }
  #Update to continuation year
  for(i in dms$unit) stf@stock.n[1,CtY,i]                     <- RECS$CtY
  for(i in dms$unit) stf@stock.n[2:(dims(stf)$age-1),CtY,i]   <- (stf@stock.n[,FcY,1]*exp(-unitSums(stf@harvest[,FcY])-stf@m[,FcY,1]))[ac(range(stf)["min"]:(range(stf)["max"]-2)),]
  for(i in dms$unit) stf@stock.n[dims(stf)$age,CtY,i]         <- apply((stf@stock.n[,FcY,1]*exp(-unitSums(stf@harvest[,FcY])-stf@m[,FcY,1]))[ac((range(stf)["max"]-1):range(stf)["max"]),],2:6,sum,na.rm=T)
  ssb.CtY                                                     <- quantSums(stf@stock.n[,CtY,1] * stf@stock.wt[,CtY,1]*stf@mat[,CtY,1]*exp(-unitSums(stf@harvest[,FcY])*stf@harvest.spwn[,CtY,1]-stf@m[,CtY,1]*stf@m.spwn[,CtY,1])) #assume same harvest as in FcY

  stf.table["-15%","Fbar 2-6 A",]                                  <- iterQuantile(quantMeans(stf@harvest[f26,FcY,"A"]))
  stf.table["-15%",grep("Fbar 0-1 ",dimnames(stf.table)$values),]  <- aperm(iterQuantile(quantMeans(stf@harvest[f01,FcY,c("B","C","D")])),c(2:6,1))
  stf.table["-15%","Fbar 2-6",]                                    <- iterQuantile(quantMeans(unitSums(stf@harvest[f26,FcY,])))
  stf.table["-15%","Fbar 0-1",]                                    <- iterQuantile(quantMeans(unitSums(stf@harvest[f01,FcY,])))
  stf.table["-15%",grep("Catch ",dimnames(stf.table)$values),]     <- aperm(iterQuantile(harvestCatch(stf,FcY)),c(2:6,1))
  stf.table["-15%",grep("SSB",dimnames(stf.table)$values)[1],]     <- iterQuantile(quantSums(stf@stock.n[,FcY,1] * stf@stock.wt[,FcY,1] *
                                                                 exp(-unitSums(stf@harvest[,FcY])*stf@harvest.spwn[,FcY,1]-stf@m[,FcY,1]*stf@m.spwn[,FcY,1]) *
                                                                 stf@mat[,FcY,1]))
  stf.table["-15%",grep("SSB",dimnames(stf.table)$values)[2],]     <- iterQuantile(ssb.CtY)
  #TACSmp            <- harvestCatch(stf,FcY)
}


### Same TAC for A-fleet as last year ###
if("tacro" %in% stf.options){
  #reset harvest for all fleets
  stf@harvest[,FcY] <- stf@harvest[,ImY]

  TACS[,FcY,"A"]            <- TACS.orig[,ImY,"A"]
  stf@harvest[,FcY]         <- fleet.harvest(stk=stf,iYr=FcY,TACS=TACS[,FcY])
  for(i in dms$unit){
    stf@catch.n[,FcY,i]     <- stf@stock.n[,FcY,i]*(1-exp(-unitSums(stf@harvest[,FcY])-stf@m[,FcY,i]))*(stf@harvest[,FcY,i]/(unitSums(stf@harvest[,FcY])+stf@m[,FcY,i]))
    stf@catch[,FcY,i]       <- computeCatch(stf[,FcY,i])
    stf@landings.n[,FcY,i]  <- stf@catch.n[,FcY,i]
    stf@landings[,FcY,i]    <- computeLandings(stf[,FcY,i])
  }
  #Update to continuation year
  for(i in dms$unit) stf@stock.n[1,CtY,i]                     <- RECS$CtY
  for(i in dms$unit) stf@stock.n[2:(dims(stf)$age-1),CtY,i]   <- (stf@stock.n[,FcY,1]*exp(-unitSums(stf@harvest[,FcY])-stf@m[,FcY,1]))[ac(range(stf)["min"]:(range(stf)["max"]-2)),]
  for(i in dms$unit) stf@stock.n[dims(stf)$age,CtY,i]         <- apply((stf@stock.n[,FcY,1]*exp(-unitSums(stf@harvest[,FcY])-stf@m[,FcY,1]))[ac((range(stf)["max"]-1):range(stf)["max"]),],2:6,sum,na.rm=T)
  ssb.CtY                                                     <- quantSums(stf@stock.n[,CtY,1] * stf@stock.wt[,CtY,1]*stf@mat[,CtY,1]*exp(-unitSums(stf@harvest[,FcY])*stf@harvest.spwn[,CtY,1]-stf@m[,CtY,1]*stf@m.spwn[,CtY,1])) #assume same harvest as in FcY

  stf.table["tacro","Fbar 2-6 A",]                                  <- iterQuantile(quantMeans(stf@harvest[f26,FcY,"A"]))
  stf.table["tacro",grep("Fbar 0-1 ",dimnames(stf.table)$values),]  <- aperm(iterQuantile(quantMeans(stf@harvest[f01,FcY,c("B","C","D")])),c(2:6,1))
  stf.table["tacro","Fbar 2-6",]                                    <- iterQuantile(quantMeans(unitSums(stf@harvest[f26,FcY,])))
  stf.table["tacro","Fbar 0-1",]                                    <- iterQuantile(quantMeans(unitSums(stf@harvest[f01,FcY,])))
  stf.table["tacro",grep("Catch ",dimnames(stf.table)$values),]     <- aperm(iterQuantile(harvestCatch(stf,FcY)),c(2:6,1))
  stf.table["tacro",grep("SSB",dimnames(stf.table)$values)[1],]     <- iterQuantile(quantSums(stf@stock.n[,FcY,1] * stf@stock.wt[,FcY,1] *
                                                                 exp(-unitSums(stf@harvest[,FcY])*stf@harvest.spwn[,FcY,1]-stf@m[,FcY,1]*stf@m.spwn[,FcY,1]) *
                                                                 stf@mat[,FcY,1]))
  stf.table["tacro",grep("SSB",dimnames(stf.table)$values)[2],]     <- iterQuantile(ssb.CtY)
}

if("fmsy" %in% stf.options){
   #reset harvest for all fleets
  stf@harvest[,FcY] <- stf@harvest[,ImY]
  TACS[,FcY,"A" ]   <- TACS.orig[,ImY,"A"]
  stf@harvest[,FcY] <- fleet.harvest(stf,FcY,TACS)

  res <- matrix(NA,nrow=dims(stf)$unit,ncol=dims(stf)$iter,dimnames=list(dimnames(stf@stock.n)$unit,dimnames(stf@stock.n)$iter))
  for(iTer in 1:dims(stf)$iter)      #stk.=stk,rec.=rec,f.=fmsy,f26.=f26,f01.=f01,TACS.=TACS
    res[,iTer]                  <- nls.lm(par=rep(1,dims(stf)$unit),lower=NULL, upper=NULL,find.F,stk=iter(stf[,FcY],iTer),f.=0.27,f26=f26,f01=f01,TACS=iter(TACS[,FcY],iTer),jac=NULL)$par

  stf@harvest[,FcY]             <- sweep(stf@harvest[,FcY],c(3,6),res,"*")
  for(i in dms$unit){
    stf@catch.n[,FcY,i]         <- stf@stock.n[,FcY,i]*(1-exp(-unitSums(stf@harvest[,FcY])-stf@m[,FcY,i]))*(stf@harvest[,FcY,i]/(unitSums(stf@harvest[,FcY])+stf@m[,FcY,i]))
    stf@catch[,FcY,i]           <- computeCatch(stf[,FcY,i])
    stf@landings.n[,FcY,i]      <- stf@catch.n[,FcY,i]
    stf@landings[,FcY,i]        <- computeLandings(stf[,FcY,i])
  }
  #Update to continuation year
  for(i in dms$unit) stf@stock.n[1,CtY,i]                     <- RECS$CtY
  for(i in dms$unit) stf@stock.n[2:(dims(stf)$age-1),CtY,i]   <- (stf@stock.n[,FcY,1]*exp(-unitSums(stf@harvest[,FcY])-stf@m[,FcY,1]))[ac(range(stf)["min"]:(range(stf)["max"]-2)),]
  for(i in dms$unit) stf@stock.n[dims(stf)$age,CtY,i]         <- apply((stf@stock.n[,FcY,1]*exp(-unitSums(stf@harvest[,FcY])-stf@m[,FcY,1]))[ac((range(stf)["max"]-1):range(stf)["max"]),],2:6,sum,na.rm=T)
  ssb.CtY                                                     <- quantSums(stf@stock.n[,CtY,1] * stf@stock.wt[,CtY,1]*stf@mat[,CtY,1]*exp(-unitSums(stf@harvest[,FcY])*stf@harvest.spwn[,CtY,1]-stf@m[,CtY,1]*stf@m.spwn[,CtY,1])) #assume same harvest as in FcY

  stf.table["fmsy","Fbar 2-6 A",]                                  <- iterQuantile(quantMeans(stf@harvest[f26,FcY,"A"]))
  stf.table["fmsy",grep("Fbar 0-1 ",dimnames(stf.table)$values),]  <- aperm(iterQuantile(quantMeans(stf@harvest[f01,FcY,c("B","C","D")])),c(2:6,1))
  stf.table["fmsy","Fbar 2-6",]                                    <- iterQuantile(quantMeans(unitSums(stf@harvest[f26,FcY,])))
  stf.table["fmsy","Fbar 0-1",]                                    <- iterQuantile(quantMeans(unitSums(stf@harvest[f01,FcY,])))
  stf.table["fmsy",grep("Catch ",dimnames(stf.table)$values),]     <- aperm(iterQuantile(harvestCatch(stf,FcY)),c(2:6,1))
  stf.table["fmsy",grep("SSB",dimnames(stf.table)$values)[1],]     <- iterQuantile(quantSums(stf@stock.n[,FcY,1] * stf@stock.wt[,FcY,1] *
                                                                      exp(-unitSums(stf@harvest[,FcY])*stf@harvest.spwn[,FcY,1]-stf@m[,FcY,1] *
                                                                      stf@m.spwn[,FcY,1]) * stf@mat[,FcY,1]))
  stf.table["fmsy",grep("SSB",dimnames(stf.table)$values)[2],]     <- iterQuantile(ssb.CtY)
}

if("newmp" %in% stf.options){

  stf@harvest[,FcY] <- stf@harvest[,ImY]
  stf@harvest[,FcY] <- fleet.harvest(stf,FcY,TACS)

  res <- matrix(NA,nrow=dims(stf)$unit,ncol=dims(stf)$iter,dimnames=list(dimnames(stf@stock.n)$unit,dimnames(stf@stock.n)$iter))
  for(iTer in 1:dims(stf)$iter)
    res[,iTer]              <- nls.lm(par=rep(1,dims(stf)$unit),lower=NULL, upper=NULL,find.newFAB,stk=iter(stf[,FcY],iTer),f01=f01,f26=f26,mp.options=mp.options,TACS=iter(TACS[,FcY],iTer),jac=NULL)$par
  stf@harvest[,FcY]         <- sweep(stf@harvest[,FcY],c(3,6),res,"*")
  for(i in dms$unit){
    stf@catch.n[,FcY,i]     <- stf@stock.n[,FcY,i]*(1-exp(-unitSums(stf@harvest[,FcY])-stf@m[,FcY,i]))*(stf@harvest[,FcY,i]/(unitSums(stf@harvest[,FcY])+stf@m[,FcY,i]))
    stf@catch[,FcY,i]       <- computeCatch(stf[,FcY,i])
    stf@landings.n[,FcY,i]  <- stf@catch.n[,FcY,i]
    stf@landings[,FcY,i]    <- computeLandings(stf[,FcY,i])
  }
  
  #Update to continuation year
  for(i in dms$unit) stf@stock.n[1,CtY,i]                     <- RECS$CtY
  for(i in dms$unit) stf@stock.n[2:(dims(stf)$age-1),CtY,i]   <- (stf@stock.n[,FcY,1]*exp(-unitSums(stf@harvest[,FcY])-stf@m[,FcY,1]))[ac(range(stf)["min"]:(range(stf)["max"]-2)),]
  for(i in dms$unit) stf@stock.n[dims(stf)$age,CtY,i]         <- apply((stf@stock.n[,FcY,1]*exp(-unitSums(stf@harvest[,FcY])-stf@m[,FcY,1]))[ac((range(stf)["max"]-1):range(stf)["max"]),],2:6,sum,na.rm=T)
  ssb.CtY                                                     <- quantSums(stf@stock.n[,CtY,1] * stf@stock.wt[,CtY,1]*stf@mat[,CtY,1]*exp(-unitSums(stf@harvest[,FcY])*stf@harvest.spwn[,CtY,1]-stf@m[,CtY,1]*stf@m.spwn[,CtY,1])) #assume same harvest as in FcY

  stf.table["newmp","Fbar 2-6 A",]                                  <- iterQuantile(quantMeans(stf@harvest[f26,FcY,"A"]))
  stf.table["newmp",grep("Fbar 0-1 ",dimnames(stf.table)$values),]  <- aperm(iterQuantile(quantMeans(stf@harvest[f01,FcY,c("B","C","D")])),c(2:6,1))
  stf.table["newmp","Fbar 2-6",]                                    <- iterQuantile(quantMeans(unitSums(stf@harvest[f26,FcY,])))
  stf.table["newmp","Fbar 0-1",]                                    <- iterQuantile(quantMeans(unitSums(stf@harvest[f01,FcY,])))
  stf.table["newmp",grep("Catch ",dimnames(stf.table)$values),]     <- aperm(iterQuantile(harvestCatch(stf,FcY)),c(2:6,1))
  stf.table["newmp",grep("SSB",dimnames(stf.table)$values)[1],]     <- iterQuantile(quantSums(stf@stock.n[,FcY,1] * stf@stock.wt[,FcY,1] *
                                                                 exp(-unitSums(stf@harvest[,FcY])*stf@harvest.spwn[,FcY,1]-stf@m[,FcY,1]*stf@m.spwn[,FcY,1]) *
                                                                 stf@mat[,FcY,1]))
  stf.table["newmp",grep("SSB",dimnames(stf.table)$values)[2],]     <- iterQuantile(ssb.CtY)
}

#- Writing the STF to file
for(i in c("catch","catch.n","stock.n","harvest")){
  slot(stf,i)[,FcY] <- ""
  slot(stf,i)[,CtY] <- ""
}

options("width"=80,"scipen"=1000)
stf.out.file <- stf.out(stf,RECS,format="TABLE 2.7.%i NORTH SEA HERRING.")
write(stf.out.file,file=paste("./","stf.out",sep="."))

#- Write the stf.table to file
write.table(stf.table[,,2],file=paste("stf.table","deterministic"),append=F,col.names=T,row.names=T)
for(i in dimnames(stf.table)$stats) write.table(stf.table[,,i],file=paste("stf.table",i),append=F,col.names=T,row.names=T)

#- Save the output to an RData file
save.image(file="ShortTermForecast Workspace.RData")
