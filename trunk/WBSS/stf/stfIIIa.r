################################################################################
# Code to do the short term forecast for WBSS herring
#
# By: Niels Hintzen
# IMARES
# 20 March 2011
#
################################################################################

rm(list=ls());
library(minpack.lm)
#Read in data
try(setwd("./stf/"))

stk     <- WBSS
stk.ica <- WBSS.ica

#-Load the libraries needed
library(MASS)#7.2-47
library(lme4)#0.999375-28
library(minpack.lm)

#-Load NSAS stf from last year
#load(file=paste("N:/Projecten/ICES WG/WKWHATSUP/wkwhatsup/","output/NSH Assessment ShortTermForecast WorkspaceII.RData", sep=""))
#rm(list=ls()[which(!ls() %in% c("FA","stf","stf.table","RECS"))])
NSAS.proj     <- stf
NSAS.stftable <- stf.table



load(file=paste(path,"data/splitModel.RData",   sep=""))
load(file=paste(path,"data/TAC.RData",          sep=""))

#-Source function
source(paste(path,"R/functions.r",sep=""))
source(paste(path,"R/dataSTF.r",sep=""))

#===============================================================================
# SETTINGS
#===============================================================================

#-Year objects
DtY   <- ac(range(WBSS)["maxyear"]) #Data year
ImY   <- ac(an(DtY)+1)              #Intermediate year
FcY   <- ac(an(DtY)+2)              #Forecast year
CtY   <- ac(an(DtY)+3)              #Continuation year
source(paste(path,"R/readStfData.r",sep=""))

rec.years                           <- (range(WBSS)["maxyear"]-5):(range(WBSS)["maxyear"]-1)  #Years to take the geometric mean of the recruitment for
smooth.years                        <- 3                                                      #Years to take to smooth the weights at age and maturity, natural mortality for
split.years                         <- (range(WBSS)["maxyear"]):range(WBSS)["maxyear"]        #Years to take to smooth the split proportions over
propFleetAinIVaE                    <- 0.039264563                                            #Proportion of A fleet TAC taken in the transfer area (susceptible to mixing)
FtargetFleetAFcY                    <- NSAS.stftable["fmsy","Fbar 2-6 A"]                     #Target F for the A fleet in the Forecast year
splitBy                             <- c("area")                                              #combinations of c("area","quarter")

#-TACS
TACS        <- list("A"=c(6451,7419,NA),  "C"=c(32952,NA,NA),  "D"=c(7515,NA,NA),"F"=c(22692,NA,NA));
TACS.orig   <- list("A"=c(6451,7419,NA),  "C"=c(33855,NA,NA),  "D"=c(7515,NA,NA),"F"=c(22692,NA,NA))

RECS        <- list("ImY"=exp(apply(log(rec(WBSS)[,ac(rec.years)]),6,mean)),"FcY"=exp(apply(log(rec(WBSS)[,ac(rec.years)]),6,mean)),
                    "CtY"=exp(apply(log(rec(WBSS)[,ac(rec.years)]),6,mean)))

FS          <- list("A"=FA,"C"=FC,"D"=FD,"F"=FF)
WS          <- list("A"=WA,"C"=WC,"D"=WD,"F"=WF)

yrs1        <- list("m","m.spwn","harvest.spwn","stock.wt","stock.n")
yrs3        <- list("mat")
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
mp.options  <- c("i") #i=increase in B fleet is allowed, fro=B fleet takes fbar as year before

WBSS.proj                           <- stf(WBSS,nyears=smooth.years,wts.nyears=smooth.years,na.rm=T)
WBSS.proj@stock.n[1,ac(ImY)]        <- RECS$ImY
WBSS.proj@stock.n[-1,ac(ImY)]       <- WBSS.ica@survivors[ac((range(WBSS.ica)["min"]+1):range(WBSS.ica)["max"])]@.Data

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
# Fill slots that are unique for the fleets
for(i in dms$unit){
  for(j in dms$season){
    stf@harvest[,ac(c(DtY,ImY,FcY,CtY)),i,j]    <- FS[[i]][,which(j==dms$season)]  #Harvest pattern, not absolute harvest
    stf@catch.wt[,ac(c(DtY,ImY,FcY,CtY)),i,j]   <- WS[[i]][,which(j==dms$season)]
    stf@landings.wt[,ac(c(DtY,ImY,FcY,CtY)),i,j]<- WS[[i]][,which(j==dms$season)]
  }
}
stf@discards.n[]  <- 0 #We don't use discards
stf@discards.wt[] <- 0

#===============================================================================
# From 4 quarters to 1 year setup
#===============================================================================

stfY                                  <- stf[,,,1]
for(iYr in ac(c(ImY,FcY,CtY))){
  stfY@harvest[,iYr]     <- seasonSums(stf@harvest[,ac(DtY)])
  stfY@catch.wt[,iYr]    <- seasonSums(stf@catch.wt[,ac(c(DtY))] * stf@catch.n[,ac(c(DtY))]) / seasonSums(stf@catch.n[,ac(c(DtY))])  #You could also take the weighted mean here
  stfY@landings.wt[,iYr] <- stfY@catch.wt[,iYr]
}
#===============================================================================
# Predict the split
#===============================================================================

newDat                              <- expand.grid(age=0:4,Q=1:4,lat=c(0.230,0.650,-2.065),long=c(-0.715,-4.222,2.162),year=split.years,yearclass=seq(max(split.years),min(split.years)-range(WBSS)["max"],-1))
newDat$split                        <- predictSplit(splitModel,newDat,fixes=NULL,ranefs=c("year","yearclass"))

newDat                              <- newDat[which((newDat[,c("lat")] == c(0.230)    & newDat[,c("long")] == c(-0.715)) |
                                                    (newDat[,c("lat")] == c(0.650)    & newDat[,c("long")] == c(-4.222)) |
                                                    (newDat[,c("lat")] == c(-2.065)   & newDat[,c("long")] == c(2.162))),]
newDat$area                               <- NA
newDat$area[which(newDat$lat == 0.230)]   <- "IIIA"
newDat$area[which(newDat$lat == 0.650)]   <- "IV"
newDat$area[which(newDat$lat == -2.065)]  <- "2224"

propQA                              <- read.csv(paste(path,"input/propQuarterArea.csv",sep=""),header=F) #take the proportion of catches by area and quarter from the actual landings
colnames(propQA)                    <- c("Area","Quarter","catchProportion")

#-The split by age is weighted by the importance of catch by area and quarter
ages                    <- sort(an(unique(c(dimnames(NSAS.proj@stock.n)$age,dimnames(WBSS@stock.n)$age))))
if(length(splitBy)==2){
  splitFactor             <- matrix(NA,nrow=length(ages),ncol=1,dimnames=list(age=ages,"split"))
  splitFactor[ac(0:4),1]  <- aggregate(newDat$split,by=list(newDat$age),            weighted.mean,w=rep(propQA$catchProportion[1:12], dim(newDat)[1]/5/3/4))$x #by 5 ages, and 4 quarters
}
if(splitBy == "area"){
splitFactor             <- matrix(NA,nrow=length(ages),ncol=3,dimnames=list(age=ages,split=c("IIIa","IV","2224")))
splitFactor[ac(0:4),c(3,1,2)]   <- matrix(aggregate(newDat$split,by=list(newDat$age,newDat$area),weighted.mean,w=rep(propQA$catchProportion[16:19],dim(newDat)[1]/5/3/4))$x,ncol=3) #by 5 ages, and 4 quarters
}
if(splitBy == "quarter"){
  splitFactor             <- matrix(NA,nrow=length(ages),ncol=4,dimnames=list(age=ages,split=c("Q1","Q2","Q3","Q4")))
  splitFactor[ac(0:4),]   <- matrix(aggregate(newDat$split,by=list(newDat$age,newDat$Q),   weighted.mean,w=rep(propQA$catchProportion[13:15],dim(newDat)[1]/5/3/4))$x,ncol=4) #by 5 ages, and 4 quarters
}
splitFactor[ac(5:9),]     <- 1
splitFactor[,"2224"]      <- 1


#===============================================================================
# Intermediate year
#===============================================================================

stfY@stock.n[,ImY]         <- stk.ica@survivors
stfY@stock.n[1,ImY]        <- RECS$ImY
stfY@stock[,ImY]           <- computeStock(stfY[,ImY])

stfY@harvest[,ImY]         <- fleet.harvest(stfY,ImY,unlist(TACS)[seq(1,12,3)] * pmin(ifelse(is.infinite(propTACUsed[,DtY]),1,propTACUsed[,DtY]),1)) #Decide on usage of TAC's by fleet
for(i in dms$unit){
  if(i %in% c("C","D")){    areas <- "IIIa"}; if(i == "A"){ areas <- "IV"};  if(i == "F"){ areas <- "2224"}
  stfY@catch.n[,ImY,i]     <- stfY@stock.n[,ImY,i]*(1-exp(-unitSums(stfY@harvest[,ImY])-stfY@m[,ImY,i]))*(stfY@harvest[,ImY,i]/(unitSums(stfY@harvest[,ImY])+stfY@m[,ImY,i]))*splitFactor[-10,areas]
  stfY@catch[,ImY,i]       <- computeCatch(stfY[,ImY,i])
  stfY@landings.n[,ImY,i]  <- stfY@catch.n[,ImY,i]
  stfY@landings[,ImY,i]    <- computeLandings(stfY[,ImY,i])
}

#Intermediate year stf option table
stf.table         <- matrix(NA,nrow=13,ncol=20,dimnames=list("options"=c("intermediate year",
                            "ToR-a No IVa catch","ToR-a A catch from share C D","ToR-a A catch from C D F share",
                            "ToR-b No IVa catch","ToR-b A catch from share C D","ToR-b A catch from C D F share",
                            "ToR-c No IVa catch","ToR-c A catch from share C D","ToR-c A catch from C D F share",
                            "ToR-d No IVa catch","ToR-d A catch from share C D","ToR-d A catch from C D F share"),
                            "values"=c("Fbar 3-6 A","Fbar 3-6 C","Fbar 0-1 D","Fbar 3-6 F","Fbar 3-6","Fbar 0-1",
                            "Catch A","Catch C","Catch D","Catch F","SSB","SSB",
                            "TAC A","TAC C","TAC D","TAC F",
                            "TAC A EC","TAC C EC","TAC D EC","TAC F EC")))
stf.table[1,1:11] <- c(round(c(apply(stfY@harvest[f36,ImY,c("A","C")],3,mean),mean(stfY@harvest[f01,ImY,c("D")]),apply(stfY@harvest[f36,ImY,c("F")],3,mean),mean(apply(stfY@harvest[f36,ImY],1,sum,na.rm=T)),mean(apply(stfY@harvest[f01,ImY],1,sum,na.rm=T))),3),
                       round(stfY@catch[,ImY],0),
                       round(sum(stfY@stock.n[,ImY,1]*stfY@stock.wt[,ImY,1]*exp(-apply(stfY@harvest[,ImY],1,sum)*stfY@harvest.spwn[,ImY,1]-stfY@m[,ImY,1]*stfY@m.spwn[,ImY,1])*stfY@mat[,ImY,1]),0))

ImYCatch          <- sum(stfY@catch[,ImY])
#===============================================================================
# Forecast year
#===============================================================================

if("fmsy" %in% stf.options){
  #- Set F target based on Fmsy transition period
  Ftarget                             <- Fmsytrans(mean(apply(stfY@harvest[f36,ac(ImY)],1,sum)),Fmsy=0.25,Fpa=NA,SSB=stf.table[1,11],Bpa=110000)

  WBSS.proj@harvest[,ImY]     <- unitSums(stfY@harvest[,ImY])
  WBSS.proj@catch.n[,ImY]     <- unitSums(stfY@catch.n[,ImY])
  WBSS.proj@landings.n[,ImY]  <- WBSS.proj@catch.n[,ImY]
  

  target            <- fwdControl(data.frame(year=c(an(ImY),an(FcY)),quantity=c("landings","f"),val=c(ImYCatch,Ftarget)))
  WBSS.proj         <- fwd(WBSS.proj,ctrl=target,sr=list(model="geomean",params=FLPar(c(RECS$ImY))),sr.residuals=FLQuant(c(unlist(RECS)[1:2]/c(RECS$ImY)),dimnames=list(age=0,year=c(an(ImY),an(FcY)),unit="unique",season="all",area="unique",iter=1)),sr.residuals.mult=TRUE)

  totalCatch.n      <- catch.n(WBSS.proj[,FcY])
  totalCatch.wt     <- catch.wt(WBSS.proj[,FcY])
  totalCatch        <- computeCatch(WBSS.proj[,FcY])

  option            <- list(excludeIVaE=T,AFleetasCDFleet=T,AFleetnotasCDFleet=T)
    ToRB            <- (1.15*stfY@catch[,ImY,"A"] + 0.7 * sum(stfY@catch[,ImY,c("C","D")]))/sum(stfY@catch[,ImY,c("A","C","D")])
  scenario          <- list(a=list(C=0.925,"D"=1),b=list(C=c(ToRB),D=1),c=list(C=0.7,D=1),d=list(C=1,D=0.7)) #Scenario b is not correct yet, but all need checking


for(scen in c("a","b","c","d")){
  if(option$excludeIVaE==T){

    TACF              <- sum(0.5 * (totalCatch.n + totalCatch.n  * (1-splitFactor[-10,"2224"])) * totalCatch.wt)
    TACCD             <- sum(0.5 * (totalCatch.n + totalCatch.n  * (1-splitFactor[-10,"IIIa"])) * totalCatch.wt)
    TACC              <- (0.8 * TACCD) * scenario[[scen]]$C
    TACD              <- (0.2 * TACCD) * scenario[[scen]]$D
    TACA              <- 0
    
    TACS[["A"]][2] <- TACA;  TACS[["C"]][2] <- TACC;  TACS[["D"]][2] <- TACD;  TACS[["F"]][2] <- TACF
    

    
    stfY@stock.n[,FcY]         <- WBSS.proj@stock.n[,FcY]
    stfY@stock[,FcY]           <- computeStock(stfY[,FcY])
    stfY@harvest[,FcY]         <- stfY@harvest[,ImY]
    stfY@harvest[,FcY]         <- fleet.harvest(stfY,FcY,unlist(TACS)[seq(2,12,3)] * pmin(ifelse(is.infinite(propTACUsed[,DtY]),1,propTACUsed[,DtY]),1)) #Decide on usage of TAC's by fleet
    
    for(i in dms$unit){
      if(i %in% c("C","D")){    areas <- "IIIa"}; if(i == "A"){ areas <- "IV"};  if(i == "F"){ areas <- "2224"}
      stfY@catch.n[,FcY,i]     <- stfY@stock.n[,FcY,i]*(1-exp(-unitSums(stfY@harvest[,FcY])-stfY@m[,FcY,i]))*(stfY@harvest[,FcY,i]/(unitSums(stfY@harvest[,FcY])+stfY@m[,FcY,i]))*splitFactor[-10,areas]
      stfY@catch[,FcY,i]       <- computeCatch(stfY[,FcY,i])
      stfY@landings.n[,FcY,i]  <- stfY@catch.n[,FcY,i]
      stfY@landings[,FcY,i]    <- computeLandings(stfY[,FcY,i])
    }

    stfY@stock.n[,CtY]         <- survivors(RECS$CtY,stfY[,FcY])

    idx                        <- rownames(stf.table)[grep("No",rownames(stf.table))][grep(paste("ToR-",scen,sep=""),rownames(stf.table)[grep("No",rownames(stf.table))])]
    ssb.CtY                    <- sum(stfY@stock.n[,CtY,1] * stfY@stock.wt[,FcY,1] * stfY@mat[,FcY,1] * exp(-apply(stfY@harvest[,FcY],1,sum) * stfY@harvest.spwn[,FcY,1] - stfY@m[,FcY,1] * stfY@m.spwn[,FcY,1]))
    stf.table[idx,1:12]        <- c(round(c(apply(stfY@harvest[f36,FcY,c("A","C")],3,mean),mean(stfY@harvest[f01,FcY,c("D")]),apply(stfY@harvest[f36,FcY,c("F")],3,mean),mean(apply(stfY@harvest[f36,FcY],1,sum,na.rm=T)),mean(apply(stfY@harvest[f01,FcY],1,sum,na.rm=T))),3),
                                    round(stfY@catch[,FcY],0),
                                    round(sum(stfY@stock.n[,FcY,1]*stfY@stock.wt[,FcY,1]*exp(-apply(stfY@harvest[,FcY],1,sum)*stfY@harvest.spwn[,FcY,1]-stfY@m[,FcY,1]*stfY@m.spwn[,FcY,1])*stfY@mat[,FcY,1]),0),
                                    ssb.CtY)
                                    
    stf.table[idx,grep("TAC A",colnames(stf.table))[1]:dim(stf.table)[2]] <- c(TACS[["A"]][2],TACS[["C"]][2]/scenario[[scen]]$C,TACS[["D"]][2]/scenario[[scen]]$D,TACS[["F"]][2],
                                                                               TACS[["A"]][2],TACS[["C"]][2]                   ,TACS[["D"]][2]                   ,TACS[["F"]][2])
    
  }
  if(option$AFleetasCDFleet==T){

    TACF              <- sum(0.5 * (totalCatch.n + totalCatch.n  * (1-splitFactor[-10,"2224"])) * totalCatch.wt)
    TACA              <- TACS.orig[["A"]][2]
    TACCD             <- sum(0.5 * (totalCatch.n + totalCatch.n  * (1-splitFactor[-10,"IIIa"])) * totalCatch.wt) - TACA
    TACC              <- (0.8 * TACCD) * scenario[[scen]]$C
    TACD              <- (0.2 * TACCD) * scenario[[scen]]$D


    TACS[["A"]][2] <- TACA;  TACS[["C"]][2] <- TACC;  TACS[["D"]][2] <- TACD;  TACS[["F"]][2] <- TACF


    stfY@stock.n[,FcY]         <- WBSS.proj@stock.n[,FcY]
    stfY@stock[,FcY]           <- computeStock(stfY[,FcY])
    stfY@harvest[,FcY]         <- stfY@harvest[,ImY]
    stfY@harvest[,FcY]         <- fleet.harvest(stfY,FcY,unlist(TACS)[seq(2,12,3)] * pmin(ifelse(is.infinite(propTACUsed[,DtY]),1,propTACUsed[,DtY]),1)) #Decide on usage of TAC's by fleet

    for(i in dms$unit){
      if(i %in% c("C","D")){    areas <- "IIIa"}; if(i == "A"){ areas <- "IV"};  if(i == "F"){ areas <- "2224"}
      stfY@catch.n[,FcY,i]     <- stfY@stock.n[,FcY,i]*(1-exp(-unitSums(stfY@harvest[,FcY])-stfY@m[,FcY,i]))*(stfY@harvest[,FcY,i]/(unitSums(stfY@harvest[,FcY])+stfY@m[,FcY,i]))*splitFactor[-10,areas]
      stfY@catch[,FcY,i]       <- computeCatch(stfY[,FcY,i])
      stfY@landings.n[,FcY,i]  <- stfY@catch.n[,FcY,i]
      stfY@landings[,FcY,i]    <- computeLandings(stfY[,FcY,i])
    }
    
    stfY@stock.n[,CtY]         <- survivors(RECS$CtY,stfY[,FcY])

    idx                        <- rownames(stf.table)[grep("share C D",rownames(stf.table))][grep(paste("ToR-",scen,sep=""),rownames(stf.table)[grep("share C D",rownames(stf.table))])]
    ssb.CtY                    <- sum(stfY@stock.n[,CtY,1] * stfY@stock.wt[,FcY,1] * stfY@mat[,FcY,1] * exp(-apply(stfY@harvest[,FcY],1,sum) * stfY@harvest.spwn[,FcY,1] - stfY@m[,FcY,1] * stfY@m.spwn[,FcY,1]))
    stf.table[idx,1:12]        <- c(round(c(apply(stfY@harvest[f36,FcY,c("A","C")],3,mean),mean(stfY@harvest[f01,FcY,c("D")]),apply(stfY@harvest[f36,FcY,c("F")],3,mean),mean(apply(stfY@harvest[f36,FcY],1,sum,na.rm=T)),mean(apply(stfY@harvest[f01,FcY],1,sum,na.rm=T))),3),
                                    round(stfY@catch[,FcY],0),
                                    round(sum(stfY@stock.n[,FcY,1]*stfY@stock.wt[,FcY,1]*exp(-apply(stfY@harvest[,FcY],1,sum)*stfY@harvest.spwn[,FcY,1]-stfY@m[,FcY,1]*stfY@m.spwn[,FcY,1])*stfY@mat[,FcY,1]),0),
                                    ssb.CtY)
    stf.table[idx,grep("TAC A",colnames(stf.table))[1]:dim(stf.table)[2]] <- c(TACS[["A"]][2],TACS[["C"]][2]/scenario[[scen]]$C,TACS[["D"]][2]/scenario[[scen]]$D,TACS[["F"]][2],
                                                                               TACS[["A"]][2],TACS[["C"]][2]                   ,TACS[["D"]][2]                   ,TACS[["F"]][2])
  }
  if(option$AFleetnotasCDFleet==T){
    TACA              <- TACS.orig[["A"]][2]
    TACCDF            <- totalCatch - TACA ; prop <- c(1-(TACA / totalCatch))
    TACF              <- sum(0.5 * (totalCatch.n * prop + totalCatch.n *prop * (1-splitFactor[-10,"2224"])) * totalCatch.wt)
    TACCD             <- sum(0.5 * (totalCatch.n * prop + totalCatch.n *prop * (1-splitFactor[-10,"IIIa"])) * totalCatch.wt)
    TACC              <- (0.8 * TACCD) * scenario[[scen]]$C
    TACD              <- (0.2 * TACCD) * scenario[[scen]]$D


    TACS[["A"]][2] <- TACA;  TACS[["C"]][2] <- TACC;  TACS[["D"]][2] <- TACD;  TACS[["F"]][2] <- TACF

    stfY@stock.n[,FcY]         <- WBSS.proj@stock.n[,FcY]
    stfY@stock[,FcY]           <- computeStock(stfY[,FcY])
    stfY@harvest[,FcY]         <- stfY@harvest[,ImY]
    stfY@harvest[,FcY]         <- fleet.harvest(stfY,FcY,unlist(TACS)[seq(2,12,3)] * pmin(ifelse(is.infinite(propTACUsed[,DtY]),1,propTACUsed[,DtY]),1)) #Decide on usage of TAC's by fleet

    for(i in dms$unit){
      if(i %in% c("C","D")){    areas <- "IIIa"}; if(i == "A"){ areas <- "IV"};  if(i == "F"){ areas <- "2224"}
      stfY@catch.n[,FcY,i]     <- stfY@stock.n[,FcY,i]*(1-exp(-unitSums(stfY@harvest[,FcY])-stfY@m[,FcY,i]))*(stfY@harvest[,FcY,i]/(unitSums(stfY@harvest[,FcY])+stfY@m[,FcY,i]))*splitFactor[-10,areas]
      stfY@catch[,FcY,i]       <- computeCatch(stfY[,FcY,i])
      stfY@landings.n[,FcY,i]  <- stfY@catch.n[,FcY,i]
      stfY@landings[,FcY,i]    <- computeLandings(stfY[,FcY,i])
    }
    
    stfY@stock.n[,CtY]         <- survivors(RECS$CtY,stfY[,FcY])

    idx                        <- rownames(stf.table)[grep("C D F share",rownames(stf.table))][grep(paste("ToR-",scen,sep=""),rownames(stf.table)[grep("C D F share",rownames(stf.table))])]
    ssb.CtY                    <- sum(stfY@stock.n[,CtY,1] * stfY@stock.wt[,FcY,1] * stfY@mat[,FcY,1] * exp(-apply(stfY@harvest[,FcY],1,sum) * stfY@harvest.spwn[,FcY,1] - stfY@m[,FcY,1] * stfY@m.spwn[,FcY,1]))
    stf.table[idx,1:12]        <- c(round(c(apply(stfY@harvest[f36,FcY,c("A","C")],3,mean),mean(stfY@harvest[f01,FcY,c("D")]),apply(stfY@harvest[f36,FcY,c("F")],3,mean),mean(apply(stfY@harvest[f36,FcY],1,sum,na.rm=T)),mean(apply(stfY@harvest[f01,FcY],1,sum,na.rm=T))),3),
                                    round(stfY@catch[,FcY],0),
                                    round(sum(stfY@stock.n[,FcY,1]*stfY@stock.wt[,FcY,1]*exp(-apply(stfY@harvest[,FcY],1,sum)*stfY@harvest.spwn[,FcY,1]-stfY@m[,FcY,1]*stfY@m.spwn[,FcY,1])*stfY@mat[,FcY,1]),0),
                                    ssb.CtY)
    stf.table[idx,grep("TAC A",colnames(stf.table))[1]:dim(stf.table)[2]] <- c(TACS[["A"]][2],TACS[["C"]][2]/scenario[[scen]]$C,TACS[["D"]][2]/scenario[[scen]]$D,TACS[["F"]][2],
                                                                               TACS[["A"]][2],TACS[["C"]][2]                   ,TACS[["D"]][2]                   ,TACS[["F"]][2])


  }
}
}