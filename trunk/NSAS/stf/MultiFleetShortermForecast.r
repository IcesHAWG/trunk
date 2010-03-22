################################################################################
# Code to do the multifleet short term forecast for North Sea Herring
#
# By: Niels Hintzen
# IMARES
# 20 March 2010
#
################################################################################
rm(list=ls());
#Read in data
path <- "N:/Projecten/ICES WG/Haring werkgroep HAWG/2010/assessment2/NSAS/"
try(setwd(path))
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
#  TAC's for the C and D fleet for the Forecast and Continuation year come from the Danish

#- Source the code to read in the weights at age and numbers at age by fleet, to compute partial F's and partial weights
source("./data/readStfData.r")
source("./data/writeSTF.out.r")

#2009 retrospect: TACS      <- list("A"=c(168400,NA,NA),"B"=c(9800,NA,NA),"C"=c(5100,4300,4300),"D"=c(1500,980,980));
#                 TACS.orig <- list("A"=c(168400,NA,NA),"B"=c(9800,NA,NA),"C"=c(5100,4300,4300),"D"=c(1500,980,980))
#2009: TACS  <- list("A"=c(194233,NA,NA),"B"=c(7310,NA,NA),"C"=c(6538,5400,5400),"D"=c(2701,2200,2200));
#      TACS.orig <- list("A"=c(171000,NA,NA),"B"=c(15985,NA,NA),"C"=c(37722,5100,5100),"D"=c(8373,2000,2000))
#2010:
       TACS  <- list("A"=c(164300+903,NA,NA), "B"=c(13587,NA,NA),"C"=c(4300,1680,1680),"D"=c(980,490,490));
       TACS.orig <- list("A"=c(164300,NA,NA), "B"=c(13587,NA,NA),"C"=c(4300,1680,1680),"D"=c(980,490,490))

RECS  <- list("ImY"=NSH.ica@param["Recruitment prediction","Value"],"FcY"=exp(mean(log(rec(NSH)[,ac((range(NSH)["maxyear"]-7):(range(NSH)["maxyear"]))]))),
              "CtY"=exp(mean(log(rec(NSH)[,ac((range(NSH)["maxyear"]-7):(range(NSH)["maxyear"]))]))))
#2010 extra:
#RECS <- list("ImY"=34135668.24,"FcY"=38770600.39,"CtY"=38770600.39 )
FS    <- list("A"=FA,"B"=FB,"C"=FC,"D"=FD)
WS    <- list("A"=WA,"B"=WB,"C"=WC,"D"=WD)   

yrs1  <- list("m","m.spwn","harvest.spwn","stock.wt","stock.n")
yrs3  <- list("mat")

dsc   <- "North Sea Herring"
nam   <- "NSH"
dms   <- dimnames(stk@m)
dms$year <- c(rev(rev(dms$year)[1:3]),ImY,FcY,CtY)
dms$unit <- c("A","B","C","D")

f01   <- ac(0:1)
f26   <- ac(2:6)

stf.options <- c("mp","-15%","+15%","nf","bpa","tacro","fmsy") #mp=according to management plan, +/-% = TAC change, nf=no fishing, bpa=reach BPA in CtY or if ssb > bpa fish at fpa,
                                                        # tacro=same catch as last year
mp.options  <- c("i") #i=increase in B fleet is allowed, fro=B fleet takes fbar as year before
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
# Fill slots that are unique for the fleets
for(i in dms$unit){
  stf@harvest[,,i]    <- FS[[i]]
  stf@catch.wt[,,i]   <- WS[[i]]
}

#===============================================================================
# Intermediate year 
#===============================================================================

stf@stock.n[,ImY]     <- stk.ica@survivors
for(i in dms$unit){
  stf@harvest[,ImY,i] <- fleet.harvest(stf,i,ImY,TACS[[i]][1])
  stf@catch  [,ImY,i] <- sum(stf@stock.n[,ImY,i]*(1-exp(-stf@harvest[,ImY,i]-stf@m[,ImY,i]))*stf@catch.wt[,ImY,i]*(stf@harvest[,ImY,i]/(stf@harvest[,ImY,i]+stf@m[,ImY,i])))
}

#Intermediate year stf option table
stf.table <- matrix(NA,nrow=c(length(stf.options)+1),ncol=12,dimnames=list("options"=c("intermediate year",stf.options),
                    "values"=c("Fbar 2-6 A","Fbar 0-1 B","Fbar 0-1 C","Fbar 0-1 D","Fbar 2-6","Fbar 0-1","Catch A","Catch B","Catch C","Catch D","SSB","SSB")))
stf.table[1,1:11] <- c(round(c(mean(stf@harvest[f26,ImY,1]),apply(stf@harvest[f01,ImY,2:4],3,mean),mean(apply(stf@harvest[f26,ImY],1,sum,na.rm=T)),mean(apply(stf@harvest[f01,ImY],1,sum,na.rm=T))),3),
                       round(c(colSums((1-exp(-stf@harvest[,ImY]-stf@m[,ImY]))*stf@stock.n[,ImY]*stf@catch.wt[,ImY]*(stf@harvest[,ImY]/(stf@harvest[,ImY]+stf@m[,ImY])))),0),
                       round(sum(stf@stock.n[,ImY,1]*stf@stock.wt[,ImY,1]*exp(-apply(stf@harvest[,ImY],1,sum)*stf@harvest.spwn[,ImY,1]-stf@m[,ImY,1]*stf@m.spwn[,ImY,1])*stf@mat[,ImY,1]),0))

#===============================================================================
# Forecast year
#===============================================================================

stf@stock.n[,FcY] <- c(RECS$FcY,(stf@stock.n[,ImY,1]*exp(-apply(stf@harvest[,ImY],1,sum)-stf@m[,ImY,1]))[ac(range(stf)["min"]:(range(stf)["max"]-2)),],
                       sum((stf@stock.n[,ImY,1]*exp(-apply(stf@harvest[,ImY],1,sum)-stf@m[,ImY,1]))[ac((range(stf)["max"]-1):range(stf)["max"]),]))

stf@harvest[,FcY] <- stf@harvest[,ImY]
for(i in dms$unit){
  if(is.na(TACS[[i]][2])==F) stf@harvest[,FcY,i] <- fleet.harvest(stf,i,FcY,TACS[[i]][2])
}    

###--- Management options ---###

### Following the management plan ###
if("mp" %in% stf.options){ 
  res                           <- optim(par=c(1,1),fn=find.FAB,stk=window(stf,an(FcY),an(FcY)),f01=f01,f26=f26,mp.options=mp.options,control=list(reltol=0.000001))$par
  stf@harvest[,FcY,c("A")]      <- stf@harvest[,FcY,c("A")] * res[1]
  stf@harvest[,FcY,c("B")]      <- stf@harvest[,FcY,c("B")] * res[2]
                  
  ssb.CtY           <- sum(c(RECS$CtY,(stf@stock.n[,FcY,1]*exp(-apply(stf@harvest[,FcY],1,sum)-stf@m[,FcY,1]))[ac(range(stf)["min"]:(range(stf)["max"]-2)),],sum((stf@stock.n[,FcY,1]*exp(-apply(stf@harvest[,FcY],1,sum)-stf@m[,FcY,1]))[ac((range(stf)["max"]-1):range(stf)["max"]),]))
                       *stf@stock.wt[,FcY,1]*stf@mat[,FcY,1]*exp(-apply(stf@harvest[,FcY],1,sum)*stf@harvest.spwn[,FcY,1]-stf@m[,FcY,1]*stf@m.spwn[,FcY,1]))
  stf.table["mp",]  <- c(round(c(mean(stf@harvest[f26,FcY,1]),apply(stf@harvest[f01,FcY,2:4],3,mean),mean(apply(stf@harvest[f26,FcY],1,sum,na.rm=T)),mean(apply(stf@harvest[f01,FcY],1,sum,na.rm=T))),3),
                         round(c(colSums((1-exp(-stf@harvest[,FcY]-stf@m[,FcY]))*stf@stock.n[,FcY]*stf@catch.wt[,FcY]*(stf@harvest[,FcY]/(stf@harvest[,FcY]+stf@m[,FcY])))),0),
                         round(c(sum(stf@stock.n[,FcY,1]*stf@stock.wt[,FcY,1]*exp(-apply(stf@harvest[,FcY],1,sum)*stf@harvest.spwn[,FcY,1]-stf@m[,FcY,1]*stf@m.spwn[,FcY,1])*stf@mat[,FcY,1]),
                         ssb.CtY),0))
  #As this is most likely the agreed TAC for the B fleet, use this in all other scenario's too                       
  TACS[["B"]][2]    <-  sum((1-exp(-stf@harvest[,FcY,"B"]-stf@m[,FcY,"B"]))*stf@stock.n[,FcY,"B"]*stf@catch.wt[,FcY,"B"]*(stf@harvest[,FcY,"B"]/(stf@harvest[,FcY,"B"]+stf@m[,FcY,"B"])))
                   
}
### No fishing ###
if("nf" %in% stf.options){
  stf@harvest[,FcY] <- 0
  ssb.CtY           <- sum(c(RECS$CtY,(stf@stock.n[,FcY,1]*exp(-apply(stf@harvest[,FcY],1,sum)-stf@m[,FcY,1]))[ac(range(stf)["min"]:(range(stf)["max"]-2)),],sum((stf@stock.n[,FcY,1]*exp(-apply(stf@harvest[,FcY],1,sum)-stf@m[,FcY,1]))[ac((range(stf)["max"]-1):range(stf)["max"]),]))
                     *stf@stock.wt[,FcY,1]*stf@mat[,FcY,1]*exp(-apply(stf@harvest[,FcY],1,sum)*stf@harvest.spwn[,FcY,1]-stf@m[,FcY,1]*stf@m.spwn[,FcY,1]))
  stf.table["nf",]  <- c(round(c(mean(stf@harvest[f26,FcY,1]),apply(stf@harvest[f01,FcY,2:4],3,mean),mean(apply(stf@harvest[f26,FcY],1,sum,na.rm=T)),mean(apply(stf@harvest[f01,FcY],1,sum,na.rm=T))),3),
                         round(c(colSums((1-exp(-stf@harvest[,FcY]-stf@m[,FcY]))*stf@stock.n[,FcY]*stf@catch.wt[,FcY]*(stf@harvest[,FcY]/(stf@harvest[,FcY]+stf@m[,FcY])))),0),
                         round(c(sum(stf@stock.n[,FcY,1]*stf@stock.wt[,FcY,1]*exp(-apply(stf@harvest[,FcY],1,sum)*stf@harvest.spwn[,FcY,1]-stf@m[,FcY,1]*stf@m.spwn[,FcY,1])*stf@mat[,FcY,1]),
                         ssb.CtY),0))
}

### 15% reduction in TAC for the A-fleet ###
if("-15%" %in% stf.options){

  #reset harvest for all fleets
  stf@harvest[,FcY] <- stf@harvest[,ImY]
  for(i in dms$unit){
    if(is.na(TACS[[i]][2])==F) stf@harvest[,FcY,i] <- fleet.harvest(stf,i,FcY,TACS[[i]][2])
  }
  #Take the original TAC, not the overshooted TAC 
  TAC.A <- TACS.orig[["A"]][1]*0.85
  stf@harvest[,FcY,"A"] <- fleet.harvest(stf,"A",FcY,TAC.A)
    
  ssb.CtY           <- sum(c(RECS$CtY,(stf@stock.n[,FcY,1]*exp(-apply(stf@harvest[,FcY],1,sum)-stf@m[,FcY,1]))[ac(range(stf)["min"]:(range(stf)["max"]-2)),],
                           sum((stf@stock.n[,FcY,1]*exp(-apply(stf@harvest[,FcY],1,sum)-stf@m[,FcY,1]))[ac((range(stf)["max"]-1):range(stf)["max"]),]))
                     *stf@stock.wt[,FcY,1]*stf@mat[,FcY,1]*exp(-apply(stf@harvest[,FcY],1,sum)*stf@harvest.spwn[,FcY,1]-stf@m[,FcY,1]*stf@m.spwn[,FcY,1]))
  stf.table["-15%",]  <- c(round(c(mean(stf@harvest[f26,FcY,1]),apply(stf@harvest[f01,FcY,2:4],3,mean),mean(apply(stf@harvest[f26,FcY],1,sum,na.rm=T)),
                           mean(apply(stf@harvest[f01,FcY],1,sum,na.rm=T))),3),
                           round(c(colSums((1-exp(-stf@harvest[,FcY]-stf@m[,FcY]))*stf@stock.n[,FcY]*stf@catch.wt[,FcY]*(stf@harvest[,FcY]/(stf@harvest[,FcY]+stf@m[,FcY])))),0),
                           round(c(sum(stf@stock.n[,FcY,1]*stf@stock.wt[,FcY,1]*exp(-apply(stf@harvest[,FcY],1,sum)*stf@harvest.spwn[,FcY,1]-stf@m[,FcY,1]*stf@m.spwn[,FcY,1])*stf@mat[,FcY,1]),
                           ssb.CtY),0))
}

### 15% increase in TAC for the A-fleet ###
if("+15%" %in% stf.options){
  #reset harvest for all fleets
  stf@harvest[,FcY] <- stf@harvest[,ImY]
  for(i in dms$unit){
    if(is.na(TACS[[i]][2])==F) stf@harvest[,FcY,i] <- fleet.harvest(stf,i,FcY,TACS[[i]][2])
  }
  #Take the original TAC, not the overshooted TAC 
  TAC.A <- TACS.orig[["A"]][1]*1.15
  stf@harvest[,FcY,"A"] <- fleet.harvest(stf,"A",FcY,TAC.A)
    
  ssb.CtY           <- sum(c(RECS$CtY,(stf@stock.n[,FcY,1]*exp(-apply(stf@harvest[,FcY],1,sum)-stf@m[,FcY,1]))[ac(range(stf)["min"]:(range(stf)["max"]-2)),],
                           sum((stf@stock.n[,FcY,1]*exp(-apply(stf@harvest[,FcY],1,sum)-stf@m[,FcY,1]))[ac((range(stf)["max"]-1):range(stf)["max"]),]))
                     *stf@stock.wt[,FcY,1]*stf@mat[,FcY,1]*exp(-apply(stf@harvest[,FcY],1,sum)*stf@harvest.spwn[,FcY,1]-stf@m[,FcY,1]*stf@m.spwn[,FcY,1]))
  stf.table["+15%",]  <- c(round(c(mean(stf@harvest[f26,FcY,1]),apply(stf@harvest[f01,FcY,2:4],3,mean),mean(apply(stf@harvest[f26,FcY],1,sum,na.rm=T)),
                           mean(apply(stf@harvest[f01,FcY],1,sum,na.rm=T))),3),
                           round(c(colSums((1-exp(-stf@harvest[,FcY]-stf@m[,FcY]))*stf@stock.n[,FcY]*stf@catch.wt[,FcY]*(stf@harvest[,FcY]/(stf@harvest[,FcY]+stf@m[,FcY])))),0),
                           round(c(sum(stf@stock.n[,FcY,1]*stf@stock.wt[,FcY,1]*exp(-apply(stf@harvest[,FcY],1,sum)*stf@harvest.spwn[,FcY,1]-stf@m[,FcY,1]*stf@m.spwn[,FcY,1])*stf@mat[,FcY,1]),
                           ssb.CtY),0))
}

### Same TAC for A-fleet as last year ###
if("tacro" %in% stf.options){
   #reset harvest for all fleets
  stf@harvest[,FcY] <- stf@harvest[,ImY]

  for(i in dms$unit){
    if(is.na(TACS[[i]][2])==F) stf@harvest[,FcY,i] <- fleet.harvest(stf,i,FcY,TACS[[i]][2])
  }
  #Take the original TAC, not the overshooted TAC 
  stf@harvest[,FcY,"A"] <- fleet.harvest(stf,"A",FcY,TACS.orig[["A"]][1])

  ssb.CtY           <- sum(c(RECS$CtY,(stf@stock.n[,FcY,1]*exp(-apply(stf@harvest[,FcY],1,sum)-stf@m[,FcY,1]))[ac(range(stf)["min"]:(range(stf)["max"]-2)),],
                           sum((stf@stock.n[,FcY,1]*exp(-apply(stf@harvest[,FcY],1,sum)-stf@m[,FcY,1]))[ac((range(stf)["max"]-1):range(stf)["max"]),]))
                     *stf@stock.wt[,FcY,1]*stf@mat[,FcY,1]*exp(-apply(stf@harvest[,FcY],1,sum)*stf@harvest.spwn[,FcY,1]-stf@m[,FcY,1]*stf@m.spwn[,FcY,1]))
  stf.table["tacro",]  <- c(round(c(mean(stf@harvest[f26,FcY,1]),apply(stf@harvest[f01,FcY,2:4],3,mean),mean(apply(stf@harvest[f26,FcY],1,sum,na.rm=T)),mean(apply(stf@harvest[f01,FcY],1,sum,na.rm=T))),3),
                            round(c(colSums((1-exp(-stf@harvest[,FcY]-stf@m[,FcY]))*stf@stock.n[,FcY]*stf@catch.wt[,FcY]*(stf@harvest[,FcY]/(stf@harvest[,FcY]+stf@m[,FcY])))),0),
                            round(c(sum(stf@stock.n[,FcY,1]*stf@stock.wt[,FcY,1]*exp(-apply(stf@harvest[,FcY],1,sum)*stf@harvest.spwn[,FcY,1]-stf@m[,FcY,1]*stf@m.spwn[,FcY,1])*stf@mat[,FcY,1]),
                            ssb.CtY),0))
}

### Bpa in continuation year ###
if("bpa" %in% stf.options){
   #reset harvest for all fleets
  stf@harvest[,FcY] <- stf@harvest[,ImY]
  #Scale harvests of all fleets to management procedure outcomes - fixed ones
  for(i in dms$unit){
    if(is.na(TACS[[i]][2])==F) stf@harvest[,FcY,i] <- fleet.harvest(stf,i,FcY,TACS[[i]][2])
  }
  stf@harvest[,FcY,"A"] <- fleet.harvest(stf,"A",FcY,stf.table["mp","Catch A"])
  stf@harvest[,FcY,"B"] <- fleet.harvest(stf,"B",FcY,stf.table["mp","Catch B"])
  
  res <- optimize(find.Bpa,c(0,10),stk=window(stf,an(FcY),an(CtY)),rec=RECS$FcY,bpa=1.3e6,fpa=0.25,f26,tol=0.000001)$minimum
  stf@harvest[,FcY,1:2] <- stf@harvest[,FcY,1:2] * res
  ssb.CtY           <- sum(c(RECS$CtY,(stf@stock.n[,FcY,1]*exp(-apply(stf@harvest[,FcY],1,sum)-stf@m[,ImY,1]))[ac(range(stf)["min"]:(range(stf)["max"]-2)),],
                           sum((stf@stock.n[,FcY,1]*exp(-apply(stf@harvest[,FcY],1,sum)-stf@m[,FcY,1]))[ac((range(stf)["max"]-1):range(stf)["max"]),]))
                     *stf@stock.wt[,FcY,1]*stf@mat[,FcY,1]*exp(-apply(stf@harvest[,FcY],1,sum)*stf@harvest.spwn[,FcY,1]-stf@m[,FcY,1]*stf@m.spwn[,FcY,1]))
  stf.table["bpa",]  <- c(round(c(mean(stf@harvest[f26,FcY,1]),apply(stf@harvest[f01,FcY,2:4],3,mean),mean(apply(stf@harvest[f26,FcY],1,sum,na.rm=T)),
                          mean(apply(stf@harvest[f01,FcY],1,sum,na.rm=T))),3),
                          round(c(colSums((1-exp(-stf@harvest[,FcY]-stf@m[,FcY]))*stf@stock.n[,FcY]*stf@catch.wt[,FcY]*(stf@harvest[,FcY]/(stf@harvest[,FcY]+stf@m[,FcY])))),0),
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
stf.out.file <- stf.out(stf,RECS,format="TABLE 3.7.%i NORTH SEA HERRING.")
write(stf.out.file,file=paste(output.base,"stf.out",sep="."))

#- Write the stf.table to file
write.table(stf.table,file=paste(output.base,"stf.table",sep="."),append=F,col.names=T,row.names=T)

#- Save the output to an RData file
save.image(file=paste(output.base,"ShortTermForecast Workspace.RData"))
