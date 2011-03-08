#Agreed TACS according to EU times over- or underfishing in the intermediate year
OOF   <- read.csv(paste("./data/over_underfishing",DtY,".csv",sep=""),header=T); dims.OOF <- dim(OOF)
#TAC.A <- (171000 +1000)   * mean(OOF[(dims.OOF[1]-2):dims.OOF[1],"A"])  #transfer for Norwegian fleet to transfer quota
#TAC.B <- 15985            * mean(OOF[dims.OOF[1],"B"]) #only last years fractions
#TAC.C <- (37722 -1000)    * mean(OOF[dims.OOF[1],"C"]) #only last years fractions     #1000 tons transfer in agreed EU-Norway paper
#TAC.D <- 8373             * mean(OOF[dims.OOF[1],"D"]) #only last years fractions

#Partial Fs per fleet
#Fs            <- read.csv(paste("./data/partial_fs",ac(an(ImY)-1),".csv",sep=""),header=T)
#FA            <- NSH@harvest[,DtY] * Fs[,"An"]
#FB            <- NSH@harvest[,DtY] * Fs[,"Bn"]
#FC            <- NSH@harvest[,DtY] * Fs[,"Cn"]
#FD            <- NSH@harvest[,DtY] * Fs[,"Dn"]

#Partial Ns per fleet to partial Fs
Ns            <- read.csv(paste("./data/partial_ns",ac(an(ImY)-1),".csv",sep=""),header=T)
FA            <- Ns[,paste("A",DtY,sep="")]/apply(Ns,1,sum) * NSH@harvest[,DtY]
FB            <- Ns[,paste("B",DtY,sep="")]/apply(Ns,1,sum) * NSH@harvest[,DtY]
FC            <- Ns[,paste("C",DtY,sep="")]/apply(Ns,1,sum) * NSH@harvest[,DtY]
FD            <- Ns[,paste("D",DtY,sep="")]/apply(Ns,1,sum) * NSH@harvest[,DtY]

#Partial Ws per fleet
Ws            <- read.csv(paste("./data/partial_ws",ac(an(ImY)-1),".csv",sep=""),header=T)
WA            <- apply(Ws[,paste("A",ac((an(DtY)-2):DtY),sep="")],1,mean,na.rm=T)
WB            <- apply(Ws[,paste("B",ac((an(DtY)-2):DtY),sep="")],1,mean,na.rm=T)
WC            <- apply(Ws[,paste("C",ac((an(DtY)-2):DtY),sep="")],1,mean,na.rm=T)
WD            <- apply(Ws[,paste("D",ac((an(DtY)-2):DtY),sep="")],1,mean,na.rm=T)

fleet.harvest <- function(stk,iYr,TACS){
                    nUnits              <- dims(stk)$unit
                    if(length(TACS)!=nUnits) stop("Number of TACS supplied is not equal to number of units")
                    res                 <- nls.lm(par=rep(1,nUnits),rescaleF,stk=stk,iYr=iYr,TACS=TACS,nls.lm.control(ftol = (.Machine$double.eps)),jac=NULL)$par
                    stk@harvest[,iYr]   <- sweep(stk@harvest[,iYr],3,res,"*")
                 return(stk@harvest[,iYr])}
                 
rescaleF      <- function(mult,stk.=stk,iYr.=iYr,TACS.=TACS){
                    stk.@harvest[,iYr.] <- sweep(stk.@harvest[,iYr.],3,mult,"*")
                    stkZ                <- unitSums(stk.@harvest[,iYr.]) + stk.@m[,iYr.,1]
                    res                 <- sqrt(c((TACS. - c(apply(sweep(stk.@stock.n[,iYr.] * stk.@catch.wt[,iYr.] * sweep(stk.@harvest[,iYr.],c(1:2,4:6),stkZ,"/"),c(1:2,4:6),(1-exp(-stkZ)),"*"),3:6,sum,na.rm=T)))^2))
                 return(res)}


find.FAB      <- function(mult,stk.=stk,f01.=f01,f26.=f26,mp.options.=mp.options,TACS.=TACS){
                    stk.@harvest[,,1]  <- stk.@harvest[,,1]*mult[1]
                    stk.@harvest[,,3]  <- stk.@harvest[,,3]*mult[3] #there is a TAC constraint for this fleet
                    stk.@harvest[,,4]  <- stk.@harvest[,,4]*mult[4] #there is a TAC constraint for this fleet
                    if(mp.options. == "i"){ stk.@harvest[,,2]  <- stk.@harvest[,,2]*mult[2]}
                    if(mp.options. == "fro"){ stk.@harvest[,,2] <- stk.@harvest[,,2]}
                    bigF              <- apply(stk.@harvest,1,sum)
                    ssb               <- sum(stk.@stock.n[,,1]*stk.@stock.wt[,,1]*exp(-bigF*stk.@harvest.spwn[,,1]-stk.@m[,,1]*stk.@m.spwn[,,1])*stk.@mat[,,1])
                    if(mp.options. == "i"){
                      if(ssb < 0.8e6){
                        resA <- 0.1
                        resB <- 0.04
                      }
                      if(ssb >= 0.8e6 & ssb <= 1.5e6){
                        resA <- 0.15/0.7*((ssb-0.8e6)/1e6)+0.1
                        resB <- 0.05
                      }
                      if(ssb > 1.5e6){
                        resA <- 0.25
                        resB <- 0.05
                      }
                      catchC    <- sum(stk.@stock.n[,,"C"]*(1-exp(-unitSums(stk.@harvest)-stk.@m[,,"C"]))*(stk.@harvest[,,"C"]/(unitSums(stk.@harvest)+stk.@m[,,"C"]))*stk.@catch.wt[,,"C"])
                      catchD    <- sum(stk.@stock.n[,,"D"]*(1-exp(-unitSums(stk.@harvest)-stk.@m[,,"D"]))*(stk.@harvest[,,"D"]/(unitSums(stk.@harvest)+stk.@m[,,"D"]))*stk.@catch.wt[,,"D"])
                      fbarB <- mean(bigF[f01.,])
                      fbarA <- mean(bigF[f26.,])
                      ret   <- c(sqrt(c((fbarA-resA)^2,(fbarB-resB)^2,(catchC - TACS.[3])^2,(catchD - TACS.[4])^2)))
                    } else {                    
                        if(ssb < 0.8e6) resA <- 0.1
                        if(ssb >= 0.8e6 & ssb <= 1.5e6) resA <- 0.15/0.7*((ssb-0.8e6)/1e6)+0.1
                        if(ssb > 1.5e6) resA <- 0.25
                        fbarA <- mean(bigF[f26.,])
                        catchC    <- sum(stk.@stock.n[,,"C"]*(1-exp(-unitSums(stk.@harvest)-stk.@m[,,"C"]))*(stk.@harvest[,,"C"]/(unitSums(stk.@harvest)+stk.@m[,,"C"]))*stk.@catch.wt[,,"C"])
                        catchD    <- sum(stk.@stock.n[,,"D"]*(1-exp(-unitSums(stk.@harvest)-stk.@m[,,"D"]))*(stk.@harvest[,,"D"]/(unitSums(stk.@harvest)+stk.@m[,,"D"]))*stk.@catch.wt[,,"D"])
                        ret   <- c(sqrt(c((fbarA-resA)^2,0,(catchC - TACS.[3])^2,(catchD - TACS.[4])^2)))
                      }
                    return(ret)}                    
#nls.lm(par=rep(1,dims(stf)$unit),find.Bpa,stk=stf,rec=RECS$FcY,bpa=1.3e6,fpa=0.25,f01=f01,f26=f26,iYr=FcY,TACS=unlist(TACS)[seq(2,12,3)],jac=NULL)$par
#find.Bpa <- function(mult,stk.=stk,rec.=rec,bpa.=bpa,fpa.=fpa,f26.=f26,f01.=f01,iYr.=iYr,TACS.=TACS){
#              FcY.                  <- iYr.
#              CtY.                  <- ac(an(FcY.)+1)
#              stk.@harvest[,FcY.]   <- sweep(stk.@harvest[,FcY.],3,mult,"*")
#              stkF                  <- unitSums(stk.@harvest[,FcY.])
#              stkZ                  <- unitSums(stk.@harvest[,FcY.]) + stk.@m[,FcY.,1]
#
#              stk.@stock.n[,CtY.]   <- c(rec.,(stk.@stock.n[,FcY.,1]*exp(-stkZ))[ac(range(stk.)["min"]:   (range(stk.)["max"]-2)),1],
#                                          sum((stk.@stock.n[,FcY.,1]*exp(-stkZ))[ac((range(stk.)["max"]-1):range(stk.)["max"]),1]))
#              ssb                   <- sum(stk.@stock.n[,CtY.,1] * stk.@stock.wt[,CtY.,1]*stk.@mat[,CtY.,1] * exp(-stkF * stk.@harvest.spwn[,CtY.,1] - stk.@m[,CtY.,1] * stk.@m.spwn[,CtY.,1]))
#              fbar26 <- mean(stkF[f26.,1])
#              fbar01 <- mean(stkF[f01.,1])
#              if(ssb < 0.8e6){
#                resA <- 0.1
#                resB <- 0.04
#              }
#              if(ssb >= 0.8e6 & ssb <= 1.5e6){
#                resA <- 0.15/0.7*((ssb-0.8e6)/1e6)+0.1
#                resB <- 0.05
#              }
#              if(ssb > 1.5e6){
#                resA <- 0.25
#                resB <- 0.05
#              }
#
#              f26mptarget <- resA
#              f01mptarget <- resB
#              catchB    <- sum(stk.@stock.n[,FcY.,"B"]*(1-exp(-unitSums(stk.@harvest[,FcY.])-stk.@m[,FcY.,"B"]))*(stk.@harvest[,FcY.,"B"]/(unitSums(stk.@harvest[,FcY.])+stk.@m[,FcY.,"B"]))*stk.@catch.wt[,FcY.,"B"])
#              catchC    <- sum(stk.@stock.n[,FcY.,"C"]*(1-exp(-unitSums(stk.@harvest[,FcY.])-stk.@m[,FcY.,"C"]))*(stk.@harvest[,FcY.,"C"]/(unitSums(stk.@harvest[,FcY.])+stk.@m[,FcY.,"C"]))*stk.@catch.wt[,FcY.,"C"])
#              catchD    <- sum(stk.@stock.n[,FcY.,"D"]*(1-exp(-unitSums(stk.@harvest[,FcY.])-stk.@m[,FcY.,"D"]))*(stk.@harvest[,FcY.,"D"]/(unitSums(stk.@harvest[,FcY.])+stk.@m[,FcY.,"D"]))*stk.@catch.wt[,FcY.,"D"])
#              if(fbar26>f26mptarget|fbar01>f01mptarget){ res <- c(sqrt((fbar26-f26mptarget)^2),sqrt((fbar01-f01mptarget)^2),sqrt((catchC - TACS.[3])^2),sqrt((catchC - TACS.[4])^2))
#              } else { res <- c(sqrt((bpa.-ssb)^2),sqrt((catchB - TACS.[2])^2),sqrt((catchC - TACS.[3])^2),sqrt((catchC - TACS.[4])^2))}
#              return(res)}
#
#nls.lm(par=rep(1,dims(stf)$unit),find.Bpa,stk=stf,rec=RECS$FcY,bpa=1.3e6,fpa=0.25,f01=f01,f26=f26,iYr=FcY,TACS=unlist(TACS)[seq(2,12,3)],jac=NULL)$par
#find.Bpa <- function(mult,stk.=stk,rec.=rec,bpa.=bpa,fpa.=fpa,f26.=f26,f01.=f01,iYr.=iYr,TACS.=TACS){
#              FcY.                  <- iYr.
#              CtY.                  <- ac(an(FcY.)+1)
#              stk.@harvest[,FcY.]   <- sweep(stk.@harvest[,FcY.],3,mult,"*")
#              stkF                  <- unitSums(stk.@harvest[,FcY.])
#              stkZ                  <- unitSums(stk.@harvest[,FcY.]) + stk.@m[,FcY.,1]
#
#
#              stk.@stock.n[,CtY.]   <- c(rec.,(stk.@stock.n[,FcY.,1]*exp(-stkZ))[ac(range(stk.)["min"]:   (range(stk.)["max"]-2)),1],
#                                          sum((stk.@stock.n[,FcY.,1]*exp(-stkZ))[ac((range(stk.)["max"]-1):range(stk.)["max"]),1]))
#              ssb                   <- sum(stk.@stock.n[,CtY.,1] * stk.@stock.wt[,CtY.,1]*stk.@mat[,CtY.,1] * exp(-stkF * stk.@harvest.spwn[,CtY.,1] - stk.@m[,CtY.,1] * stk.@m.spwn[,CtY.,1]))
#              fbar26 <- mean(stkF[f26.,1])
#              fbar01 <- mean(stkF[f01.,1])
#              catchB    <- sum(stk.@stock.n[,FcY.,"B"]*(1-exp(-unitSums(stk.@harvest[,FcY.])-stk.@m[,FcY.,"B"]))*(stk.@harvest[,FcY.,"B"]/(unitSums(stk.@harvest[,FcY.])+stk.@m[,FcY.,"B"]))*stk.@catch.wt[,FcY.,"B"])
#              catchC    <- sum(stk.@stock.n[,FcY.,"C"]*(1-exp(-unitSums(stk.@harvest[,FcY.])-stk.@m[,FcY.,"C"]))*(stk.@harvest[,FcY.,"C"]/(unitSums(stk.@harvest[,FcY.])+stk.@m[,FcY.,"C"]))*stk.@catch.wt[,FcY.,"C"])
#              catchD    <- sum(stk.@stock.n[,FcY.,"D"]*(1-exp(-unitSums(stk.@harvest[,FcY.])-stk.@m[,FcY.,"D"]))*(stk.@harvest[,FcY.,"D"]/(unitSums(stk.@harvest[,FcY.])+stk.@m[,FcY.,"D"]))*stk.@catch.wt[,FcY.,"D"])
#
#              if(fbar26>0.25|fbar01>0.05){ res <- c(sqrt((fbar26-fpa.)^2),sqrt((fbar01-0.05)^2),sqrt((catchC - TACS.[3])^2),sqrt((catchC - TACS.[4])^2))
#              } else { res <- c(sqrt((bpa.-ssb)^2),sqrt((catchB - TACS.[2])^2),sqrt((catchC - TACS.[3])^2),sqrt((catchC - TACS.[4])^2))}
#              return(res)}




find.Bpa <- function(mult,stk.=stk,rec.=rec,bpa.=bpa,fpa.=fpa,f26.=f26,f01.=f01,iYr.=iYr,TACS.=TACS){
              stk.@harvest[,1,1] <- stk@harvest[,1,1] * mult[1]
              stk.@harvest[,1,2] <- stk@harvest[,1,2] * mult[2]

              bigF <- unitSums(stk@harvest[,1])
              stk@stock.n[,2] <- c(rec,(stk@stock.n[,1,1]*exp(-bigF-stk@m[,1,1]))[ac(range(stk)["min"]:(range(stk)["max"]-2)),1],
                                sum((stk@stock.n[,1,1]*exp(-bigF-stk@m[,1,1]))[ac((range(stk)["max"]-1):range(stk)["max"]),1]))
              ssb <- sum(stk@stock.n[,2,1]*stk@stock.wt[,2,1]*stk@mat[,2,1]*exp(-bigF*stk@harvest.spwn[,2,1]-stk@m[,2,1]*stk@m.spwn[,2,1]))
              fbar26 <- mean(bigF[f26,1])
              fbar01 <- mean(bigF[f01,1])
              return(ifelse(fbar26>0.25|fbar01>0.05,(fbar26-fpa)^2+(fbar01-0.05)^2,(bpa-ssb)^2))}


                    
harvestCatch  <-  function(stk.,iYr){
                    stkZ      <- unitSums(stk.@harvest[,iYr]) + stk.@m[,iYr,1]
                    res       <- apply(sweep(stk.@stock.n[,iYr] * stk.@catch.wt[,iYr] * sweep(stk.@harvest[,iYr],c(1:2,4:6),stkZ,"/"),c(1:2,4:6),(1-exp(-stkZ)),"*"),3:6,sum,na.rm=T)
                  return(res)}