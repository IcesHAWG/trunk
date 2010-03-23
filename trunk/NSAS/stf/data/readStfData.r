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

fleet.harvest <- function(stk,fleet,yr,TAC){
                    res <- optimize(rescale.F,c(0,10),stk=stk,fleet=fleet,yr=yr,TAC=TAC,tol=0.000001)$minimum
                    stk@harvest[,yr,fleet] <- stk@harvest[,yr,fleet]*res
                    return(stk@harvest[,yr,fleet])}

rescale.F     <- function(mult,stk,fleet,yr,TAC){
                    stk@harvest[,yr,fleet] <- stk@harvest[,yr,fleet]*mult
                    return(c(TAC - sum(stk@stock.n[,yr,fleet]*(1-exp(-stk@harvest[,yr,fleet]-stk@m[,yr,fleet]))*stk@catch.wt[,yr,fleet]*
                                      (stk@harvest[,yr,fleet]/(stk@harvest[,yr,fleet]+stk@m[,yr,fleet]))))^2)}


find.F        <- function(mult,stk,fleet,f26,flim){
                    stk@harvest[,,1] <- stk@harvest[,,1] * mult
                    return((mean(apply(stk@harvest[f26,],1,sum))-0.1)^2)}
                    
find.FAB      <- function(mult,stk,f01,f26,mp.options){
                    stk@harvest[,,1]  <- stk@harvest[,,1]*mult[1]
                    if(mp.options == "i"){ stk@harvest[,,2]  <- stk@harvest[,,2]*mult[2]}
                    if(mp.options == "fro"){ stk@harvest[,,2] <- stk@harvest[,,2]}
                    bigF              <- apply(stk@harvest,1,sum)
                    ssb               <- sum(stk@stock.n[,,1]*stk@stock.wt[,,1]*exp(-bigF*stk@harvest.spwn[,,1]-stk@m[,,1]*stk@m.spwn[,,1])*stk@mat[,,1])
                    if(mp.options == "i"){
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
  
                      fbarB <- mean(bigF[f01,])
                      fbarA <- mean(bigF[f26,])
                      ret <- (fbarB-resB)^2+(fbarA-resA)^2
                    } else {                    
                        if(ssb < 0.8e6) resA <- 0.1
                        if(ssb >= 0.8e6 & ssb <= 1.5e6) resA <- 0.15/0.7*((ssb-0.8e6)/1e6)+0.1
                        if(ssb > 1.5e6) resA <- 0.25
                        fbarA <- mean(bigF[f26,])
                        ret   <- (fbarA-resA)^2+ (mult[2]-1)^2
                      }
                    return(ret)}                    
                    
find.Bpa <- function(mult,stk,rec,bpa,fpa,f26,f01){
              stk@harvest[,1,1] <- stk@harvest[,1,1] * mult[1]
              stk@harvest[,1,2] <- stk@harvest[,1,2] * mult[2]
              bigF <- unitSums(stk@harvest[,1])
              stk@stock.n[,2] <- c(rec,(stk@stock.n[,1,1]*exp(-bigF-stk@m[,1,1]))[ac(range(stk)["min"]:(range(stk)["max"]-2)),1],
                                sum((stk@stock.n[,1,1]*exp(-bigF-stk@m[,1,1]))[ac((range(stk)["max"]-1):range(stk)["max"]),1]))
              ssb <- sum(stk@stock.n[,2,1]*stk@stock.wt[,2,1]*stk@mat[,2,1]*exp(-bigF*stk@harvest.spwn[,2,1]-stk@m[,2,1]*stk@m.spwn[,2,1]))
              fbar26 <- mean(bigF[f26,1])
              fbar01 <- mean(bigF[f01,1])
              return(ifelse(fbar26>0.25|fbar01>0.05,(fbar26-fpa)^2+(fbar01-0.05)^2,(bpa-ssb)^2))}
              
find.FB      <- function(mult,stk,f01,f26){
                    stk@harvest[,,1]  <- stk@harvest[,,1]
                    stk@harvest[,,2]  <- stk@harvest[,,2]*mult
                    bigF              <- apply(stk@harvest,1,sum)
                    ssb               <- sum(stk@stock.n[,,1]*stk@stock.wt[,,1]*exp(-bigF*stk@harvest.spwn[,,1]-stk@m[,,1]*stk@m.spwn[,,1])*stk@mat[,,1])

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

                      fbarB <- mean(bigF[f01,])
                      ret <- (fbarB-resB)^2

                    return(ret)}
                    
