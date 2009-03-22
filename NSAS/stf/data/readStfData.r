#Agreed TACS according to EU times over- or underfishing
OOF   <- read.csv(paste("./data/over_underfishing",ac(an(ImY)-1),".csv",sep=""),header=T); dims.OOF <- dim(OOF)
TAC.A <- (171000 +1000)   * mean(OOF[(dims.OOF[1]-2):dims.OOF[1],"A"])  #transfer for Norwegian fleet to transfer quota
TAC.B <- 15985            * mean(OOF[dims.OOF[1],"B"]) #only last years fractions
TAC.C <- (37722 -1000)    * mean(OOF[dims.OOF[1],"C"]) #only last years fractions     #in agreed EU-Norway paper
TAC.D <- 8373             * mean(OOF[dims.OOF[1],"D"]) #only last years fractions

#Partial Fs per fleet
Fs            <- read.csv(paste("./data/partial_fs",ac(an(ImY)-1),".csv",sep=""),header=T)
FA            <- NSH@harvest[,ac(an(ImY)-1)] * Fs[,"An"]
FB            <- NSH@harvest[,ac(an(ImY)-1)] * Fs[,"Bn"]
FC            <- NSH@harvest[,ac(an(ImY)-1)] * Fs[,"Cn"]
FD            <- NSH@harvest[,ac(an(ImY)-1)] * Fs[,"Dn"]

#Partial Ws per fleet
Ws            <- read.csv(paste("./data/partial_ws",ac(an(ImY)-1),".csv",sep=""),header=T)
WA            <- apply(Ws[,paste("A",ac((an(ImY)-3):(an(ImY)-1)),sep="")],1,mean,na.rm=T)
WB            <- apply(Ws[,paste("B",ac((an(ImY)-3):(an(ImY)-1)),sep="")],1,mean,na.rm=T)
WC            <- apply(Ws[,paste("C",ac((an(ImY)-3):(an(ImY)-1)),sep="")],1,mean,na.rm=T)
WD            <- apply(Ws[,paste("D",ac((an(ImY)-3):(an(ImY)-1)),sep="")],1,mean,na.rm=T)

fleet.harvest <- function(stk,fleet,yr,TAC){
                    res <- optimize(rescale.F,c(0,2),stk=stk,fleet=fleet,yr=yr,TAC=TAC)$minimum
                    stk@harvest[,yr,fleet] <- stk@harvest[,yr,fleet]*res
                    return(stk@harvest[,yr,fleet])}

rescale.F     <- function(mult,stk,fleet,yr,TAC){
                    stk@harvest[,yr,fleet] <- stk@harvest[,yr,fleet]*mult
                    return(c(TAC - sum(stk@stock.n[,yr,fleet]*(1-exp(-stk@harvest[,yr,fleet]-stk@m[,yr,fleet]))*stk@catch.wt[,yr,fleet]*(stk@harvest[,yr,fleet]/(stk@harvest[,yr,fleet]+stk@m[,yr,fleet]))))^2)}


find.FAB      <- function(mult,stk,f01,f26){
                    stk@harvest[,,1]  <- stk@harvest[,,1]*mult[1]
                    stk@harvest[,,2]  <- stk@harvest[,,2]*mult[2]
                    bigF              <- apply(stk@harvest,1,sum)
                    ssb               <- sum(stk@stock.n[,,1]*stk@stock.wt[,,1]*exp(-bigF*stk@harvest.spwn[,,1]-stk@m[,,1]*stk@m.spwn[,,1])*stk@mat[,,1])
                    if(ssb < 0.8e6){
                      resA <- 0.1
                      resB <- 0.04
                    }
                    if(ssb >= 0.8e6 & ssb <= 1.5e6){
                      resA <- 0.15/0.7*((ssb-0.8e6)/1e6)+0.1
                      resB <- 0.04
                    }
                    if(ssb > 1.5e6){
                      resA <- 0.25
                      resB <- 0.05
                    }

                    fbarB <- mean(bigF[f01,])
                    fbarA <- mean(bigF[f26,])
                    return((fbarB-resB)^2+(fbarA-resA)^2)}
                    
find.Bpa <- function(mult,stk,rec,bpa){
              bigF <- unitSums(stk@harvest[,1]) * mult
              stk@stock.n[,2] <- c(rec,(stk@stock.n[,1,1]*exp(-bigF-stk@m[,1,1]))[ac(range(stk)["min"]:(range(stk)["max"]-2)),],sum((stk@stock.n[,1,1]*exp(-bigF-stk@m[,1,1]))[ac((range(stk)["max"]-1):range(stk)["max"]),]))
              ssb <- sum(stk@stock.n[,2,1]*stk@stock.wt[,2,1]*stk@mat[,2,1]*exp(-bigF*stk@harvest.spwn[,2,1]-stk@m[,2,1]*stk@m.spwn[,2,1]))
              return((bpa-ssb)^2)}