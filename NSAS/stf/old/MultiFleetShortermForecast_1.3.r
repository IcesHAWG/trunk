#Code to do the multifleet short term forecast

#===============================================================================
# Intermediate year
#===============================================================================
ImY <- ac(2009)
FcY <- ac(2010)

#Agreed TACS according to EU times over- or underfishing
OOF   <- read.csv(paste("./stf/data/over_underfishing",ac(an(ImY)-1),".csv",sep=""),header=T); dims.OOF <- dim(OOF)
TAC.A <- (171000 +1000)   * mean(OOF[(dims.OOF[1]-2):dims.OOF[1],"A"])  #transfer for Norwegian fleet to transfer quota
TAC.B <- 15985            * mean(OOF[(dims.OOF[1]-2):dims.OOF[1],"B"])
TAC.C <- (37722 -1000)    * mean(OOF[(dims.OOF[1]-2):dims.OOF[1],"C"])           #(max 50%)
TAC.D <- 8373             * mean(OOF[(dims.OOF[1]-2):dims.OOF[1],"D"])

#Predicted recruitment and geometric mean
REC.ImY         <- NSH.ica@param["Recruitment prediction","Value"]
REC.FcY         <- exp(mean(log(rec(NSH)[,ac((range(NSH)["maxyear"]-6):(range(NSH)["maxyear"]))])))

#Partial Fs per fleet
Fs            <- read.csv(paste("./stf/data/partial_fs",ac(an(ImY)-1),".csv",sep=""),header=T)
FA            <- NSH@harvest[,ac(an(ImY)-1)] * Fs[,"An"]
FB            <- NSH@harvest[,ac(an(ImY)-1)] * Fs[,"Bn"]
FC            <- NSH@harvest[,ac(an(ImY)-1)] * Fs[,"Cn"]
FD            <- NSH@harvest[,ac(an(ImY)-1)] * Fs[,"Dn"]

#Partial Ws per fleet
Ws            <- read.csv(paste("./stf/data/partial_ws",ac(an(ImY)-1),".csv",sep=""),header=T)
WA            <- apply(Ws[,paste("A",ac((an(ImY)-3):(an(ImY)-1)),sep="")],1,mean,na.rm=T)
WB            <- apply(Ws[,paste("B",ac((an(ImY)-3):(an(ImY)-1)),sep="")],1,mean,na.rm=T)
WC            <- apply(Ws[,paste("C",ac((an(ImY)-3):(an(ImY)-1)),sep="")],1,mean,na.rm=T)
WD            <- apply(Ws[,paste("D",ac((an(ImY)-3):(an(ImY)-1)),sep="")],1,mean,na.rm=T)

#Extend year-range in stock object
NSH.ImY       <- window(NSH,1960,an(ImY))

#Fill the slots
for(slot. in c("m","mat","m.spwn","harvest.spwn","stock.wt")) slot(NSH.ImY,slot.)[,ImY] <- slot(NSH.ImY,slot.)[,ac(an(ImY)-1)]
NSH.ImY@stock.n[,ImY]           <- c(REC.ImY,(NSH.ImY@stock.n[,ac(an(ImY)-1)]*exp(-NSH@harvest[,ac(an(ImY)-1)]-NSH@m[,ac(an(ImY)-1)]))[1:8,],sum((NSH.ImY@stock.n[,ac(an(ImY)-1)]*exp(-NSH@harvest[,ac(an(ImY)-1)]-NSH@m[,ac(an(ImY)-1)]))[9:10,]))

NSH.ImY@stock.wt[,ImY]          <- NSH.ImY@stock.wt[,ac(an(ImY)-1)]

#Compute the Catch by each fleet according to their fpattern and TAC
rescale.F <- function(mult,NSH.ImY,ImY,F.,W.,TAC){
              F. <- F.*mult
              return(abs(TAC - sum(NSH.ImY@stock.n[,ac(an(ImY)-1)]*(1-exp(-F.-NSH.ImY@m[,ac(an(ImY)-1)]))*W.*(F./(F.+NSH.ImY@m[,ac(an(ImY)-1)])))))}
              
fmult               <- list()                         
fmult[["A"]]        <- optimize(rescale.F,c(0,2),NSH.ImY=NSH.ImY,ImY=ImY,F.=FA,W.=WA,TAC=TAC.A)$minimum
fmult[["B"]]        <- optimize(rescale.F,c(0,2),NSH.ImY=NSH.ImY,ImY=ImY,F.=FB,W.=WB,TAC=TAC.B)$minimum
fmult[["C"]]        <- optimize(rescale.F,c(0,2),NSH.ImY=NSH.ImY,ImY=ImY,F.=FC,W.=WC,TAC=TAC.C)$minimum
fmult[["D"]]        <- optimize(rescale.F,c(0,2),NSH.ImY=NSH.ImY,ImY=ImY,F.=FD,W.=WD,TAC=TAC.D)$minimum
fmult[["tot"]]      <- optimize(rescale.F,c(0,2),NSH.ImY=NSH.ImY,ImY=ImY,F.=FA+FB+FC+FD,W.=(WA*Fs[,"An"]+WB*Fs[,"Bn"]+WC*Fs[,"Cn"]+WD*Fs[,"Dn"]),TAC=TAC.A+TAC.B+TAC.C+TAC.D)$minimum
NSH.ImY@harvest[,ImY] <- FA*fmult[["A"]]+FB*fmult[["B"]]+FC*fmult[["C"]]+FD*fmult[["D"]]

Catch.ImY           <- list()  
Catch.ImY[["A"]]    <- sum(NSH.ImY@stock.n[,ac(an(ImY)-1)]*(1-exp(-FA*fmult[["A"]]-NSH.ImY@m[,ac(an(ImY)-1)]))*WA*(FA*fmult[["A"]]/(FA*fmult[["A"]]+NSH.ImY@m[,ac(an(ImY)-1)])))
Catch.ImY[["B"]]    <- sum(NSH.ImY@stock.n[,ac(an(ImY)-1)]*(1-exp(-FB*fmult[["B"]]-NSH.ImY@m[,ac(an(ImY)-1)]))*WB*(FB*fmult[["B"]]/(FB*fmult[["B"]]+NSH.ImY@m[,ac(an(ImY)-1)])))
Catch.ImY[["C"]]    <- sum(NSH.ImY@stock.n[,ac(an(ImY)-1)]*(1-exp(-FC*fmult[["C"]]-NSH.ImY@m[,ac(an(ImY)-1)]))*WC*(FC*fmult[["C"]]/(FC*fmult[["C"]]+NSH.ImY@m[,ac(an(ImY)-1)])))
Catch.ImY[["D"]]    <- sum(NSH.ImY@stock.n[,ac(an(ImY)-1)]*(1-exp(-FD*fmult[["D"]]-NSH.ImY@m[,ac(an(ImY)-1)]))*WD*(FD*fmult[["D"]]/(FD*fmult[["D"]]+NSH.ImY@m[,ac(an(ImY)-1)])))
Catch.ImY[["tot"]]  <- sum(NSH.ImY@stock.n[,ac(an(ImY)-1)]*(1-exp(-(FA+FB+FC+FD)*fmult[["tot"]]-NSH.ImY@m[,ac(an(ImY)-1)]))*(WA*Fs[,"An"]+WB*Fs[,"Bn"]+WC*Fs[,"Cn"]+WD*Fs[,"Dn"])*((FA+FB+FC+FD)*fmult[["tot"]]/((FA+FB+FC+FD)*fmult[["tot"]]+NSH.ImY@m[,ac(an(ImY)-1)])))

Fbar.ImY            <- list()  
Fbar.ImY[["A"]]     <- mean(FA[3:7]*fmult[["A"]])
Fbar.ImY[["B"]]     <- mean(FB[1:2]*fmult[["B"]])
Fbar.ImY[["C"]]     <- mean(FC[4:7]*fmult[["C"]])
Fbar.ImY[["D"]]     <- mean(FD[1:2]*fmult[["D"]])
mean(NSH.ImY@harvest[3:7,ImY])
mean(NSH.ImY@harvest[1:2,ImY])
ssb(NSH)
#===============================================================================
# Forecast year
#===============================================================================

NSH.FcY <- window(NSH.ImY,1960,an(FcY))

#Fill the slots

#Or copy these slot directly from the ImY!!
for(slot. in c("m","mat","m.spwn","harvest.spwn","stock.wt")) slot(NSH.FcY,slot.)[,FcY] <- slot(NSH.ImY,slot.)[,ac(an(ImY)-1)]
NSH.FcY@stock.n[,FcY]           <- c(REC.FcY,(NSH.FcY@stock.n[,ac(an(FcY)-1)]*exp(-NSH.FcY@harvest[,ac(an(FcY)-1)]-NSH.FcY@m[,ac(an(FcY)-1)]))[1:8,],sum((NSH.FcY@stock.n[,ac(an(FcY)-1)]*exp(-NSH.FcY@harvest[,ac(an(FcY)-1)]-NSH.FcY@m[,ac(an(FcY)-1)]))[9:10,]))
NSH.FcY@stock.wt[,FcY]          <- NSH.FcY@stock.wt[,ac(an(FcY)-1)]

#Update harvest in ImY
FA <- fmult[["A"]]*FA 
FB <- fmult[["B"]]*FB 
FC <- fmult[["C"]]*FC 
FD <- fmult[["D"]]*FD 

### Option: Following management plan ###

res <- optim(c(1,1),find.FAB,stk=window(NSH.FcY,an(FcY),an(FcY)),F.A=FA,F.B=FB,F.C=FC,F.D=FD)$par

FA <- FA*res[1]
FB <- FB*res[2]

Catch.FcY           <- list()
Catch.FcY[["A"]]    <- sum(NSH.FcY@stock.n[,ac(an(FcY)-1)]*(1-exp(-FA-NSH.FcY@m[,ac(an(FcY)-1)]))*WA*(FA/(FA+NSH.FcY@m[,ac(an(FcY)-1)])))
Catch.FcY[["B"]]    <- sum(NSH.FcY@stock.n[,ac(an(FcY)-1)]*(1-exp(-FB-NSH.FcY@m[,ac(an(FcY)-1)]))*WB*(FB/(FB+NSH.FcY@m[,ac(an(FcY)-1)])))
Catch.FcY[["C"]]    <- sum(NSH.FcY@stock.n[,ac(an(FcY)-1)]*(1-exp(-FC-NSH.FcY@m[,ac(an(FcY)-1)]))*WC*(FC/(FC+NSH.FcY@m[,ac(an(FcY)-1)])))
Catch.FcY[["D"]]    <- sum(NSH.FcY@stock.n[,ac(an(FcY)-1)]*(1-exp(-FD-NSH.FcY@m[,ac(an(FcY)-1)]))*WD*(FD/(FD+NSH.FcY@m[,ac(an(FcY)-1)])))

Fbar.FcY            <- list()  
Fbar.FcY[["A"]]     <- mean(FA[3:7])
Fbar.FcY[["B"]]     <- mean(FB[1:2])
Fbar.FcY[["C"]]     <- mean(FC[4:7])
Fbar.FcY[["D"]]     <- mean(FD[1:2])
NSH.FcY@harvest[,FcY] <- FA+FB+FC+FD
mean(NSH.FcY@harvest[3:7,FcY])
mean(NSH.FcY@harvest[1:2,FcY])
ssb(NSH.FcY)

NSH.FcY2                          <- window(NSH.FcY,1960,an(FcY)+1)

for(slot. in c("m","mat","m.spwn","harvest.spwn","stock.wt")) slot(NSH.FcY2,slot.)[,ac(an(FcY)+1)] <- slot(NSH.ImY,slot.)[,ac(an(ImY)-1)]
NSH.FcY2@harvest[,ac(an(FcY)+1)]  <- NSH.FcY@harvest[,FcY]
NSH.FcY2@stock.n[,ac(an(FcY)+1)]  <- c(REC.FcY,c(NSH.FcY@stock.n[,FcY]*exp(-NSH.FcY@harvest[,FcY]-NSH.FcY@m[,FcY]))[1:8],sum((NSH.FcY@stock.n[,FcY]*exp(-NSH.FcY@harvest[,FcY]-NSH.FcY@m[,FcY]))[9:10]))

ssb(NSH.FcY2)

### Option: No fishing ###
NSH.FcY@harvest[,FcY] <- 0
ssb(NSH.FcY)

### Option: 15% TAC reduction for A fleet ###
#including overshoot
TAC.A               <- 171000 * 0.85
fmult[["A"]]        <- optimize(rescale.F,c(0,2),NSH.ImY=NSH.FcY,ImY=FcY,F.=FA,W.=WA,TAC=TAC.A)$minimum
FA                  <- FA * fmult[["A"]]
fmult[["B"]]        <- optimize(find.FB,c(0,2),stk=window(NSH.FcY,an(FcY),an(FcY)),F.A=FA,F.B=FB,F.C=FC,F.D=FD)$minimum
FB                  <- FB * fmult[["B"]]
Catch.FcY[["A"]]    <- sum(NSH.FcY@stock.n[,ac(an(FcY)-1)]*(1-exp(-FA-NSH.FcY@m[,ac(an(FcY)-1)]))*WA*(FA/(FA+NSH.FcY@m[,ac(an(FcY)-1)])))
Catch.FcY[["B"]]    <- sum(NSH.FcY@stock.n[,ac(an(FcY)-1)]*(1-exp(-FB-NSH.FcY@m[,ac(an(FcY)-1)]))*WB*(FB/(FB+NSH.FcY@m[,ac(an(FcY)-1)])))
Catch.FcY[["C"]]    <- sum(NSH.FcY@stock.n[,ac(an(FcY)-1)]*(1-exp(-FC-NSH.FcY@m[,ac(an(FcY)-1)]))*WC*(FC/(FC+NSH.FcY@m[,ac(an(FcY)-1)])))
Catch.FcY[["D"]]    <- sum(NSH.FcY@stock.n[,ac(an(FcY)-1)]*(1-exp(-FD-NSH.FcY@m[,ac(an(FcY)-1)]))*WD*(FD/(FD+NSH.FcY@m[,ac(an(FcY)-1)])))
NSH.FcY@harvest[,FcY] <- FA+FB+FC+FD

mean(NSH.FcY@harvest[3:7,FcY])
mean(NSH.FcY@harvest[1:2,FcY])
Fbar.FcY[["A"]]     <- mean(FA[3:7])
Fbar.FcY[["B"]]     <- mean(FB[1:2])
Fbar.FcY[["C"]]     <- mean(FC[4:7])
Fbar.FcY[["D"]]     <- mean(FD[1:2])
ssb(NSH.FcY)

NSH.FcY2                          <- window(NSH.FcY,1960,an(FcY)+1)
for(slot. in c("m","mat","m.spwn","harvest.spwn","stock.wt")) slot(NSH.FcY2,slot.)[,ac(an(FcY)+1)] <- slot(NSH.ImY,slot.)[,ac(an(ImY)-1)]

NSH.FcY2@harvest[,ac(an(FcY)+1)]  <- NSH.FcY@harvest[,FcY]
NSH.FcY2@stock.n[,ac(an(FcY)+1)]  <- c(REC.FcY,c(NSH.FcY@stock.n[,FcY]*exp(-NSH.FcY@harvest[,FcY]-NSH.FcY@m[,FcY]))[1:8],sum((NSH.FcY@stock.n[,FcY]*exp(-NSH.FcY@harvest[,FcY]-NSH.FcY@m[,FcY]))[9:10]))

ssb(NSH.FcY2)
### Same catch as last year ###




### Bpa in 2011 ###




find.FAB <- function(mult,stk,F.A,F.B,F.C,F.D){
                F.A               <- F.A*mult[1]
                F.B               <- F.B*mult[2]
                F.C               <- F.C
                F.D               <- F.D
                bigF              <- F.A+F.B+F.C+F.D
                stk@harvest@.Data <- bigF
                
                if(ssb(stk)<0.8e6){
                  resA <- 0.1
                  resB <- 0.04
                }
                if(ssb(stk)>=0.8e6 & ssb(stk) <= 1.5e6){
                  resA <- 0.15/0.7*((ssb(stk)-0.8e6)/1e6)+0.1
                  resB <- 0.04
                }
                if(ssb(stk)>1.5e6){
                  resA <- 0.25
                  resB <- 0.05
                }
                
                fbarB <- mean(stk@harvest[1:2,])
                fbarA <- mean(stk@harvest[3:7,]) 
            return(abs(fbarB-resB)+abs(fbarA-resA))}
                
find.FB <- function(mult,stk,F.A,F.B,F.C,F.D){
                F.A               <- F.A
                F.B               <- F.B*mult
                F.C               <- F.C
                F.D               <- F.D
                bigF              <- F.A+F.B+F.C+F.D
                stk@harvest@.Data <- bigF
                
                if(ssb(stk) < 1.5e6) resB <- 0.04
                if(ssb(stk) >=1.5e6) resB <- 0.05
                
                fbarB <- mean(stk@harvest[1:2,])
            return(abs(fbarB-resB))}                



  