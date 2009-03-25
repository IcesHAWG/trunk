#Code to do the multifleet short term forecast

#===============================================================================
# Intermediate year
#===============================================================================
ImY <- ac(2009)
FcY <- ac(2010)

#Agreed TACS according to EU times over- or underfishing
OOF   <- read.csv(paste("./stf/data/over_underfishing",ac(an(ImY)-1),".csv",sep=""),header=T); dims.OOF <- dim(OOF)
TAC.A <- 171000    * mean(OOF[(dims.OOF[1]-2):dims.OOF[1],"A"])
TAC.B <- 15985     * mean(OOF[(dims.OOF[1]-2):dims.OOF[1],"B"])
TAC.C <- 37722     * mean(OOF[(dims.OOF[1]-2):dims.OOF[1],"C"])           #(max 50%)
TAC.D <- 8373      * mean(OOF[(dims.OOF[1]-2):dims.OOF[1],"D"])

#Predicted recruitment and geometric mean
REC.ImY         <- NSH.ica@param["Recruitment prediction","Value"]
REC.FcY         <- exp(mean(log(rec(NSH)[,ac((range(NSH)["maxyear"]-5):(range(NSH)["maxyear"]))])))

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
for(slot. in c("m","mat","m.spwn","harvest.spwn","stock.wt")) slot(NSH.ImY,slot.)[,ImY] <- apply(slot(NSH.ImY,slot.)[,ac((an(ImY)-3):(an(ImY)-1))],1,mean)
NSH.ImY@stock.n[,ImY]           <- c(REC.ImY,(NSH.ImY@stock.n[,ac(an(ImY)-1)]*exp(-NSH@harvest[,ac(an(ImY)-1)]-NSH@m[,ac(an(ImY)-1)]))[1:8,],sum((NSH.ImY@stock.n[,ac(an(ImY)-1)]*exp(-NSH@harvest[,ac(an(ImY)-1)]-NSH@m[,ac(an(ImY)-1)]))[9:10,]))

NSH.ImY@stock.wt[,ImY]          <- NSH.ImY@stock.wt[,ac(an(ImY)-1)]

rescale.F <- function(mult,NSH.ImY,ImY,F.,W.,TAC){
              return(abs(TAC - sum(NSH.ImY@stock.n[,ac(an(ImY)-1)]*(1-exp(-F.*mult-NSH.ImY@m[,ac(an(ImY)-1)]))*W.*(F.*mult/(F.+NSH.ImY@m[,ac(an(ImY)-1)])))))}
fmult               <- list()                         
fmult[["A"]]        <- optimize(rescale.F,c(0,2),NSH.ImY=NSH.ImY,ImY=ImY,F.=FA,W.=WA,TAC=TAC.A)$minimum
fmult[["B"]]        <- optimize(rescale.F,c(0,2),NSH.ImY=NSH.ImY,ImY=ImY,F.=FB,W.=WB,TAC=TAC.B)$minimum
fmult[["C"]]        <- optimize(rescale.F,c(0,2),NSH.ImY=NSH.ImY,ImY=ImY,F.=FC,W.=WC,TAC=TAC.C)$minimum
fmult[["D"]]        <- optimize(rescale.F,c(0,2),NSH.ImY=NSH.ImY,ImY=ImY,F.=FD,W.=WD,TAC=TAC.D)$minimum
fmult[["tot"]]      <- optimize(rescale.F,c(0,2),NSH.ImY=NSH.ImY,ImY=ImY,F.=FA+FB+FC+FD,W.=(WA*Fs[,"An"]+WB*Fs[,"Bn"]+WC*Fs[,"Cn"]+WD*Fs[,"Dn"]),TAC=TAC.A+TAC.B+TAC.C+TAC.D)$minimum
NSH.ImY@harvest[,ImY] <- FA*fmult[["A"]]+FB*fmult[["B"]]+FC*fmult[["C"]]+FD*fmult[["D"]]

Catch.ImY           <- list()  
Catch.ImY[["A"]]    <- sum(NSH.ImY@stock.n[,ac(an(ImY)-1)]*(1-exp(-FA*fmult[["A"]]-NSH.ImY@m[,ac(an(ImY)-1)]))*WA*(FA*fmult[["A"]]/(FA+NSH.ImY@m[,ac(an(ImY)-1)])))
Catch.ImY[["B"]]    <- sum(NSH.ImY@stock.n[,ac(an(ImY)-1)]*(1-exp(-FB*fmult[["B"]]-NSH.ImY@m[,ac(an(ImY)-1)]))*WB*(FB*fmult[["B"]]/(FB+NSH.ImY@m[,ac(an(ImY)-1)])))
Catch.ImY[["C"]]    <- sum(NSH.ImY@stock.n[,ac(an(ImY)-1)]*(1-exp(-FC*fmult[["C"]]-NSH.ImY@m[,ac(an(ImY)-1)]))*WC*(FC*fmult[["C"]]/(FC+NSH.ImY@m[,ac(an(ImY)-1)])))
Catch.ImY[["D"]]    <- sum(NSH.ImY@stock.n[,ac(an(ImY)-1)]*(1-exp(-FD*fmult[["D"]]-NSH.ImY@m[,ac(an(ImY)-1)]))*WD*(FD*fmult[["D"]]/(FD+NSH.ImY@m[,ac(an(ImY)-1)])))
Catch.ImY[["tot"]]  <- sum(NSH.ImY@stock.n[,ac(an(ImY)-1)]*(1-exp(-(FA+FB+FC+FD)*fmult[["tot"]]-NSH.ImY@m[,ac(an(ImY)-1)]))*(WA*Fs[,"An"]+WB*Fs[,"Bn"]+WC*Fs[,"Cn"]+WD*Fs[,"Dn"])*((FA+FB+FC+FD)*fmult[["tot"]]/((FA+FB+FC+FD)+NSH.ImY@m[,ac(an(ImY)-1)])))

Fbar.ImY            <- list()  
Fbar.ImY[["A"]]     <- mean(FA[3:7]*fmult[["A"]])
Fbar.ImY[["B"]]     <- mean(FB[1:2]*fmult[["B"]])
Fbar.ImY[["C"]]     <- mean(FC[4:7]*fmult[["C"]])
Fbar.ImY[["D"]]     <- mean(FD[1:2]*fmult[["D"]])

#===============================================================================
# Forecast year
#===============================================================================

NSH.FcY <- window(NSH.ImY,1960,an(FcY))

#Fill the slots
for(slot. in c("m","mat","m.spwn","harvest.spwn","stock.wt")) slot(NSH.FcY,slot.)[,FcY] <- apply(slot(NSH.FcY,slot.)[,ac((an(FcY)-3):(an(FcY)-1))],1,mean)
NSH.FcY@stock.n[,FcY]           <- c(REC.FcY,(NSH.FcY@stock.n[,ac(an(FcY)-1)]*exp(-NSH.FcY@harvest[,ac(an(FcY)-1)]-NSH.FcY@m[,ac(an(FcY)-1)]))[1:8,],sum((NSH.FcY@stock.n[,ac(an(FcY)-1)]*exp(-NSH.FcY@harvest[,ac(an(FcY)-1)]-NSH.FcY@m[,ac(an(FcY)-1)]))[9:10,]))
NSH.FcY@stock.wt[,FcY]          <- NSH.FcY@stock.wt[,ac(an(FcY)-1)]




function(mults

  