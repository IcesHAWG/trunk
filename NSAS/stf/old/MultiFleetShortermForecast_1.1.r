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
for(slot. in c("m","m.spwn","harvest.spwn","stock.wt")) slot(NSH.ImY,slot.)[,ImY] <- apply(slot(NSH.ImY,slot.)[,ac((an(ImY)-3):(an(ImY)-1))],1,mean)
NSH.ImY@stock.n[,ImY]           <- c(REC.ImY,(NSH.ImY@stock.n[,ac(an(ImY)-1)]*exp(-NSH@harvest[,ac(an(ImY)-1)]-NSH@m[,ac(an(ImY)-1)]))[1:8,],sum((NSH.ImY@stock.n[,ac(an(ImY)-1)]*exp(-NSH@harvest[,ac(an(ImY)-1)]-NSH@m[,ac(an(ImY)-1)]))[9:10,]))

NSH.ImY@stock.wt[,ImY]          <- NSH.ImY@stock.wt[,ac(an(ImY)-1)]
#RAW.stock.wt                    <- read.delim(file="./data/west_raw.txt",header=F,skip=5,sep="\t"); dims.RAW <- dim(RAW.stock.wt)
#NSH.ImY@stock.wt[,ImY]          <- apply(RAW.stock.wt[(dims.RAW[1]-2):dims.RAW[1],],2,mean)




rescale.F <- function(mult,NSH.ImY,ImY,F.,TAC){
              return(abs(TAC - (NSH.ImY@stock.n[,ac(an(ImY)-1)]*(1-exp(-F.*mult-NSH.ImY@m[,ac(an(ImY)-1)]))*NSH.ImY@stock.wt[,ac(an(ImY)-1)]*(F./(FA+NSH.ImY@m[,ac(an(ImY)-1)])))))}
optimize(rescale.F,c(0,2),NSH.ImY=NSH.ImY,ImY=ImY,F.=FA,TAC=TAC.A)$minimum
#sum(NSH.ImY@stock.n[,ac(an(ImY)-1)]*(1-exp(-FA-NSH.ImY@m[,ac(an(ImY)-1)]))*NSH.ImY@stock.wt[,ac(an(ImY)-1)]*(FA/(FA+NSH.ImY@m[,ac(an(ImY)-1)])))


