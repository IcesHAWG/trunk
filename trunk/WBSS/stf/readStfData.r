#------------------------------------
# Convert partial Ns per fleet to partial Fs
#------------------------------------

#- Read in the N's by fleet, by quarter in the data year
Ns            <- read.csv(paste("./data/partial_nsWBSS",DtY,".csv",sep=""),header=T)

#- The partial F's are calculated proportionally to the N's at age (by year, not by quarter)
FA            <- sweep(sweep(Ns[,grep("A",colnames(Ns))],1,apply(Ns[,-1],1,sum,na.rm=T),"/"),1,WBSS@harvest[,ac(DtY)],"*")
FC            <- sweep(sweep(Ns[,grep("C",colnames(Ns))],1,apply(Ns[,-1],1,sum,na.rm=T),"/"),1,WBSS@harvest[,ac(DtY)],"*")
FD            <- sweep(sweep(Ns[,grep("D",colnames(Ns))],1,apply(Ns[,-1],1,sum,na.rm=T),"/"),1,WBSS@harvest[,ac(DtY)],"*")
FF            <- sweep(sweep(Ns[,grep("F",colnames(Ns))],1,apply(Ns[,-1],1,sum,na.rm=T),"/"),1,WBSS@harvest[,ac(DtY)],"*")

#------------------------------------
# Read in partial N's and Wt's
#------------------------------------

#- Read in the partial N's by fleet by quarter for the past three years
NsDtYmin2     <- read.csv(paste("./data/partial_nsWBSS",ac(an(DtY)-2),".csv",sep=""),header=T)*1000; NsDtYmin2$age  <-  NsDtYmin2$age / 1000
NsDtYmin1     <- read.csv(paste("./data/partial_nsWBSS",ac(an(DtY)-1),".csv",sep=""),header=T)*1000; NsDtYmin1$age  <-  NsDtYmin1$age / 1000
NsDtY         <- read.csv(paste("./data/partial_nsWBSS",ac(an(DtY)-0),".csv",sep=""),header=T)*1000; NsDtY$age      <-  NsDtY$age     / 1000

#- Read in the partial Weights'  by fleet by quarter for the past three years
WsDtYmin2     <- read.csv(paste("./data/partial_wsWBSS",ac(an(DtY)-2),".csv",sep=""),header=T); WsDtYmin2[WsDtYmin2==0] <- NA; WsDtYmin2$age[1] <- 0
WsDtYmin1     <- read.csv(paste("./data/partial_wsWBSS",ac(an(DtY)-1),".csv",sep=""),header=T); WsDtYmin1[WsDtYmin1==0] <- NA; WsDtYmin1$age[1] <- 0
WsDtY         <- read.csv(paste("./data/partial_wsWBSS",ac(an(DtY)-0),".csv",sep=""),header=T); WsDtY[WsDtY==0]         <- NA; WsDtY$age[1]     <- 0

#------------------------------------
# Store partial N's and Wt's
#------------------------------------

weights       <- array(NA,dim=c(length(na.omit(WsDtY$age)),4,4,3),dimnames=list(age=na.omit(WsDtY$age),fleet=c("A","C","D","F"),season=c("Q1","Q2","Q3","Q4"),year=ac(an(DtY)-c(2,1,0))))
for(iYr in ac(an(DtY)-c(2,1,0))){
  for(iQ in c("Q1","Q2","Q3","Q4")){
    if(iYr == ac(an(DtY)-2)) weights[,,iQ,iYr][] <- as.matrix(WsDtYmin2[,grep(iQ,colnames(WsDtYmin2))])
    if(iYr == ac(an(DtY)-1)) weights[,,iQ,iYr][] <- as.matrix(WsDtYmin1[,grep(iQ,colnames(WsDtYmin1))])
    if(iYr == ac(an(DtY)-0)) weights[,,iQ,iYr][] <- as.matrix(WsDtY[,    grep(iQ,colnames(WsDtY))])
  }
}
weights       <- weights / 1000

numbers       <- array(NA,dim=c(length(na.omit(NsDtY$age)),4,4,3),dimnames=list(age=na.omit(WsDtY$age),fleet=c("A","C","D","F"),season=c("Q1","Q2","Q3","Q4"),year=ac(an(DtY)-c(2,1,0))))
for(iYr in ac(an(DtY)-c(2,1,0))){
  for(iQ in c("Q1","Q2","Q3","Q4")){
    if(iYr == ac(an(DtY)-2)) numbers[,,iQ,iYr][] <- as.matrix(NsDtYmin2[,grep(iQ,colnames(NsDtYmin2))])
    if(iYr == ac(an(DtY)-1)) numbers[,,iQ,iYr][] <- as.matrix(NsDtYmin1[,grep(iQ,colnames(NsDtYmin1))])
    if(iYr == ac(an(DtY)-0)) numbers[,,iQ,iYr][] <- as.matrix(NsDtY[,    grep(iQ,colnames(NsDtY))])
  }
}

#------------------------------------
# Calculate weights at age by quarter
#------------------------------------

#- Calculate the average weight at age by quarter, by fleet (so average = over the years)
WA            <- apply(weights[,"A",,] * numbers[,"A",,],c(1,2),sum,na.rm=T) / apply(numbers[,"A",,],c(1,2),sum,na.rm=T)
WC            <- apply(weights[,"C",,] * numbers[,"C",,],c(1,2),sum,na.rm=T) / apply(numbers[,"C",,],c(1,2),sum,na.rm=T)
WD            <- apply(weights[,"D",,] * numbers[,"D",,],c(1,2),sum,na.rm=T) / apply(numbers[,"D",,],c(1,2),sum,na.rm=T)
WF            <- apply(weights[,"F",,] * numbers[,"F",,],c(1,2),sum,na.rm=T) / apply(numbers[,"F",,],c(1,2),sum,na.rm=T)

#------------------------------------
# Read in the split ogive by age (no years, no quarter)
#------------------------------------

Spl           <- read.csv(paste("./data/splitOgive",ac(an(ImY)-1),".csv",sep=""),header=T)
splt          <- Spl[,grep(ac(an(ImY)-1),colnames(Spl))]

