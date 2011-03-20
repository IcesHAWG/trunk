#Description of the fleets
descFleet <- list(A = "Human consumption IV",C = "Human consumption IIIa",D = "Bycatch sprat industrial",F = "Human consumption 22-24")

#Time series of catches for the different fleets
tsCatchFleet                  <- matrix(NA,nrow=8,ncol=11,dimnames=list(Fleet=c("A - WBSS","A - NSAS","C - WBSS","C - NSAS","D - WBSS","D - NSAS","F - WBSS","F - NSAS"),years=2000:2010))
tsCatchFleet["A - WBSS",-11]  <- c(7,6,7,2.8,7.1,7.0,11.0,1.1,0.1,3.9)
tsCatchFleet["A - NSAS",-11]  <- rep(0,10) #c(322,296,323,434.9,529.5,610.0,487.1,379.6,236.3,152.1)
tsCatchFleet["C - WBSS",-11]  <- c(45,33,38,31.6,16.8,32.5,30.2,25.3,23.0,29.4)
tsCatchFleet["C - NSAS",-11]  <- c(36,34,17,24.1,13.4,22.9,11.6,16.4,9.2,5.1)
tsCatchFleet["D - WBSS",-11]  <- c(5,3,9,4.0,11.2,5.1,5.9,2.3,2.2,2.9)
tsCatchFleet["D - NSAS",-11]  <- c(13,12,9,8.4,10.8,9.0,3.4,3.4,3.7,1.5)
tsCatchFleet["F - WBSS",-11]  <- c(53.3,62.9,46.2,38.7,41.2,41.8,39.4,37.6,38.5,27.4) + c(1.0,0.8,4.6,2.6,0.4,2.2,2.5,2.9,5.7,3.6)
tsCatchFleet["F - NSAS",-11]  <- 0


#Time series of shares of WBSS by fleet (from the total catch of WBSS and NSAS)
tsShareFleet                  <- matrix(NA,nrow=4,ncol=11,dimnames=list(Fleet=c("A","C","D","F"),years=2000:2010))
tsShareFleet["A",]            <- tsCatchFleet["A - WBSS",] / apply(tsCatchFleet[grep("A -",dimnames(tsCatchFleet)[[1]]),],2,sum,na.rm=T)
tsShareFleet["C",]            <- tsCatchFleet["C - WBSS",] / apply(tsCatchFleet[grep("C -",dimnames(tsCatchFleet)[[1]]),],2,sum,na.rm=T)
tsShareFleet["D",]            <- tsCatchFleet["D - WBSS",] / apply(tsCatchFleet[grep("D -",dimnames(tsCatchFleet)[[1]]),],2,sum,na.rm=T)
tsShareFleet["F",]            <- tsCatchFleet["F - WBSS",] / apply(tsCatchFleet[grep("F -",dimnames(tsCatchFleet)[[1]]),],2,sum,na.rm=T)

#Time series of WBSS TAC's
tsTACFleet                    <- matrix(NA,nrow=4,ncol=11,dimnames=list(Fleet=c("A","C","D","F"),years=2000:2010))
tsTACFleet["A",]              <- rep(0,11) #c(265,265,265,400,460,535,455,341,201,171,164)
tsTACFleet["C",]              <- c(80,80,80,80.0,70.0,96.0,81.6,69.4,51.7,37.7-5.032*0.2,33.9-4.515*0.2)
tsTACFleet["D",]              <- c(21,21,21,21.0,21.0,24.2,20.5,15.4,11.5,8.4,7.5)
tsTACFleet["F",]              <- c(0,0,0,0,0,0,47.5,49.5,45,27.2,22.7)



#Fraction of TAC used
propTACUsed                   <- aggregate(tsCatchFleet,by=list(substr(dimnames(tsCatchFleet)[[1]],1,1)),sum)[,-1] / tsTACFleet[-5,]

#2011 catch
#sum(tsTACFleet[,"2010"] * tsShareFleet[,"2009"])



