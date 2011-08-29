
 #################################
 #    about the biological ref points
 #
 ################################
 library(FLBRP)



## Create the corresponding FLBRP object
NEA.Mac.brp<-FLBRP(NEA.Mac,nyrs=5,fbar = seq(0, 1.8, length.out=100))
NEA.Mac.brp<-brp(NEA.Mac.brp)
refpts(NEA.Mac.brp)

## plot per recruit values

NEA.Mac.brp<-brp(NEA.Mac.brp)
plot(NEA.Mac.brp)

#Yield per Recruit and Spawners per recruit plot for the report
#slotNames(NEA.Mac.brp)
#pdf(paste("YieldandSSBperRecruit", ".pdf", sep=""),width=210/25.4,height=297/25.4)
par(mar=c(5,5,5,1))

NEA.Mac.brp_LandingsInWeight = sweep(NEA.Mac.brp@landings.n, c(1,5), FUN="*", NEA.Mac.brp@landings.wt)
numYPR = quantSums(NEA.Mac.brp_LandingsInWeight)

NEA.Mac.brp_F_BeforeAtSpawningTime = sweep(NEA.Mac.brp@harvest, c(1,5), FUN="*", NEA.Mac.brp@harvest.spwn)
NEA.Mac.brp_Z_BeforeAtSpawningTime = sweep(NEA.Mac.brp_F_BeforeAtSpawningTime, c(1,5), FUN="+", (NEA.Mac.brp@m*NEA.Mac.brp@m.spwn))
NEA.Mac.brp_MatureNumbersAtSpawningTime = sweep((NEA.Mac.brp@stock.n*exp(-NEA.Mac.brp_Z_BeforeAtSpawningTime)), c(1,5), FUN="*",
                                                (NEA.Mac.brp@mat*NEA.Mac.brp@stock.wt))
numSPR = quantSums(NEA.Mac.brp_MatureNumbersAtSpawningTime)

plot(NA, NA,
      ylim=range(0,0.2),  
      xlim=range(0,1.8),
      xlab="Fbar",
      ylab="Yield per recruit",
      main="",
      axes=F, bty='c')   
axis(1)   
axis(2)               
lines(NEA.Mac.brp@fbar, numYPR, type="l", lty=1)  
par(new=TRUE)
plot(NA, NA, 
      ylim=range(0,2),  
      xlim=range(0,1.8),
      xlab="",
      ylab="", 
      axes=FALSE, bty='c')         
axis(4)
lines(NEA.Mac.brp@fbar, numSPR, type="l", lty=2)   
mtext("Spawning stock per recruit ()",4, line=2.5)
legend(0.1, 0.25, c("YpR", "SSpR"), pt.bg="white", lty=c(1,2), cex=1)
