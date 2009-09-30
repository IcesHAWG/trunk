
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

