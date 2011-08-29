

oj=NEA.Mac[,26:39]
plot(ssb(oj)/1e6, apply(oj@harvest[5:9,],2,FUN=mean),
      main="Management plan for NEA Mackerel", xlab="SSB in million tonnes",ylab="Fbar4-8",type="b",pch=19,
           xlim=c(1.2,3.2),ylim=c(0.1,0.5))
text(ssb(oj)/1e6,0.01+apply(oj@harvest[5:9,],2,FUN=mean),labels=dimnames(oj@harvest)[[2]],cex=0.7)

#Last point
points(2.904690, 0.312, pch=19)
text(2.904690, 0.312, "2011",cex=0.7, pos=3)
segments(2.904690, 0.312, ssb(oj)[1,14]/1e6, apply(oj@harvest[5:9,],2,FUN=mean)[1,14])

polygon(c(2.2,5,5,2.2,2.2),c(0.2,0.2,0.22,0.22,0.2),lwd=2,col="grey95")
segments(1.67,0.167,2.2,0.22,lwd=2)
abline(v=1.67,col='red',lty="dashed")
abline(v=2.2,col="blue",lty="dashed")
abline(v=2.3,col="green",lty="dashed")
abline(h=0.42,col='red',lty="dashed")
abline(h=0.23,col="green",lty="dashed")
points(ssb(oj)/1e6,apply(oj@harvest[5:9,],2,FUN=mean),pch=19)
text(x=c(1.6 ,2.1051,2.374),y=c(0.1,0.1,0.1),labels=c("Blim","Btrigger","Bpa"),col=c("red","blue","green"),cex=0.7)
text(x=c(1.2,1.2),c(0.24,0.43),labels=c("Fpa","Flim"),col=c("green","red"),cex=0.7)


