
################################################################################

#  For showing stock size, reference points and management plan for NEA mackerel

################################################################################

windows(width=480, height=480)

# Retrieve the data from the assessment 
oj=NEA.Mac[,30:37]
plot(ssb(oj)/1e6,apply(oj@harvest[5:9,],2,FUN=mean),main="Stock size and management for NEA Mackerel", xlab="SSB in million tonnes",ylab="Fbar4-8",type="b",pch=19,
           xlim=c(1.2,2.8),ylim=c(0.1,0.5))
           
#plot(ssb(oj)/1e6,apply(oj@harvest[5:9,],2,FUN=mean),xlab=list("SSB in million tonnes", cex=1.8), ylab=list("Fbar4-8", cex=1.6),type="b",pch=19,
           #xlim=c(1.2,2.8),ylim=c(0.1,0.5))           

# plot the management plan area
#polygon(c(2.2,5,5,2.2,2.2),c(0.2,0.2,0.22,0.22,0.2),lwd=2,col="grey95")

# box for green management area : harvested sustainably
polygon(c(2.3,2.863,2.863,2.3,2.3),c(0.084,0.084,0.23,0.23,0.084),lwd=0.25,col="green")
#lines(c(2.2,5),c(0.084,0.084))
#lines(c(2.863,2.863),c(0.084,0.23))

# box for management plan area
polygon(c(2.2,2.863,2.863,2.2,2.2),c(0.2,0.2,0.22,0.22,0.2),lwd=2,col="green")

# box for yellow management area : at risk of being harvested unsustainably
polygon(c(2.3,2.863,2.863,2.3,2.3),c(0.23,0.23,0.42,0.42,0.23),lwd=0.25,col="yellow")

# box for dark yellow management area : harvested unsustainably
polygon(c(2.3,2.863,2.863,2.3,2.3),c(0.42,0.42,0.516,0.516,0.42),lwd=0.25,col="gold")

# box for orange area: at risk of suffering reduced reproductive capacity
polygon(c(1.67,2.3,2.3,1.67,1.67),c(0.42,0.42,0.516,0.516,0.42),lwd=0.25,col="orange") 

# box for red management area: sufferend reduced reproductive capacity
polygon(c(1.137,1.67,1.67,1.137,1.137),c(0.42,0.42,0.516,0.516,0.42),lwd=0.25,col="red")

# box for orange area: 
polygon(c(1.137,1.67,1.67,1.137,1.137),c(0.23,0.23,0.42,0.42,0.23),lwd=0.25,col="orange")

# box for gold area:
polygon(c(1.137,1.67,1.67,1.137,1.137),c(0.084,0.084,0.23,0.23,0.084),lwd=0.25,col="gold")

# box for yellow area:
polygon(c(1.67,2.3,2.3,1.67,1.67),c(0.084,0.084,0.23,0.23,0.084),lwd=0.25,col="yellow")

# And the little bit of management plan that's not green!
polygon(c(2.2,2.3,2.3,2.2,2.2),c(0.2,0.2,0.22,0.22,0.2),lwd=2,col="yellow") 

# middle area:
polygon(c(1.67,2.3,2.3,1.67,1.67),c(0.23,0.23,0.42,0.42,0.23),lwd=0.25,col="lightyellow")

# replot data over colours
lines(ssb(oj)/1e6,apply(oj@harvest[5:9,],2,FUN=mean),main="Management plan for NEA Mackerel", xlab="SSB in million tonnes",ylab="Fbar4-8",type="b",pch=19,
           xlim=c(1.2,2.8),ylim=c(0.1,0.5))
           
# plot data for 2009 SSB and F
points(2.59,0.31,pch=19)
lines(c(2.59,2.49),c(0.31,0.237))
text(2.59,0.32,labels="2009",cex=0.7)
text(ssb(oj)/1e6,0.01+apply(oj@harvest[5:9,],2,FUN=mean),labels=dimnames(oj@harvest)[[2]],cex=0.7)           
                   
# the sliding F rule
segments(1.67,0.167,2.2,0.22,lwd=2)

# Blim
abline(v=1.67,col='black',lty="solid")
# Btrig
abline(v=2.2,col="blue",lty="dashed")
# Bpa
#abline(v=2.3,col="green",lty="dashed")
abline(v=2.3,col="black",lty="dashed")
# Flim
abline(h=0.42,col='black',lty="dashed")
# Fpa
abline(h=0.23,col="black",lty="dashed")

#points(ssb(oj)/1e6,apply(oj@harvest[5:9,],2,FUN=mean),pch=19)

text(x=c(1.63 ,2.139,2.35),y=c(0.5,0.5,0.5),labels=c("Blim","Btrigger","Bpa"),col=c("black","black","black"),cex=0.8)
text(x=c(2.8,2.8),c(0.238,0.428),labels=c("Fpa","Flim"),col=c("black","black"),cex=0.8)


