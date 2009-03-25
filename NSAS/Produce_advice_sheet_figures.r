#Make stock summary plots (ie SSB, Fbar, recs)
    stck <- NSH
    summary.data <- as.data.frame(FLQuants(Biomass=stock(stck),"Catches"=landings(stck),"Mean F"=fbar(stck),Recruits=rec(stck)))
    scaling.factors <- tapply(summary.data$data,summary.data$qname,function(x) trunc(log10(max(pretty(c(0,x))))/3)*3)
    summary.data$data <- summary.data$data/10^scaling.factors[summary.data$qname]
    ylabels <- apply(cbind(lbl=names(scaling.factors),fctr=scaling.factors),1,function(x) {
        if(x[2]=="0"){x[1]}else {bquote(expression(paste(.(x[1])," [",10^.(x[2]),"]"))) }})
    summary.plot <-xyplot(data~year|qname,data=summary.data,
                      as.table=TRUE,
                      main=list(paste(stck@name,"Stock Summary Plot"),cex=0.9),
                      ylab=do.call(c,rev(ylabels)),
                      layout=c(1,4),
                      type="l",
                      panel=function(...) {
                        panel.grid(h=-1,v=-1)

                        if(panel.number()==2) { #Do recruits as bar plot
                            panel.barchart(...,horizontal=FALSE,origin=0,box.width=1,col="grey",ylim=c(0,120))
                        } else {
                          if(panel.number()==1){
                            tmp <- as.data.frame(ssb(stck))
                            panel.xyplot(tmp$year,tmp$data/1e6,col="black",lwd=2,type="l")
                            panel.xyplot(...,col="black",lwd=2,lty=2,pch=19)
                            panel.abline(h=c(0.8,1.3),lty=c(2,3))
                            panel.text(1959,0.8,labels=expression(B[lim]),cex=0.7)
                            panel.text(1959,1.3,labels=expression(B[pa]),cex=0.7)
                            draw.key(list(text=list(lab='TSB',cex=0.8),lines=list(lty=2,lwd=c(2)),
                                          text=list(lab='SSB',cex=0.8),lines=list(lty=1,lwd=c(2))),
                                      vp = viewport(x = unit(0.8, "npc"), y = unit(0.9, "npc")), draw=TRUE)
                          }
                          if(panel.number()==3){
                            panel.xyplot(...,col="black",lwd=2,ylim=c(0,1.5))
                            tmp <- as.data.frame(apply(stck@harvest[1:2],2,mean))
                            panel.xyplot(tmp$year,tmp$data,lty=2,lwd=2,col="black",type="l")
                            panel.abline(h=c(0.12,0.25),lty=c(2,3))
                            panel.text(1959,0.12,labels=expression(F[pa][0-1]),cex=0.7)
                            panel.text(1959,0.25,labels=expression(F[pa][2-6]),cex=0.7)
                            draw.key(list(text=list(lab=expression(F[2-6]),cex=0.8),lines=list(lty=1,lwd=c(2)),
                                          text=list(lab=expression(F[0-1]),cex=0.8),lines=list(lty=2,lwd=c(2))),
                                      vp = viewport(x = unit(0.8, "npc"), y = unit(0.9, "npc")), draw=TRUE)
                          }
                          if(panel.number()==4){
                             panel.barchart(...,horizontal=FALSE,origin=0,box.width=1,col="grey",ylim=c(0,10))
                          }

                        }

                      },
                      scales=list(alternating=1,y=list(relation="free",rot=0)))
    print(summary.plot)
    
par(mfrow=c(3,1),oma=c(0.2,1,0.2,4))
plot(rec(NSH)~ssb(NSH),pch=19,xlab="SSB in 1000 t",ylab="Recruits (age 0) in thousands",main="Stock - Recruitment")
abline(v=c(0.8e6,1.3e6),lty=c(2,3),lwd=2)
legend("topright",legend=c("SSB",expression(B[lim]),expression(B[pa])),lwd=c(0,2,2),lty=c(0,2,3),box.lty=0,pch=c(19,-1,-1))

rpts<-refpts()
NSH.brp <- brp(FLBRP(NSH,sr=NSH.sr,fbar=seq(0,1,length.out=100),nyrs=3,refpts=rpts))
plot(colSums(sweep(NSH.brp@landings.n,1,NSH.brp@landings.wt,"*"))~c(NSH.brp@fbar),type="l",lwd=2,xlab="Fbar",ylab="Yield",main="Yield and Spawning Stock Biomass per Recruit")
par(new=T)
plot(c(ssb(NSH.brp))~c(NSH.brp@fbar),type="l",lwd=2,lty=2,yaxt="n",ylab="",xlab="")
axis(4)
mtext("SSB per recruit",4,line=2,cex=0.7)
legend("right",legend=c("Yield","SSB per recruit"),lwd=c(2,2),lty=c(1,2),box.lty=0)

plot(c(ssb(NSH))~c(fbar(NSH)),type="l",lwd=2,xlab="Fbar",ylab="SSB in 1000 t",main=" Precautionary Approach Plot",sub="Period 1960-2008")
points(c(ssb(NSH[,"2008"]))~c(fbar(NSH[,"2008"])),pch=19,cex=2,col="grey")
points(c(ssb(NSH[,"2008"]))~c(fbar(NSH[,"2008"])),cex=2,lwd=1)
abline(v=c(0.25),lty=2,lwd=2)
abline(h=c(0.8e6,1.3e6),lty=c(3,4),lwd=2)
legend("topright",legend=c("F-SSB","2008","",expression(F[pa]),expression(B[lim]),expression(B[pa])),pch=c(-1,19,-1,-1,-1,-1),col=c("black","grey","black","black","black","black"),lty=c(1,0,0,2,3,4),lwd=c(2,0,0,2,2,2),ncol=2,box.lty=0)
