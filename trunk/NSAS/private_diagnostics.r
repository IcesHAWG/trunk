#===============================================================================
#LNV plots + also for HAWG extra plots
#===============================================================================

cl  <- 1.2
ca  <- 1
fam <- ""
cols    <- c("black","grey50","grey20","grey80","red2","green3","red","white")
fonts   <- 2
parmar <- rep(0.4,4)
paroma <- (c(6,6,2,2)+0.1)
mtextline <- 3
ltextcex <- 1

LNV.ssb <- function(stk,btrigger,blim){
              plot(c(ssb(stk)@.Data)~seq(range(stk)[c("minyear")],range(stk)[c("maxyear")],1),
              type="b",pch=19,cex.lab=cl,cex.axis=ca,main=paste("SSB",stk@name),font=fonts,family=fam,
              ylim=range(pretty(range(c(ssb(stk)@.Data),na.rm=T))),xlab="Years",ylab="SSB",lwd=2)
              abline(h=btrigger,col="darkgreen",lwd=2,lty=2)
              abline(h=blim,col="red",lwd=2,lty=2)
              #text(btrigger~range(stk)[c("maxyear")],expression(B[trigger]),cex=ltextcex,adj=c(0.6,0),col="darkgreen",las=2,pos=3)
              #text(blim~range(stk)[c("maxyear")],expression(B[lim]),cex=ltextcex,adj=c(0.6,0),col="red",las=2,pos=3)
              if(is.na(btrigger) == F) mtext(expression(B[trigger]),4,at=btrigger,cex=ltextcex,col="darkgreen",outer=F,line=0.2,las=2,font=fonts)
              if(is.na(blim) == F)     mtext(expression(B[lim]),    4,at=blim,    cex=ltextcex,col="red",      outer=F,line=0.2,las=2,font=fonts)
              }
              
LNV.retro <- function(stk,slot.){
                xrange <- range(unlist(lapply(stk,function(x){return(range(x)[c("minyear","maxyear")])})),na.rm=T)

                if(slot. == "ssb"){
                  yrange <- range(unlist(lapply(stk,function(x){return(ssb(x))})),na.rm=T)
                  plot(c(ssb(stk[[1]])@.Data)~seq(range(stk[[1]])[c("minyear")],range(stk[[1]])[c("maxyear")],1),
                    type="l",cex.lab=cl,cex.axis=ca,main=paste("SSB retrospective",stk[[1]]@name),font=fonts,family=fam,
                    ylim=yrange,xlim=xrange,xlab="Years",ylab="SSB",lwd=1)
                  for(iYr in 2:length(stk)){
                    lines(c(ssb(stk[[iYr]])@.Data)~seq(range(stk[[iYr]])[c("minyear")],range(stk[[iYr]])[c("maxyear")],1),lwd=1)
                    if(iYr == length(stk)) lines(c(ssb(stk[[iYr]])@.Data)~seq(range(stk[[iYr]])[c("minyear")],range(stk[[iYr]])[c("maxyear")],1),lwd=3)
                  }
                }
                if(slot. == "rec"){
                  yrange <- range(unlist(lapply(stk,function(x){return(rec(x))})),na.rm=T)
                  plot(c(rec(stk[[1]])@.Data)~seq(range(stk[[1]])[c("minyear")],range(stk[[1]])[c("maxyear")],1),
                    type="l",cex.lab=cl,cex.axis=ca,main=paste("Recruitment retrospective",stk[[1]]@name),font=fonts,family=fam,
                    ylim=yrange,xlim=xrange,xlab="Years",ylab="SSB",lwd=1)
                  for(iYr in 2:length(stk)){
                    lines(c(rec(stk[[iYr]])@.Data)~seq(range(stk[[iYr]])[c("minyear")],range(stk[[iYr]])[c("maxyear")],1))
                    if(iYr == length(stk)) lines(c(rec(stk[[iYr]])@.Data)~seq(range(stk[[iYr]])[c("minyear")],range(stk[[iYr]])[c("maxyear")],1),lwd=3)
                  }
                }
                if(slot. == "fbar"){
                  yrange <- range(unlist(lapply(stk,function(x){return(fbar(x))})),na.rm=T)
                  plot(c(fbar(stk[[1]])@.Data)~seq(range(stk[[1]])[c("minyear")],range(stk[[1]])[c("maxyear")],1),
                    type="l",cex.lab=cl,cex.axis=ca,main=paste("F retrospective",stk[[1]]@name),font=fonts,family=fam,
                    ylim=yrange,xlim=xrange,xlab="Years",ylab="SSB",lwd=1)
                  for(iYr in 2:length(stk)){
                    lines(c(fbar(stk[[iYr]])@.Data)~seq(range(stk[[iYr]])[c("minyear")],range(stk[[iYr]])[c("maxyear")],1))
                    if(iYr == length(stk)) lines(c(fbar(stk[[iYr]])@.Data)~seq(range(stk[[iYr]])[c("minyear")],range(stk[[iYr]])[c("maxyear")],1),lwd=3)
                  }
                }
              }
#LNV.ssb(NSH,1.3e6,0.8e6)

LNV.fbar <- function(stk,ftarget,flim,range.){
              range2 <- range.[1]:range.[2]
              plot(c(apply(stk@harvest[ac(range2),],2,mean)@.Data)~seq(range(stk)[c("minyear")],range(stk)[c("maxyear")],1),
              type="b",pch=19,cex.lab=cl,cex.axis=ca,main=paste("Fbar",name(stk)),font=fonts,family=fam,
              xlab="Years",ylab="Fbar",ylim=range(pretty(c(apply(stk@harvest[ac(range2),],2,mean)@.Data))),lwd=2)
              abline(h=ftarget,col="darkgreen",lwd=2,lty=2)
              abline(h=flim,col="red",lwd=2,lty=2)
              mtext(text=bquote(F[.(range.[1])-.(range.[2])[upper]]),4,at=ftarget,cex=ltextcex,outer=F,line=0.2,las=2,font=fonts,col="darkgreen")
              mtext(bquote(F[.(range.[1])-.(range.[2])[lower]]),4,at=flim,cex=ltextcex,,outer=F,line=0.2,las=2,font=fonts,col="red")
              legend("topright",legend=bquote(F[.(range.[1])-.(range.[2])]),lty=1,pch=19,box.lty=0)
            }
#LNV.fbar(NSH,0.25,0.1,c(2,6))
#LNV.fbar(NSH,0.1,0.04,c(0,1))

LNV.rec <- function(stk,stk.ica){
              plot(c(rec(stk)@.Data)~seq(range(stk)[c("minyear")],range(stk)[c("maxyear")],1),
              type="b",pch=19,,cex.lab=cl,cex.axis=ca,main=paste("Recruitment",name(stk)),font=fonts,family=fam,
              xlab="Years",ylab="Recruitment [thousands]",xlim=c(range(stk)["minyear"],range(stk)["maxyear"]+1),lwd=2)
              points(range(stk)["maxyear"]+1,stk.ica@param["Recruitment prediction","Value"],col="blue",pch=19)
              legend("topright",legend=paste("Recuitment estimate",range(stk)["maxyear"]+1),col="blue",pch=19,box.lty=0,lty=0)
            }
#LNV.rec(NSH)            

LNV.catchcoh <- function(stk){                  
                 rel <- c(colSums(FLCohort(stk@catch.n)[,ac((dims(stk)$minyear):(dims(stk)$maxyear-dims(stk)$age+1))]))  
                 plot((c(FLCohort(stk@catch.n)[1,ac((dims(stk)$minyear):(dims(stk)$maxyear-dims(stk)$age+1))])/rel)~c(((dims(stk)$minyear):(dims(stk)$maxyear-dims(stk)$age+1))),
                 type="l",cex.lab=cl,cex.axis=ca,main=paste("Proportion of a cohort in the catch",name(stk)),font=fonts,family=fam,
                 xlab="Years",ylab="Relative propotion of the cohort",ylim=c(0,1))
                 old.polygon <- rep(0,length((dims(stk)$minyear):(dims(stk)$maxyear-dims(stk)$age+1)))
                 for(i in 1:((range(stk)[c("max")]-range(stk)[c("min")])+1)){
                    polygon(c(c(((dims(stk)$minyear):(dims(stk)$maxyear-dims(stk)$age+1))),rev(c(((dims(stk)$minyear):(dims(stk)$maxyear-dims(stk)$age+1))))),
                    c(old.polygon,rev((c(FLCohort(stk@catch.n)[i,ac(((dims(stk)$minyear):(dims(stk)$maxyear-dims(stk)$age+1)))])/rel)+old.polygon)),col=grey(i/((range(stk)[c("max")]-range(stk)[c("min")])+1)))
                    text((dims(stk)$maxyear-dims(stk)$age+1),mean(c(old.polygon[length(old.polygon)],rev((c(FLCohort(stk@catch.n)[i,ac(((dims(stk)$minyear):(dims(stk)$maxyear-dims(stk)$age+1)))]@.Data)/rel)+old.polygon)[1])),labels=seq(range(stk)["min"],range(stk)["max"],1)[i],cex=ltextcex,adj=c(-0.6,0),font=fonts,col="black")
                    old.polygon <- (c(FLCohort(stk@catch.n)[i,ac(((dims(stk)$minyear):(dims(stk)$maxyear-dims(stk)$age+1)))])/rel)+old.polygon 
                 }
                }

#LNV.catchcoh(window(NSH,1960,2007))                


LNV.catch <- function(stk,slnm){
                rel <- colSums(slot(stk,slnm))
                plot((c(slot(stk,slnm)[1,]@.Data)/rel)~seq(range(stk)[c("minyear")],range(stk)[c("maxyear")],1),
                type="l",cex.lab=cl,cex.axis=ca,main=paste("Proportion of age-groups in the",slnm,name(stk)),font=fonts,family=fam,
                xlab="Years",ylab=paste("Relative propotion of the",slnm),ylim=c(0,1))
                old.polygon <- rep(0,range(stk)[c("maxyear")]-range(stk)[c("minyear")]+1)
                for(i in 1:((range(stk)[c("max")]-range(stk)[c("min")])+1)){
                  polygon(c(seq(range(stk)[c("minyear")],range(stk)[c("maxyear")],1),rev(seq(range(stk)[c("minyear")],range(stk)[c("maxyear")],1))),
                  c(old.polygon,rev((c(slot(stk,slnm)[i,]@.Data)/rel)+old.polygon)),col=grey(i/((range(stk)[c("max")]-range(stk)[c("min")])+1)))
                  text(range(stk)[c("maxyear")],mean(c(old.polygon[length(old.polygon)],rev((c(slot(stk,slnm)[i,]@.Data)/rel)+old.polygon)[1])),labels=seq(range(stk)["min"],range(stk)["max"],1)[i],cex=ltextcex,adj=c(-0.6,0),font=fonts,col="black")
                  old.polygon <- (c(slot(stk,slnm)[i,]@.Data)/rel)+old.polygon 
                }
              }
              
#LNV.catch(NSH.tun[[3]],"index")
#LNV.catch(NSH.tun[[4]],"index")  
#LNV.catch(window(NSH,1960,2007),"stock.n")             

#Survey MLAI
MLAI.hist     <- read.csv("./data/historic survey data/MLAI-historic.csv",header=T)
MIK.hist      <- read.csv("./data/historic survey data/MIK-historic.csv",header=T)
IBTS.hist     <- read.csv("./data/historic survey data/IBTS-historic.csv",header=T); IBTS.hist[IBTS.hist==-1] <- NA
ACOUSTIC.hist <- read.csv("./data/historic survey data/ACOUSTIC-historic.csv",header=T); ACOUSTIC.hist[ACOUSTIC.hist==-1] <- NA

MLAI.histst     <- cbind(MLAI.hist[,"ageall"]/mean(MLAI.hist[,"ageall"],na.rm=T),MLAI.hist[,"measurement_year"]); colnames(MLAI.histst) <- colnames(MLAI.hist)
MIK.histst      <- cbind(MIK.hist[,"age0"]/mean(MIK.hist[,"age0"],na.rm=T),MIK.hist[,"measurement_year"]); colnames(MIK.histst) <- colnames(MIK.hist)
IBTS.histst     <- cbind(sweep(IBTS.hist[,1:5],2,mapply(mean,IBTS.hist[,1:5],na.rm=T),"/"),IBTS.hist[,"measurement_year"]); colnames(IBTS.histst) <- colnames(IBTS.hist); IBTS.histst[IBTS.histst[,"measurement_year"]==1988,2] <- NA
ACOUSTIC.histst <- cbind(sweep(ACOUSTIC.hist[,1:9],2,mapply(mean,ACOUSTIC.hist[,1:9],na.rm=T),"/"),ACOUSTIC.hist[,"measurement_year"]); colnames(ACOUSTIC.histst) <- colnames(ACOUSTIC.hist)

plot(MLAI.hist[,"ageall"]~MLAI.hist[,"measurement_year"],type="b",pch=19,xlab="Years",ylab="Larval abundance",cex.lab=cl,cex.axis=ca,font=fonts,family=fam,main="Historic MLAI index")
text(MLAI.hist[,"ageall"]~MLAI.hist[,"measurement_year"],labels=MLAI.hist[,"measurement_year"],cex=0.7,pos=3)

plot(MIK.hist[,"age0"]~MIK.hist[,"measurement_year"],type="b",pch=19,xlab="Years",ylab="Larval abundance",cex.lab=cl,cex.axis=ca,font=fonts,family=fam,main="Historic MIK index")
text(MIK.hist[,"age0"]~MIK.hist[,"measurement_year"],labels=MIK.hist[,"measurement_year"],cex=0.7,pos=3)

par(mfrow=c(2,2),mar=c(0,4,2,1),oma=c(3,3,1,1))
boxplot(log(IBTS.hist[,1:5]),ylab="log(IBTS) index",cex.lab=cl,cex.axis=ca,font=fonts,family=fam,main="Variation in IBTS data per age",xaxt="n"); grid()
boxplot(log(ACOUSTIC.hist[,1:9]),ylab="log(Acoustic) index",cex.lab=cl,cex.axis=ca,font=fonts,family=fam,main="Variation in Acoustic data per age",xaxt="n");grid()
boxplot(IBTS.hist[,1:5],ylab="IBTS index",cex.lab=cl,cex.axis=ca,font=fonts,family=fam);grid()
boxplot(ACOUSTIC.hist[,1:9],ylab="Acoustic index",cex.lab=cl,cex.axis=ca,font=fonts,family=fam);grid()


par(mfrow=c(1,1))
range. <- c(0,max(c(c(MIK.histst[,"age0"]),unlist(IBTS.histst[,c("age1","age2")])),na.rm=T))
plot(  MIK.histst[,"age0"]~MIK.histst[,"measurement_year"],type="b",pch=19,ylim=range.,main="Relative abundance MIK-0 and IBTS-1,2 indices",ylab="Relative abundance",xlab="Years",cex.lab=cl,cex.axis=ca,font=fonts,family=fam)
lines(IBTS.histst[,"age1"]~c(IBTS.histst[,"measurement_year"]-1),type="b",pch=1,lty=2)
lines(IBTS.histst[,"age2"]~c(IBTS.histst[,"measurement_year"]-2),type="b",pch=6,lty=3)
legend("topright",legend=c("Standardized MIK 0wr index","Standardized IBTS 1wr","Standardized IBTS 2wr"),pch=c(19,1,6),lty=c(1,2,3),box.lty=0)


range.          <- range(as.numeric(dimnames(NSH.tun[[4]]@index)[[2]]))
ACOUSTIC.mat    <- window(NSH@mat,range.[1],range.[2])[ac(dimnames(NSH.tun[[4]]@index)[[1]]),] * NSH.tun[[4]]@index
ACOUSTIC.matst  <- colSums(ACOUSTIC.mat)/mean(colSums(ACOUSTIC.mat))

plot(MLAI.histst[,"ageall"]~MLAI.histst[,"measurement_year"],type="b",pch=19,main="Relative abundance MLAI and Acoustic mature part",ylab="Relative abundance",xlab="Years",cex.lab=cl,cex.axis=ca,font=fonts,family=fam)
lines(ACOUSTIC.matst~as.numeric(dimnames(ACOUSTIC.matst)$year),type="b",pch=1,lty=2)
legend("topleft",legend=c("Standardized MLAI index","Standardized Acoustic mature part index"),pch=c(19,1),lty=c(1,2),box.lty=0)


mikyears    <- range(MIK.hist[,"measurement_year"])
ibtsyears   <- range(IBTS.hist[,"measurement_year"]+1)
range.      <- c(max(mikyears[1],ibtsyears[1]),min(mikyears[2],ibtsyears[2])-1)
start.mik   <- range.[1];   end.mik <- range.[2]
start.ibts  <- start.mik+1; end.ibts <- end.mik+1

MIK  <- MIK.hist[which(MIK.hist[,"measurement_year"]==start.mik):which(MIK.hist[,"measurement_year"]==end.mik),"age0"]
IBTS <- IBTS.hist[which(IBTS.hist[,"measurement_year"]==start.ibts):which(IBTS.hist[,"measurement_year"]==end.ibts),"age1"]

plot(MIK,IBTS,pch=19,ylab="IBTS 1wr",xlab="MIK 0wr",cex.lab=cl,cex.axis=ca,font=fonts,family=fam)
lines(predict(lm(IBTS~MIK),newdata=data.frame(MIK=range(MIK)))~range(MIK),lwd=2)
mtext(bquote(R^2 [(.(round(summary(lm(IBTS~MIK))$r.squared,2)))]),side=3,line=-2,outer=F,at=40,font=2,cex=1.2)
points(rev(MIK)[1],rev(IBTS)[1],col="red",pch=19); text(rev(MIK)[1],rev(IBTS)[1],labels=c(end.mik-1),col="red",pos=3)
lines(x=c(rep(rev(MIK.hist[,"age0"])[1],2)),y=c(0,max(IBTS,na.rm=T)/7),col="darkgreen",lwd=2); text(rev(MIK.hist[,"age0"])[1],max(IBTS,na.rm=T)/8,labels=end.mik,col="darkgreen",pos=2)
legend("bottomright",c("MIK 0wr vs. IBTS 1wr",paste(end.mik-1,"yearclass"),paste(end.mik,"yearclass")),col=c("black","red","darkgreen"),lty=c(0,0,1),pch=c(19,19,-1),lwd=3,box.lty=0)


