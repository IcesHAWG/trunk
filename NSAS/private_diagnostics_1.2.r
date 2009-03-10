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
              par(oma=c(0,0,0,2))
              plot(c(ssb(stk)@.Data)~seq(range(stk)[c("minyear")],range(stk)[c("maxyear")],1),
              type="b",pch=19,cex.lab=cl,cex.axis=ca,main=paste("SSB",name(stk)),font=fonts,family=fam,
              ylim=range(pretty(range(c(ssb(stk)@.Data)))),xlab="Years",ylab="SSB")
              abline(h=btrigger,col="darkgreen",lwd=2,lty=2)
              abline(h=blim,col="red",lwd=2,lty=2)
              #text(btrigger~range(stk)[c("maxyear")],expression(B[trigger]),cex=ltextcex,adj=c(0.6,0),col="darkgreen",las=2,pos=3)
              #text(blim~range(stk)[c("maxyear")],expression(B[lim]),cex=ltextcex,adj=c(0.6,0),col="red",las=2,pos=3)
              mtext(expression(B[trigger]),4,at=btrigger,cex=ltextcex,col="darkgreen",outer=F,line=0.2,las=2)
              mtext(expression(B[lim]),    4,at=blim,    cex=ltextcex,col="red",      outer=F,line=0.2,las=2)
              }
#LNV.ssb(NSH,1.3e6,0.8e6)

LNV.fbar <- function(stk,ftarget,flim,range.){
              par(oma=c(0,0,0,2))
              range2 <- range.[1]:range.[2]
              plot(c(apply(stk@harvest[ac(range2),],2,mean)@.Data)~seq(range(stk)[c("minyear")],range(stk)[c("maxyear")],1),
              type="b",pch=19,cex.lab=cl,cex.axis=ca,main=paste("Fbar",name(stk)),font=fonts,family=fam,
              xlab="Years",ylab="Fbar",ylim=c(0,1.6))
              abline(h=ftarget,col="darkgreen",lwd=2,lty=2)
              abline(h=flim,col="red",lwd=2,lty=2)
              mtext(text=bquote(F[.(range.[1])-.(range.[2])[target]]),4,at=ftarget,cex=ltextcex,outer=F,line=0.2,las=2,font=fonts,col="darkgreen")
              mtext(bquote(F[.(range.[1])-.(range.[2])[lim]]),4,at=flim,cex=ltextcex,,outer=F,line=0.2,las=2,font=fonts,col="red")
              legend("topright",legend=bquote(F[.(range.[1])-.(range.[2])]),lty=1,pch=19,box.lty=0)
            }
#LNV.fbar(NSH,0.25,0.1,c(2,6))
#LNV.fbar(NSH,0.1,0.04,c(0,1))

LNV.rec <- function(stk){
              plot(c(rec(stk)@.Data)~seq(range(stk)[c("minyear")],range(stk)[c("maxyear")],1),
              type="b",pch=19,,cex.lab=cl,cex.axis=ca,main=paste("Recruitment",name(stk)),font=fonts,family=fam,
              xlab="Years",ylab="Recruitment [thousands]")
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
MLAI.hist   <- matrix(c(13.3,7.945,2.73,2.458,6.105,7.369,14.428,9.859,14.537,21.029,26.975,48.816,74.347,38.632,67.99,134.915,131.571,170.589,89.67,41.885,29.848,20.652,22.736,44.734,57.253,74.389,61.909,41.087,130.313,110.964,271.653,321.995,192.708,117.856,173.003,150),nrow=1,dimnames=list(age=c("all"),year=seq(range(NSH.tun[[1]])["minyear"],range(NSH.tun[[1]])["maxyear"],1)))
plot(as.numeric(dimnames(MLAI.hist)$year),c(MLAI.hist),type="b",pch=19,xlab="Years",ylab="Larval abundance",cex.lab=cl,cex.axis=ca,font=fonts,family=fam,main="Historic MLAI index")
text(as.numeric(dimnames(MLAI.hist)$year),MLAI.hist,labels=dimnames(MLAI.hist)$year,col="black",cex=0.7,adj=c(0.5,1.5))
#Survey MIK
MIK.hist  <- matrix(c(17.1,13.1,52.1,101.1,76.7,133.9,91.8,115,181.3,177.4,270.9,168.9,71.4,25.9,69.9,200.7,190.1,101.7,127,106.5,148.1,53.1,244,137.1,214.8,161.8,54.4,47.3,61.3,83.1,37.2,27.8,96.6),nrow=1,dimnames=list(age=c(0),yearclass=seq(1976,2008,1)))
MIK.st <- MIK.hist/mean(MIK.hist)
plot(as.numeric(dimnames(MIK.hist)$year),c(MIK.hist),type="b",pch=19,xlab="Years",ylab="Larval abundance",cex.lab=cl,cex.axis=ca,font=fonts,family=fam,main="Historic MIK index")
text(as.numeric(dimnames(MIK.hist)$year),MIK.hist,labels=dimnames(MIK.hist)$year,col="black",cex=0.7,adj=c(0.5,1.5))

#Survey IBTS1-2 vs. MIK
IBTS.hist                 <- read.csv("M:/My Documents/IMARES/Werkgroepen/HAWG/2008/IBTS-historic.csv",header=T)
IBTS.hist[IBTS.hist==-1]  <- NA
IBTS1.st                  <- matrix(IBTS.hist[,2]/mean(IBTS.hist[,2],na.rm=T),nrow=1,dimnames=list(c("1"),c(IBTS.hist[,1]-2)))
IBTS2.st                  <- matrix(IBTS.hist[,3]/mean(IBTS.hist[,3],na.rm=T),nrow=1,dimnames=list(c("1"),c(IBTS.hist[,1]-3)))
IBTS2.st[,"1985"]         <- NA

plot(as.numeric(dimnames(MIK.hist)$year),MIK.st,type="b",pch=19,ylim=c(0,3),ylab="Relative abundance",xlab="Years",main="Relative abundance surveys MIK vs. IBTS 1-2",cex.lab=cl,cex.axis=ca,font=fonts,family=fam,xlim=c(min(as.numeric(dimnames(MIK.st)$year),as.numeric(dimnames(IBTS1.st)[[2]]),as.numeric(dimnames(IBTS2.st)[[2]])),max(as.numeric(dimnames(MIK.st)$year),as.numeric(dimnames(IBTS1.st)[[2]]),as.numeric(dimnames(IBTS2.st)[[2]]))))
lines(as.numeric(dimnames(IBTS1.st)[[2]]),IBTS1.st,lty=2,pch=1,type="b")
lines(as.numeric(dimnames(IBTS2.st)[[2]]),IBTS2.st,lty=3,pch=6,type="b")
legend("topright",legend=c("Standardized MIK index","Standardized IBTS 1wr","Standardized IBTS 2wr"),pch=c(19,1,6),lty=c(1,1,1),box.lty=0)

#Survey IBTS1 vs. MIK
range. <- c(range(as.numeric(dimnames(MIK.hist)$year)),range(IBTS.hist[,1]-2))
range. <- c(max(range.[c(1,3)]),min(range.[c(2,4)]))
plot(MIK.hist[,ac(range.[1]:range.[2])],array(IBTS.hist[,2],dimnames=list(IBTS.hist[,1]-2))[ac(range.[1]:range.[2])],xlab="MIK index",ylab="IBTS 1-wr index",cex.lab=cl,cex.axis=ca,font=fonts,family=fam,pch=19)
points(rev(MIK.hist[,ac(range.[1]:range.[2])])[1],rev(array(IBTS.hist[,2],dimnames=list(IBTS.hist[,1]-2))[ac(range.[1]:range.[2])])[1],col="red",pch=19)
text(rev(MIK.hist[,ac(range.[1]:range.[2])])[1],rev(array(IBTS.hist[,2],dimnames=list(IBTS.hist[,1]-2))[ac(range.[1]:range.[2])])[1],labels=names(rev(MIK.hist[,ac(range.[1]:range.[2])])[1]),pos=1,col="red",cex=ltextcex,font=fonts,family=fam)
lines(c(rev(MIK.hist)[1],rev(MIK.hist)[1]),c(0,range(IBTS.hist[,2])[2]/20),col="darkgreen",lwd=3)
a<-array(IBTS.hist[,2],dimnames=list(IBTS.hist[,1]-2))[ac(range.[1]:range.[2])]
b<-MIK.hist[,ac(range.[1]:range.[2])]
lin.mik_ibts <- lm(a~b)
lines(x=seq(range(MIK.hist)[1],range(MIK.hist)[2],length.out=2),y=predict.lm(lin.mik_ibts,newdata=data.frame(b=seq(range(MIK.hist)[1],range(MIK.hist)[2],length.out=2))))
text(max(range(MIK.hist),na.rm=T),max(IBTS.hist[,2],na.rm=T),labels=bquote(R^2 [(.(round(summary(lin.mik_ibts)$r.squared,2)))]),pos=2,cex=ltextcex,font=fonts,family=fam)
text(rev(MIK.hist)[1],0,labels=rev(dimnames(MIK.hist)$year)[1],adj=c(-0.2,-0.2),col="darkgreen",cex=ltextcex,font=fonts,family=fam)
legend("bottomright",c("Last years estimate","This years estimate"),col=c("red","darkgreen"),pch=c(19,-1),lty=c(0,1),lwd=3,box.lty=0)
 
 

 
 
#library(FLBRP);
#library(FLEDA);
#
#
#load("N:\\Projecten\\ICES WG\\Haring werkgroep HAWG\\2009\\controle\\nsh07.RData")
#
#################################################################################
##FLEDA plots
#################################################################################
#
##===============================================================================
##Look at the ratio between mature and immature biomass over time
##===============================================================================
#               
#mat.immat.ratio <- function(stk){
#                    stk.bmass <- bmass(stk)
#                    akey <- simpleKey(text=c("Mature", "Immature"), points=F, lines=T)
#                    xyplot(data~year, data=stk.bmass, groups=qname,type="l", main="Mature - Immature biomass ratio", key=akey, ylab="Relative biomass",sub=stk@name)
#                  }
#                  
#mat.immat.ratio(NSH)
#
#
##===============================================================================
##Look at strong years in the catch and how they move trough time, and look at strong yearclasses
##all on catch proportions
##===============================================================================
#
#yearclass.bubble <- function(stk){
#                      stk.spay <- spay(stk@catch.n)                      
#                      bubbles(age~year, data=stk.spay, bub.scale=5,main="Standardized catch number proportion at age",  sub=stk@name)
#                    }
#
#yearclass.bubble(NSH)
#
#
#cohort.bubble <- function(stk){
#                    stk.spay <- spay(stk@catch.n)                      
#                    bubbles(age~cohort, data=FLCohort(stk.spay), bub.scale=5,main="Standardized catch number proportion at age",  sub=stk@name)
#                  }
#
#cohort.bubble(NSH)
#
#
##===============================================================================
##Look at correlation between year classes and look at correlation within a cohort
##===============================================================================
#
#cor.tun <- function(stk.tun){ for(i in names(stk.tun)) if(dim(stk.tun[[i]]@index)[1]>1) plot(stk.tun[[i]],type="internal",main=name(stk.tun[[i]]))}
#  
#cor.tun(NSH.tun)
#
#
#
#
#
#
##===============================================================================
##Look at survey CPUE's
##===============================================================================
#
#cpue.survey <- function(stk.tun,slot.){
#            lst <- lapply(stk.tun, function(x){return(slot(x,slot.))})
#            # now a nice FLQuants
#            stk.inds <- mcf(lst)
#            # scale
#            stk.indsN01 <- lapply(stk.inds, function(x){
#                              arr <- apply(x@.Data, c(1,3,4,5,6), scale)
#                              arr <- aperm(arr, c(2,1,3,4,5,6))
#                              # small trick to fix an apply "feature"
#                              dimnames(arr) <- dimnames(x)
#                              x <- FLQuant(arr)
#                            })
#            
#            stk.indsN01 <- FLQuants(stk.indsN01)
#            # stupid hack to correct names (fixed in version 2)
#            names(stk.indsN01) <- names(lst)
#            # fine tune
#            ttl <- list("Surveys CPUE", cex=1)
#            xttl <- list(cex=0.8)
#            yttl <- list("Standardized CPUE", cex=0.8)
#            stripttl <- list(cex=0.8)
#            ax <- list(cex=0.7)
#            akey <- simpleKey(text=names(stk.indsN01), points=F, lines=T, columns=2, cex=0.8,col=c(0,6,3,2))
#            akey$lines$lty<-c(0,1,1,1)    #1=mlai, #2=MIK #3=IBTS #4=Acoust
#            # plot                                                                   #1=Acoust, #2=IBTS #3=MIK #4=MLAI
#            print(xyplot(data~year|factor(age), data=stk.indsN01, type="l",col=c(2,3,6,0),
#            main=ttl, xlab=xttl, ylab=yttl, key=akey, striptext=stripttl,
#            scales=ax, groups=qname,as.table=TRUE, layout=c(5,2,1)))
#            }
#
#cpue.survey(NSH.tun,"index")
##===============================================================================
##Look at weight at age in catch, and weight at age in stock
##===============================================================================
#wt.at.age <- function(stk,start.,end.){
#              print(xyplot(data~year,data=window(stk@catch.wt,start=start.,end=end.),groups=age,type="l",
#                    col=1:(length(seq(range(stk)["max"]-range(stk)["min"]))+1),ylab="Weight in Kg",
#                    main=paste("Catch.wt vs stock.wt in",end.) ,panel = function(...) { 
#              
#                     panel.xyplot(...)
#                     panel.text(x=rep((start.-1),(length(seq(range(stk)["max"]-range(stk)["min"]))+1)), y=stk@catch.wt[,ac(start.)], labels=seq(range(stk)["max"]-range(stk)["min"]),col=1:(length(seq(range(stk)["max"]-range(stk)["min"]))+1))
#                     panel.points(x=rep(end.,(length(seq(range(stk)["max"]-range(stk)["min"]))+1)),y=stk@stock.wt[,ac(end.)],col=1:(length(seq(range(stk)["max"]-range(stk)["min"]))+1),pch=19)
#              }))
#              }
#wt.at.age(NSH,1980,2007) 
##===============================================================================
##Look at catch curves
##===============================================================================
#
#catch.curves <- function(stk,start.,end.){
#                  stk.cc.l      <- logcc(window(stk@catch.n,start=start.,end=end.))
#                  stk.cc.coh    <- FLCohort(window(stk@catch.n,start=start.,end=end.))
#                  color         <- c("black","green","red","blue","orange","grey","pink","lightblue","darkviolet","darkblue")
#                  
#                  stk.age       <- apply(stk.cc.l,1:2,mean)
#                  years         <- as.numeric(dimnames(stk.age)$cohort); ages        <- as.numeric(dimnames(stk.age)$age)
#                  
#                  
#                  akey <- simpleKey(text=paste(seq(start.,end.,1)), points=F, lines=T, columns=3, cex=0.8)
#                  print(ccplot(data~age, data=stk.cc.l, type="l",lwd=2,main="Log catch ratios",key=akey))
#                  akey <- simpleKey(text=paste("yearclass",dimnames(stk.cc.coh)$cohort), points=F, lines=T, columns=3, cex=0.8)
#                  print(ccplot(data~age, data=stk.cc.coh, type="l",main="Cohort absolute catch ratios",lwd=2,key=akey))
#                  
#                  plot(stk.age[1,ac(years)]~years,type="l",ylim=c(-3,3),col=color[1],lwd=2,main="Selectivity at age",ylab="data")
#                  for(i in 2:length(ages)) lines(stk.age[i,ac(years)]~years,col=color[i],lwd=2)
#                  legend("bottomleft",c(ac(ages)),lwd=2,col=color,box.lty=0,ncol=4)
#                  }
#
#
#catch.curves(NSH,1990,2007)
#################################################################################
##FLBRP plots
#################################################################################
#
##===============================================================================
##Look at reference points and plot them
##===============================================================================
#ref.pts <- function(stk,sr.model.,factor.){
#                bevholtfactor   <- factor.
#                stk.sr  <- fmle(as.FLSR(transform(stk, stock.n = stk@stock.n/bevholtfactor),model=model.)); 
#                if(model.=="bevholt"){ stk.sr@params<-stk.sr@params * bevholtfactor   
#                } else {
#                  stk.sr@params[2]<-stk.sr@params[2] * bevholtfactor; }
#                rpts<-refpts()[4:5,]
#                dimnames(rpts)[[1]][2]<-"crash"
#                stk.brp    <- brp(FLBRP(stk,sr=stk.sr,fbar=seq(0,1,length.out=100),nyrs=3,refpts=rpts))
#                refpts(stk.brp)
#                plot(nsh.brp)
#                return(stk.sr)
#            }
#
#NSH.sr <- ref.pts(NSH,"bevholt",100000)[[1]]
##===============================================================================
##Look at landings selectivity 
##===============================================================================
#
#retro.landings.sel <- function(stk.ica,stk.sr,mnYrs,rpts){
#  for(i in 1:10){
#    range. <- c(range(stk)[c("minyear","maxyear")])
#    stk. <- window(stk.ica,(range.[2]-mnYrs-i+1),(range.[2]-1-i+1))
#    print(c((range.[2]-mnYrs-i+1),(range.[2]-1-i+1)))
#    if(i==1){ plot(c(landings.sel(brp(FLBRP(stk.,fbar=seq(0,1,length.out=100),nyrs=mnYrs,refpts=rpts))))~c(range(stk.brp)[c("min")]:range(stk.brp)[c("max")]),type="l",xlab="Age",ylab="Landings selectivity")
#    } else { lines(c(landings.sel(brp(FLBRP(stk.,fbar=seq(0,1,length.out=100),nyrs=mnYrs,refpts=rpts))))~c(range(stk.brp)[c("min")]:range(stk.brp)[c("max")]),col=i)
#      }
#  }
#}  
#
#retro.landings.sel(NSH,NSH.sr,10,rpts)
#
#
#
#                                                                     