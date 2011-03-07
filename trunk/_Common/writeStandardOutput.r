# Code to produce the standard graph output, as well as creating the standard graphs and writing everyting to file (added by NTH at 18-03-2010)
writeStandardOutput <- function(stck.,stck.sr,retro.,nyrs.=3,recImY=NULL,output.base="./",Blim=NULL,Bpa=NULL,Flim=NULL,Fpa=NULL,Bmsy=NULL,Fmsy=NULL){
                          an                                <- function(x){return(as.numeric(x))}
                          rpts<-refpts()
                          dimnames(rpts)[[1]][5]            <-"crash"
                          dimnames(rpts)[[1]][3]            <-"spr.35"
                          dimnames(rpts)[[1]][2]            <-"fmed"
                          stck.brp                          <- brp(FLBRP(stck.,sr=stck.sr,fbar=seq(0,1,length.out=100),nyrs=nyrs.,refpts=rpts))
                          # Calculate the spawners in number
                          spawners                          <- colSums(stck.brp@stock.n * sweep(exp(sweep(-sweep(stck.brp@harvest,c(1,3:6),stck.brp@harvest.spwn,"*"),
                                                                       c(1,3:6),stck.brp@m*stck.brp@m.spwn,"-")),c(1,3:6),stck.brp@mat,"*"))
                          # Put all the standard input in a dataframe in columns
                          standardGraphTable                <- cbind(stck.brp@fbar,yield(stck.brp),ssb(stck.brp),rec(stck.brp),yield(stck.brp)/rec(stck.brp),
                                                                     ssb(stck.brp)/rec(stck.brp),spawners,landings(stck.brp))
                          standardGraphTable                <- data.frame(standardGraphTable)
                          colnames(standardGraphTable)      <- c("Fbar","Yield","SSB","Recruits","Yield.Recruit","SSB.Recruit","Spawners","Landings")
                          # Round some values
                          standardGraphTable$Fbar           <- round(an(ac(standardGraphTable$Fbar)),3)
                          standardGraphTable$Yield          <- round(an(ac(standardGraphTable$Yield)))
                          standardGraphTable$SSB            <- round(an(ac(standardGraphTable$SSB)))
                          standardGraphTable$Recruits       <- round(an(ac(standardGraphTable$Recruits)))
                          standardGraphTable$Yield.Recruit  <- round(an(ac(standardGraphTable$Yield.Recruit)),4)
                          standardGraphTable$SSB.Recruit    <- round(an(ac(standardGraphTable$SSB.Recruit)),3)
                          standardGraphTable$Spawners       <- round(an(ac(standardGraphTable$Spawners)))
                          standardGraphTable$Landings       <- round(an(ac(standardGraphTable$Landings)))

                          # Give it the right units
                          standardGraphTable                <- rbind(c(paste("Ages ",range(stck.)["minfbar"],"-",range(stck.)["maxfbar"],sep=""),
                                                                     "Tonnes","Tonnes","Number","","","Number","Tonnes"),standardGraphTable)
                          # Write the standard graph to file and the reference points as well
                          write.table(standardGraphTable,file=paste(output.base,"standardGraphTable.csv",sep=""),col.names=T,row.names=F,append=F,sep=",")

                          #-Put all the reference points together and include Fmed (3 year average)
                          refpoints <- cbind(refpts(stck.brp)@.Data[,1:5,1],refpts(stck.brp)[,"yield"]/refpts(stck.brp)[,"rec"],refpts(stck.brp)[,"ssb"]/refpts(stck.brp)[,"rec"])
                          res       <- yearMeans(fbar(stck.)[,ac((range(stck.)["maxyear"]-2):range(stck.)["maxyear"])])
                          resfmed   <- which.min(abs(outer(c(res),c(stck.brp@fbar),FUN="-")))
                          refpoints <- rbind(refpoints,c(yearMeans(fbar(stck.)[,ac((range(stck.)["maxyear"]-2):range(stck.)["maxyear"])]),
                                                         yield(stck.brp)[,resfmed],
                                                         yearMeans(rec(stck.)[,ac((range(stck.)["maxyear"]-2):range(stck.)["maxyear"])]),
                                                         yearMeans(ssb(stck.)[,ac((range(stck.)["maxyear"]-2):range(stck.)["maxyear"])]),
                                                         yearMeans(stock(stck.)[,ac((range(stck.)["maxyear"]-2):range(stck.)["maxyear"])]),
                                                         yield(stck.brp)[,resfmed]/rec(stck.brp)[,resfmed],
                                                         ssb(stck.brp)[,resfmed]/rec(stck.brp)[,resfmed]))
                          colnames(refpoints) <- c(colnames(refpoints)[-c(6,7)],"yield/R","SSB/R")
                          rownames(refpoints) <- c(rownames(refpoints)[-c(6)],"3y_aver")

                          #- Write the refpoints to file
                          write.table(refpoints,file=paste(output.base,"referencePoints.csv",sep=""),col.names=T,row.names=T,append=F,sep=",")


                          #-----------------------------------------------------
                          # Create the Precautionary approach plot. Size of the
                          #  plot needs to be 5.4 cm by 5.4 cm, so reduce in
                          #  Word when pasting
                          #-----------------------------------------------------
                          png(paste(output.base,"PAplot.png"),units = "px", height=540,width=540,pointsize = 24, bg = "white",res=72)
                          par(yaxs="i",las=1,oma=c(0,1,0,0),mar=c(5.1,4.1,2.1,2.1))
                          yrange <- range(c(ssb(stck.))/1000,na.rm=T)*c(0,1.05)
                          xrange <- range(c(Fmsy,Fpa,Flim),c(fbar(stck.)),na.rm=T)*c(0.95,1.05)
                          plot(c(ssb(stck.))/1000~c(fbar(stck.)),type="l",lwd=2,xlab=paste("Fishing Mortality (ages ",range(stck.)["minfbar"],"-",range(stck.)["maxfbar"],")",sep=""),
                               ylab="",ylim=yrange,xlim=xrange,font.lab=2,cex.lab=1)
                          par(las=0)
                          mtext("SSB in 1000t",side=2,line=4,cex=1,font=2)
                          abline(v=c(Flim),lty=2,lwd=2.5,col="blue")
                          abline(v=c(Fpa),lty=4,lwd=2.5,col="blue")
                          abline(h=c(Blim/1000),lty=2,lwd=2.5,col="blue")
                          abline(h=c(Bpa/1000),lty=4,lwd=2.5,col="blue")
                          abline(h=c(Bmsy/1000),lty=2,lwd=3.5,col="green")
                          abline(v=c(Fmsy),lty=2,lwd=3.5,col="green")
                          points(c(ssb(stck.[,ac(range(stck.)["maxyear"])]))/1000~c(fbar(stck.[,ac(range(stck.)["maxyear"])])),pch=19,cex=1,col="black")
                          legend("topright",legend=c(range(stck.)["maxyear"]),pch=19,cex=1,col="black",lty=0,box.lty=0)
                          box()
                          dev.off()

                          #-----------------------------------------------------
                          # Create the historical trends plot with landings
                          #  recruitment, Fbar and SSB. Size of the plot needs
                          #  to be 8.8 by 18 cm
                          #-----------------------------------------------------
                          png(paste(output.base,"HistoricalTrendsplot.png"),units = "px", height=880,width=1800,pointsize = 24, bg = "white",res=72)
                          par(mfrow=c(2,2),yaxs="i",las=1,mar=c(3.1,4.1,2.1,2.1))

                          yrange <- range(landings(stck.)/1000,na.rm=T) *c(0,1.05)
                          if(is.null(recImY)==F){
                            xrange <- range(pretty(unique(c(dimnames(stck.@landings)$year,dimnames(ssb(stck.))$year,dimnames(rec(stck.))$year,
                                                     dimnames(fbar(stck.))$year,ac(an(rev(dimnames(stck.@landings)$year)[1])+1)))))
                          } else {
                              xrange <- range(pretty(c(dimnames(stck.@landings)$year)))
                          }

                          #-Plot the landings

                          landings <- data.frame(year=an(c(dimnames(landings(stck.))$year)),catch=an(c(landings(stck.)/1000)))
                          plot(0,0,pch=NA,main="Landings",xlab="",ylab="Landings in 1000 t",
                                  cex.lab=1.1,font.lab=2,ylim=yrange,xlim=xrange)
                          rect(landings$year-0.5,0,landings$year+0.5,landings$catch,col="grey")

                          #-Plot the Recruitment
                          if(is.null(recImY)==F){
                            yrange <- range(rec(stck.)/1000,recImY/1000,na.rm=T) *c(0,1.05)
                          } else {
                              yrange <- range(rec(stck.)/1000,na.rm=T) *c(0,1.05)
                            }

                          if(is.null(recImY)==T)  recruits <- data.frame(year=an(c(dimnames(rec(stck.))$year)),recruits=an(c(rec(stck.)/1000)))
                          if(is.null(recImY)==F){ recruits <- data.frame(year=an(c(dimnames(rec(stck.))$year,ac(an(rev(dimnames(rec(stck.))$year)[1])+1))),
                                                                        recruits=an(c(rec(stck.)/1000,an(recImY)/1000)))}
                          plot(0,0,pch=NA,main=paste("Recruitment (age ",dimnames(rec(stck.))$age,")",sep=""),xlab="",ylab="",
                                  cex.lab=1.1,font.lab=2,ylim=yrange,xlim=xrange)
                          rect(recruits$year-0.5,0,recruits$year+0.5,recruits$recruits,col="grey")
                          par(las=0)
                          mtext("Recruitment in millions",side=2,line=4,cex=1,font=2)
                          #-Plot the Fishing mortality
                          par(yaxs="i",las=1)
                          yrange <- range(fbar(stck.),na.rm=T) * c(0,1.05)

                          plot(c(fbar(stck.))~c(dimnames(fbar(stck.))$year),type="l",ylim=yrange,lwd=2,main="Fishing Mortality",
                               xlab="",ylab=paste("F (ages ",range(stck.)["minfbar"],"-",range(stck.)["maxfbar"],")",sep=""), cex.lab=1.1,font.lab=2,xlim=xrange)
                          abline(h=c(Flim),lty=2,lwd=2.5,col="blue")
                          abline(h=c(Fpa),lty=4,lwd=2.5,col="blue")
                          abline(h=c(Fmsy),lty=2,lwd=3.5,col="green")

                          managementPoints <- which(c(is.null(Blim),is.null(Bpa),is.null(Bmsy),is.null(Flim),is.null(Fpa),is.null(Fmsy))==F)
                          managementPlots  <- c(expression(B[lim]),expression(B[pa]),expression(B[MSY]),expression(F[lim]),expression(F[pa]),expression(F[MSY]))

                          if(length(managementPlots[managementPoints[managementPoints>3]])>0){
                            legend("topright",legend=c(managementPlots[managementPoints[managementPoints>3]]),
                                 lty=na.omit(c(ifelse(is.null(Flim)==F,2,numeric()),ifelse(is.null(Fpa)==F,4,numeric()),ifelse(is.null(Fmsy)==F,2,numeric()))),
                                 lwd=na.omit(c(ifelse(is.null(Flim)==F,2.5,numeric()),ifelse(is.null(Fpa)==F,2.5,numeric()),ifelse(is.null(Fmsy)==F,3.5,numeric()))),
                                 col=na.omit(c(ifelse(is.null(Flim)==F,"blue",numeric()),ifelse(is.null(Fpa)==F,"blue",numeric()),ifelse(is.null(Fmsy)==F,"green",numeric()))),
                                 box.lty=0)
                          }
                          lines(c(fbar(stck.))~c(dimnames(fbar(stck.))$year),lwd=2)
                          box()

                          #-Plot SSB
                          yrange <- range(ssb(stck.)/1000,na.rm=T) * c(0,1.05)
                          plot(c(ssb(stck.)/1000)~c(dimnames(ssb(stck.))$year),type="l",ylim=yrange,lwd=2,
                               xlab="",ylab="SSB in 1000 t", cex.lab=1.1,font.lab=2,main="Spawning Stock Biomass",xlim=xrange)

                          abline(h=c(Blim)/1000,lty=2,lwd=2.5,col="blue")
                          abline(h=c(Bpa)/1000,lty=4,lwd=2.5,col="blue")
                          abline(h=c(Bmsy)/1000,lty=2,lwd=3.5,col="green")
                          if(length(managementPlots[managementPoints[managementPoints<=3]])>0){
                          legend("topright",legend=c(managementPlots[managementPoints[managementPoints<=3]]),
                                 lty=na.omit(c(ifelse(is.null(Blim)==F,2,numeric()),ifelse(is.null(Bpa)==F,4,numeric()),ifelse(is.null(Bmsy)==F,2,numeric()))),
                                 lwd=na.omit(c(ifelse(is.null(Blim)==F,2.5,numeric()),ifelse(is.null(Bpa)==F,2.5,numeric()),ifelse(is.null(Bmsy)==F,3.5,numeric()))),
                                 col=na.omit(c(ifelse(is.null(Blim)==F,"blue",numeric()),ifelse(is.null(Bpa)==F,"blue",numeric()),ifelse(is.null(Bmsy)==F,"green",numeric()))),
                                 box.lty=0)
                          }
                          lines(c(ssb(stck.)/1000)~c(dimnames(ssb(stck.))$year),lwd=2)
                          box()
                          dev.off()

                          #-----------------------------------------------------
                          # Create the Stock to recruit plot and the yield per
                          #  recruit and SSB by recruit plot
                          #  Size of the plot has to be 4.4 by 18 cm
                          #-----------------------------------------------------

                          png(paste(output.base,"SR_YRplot.png"),units = "px", height=440,width=1800,pointsize = 24, bg = "white",res=72)
                          par(mfrow=c(1,2),oma=c(0,1,0,3),yaxs="i",las=1,mar=c(5.1,4.1,2.1,2.1))

                          yrange <- range(c(rec(stck.)/1000),na.rm=T)*c(0,1.05)
                          plot(c(rec(stck.)/1000)~c(ssb(stck.)/1000),type="p",pch=19,xlab="SSB in 1000 t",cex.lab=1.1,font.lab=2,ylim=yrange,ylab="",
                               main="Stock - Recruitment")
                          par(las=0)
                          mtext("Recruitment in millions",side=2,line=4,cex=1,font=2)
                          abline(v=c(Blim)/1000,lty=2,lwd=2.5,col="blue")
                          abline(v=c(Bpa)/1000,lty=4,lwd=2.5,col="blue")
                          abline(v=c(Bmsy)/1000,lty=2,lwd=3.5,col="green")
                          if(length(managementPlots[managementPoints[managementPoints<=3]])>0){
                          legend("topright",legend=c(managementPlots[managementPoints[managementPoints<=3]]),
                                 lty=na.omit(c(ifelse(is.null(Blim)==F,2,numeric()),ifelse(is.null(Bpa)==F,4,numeric()),ifelse(is.null(Bmsy)==F,2,numeric()))),
                                 lwd=na.omit(c(ifelse(is.null(Blim)==F,2.5,numeric()),ifelse(is.null(Bpa)==F,2.5,numeric()),ifelse(is.null(Bmsy)==F,3.5,numeric()))),
                                 col=na.omit(c(ifelse(is.null(Blim)==F,"blue",numeric()),ifelse(is.null(Bpa)==F,"blue",numeric()),ifelse(is.null(Bmsy)==F,"green",numeric()))),
                                 box.lty=0)
                          }
                          points(c(rec(stck.)/1000)~c(ssb(stck.)/1000),pch=19)
                          box()

                          #- Create the Yield / Recruit plot
                          yrange <- range(c(yield(stck.brp)/rec(stck.brp)),na.rm=T)*c(0,1.05)
                          par(las=1)
                          plot(c(yield(stck.brp)/rec(stck.brp))~c(fbar(stck.brp)),type="l",lty=2,lwd=2,ylim=yrange,
                               ylab="",xlab=paste("Fishing mortality (ages ",range(stck.)["minfbar"],"-",range(stck.)["maxfbar"],")",sep=""),
                               main="Yield and Spawning Stock Biomass per Recruit",font.lab=2,cex.lab=1.1)
                          par(las=0)
                          mtext("Yield/R (dashed line)",side=2,line=4,font=2)
                          par(new=T,las=1)
                          yrange <- range(c(ssb(stck.brp)/rec(stck.brp)),na.rm=T)*c(0,1.05)
                          plot(c(ssb(stck.brp)/rec(stck.brp))~c(fbar(stck.brp)),lwd=2,lty=1,type="l",ylim=yrange,ylab="",yaxt="n",xlab="")
                          axis(4)
                          par(las=0)
                          mtext("SSB/R (line)",side=4,font=2,line=3)

                          dev.off()

                          #-----------------------------------------------------
                          # Create the retrospecive plots
                          #  for SSB retrospective, Fishing mortality and
                          #  recruitment. Size of plot has to be 4.4 by 18 cm
                          #-----------------------------------------------------

                          png(paste(output.base,"Retroplot.png"),units = "px", height=440,width=1800,pointsize = 24, bg = "white",res=72)
                          par(mfrow=c(1,3),yaxs="i",las=1,mar=c(4.1,3.1,2.1,1.1))

                          xrange <- range(unlist(lapply(retro.,function(x){return(c(range(x)["minyear"],range(x)["maxyear"]))})))
                          yrange <- range(unlist(lapply(retro.,function(x){return(c(range(ssb(x),na.rm=T)))})),na.rm=T)/1000 * c(0,1.05)
                          for(i in 1:length(retro.)){
                            if(i == 1){plot(c(ssb(retro.[[i]])/1000)~c(dimnames(ssb(retro.[[i]]))$year),xlim=xrange,ylim=yrange,main="SSB in 1000 t",
                                 xlab="",ylab="",type="l")
                            } else {
                                if(i == length(retro.)) {
                                  lines(c(ssb(retro.[[i]])/1000)~c(dimnames(ssb(retro.[[i]]))$year),col="red",lwd=2)
                                } else { lines(c(ssb(retro.[[i]])/1000)~c(dimnames(ssb(retro.[[i]]))$year))}
                              }
                          }
                          xrange <- range(unlist(lapply(retro.,function(x){return(c(range(x)["minyear"],range(x)["maxyear"]))})))
                          yrange <- range(unlist(lapply(retro.,function(x){return(c(range(fbar(x),na.rm=T)))})),na.rm=T) * c(0,1.05)
                          for(i in 1:length(retro.)){
                            if(i == 1){plot(c(fbar(retro.[[i]]))~c(dimnames(fbar(retro.[[i]]))$year),xlim=xrange,ylim=yrange,
                                            main=paste("Fishing Mortality (ages ",range(retro.[[i]])["minfbar"],"-",range(retro.[[i]])["maxfbar"],")",sep=""),
                                 xlab="",ylab="",type="l")
                            } else {
                                if(i == length(retro.)) {
                                  lines(c(fbar(retro.[[i]]))~c(dimnames(fbar(retro.[[i]]))$year),col="red",lwd=2)
                                } else { lines(c(fbar(retro.[[i]]))~c(dimnames(fbar(retro.[[i]]))$year))}
                              }
                          }
                          xrange <- range(unlist(lapply(retro.,function(x){return(c(range(x)["minyear"],range(x)["maxyear"]))})))
                          yrange <- range(unlist(lapply(retro.,function(x){return(c(range(rec(x),na.rm=T)))})),na.rm=T)/1000 * c(0,1.05)
                          for(i in 1:length(retro.)){
                            if(i == 1){plot(c(rec(retro.[[i]])/1000)~c(dimnames(rec(retro.[[i]]))$year),xlim=xrange,ylim=yrange,
                                            main=paste("Recruitment (age ",dimnames(rec(stck.))$age,") in millions",sep=""),
                                 xlab="",ylab="",type="l")
                            } else {
                                if(i == length(retro.)) {
                                  lines(c(rec(retro.[[i]])/1000)~c(dimnames(rec(retro.[[i]]))$year),col="red",lwd=2)
                                } else { lines(c(rec(retro.[[i]])/1000)~c(dimnames(rec(retro.[[i]]))$year))}
                              }
                          }
                          dev.off()

                      }
