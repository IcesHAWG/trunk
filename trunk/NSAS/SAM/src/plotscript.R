# Script to generate plots from the state-space assessment model 
# 
# Anders Nielsen <anders@nielsensweb.org> Oct. 2008
oldwd<-getwd()

source('src/common.R')

############################## read data ##############################

fit.base<-read.fit('baserun/ssass')
fit.current<-read.fit('run/ssass')
data.dir <- file.path("..","data")

read.ices<-function(filen){
  # Function to read ices data files 
  head<-scan(filen, skip=1, n=7, quiet=TRUE)
  minY<-head[3]
  maxY<-head[4]
  minA<-head[5]
  maxA<-head[6]
  C<-as.matrix(read.table(filen, skip=5, header=F))
  C<-C[,1:(maxA-minA+1)]
  rownames(C)<-minY:maxY
  colnames(C)<-minA:maxA
  return(C)
}


# Read in data 

# Read in data 
catch.no<-read.ices(file.path(data.dir,'canum.txt'))
catch.mean.weight<-read.ices(file.path(data.dir,'west.txt'))
#stock.mean.weight<-read.ices('west.txt')
#prop.mature<-read.ices('matprop.txt')
#natural.mortality<-read.ices('natmor.txt')
#surveys<-read.surveys('fleet.txt')
#fprop<-matrix(0,nrow=nrow(prop.mature), ncol=ncol(prop.mature))
#mprop<-matrix(0,nrow=nrow(prop.mature), ncol=ncol(prop.mature))
#land.frac<-matrix(1,nrow=nrow(prop.mature), ncol=ncol(prop.mature))
ICA.results <- read.table("../results/NSH Assessment Summary Table.txt",header=TRUE)

############################## plots ##############################

LO.OK<-file.exists('run/LO.RData')
if(LO.OK){
  load('run/LO.RData')
}

RETRO.OK<-file.exists('run/RETRO.RData')
if(RETRO.OK){
  load('run/RETRO.RData')
}

plots<-function(){
  # SSB plot
  baseplot(fit.base, 'ssb', trans=function(x)x/1000, ylab='SSB (kt)', las=1)
  #addlines(fit.current, 'ssb', col='red', trans=function(x)x/1000)
  lines(ICA.results$year, ICA.results$ssb/1000, col='blue')

  # Fbar plot
  baseplot(fit.base, 'fbar', ylab=expression(bar(F)[2-6]), las=1, drop=1)
  #addlines(fit.current, 'fbar', col='red', drop=1)
  lines(ICA.results$year, ICA.results$fbar, col='blue')

  # Recruit plot
  baseplot(fit.base, 'R', ylab='Recruits (millions)', trans=function(x)x/1000, las=1)
  #addlines(fit.current, 'R', col='red', trans=function(x)x/1000)
  lines(ICA.results$year, ICA.results$rec/1000, col='blue')

  # Residual plot 
  op<-par(mfrow=c(2,3), mar=c(4,4,1,2), mgp=c(2,1,0))
  fleet.names<-c('Total catches', 'S1', 'S2', 'S3', 'SSB')
  for(f in 1:5){
    base<-fit.base$res[fit.base$res[,2]==f,]
    cur<-fit.current$res[fit.current$res[,2]==f,]
    scale<-3
    bp(base[,1],base[,3],base[,6], ylim=c(-0.5,9.5), xlim=range(fit.base$years), 
       las=1, xlab='year', ylab='Age', main=fleet.names[f], scale=scale)
    points(cur[,1],cur[,3],cex=sqrt(abs(cur[,6]))*scale, col='red', pch=1)
  }

  par(op)

  F<-exp(fit.current$stateEst[,-c(1:10)])
  colnames(F)<-c(0:4,'5+')
  rownames(F)<-fit.current$years
  matplot(rownames(F),F,lty=c(1:5,1),col=c(1:4,6,5), type='l', xlab='Year', lwd=2)
  legend('topleft', col=c(1:4,6,5), lty=c(1:5,1), legend=colnames(F), bty='n', lwd=rep(2,6))

  #nF<-apply(F,2,function(x)x/mean(x))
  #matplot(rownames(nF),nF,lty=c(1:5,1),col=c(1:4,6,5), type='l', xlab='Year', lwd=2)
  #legend('topleft', col=c(1:4,6,5), lty=c(1:5,1), legend=colnames(nF), bty='n', lwd=rep(2,6))

  if(LO.OK){
    # SSB plot 
    baseplot(fit.current, 'ssb', trans=function(x)x/1000, ylab='SSB (tounsand tonnes)', 
             las=1, ci=FALSE, line=FALSE)
    addlines(fit.current, 'ssb', col='red', trans=function(x)x/1000, lwd=2)
    for(i in 1:length(LO)){
      addlines(LO[[i]], 'ssb', col=i+2, trans=function(x)x/1000)
    }
    legend('bottomleft', legend=c('All data', 'w.o. S1', 'w.o. S2', 'w.o. S3'), 
           lty=rep('solid',4), col=c('red',3,4,5), bty='n')

    # Fbar plot
    baseplot(fit.current, 'fbar', ylab=expression(bar(F)[2-4]), las=1, ci=FALSE, line=FALSE)
    addlines(fit.current, 'fbar', col='red', lwd=2)
    for(i in 1:length(LO)){
      addlines(LO[[i]], 'fbar', col=i+2)
    }
    legend('bottomleft', legend=c('All data', 'w.o. S1', 'w.o. S2', 'w.o. S3'), 
           lty=rep('solid',4), col=c('red',3,4,5), bty='n')
  }

  if(RETRO.OK){
    # SSB plot 
    baseplot(fit.current, 'ssb', trans=function(x)x/1000, ylab='SSB (tounsand tonnes)', 
             las=1, ci=FALSE, line=FALSE, drop=0)
    addlines(fit.current, 'ssb', col='red', trans=function(x)x/1000, lwd=2, drop=0)
    for(i in 1:length(RETRO)){
      addlines(RETRO[[i]], 'ssb', col=i+2, trans=function(x)x/1000, with.dot=TRUE, drop=0)
    }
  
    # Fbar plot
    baseplot(fit.current, 'fbar', ylab=expression(bar(F)[3-6]), las=1, ci=FALSE, line=FALSE, drop=0)
    addlines(fit.current, 'fbar', col='red', lwd=2,drop=0)
    for(i in 1:length(RETRO)){
      addlines(RETRO[[i]], 'fbar', col=i+2, with.dot=TRUE, drop=0)
    }
  }else{
    #dummyplot()
    #dummyplot()
  }
}

setwd('outputs')
file.remove(dir(pattern='png$'))
stamp<-gsub('-[[:digit:]]{4}$','',gsub(':','.',gsub(' ','-',gsub('^[[:alpha:]]{3} ','',date()))))
png(filename = paste(stamp,"_%03d.png", sep=''), width = 480, height = 480, 
    units = "px", pointsize = 10, bg = "white")
  plots()    
dev.off()

png(filename = paste("big_",stamp,"_%03d.png", sep=''), width = 1200, height = 1200, 
    units = "px", pointsize = 20, bg = "white")
  plots()    
dev.off()

pdf(onefile=TRUE, width = 8, height = 8)
  plots()    
dev.off()


# SSB and Fbar table 
file.remove(dir(pattern='html$'))

R<-fit.current$R[,c(1,3,4)]
tsb<-exp(fit.current$logtsb[,c(1,3,4)])
ssb<-exp(fit.current$logssb[,c(1,3,4)])
fbar<-exp(fit.current$logfbar[,c(1,3,4)])

tab1<-cbind(R,tsb,ssb,fbar)
#tab1[nrow(tab1),-c(7:9)]<-NA
colnames(tab1)<-c('Recruits', 'Low', 'High', 'TSB', 'Low', 'High', 'SSB', 'Low', 'High', 'F36', 'Low', 'High')
rownames(tab1)<-fit.current$years
xtab(tab1, caption='Table 1. Estimated recruitment, total stock biomass (TBS), spawning stock biomass (SSB), 
                    and average fishing mortality for ages 3 to 6 (F36).', cornername='Year', 
     file=paste(stamp,'_tab1.html',sep=''), dec=c(0,0,0,0,0,0,0,0,0,3,3,3))

idxn<-1:ncol(catch.no)
tab2<-exp(fit.current$stateEst[,idxn])
rownames(tab2)<-fit.current$years
lab<-colnames(catch.no)
lab[length(lab)]<-paste(lab[length(lab)],'+')
colnames(tab2)<-lab
xtab(tab2, caption='Table 2. Estimated stock numbers.', cornername='Year\\Age', 
     file=paste(stamp,'_tab2.html',sep=''), dec=rep(0,ncol(tab2)))

tab3<-exp(fit.current$stateEst[,-idxn])
rownames(tab3)<-fit.current$years[1:nrow(tab3)]
minAge<-min(as.numeric(colnames(catch.no)))
lab<-as.character(1:ncol(tab3)+minAge-1)
lab[length(lab)]<-paste(lab[length(lab)],'+')
colnames(tab3)<-lab
xtab(tab3, caption='Table 3. Estimated fishing mortalities.', cornername='Year\\Age', 
     file=paste(stamp,'_tab3.html',sep=''), dec=rep(3,ncol(tab3)))
setwd(oldwd)

                                      
