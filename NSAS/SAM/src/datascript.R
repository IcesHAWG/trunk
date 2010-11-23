# Script to reshape data to match the state-space assessment model 
# 
# Anders Nielsen <anders@nielsensweb.org> Oct. 2009

read.ices<-function(filen){
  # Function to read ices data files 
  head<-scan(filen, skip=1, n=7, quiet=TRUE)
  minY<-head[3]
  maxY<-head[4]
  minA<-head[5]
  maxA<-head[6]
  C<-as.matrix(read.table(filen, skip=5, header=F))
  if(length(as.vector(C))==1){
    C<-matrix(C,ncol=length(minA:maxA),nrow=length(minY:maxY))
  }
  C<-C[,1:(maxA-minA+1)]
  rownames(C)<-minY:maxY
  colnames(C)<-minA:maxA
  return(C)
}

read.surveys<-function(filen){
  # Function to read ices survey file 
  lin<-readLines(filen)[-c(1:2)]
  lin<-gsub("^[[:space:]]+",'',lin)
  empty<-which(lapply(lapply(strsplit(lin, split='[[:space:]]+'), 
               paste, collapse=''), nchar)==0)
  if(length(empty)>0){
    lin<-lin[-empty]
  }
  idx1<-grep('^[A-Z]', lin, ignore.case=TRUE)
  idx2<-c(idx1[-1]-1,length(lin))
  names<-lin[idx1]
  tc<-textConnection(lin[idx1+1])
  years<-as.matrix(read.table(tc, head=FALSE))
  close(tc)
  tc<-textConnection(lin[idx1+2])
  times<-as.matrix(read.table(tc, head=FALSE))[,3:4]
  close(tc)
  tc<-textConnection(lin[idx1+3])
  ages<-as.matrix(read.table(tc, head=FALSE))
  close(tc)
  onemat<-function(i){
    ret<-matrix(as.numeric(
      unlist((strsplit(lin[(idx1[i]+4):idx2[i]],'[[:space:]]+')))),
      nrow=idx2[i]-idx1[i]-3, byrow=TRUE)[,2:(2+ages[i,2]-ages[i,1]),drop=FALSE]
    rownames(ret)<-years[i,1]:years[i,2]
    colnames(ret)<-ages[i,1]:ages[i,2]
    attr(ret,'time')<-times[i,]
    ret[ret<0]<-NA
    ret
  }
  obs<-lapply(1:length(names),onemat)  
  names(obs)<-names
  obs
}

read.ssb.survey<-function(filen){
  # Function to read ices survey file 
  dat<-read.table(filen,skip=3, head=FALSE)[,-2]
  ret<-dat[,2,drop=FALSE]
  rownames(ret) <- dat[,1]
  colnames(ret) <- 2
  attr(ret,'time')<-0
  ret[ret<0]<-NA
  obs<-list(ssb=ret)  
  names(obs)<-"SSB"
  obs
}




write.stored.records<-function(dat, file=""){
  # Function to write stored records in state-space assessment format 
  cat("# Auto generated file\n", file=file)
  cat("# \n", file=file, append=TRUE)
  cat("# Number of fleets (res+con+sur)\n",length(attr(dat,'type')),"\n", file=file, append=TRUE)
  cat("# Fleet types (res=0, con=1, sur=2, ssb=3)\n",attr(dat,'type'),"\n", file=file, append=TRUE)
  cat("# Sample times (only relevent for sur)\n",attr(dat,'time'),"\n", file=file, append=TRUE)
  cat("# Number of years\n",attr(dat,'nyear'),"\n", file=file, append=TRUE)
  cat("# Years\n",attr(dat,'year'),"\n", file=file, append=TRUE)
  cat("# Number of observations \n",nrow(dat),"\n", file=file, append=TRUE)
  cat("# Index1 (index of first obs in each year) \n",attr(dat,'idx1'),"\n", file=file, append=TRUE)
  cat("# Index2 (index of last obs in each year) \n",attr(dat,'idx2'),"\n", file=file, append=TRUE)
  cat("# The observation matrix \n", file=file, append=TRUE)
  write.table(dat, row.names=FALSE, col.names=FALSE, quote=FALSE, file=file, append=TRUE)
  cat("# Proportion mature\n", file=file, append=TRUE)
  write.table(attr(dat,'prop.mature'), row.names=FALSE, col.names=FALSE, quote=FALSE, file=file, append=TRUE)
  cat("# Stock mean weights\n", file=file, append=TRUE)
  write.table(attr(dat,'stock.mean.weight'), row.names=FALSE, col.names=FALSE, quote=FALSE, file=file, append=TRUE)
  cat("# Catch mean weights\n", file=file, append=TRUE)
  write.table(attr(dat,'catch.mean.weight'), row.names=FALSE, col.names=FALSE, quote=FALSE, file=file, append=TRUE)
  cat("# Natural Mortality\n", file=file, append=TRUE)
  write.table(attr(dat,'natural.mortality'), row.names=FALSE, col.names=FALSE, quote=FALSE, file=file, append=TRUE)
  cat("# Landing Fraction L/(L+D)\n", file=file, append=TRUE)
  write.table(attr(dat,'land.frac'), row.names=FALSE, col.names=FALSE, quote=FALSE, file=file, append=TRUE)
  cat("# Fprop\n", file=file, append=TRUE)
  write.table(attr(dat,'fprop'), row.names=FALSE, col.names=FALSE, quote=FALSE, file=file, append=TRUE)
  cat("# Mprop\n", file=file, append=TRUE)
  write.table(attr(dat,'mprop'), row.names=FALSE, col.names=FALSE, quote=FALSE, file=file, append=TRUE)
}

write.records<-function(fleets=NULL, surveys=NULL, residual.fleet=NULL, ssb.surveys=NULL,
                        prop.mature=NULL, stock.mean.weight=NULL, catch.mean.weight=NULL, 
                        natural.mortality=NULL, land.frac=NULL, fprop=NULL, mprop=NULL, file="", drop.years=0){
  # Function to write records in state-space assessment format and create 
  # collected data object for future use 

  if(drop.years>0){
    dy<-function(x,n=drop.years){
      atx<-attributes(x)
      xcut<-x[1:(nrow(x)-n),]
      attributes(xcut)<-c(attributes(xcut),atx[-c(1,2)])
      return(xcut)
    }
    fleets<-lapply(fleets,dy)
    surveys<-lapply(surveys,dy)   
    residual.fleet<-dy(residual.fleet)   
    prop.mature<-dy(prop.mature)
    stock.mean.weight<-dy(stock.mean.weight)
    catch.mean.weight<-dy(catch.mean.weight)       
    natural.mortality<-dy(natural.mortality)       
    land.frac<-dy(land.frac)       
  }
  fleet.idx<-0
  type<-NULL
  time<-NULL
  dat<-data.frame(year=NA,fleet=NA,age=NA,obs=NA)
  doone<-function(m){
    year<-rownames(m)[row(m)]
    fleet.idx<<-fleet.idx+1
    fleet<-rep(fleet.idx,length(year))
    age<-colnames(m)[col(m)]
    obs<-as.vector(as.matrix(m))
    dat<<-rbind(dat,data.frame(year,fleet,age,obs))
  }
  if(!is.null(residual.fleet)){
    doone(residual.fleet)
    type<-c(type,0)
    time<-c(time,0)
  }
  if(!is.null(fleets)){
    if(is.data.frame(fleets)|is.matrix(fleets)){
      doone(fleets)
      type<-c(type,1)
      time<-c(time,0)
    }else{
      dummy<-lapply(fleets,doone)
      type<-c(type,rep(1,length(fleets)))
      time<-c(time,rep(0,length(fleets)))
    }
  }
  if(!is.null(surveys)){
    if(is.data.frame(surveys)|is.matrix(surveys)){
      doone(surveys)
      type<-c(type,2)
      time<-c(time,mean(attr(surveys,'time')))
    }else{
      dummy<-lapply(surveys,doone)
      type<-c(type,rep(2,length(surveys)))
      time<-c(time,unlist(lapply(surveys, function(x)mean(attr(x,'time')))))
    }
  }
  if(!is.null(ssb.surveys)){
    if(is.data.frame(ssb.surveys)|is.matrix(ssb.surveys)){
      doone(ssb.surveys)
      type<-c(type,3)
      time<-c(time,mean(attr(ssb.surveys,'time')))
    }else{
      dummy<-lapply(ssb.surveys,doone)
      type<-c(type,rep(3,length(ssb.surveys)))
      time<-c(time,unlist(lapply(ssb.surveys, function(x)mean(attr(x,'time')))))
    }
  }
  if(is.null(land.frac)){
    land.frac<-matrix(1,nrow=nrow(residual.fleet), ncol=nrow(residual.fleet))
  }
  if(is.null(fprop)){
    fprop<-matrix(0,nrow=nrow(residual.fleet), ncol=nrow(residual.fleet))
  }
  if(is.null(mprop)){
    mprop<-matrix(0,nrow=nrow(residual.fleet), ncol=nrow(residual.fleet))
  }
  dat<-dat[complete.cases(dat),]
  dat<-dat[dat$obs>0,]
  o<-order(dat$year,dat$fleet,dat$age)
  attr(dat,'type')<-type
  names(time)<-NULL
  attr(dat,'time')<-time
  dat<-dat[o,]
  idx1<-which(!duplicated(dat$year))
  idx2<-c(idx1[-1]-1,nrow(dat))
  attr(dat,'idx1')<-idx1
  attr(dat,'idx2')<-idx2
  attr(dat,'year')<-unique(dat$year)
  attr(dat,'nyear')<-length(unique(dat$year))
  attr(dat,'prop.mature')<-prop.mature
  attr(dat,'stock.mean.weight')<-stock.mean.weight
  attr(dat,'catch.mean.weight')<-catch.mean.weight
  attr(dat,'natural.mortality')<-natural.mortality
  attr(dat,'land.frac')<-land.frac
  attr(dat,'mprop')<-mprop
  attr(dat,'fprop')<-fprop
  write.stored.records(dat,file)
  return(dat)
}

################################################################################
#############  Below this line are the specifics for this stock  ###############
################################################################################
add2010<-function(mat){
  mat<-rbind(mat,'2010'=mat[nrow(mat),])
  mat
}

# Read in data 
od<-getwd()
setwd('../../data')
catch.no<-read.ices('canum.txt')
catch.mean.weight<-add2010(read.ices('west.txt'))
stock.mean.weight<-add2010(read.ices('west.txt'))
prop.mature<-add2010(read.ices('matprop.txt'))
natural.mortality<-add2010(read.ices('natmor.txt'))
surveys<-read.surveys('fleet.txt')
ssb.surveys<-read.ssb.survey('ssb.txt')
fprop<-matrix(0,nrow=nrow(prop.mature), ncol=ncol(prop.mature))
mprop<-matrix(0,nrow=nrow(prop.mature), ncol=ncol(prop.mature))
land.frac<-matrix(1,nrow=nrow(prop.mature), ncol=ncol(prop.mature))
setwd(od)



# Finally write the file with data prepared for state-space assessment 
data<-write.records(surveys=surveys, 
                    residual.fleet=catch.no, 
		    ssb.surveys=ssb.surveys, 
                    prop.mature=prop.mature, 
                    stock.mean.weight=stock.mean.weight, 
                    catch.mean.weight=catch.mean.weight, 
                    natural.mortality=natural.mortality,
                    land.frac=land.frac, 
                    fprop=fprop, 
                    mprop=mprop, 
                    file='../run/ssass.dat', drop.years=0)


