  source(file.path(".","FLSAM","R","FLSAM.class.r"))
  run.filestem   <- file.path(".","run","ssass")
  miss.val  <- -99999

  #------------------------------------------------
  #Preliminaries
  #------------------------------------------------
  #Get survey (tun) meta data
  idx.types   <- factor(sapply(tun,type),levels=c("con","number","biomass"))
  idx.types   <- as.numeric(idx.types)
  idx.times   <- sapply(tun,function(x) mean(x@range[c("startf","endf")]))
  idx.times   <- ifelse(is.na(samp.times),miss.val,samp.times)

  #Get stock metadata
  yrs         <- seq(dims(stck)$minyear,dims(stck)$maxyear)
  nyrs        <- dims(stck)$year

  #Generate observation matrix
  fleets        <- do.call(FLQuants,c(list(catch=stck@catch.n),lapply(tun,index)))
  fleet.names   <- names(fleets)
  fleets        <- lapply(fleets,function(x) { x[x<=0] <- NA; return(x)})
  obs.dat       <- as.data.frame(fleets)
  obs.dat       <- rbind(obs.dat,as.data.frame(FLQuants(catch=stck@catch.n)))
  obs.dat       <- subset(obs.dat,obs.dat$year %in% yrs)
  obs.dat$fleet <- as.numeric(factor(obs.dat$qname,levels=fleet.names))
  obs.dat$age[which(fleet.types[obs.dat$fleet]==3)] <- median(as.numeric(obs.dat$age),na.rm=TRUE)   #Set ssb indices equal to median age
  obs.dat       <- obs.dat[,c("year","fleet","age","data")]
  obs.dat       <- obs.dat[order(obs.dat$year,obs.dat$fleet,obs.dat$age),]
  obs.dat$fleet.name <- paste("#",fleet.names[obs.dat$fleet],sep="")
  obs.dat       <- subset(obs.dat,!(obs.dat$data<=0 | is.na(obs.dat$data)))
  idx.start     <-which(!duplicated(obs.dat$year))
  idx.end       <-c(idx.start[-1]-1,nrow(obs.dat))
  nobs          <- nrow(obs.dat)

  #------------------------------------------------
  #Write outputs
  #------------------------------------------------

  #------------------------------------------------
  #Run model
  #------------------------------------------------

  #------------------------------------------------
  #Load results using the default SAM code (more or less)
  #------------------------------------------------
  ret       <- list()
  #Read parameter file
  parfile     <-  as.numeric(scan(paste(run.filestem ,".par", sep=""),what="", n=16, quiet=TRUE)[c(6,11,16)])
  ret$nopar   <-as.integer(parfile[1])
  ret$nlogl   <-parfile[2]
  ret$maxgrad <-parfile[3]
  
  #Read report file
  rept          <-  scan(paste(run.filestem ,".rep", sep=""),what=integer(), quiet=TRUE)
  ret$stateDim  <-  rept[1]
  ret$years     <-  rept[-1]

  #Read residual files
  ret$res   <-  read.table(paste(run.filestem ,".res", sep=""),
                    header=FALSE,col.names=c("year","fleet","age","log.obs","log.mdl","std.res"))

  #Read correlation matrix
  lin             <-  readLines(paste(run.filestem,".cor", sep=""))
  ret$npar        <-  as.integer(length(lin)-2)
  ret$logDetHess  <-  as.numeric(strsplit(lin[1], '=')[[1]][2])

  sublin<-lapply(strsplit(lin[1:ret$npar+2], ' '),function(x)x[x!=''])
  ret$names<-unlist(lapply(sublin,function(x)x[2]))
  ret$est<-as.numeric(unlist(lapply(sublin,function(x)x[3])))
  ret$std<-as.numeric(unlist(lapply(sublin,function(x)x[4])))

  ret$cor<-matrix(NA, ret$npar, ret$npar)
  for(i in 1:ret$npar){
    ret$cor[1:i,i]<-as.numeric(unlist(lapply(sublin[i],
      function(x)x[5:(4+i)])))
    ret$cor[i,1:i]<-ret$cor[1:i,i]
  }
  ret$cov<-ret$cor*(ret$std%o%ret$std)
  mslh<-function(name){
    idx<-which(ret$names==name)
    x<-cbind(ret$est[idx], ret$std[idx], ret$est[idx]-2*ret$std[idx],
             ret$est[idx]+2*ret$std[idx])
    colnames(x)<-c('est', 'std', 'low', 'hig')
    if(nrow(x)==length(ret$years)) rownames(x) <- ret$year
    return(x)
  }
  ret$ssb<-mslh('ssb')
  ret$fbar<-mslh('fbar')
  ret$tsb<-mslh('tsb')
  ret$logssb<-mslh('logssb')
  ret$logfbar<-mslh('logfbar')
  ret$logtsb<-mslh('logtsb')
  ret$logscale<-mslh('logScale')
  ret$logFpar<-mslh('logFpar')
  ret$logCatch<-mslh('logCatch')
  x<-mslh('U')
  ret$stateEst<-matrix(x[,1],ncol=ret$stateDim, byrow=TRUE,dimnames=list(year=ret$years,state=NULL))
  ret$stateStd<-matrix(x[,2],ncol=ret$stateDim, byrow=TRUE,dimnames=list(year=ret$years,state=NULL))
  ret$stateLow<-matrix(x[,3],ncol=ret$stateDim, byrow=TRUE,dimnames=list(year=ret$years,state=NULL))
  ret$stateHig<-matrix(x[,4],ncol=ret$stateDim, byrow=TRUE,dimnames=list(year=ret$years,state=NULL))
  ret$R<-cbind(est=exp(ret$stateEst[,1]), std=NA, low=exp(ret$stateLow[,1]),high= exp(ret$stateHig[,1]))
  rownames(ret$R) <- ret$years

  #------------------------------------------------
  #Export results to FLSAM object
  #------------------------------------------------
  #Now export to FLSAM object
  ass <- new("FLSAM")
  ass@nopar         <- ret$nopar
  ass@nlogl         <- ret$nlogl
  ass@maxgrad       <- ret$maxgrad

  #Modelled and observed fleets
  ass@res           <- ret$res
  ass@index         <- fleets     #Copy in observations to start with
  ass@index.hat     <- fleets
  ass@index.res     <- fleets
  for(f in seq(fleet.names)) {    #Now overwrite
      fleet.res <- subset(ret$res,ret$res$fleet==f)
      ass@index.res[[f]][cbind(ac(fleet.res$age),ac(fleet.res$year))]@.Data <- fleet.res$std.res
  }
