  source(file.path("..","Pkgs","FLSAM","R","FLSAM.class.r"))
  filestem <- file.path(".","baserun","ssass")
  # Function to read a basic fit
  
  ass       <-  new("FLSAM")
  ret       <- list()
  #Read parameter file
  parfile     <-  as.numeric(scan(paste(filestem,".par", sep=""),what="", n=16, quiet=TRUE)[c(6,11,16)])
  ass@nopar   <-as.integer(parfile[1])
  ass@nlogl   <-parfile[2]
  ass@maxgrad <-parfile[3]
  
  #Read report file
  rept          <-  scan(paste(filestem,".rep", sep=""),what=integer(), quiet=TRUE)
  ret$stateDim  <-  rept[1]
  ret$years     <-  rept[-1]

  #Read residual files
  ass@res   <-  read.table(paste(filestem,".res", sep=""),
                    header=FALSE,col.names=c("year","fleet","age","log.obs","log.mdl","std.res"))

  #Read correlation matrix
  lin             <-  readLines(paste(filestem,".cor", sep=""))
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
  ret$stateEst<-matrix(x[,1],ncol=ret$stateDim, byrow=TRUE)
  ret$stateStd<-matrix(x[,2],ncol=ret$stateDim, byrow=TRUE)
  ret$stateLow<-matrix(x[,3],ncol=ret$stateDim, byrow=TRUE)
  ret$stateHig<-matrix(x[,4],ncol=ret$stateDim, byrow=TRUE)
  ret$R<-cbind(exp(ret$stateEst[,1]), NA, exp(ret$stateLow[,1]),
               exp(ret$stateHig[,1]))
