read.ADMB.outputs <- function(run.filestem=file.path(".","run","ssass"),stck,ctrl) {
  #Create return object
  ret       <- new("FLSAM")
  
  #Read parameter file
  parfile     <-  as.numeric(scan(sprintf("%s.par",run.filestem),what="", n=16, quiet=TRUE)[c(6,11,16)])
  ret@nopar   <-as.integer(parfile[1])
  ret@nlogl   <-parfile[2]
  ret@maxgrad <-parfile[3]
  
  #Read report file
  rept          <-  scan(paste(run.filestem ,".rep", sep=""),what=integer(), quiet=TRUE)
  stateDim  <-  rept[1]
  yrs     <-  rept[-1]

  #Read residual files
  ret@residuals  <-  read.table(paste(run.filestem ,".res", sep=""),
                    header=FALSE,col.names=c("year","fleet","age","log.obs","log.mdl","std.res"))

  #Read (parts of) the correlation matrix - first the determinant of the hessian
  lin             <-  readLines(paste(run.filestem,".cor", sep=""),n=1)
  ret@logDetHess  <-  as.numeric(gsub("^.*=(.+)$","\\1",lin[1]))
  
  #Then the correlation matrix
  npar           <-  as.integer(length(lin)-2)
  cor.dat        <- read.table(sprintf("%s.cor",run.filestem),skip=1,header=TRUE,fill=TRUE)
  cor.df         <- cor.dat[,c(1:4)]
  cor.mat        <- as.matrix(cor.dat[,-c(1:4)])

  #Am not sure what is going on here. Will leave this out until we get clarification
  #Problem is is that cor.mat is not square
#  ret@cor<-matrix(NA, ret@npar, ret@npar)
#  for(i in 1:ret@npar){
#    ret@cor[1:i,i]<-as.numeric(unlist(lapply(sublin[i],
#      function(x)x[5:(4+i)])))
#    ret@cor[i,1:i]<-ret@cor[1:i,i]
#  }
#  ret@cov<-ret@cor*(ret@std%o%ret@std)

  #Now extract parameter estimates together with uncertainties
  #These should really be put into FLQuants or similar
  mslh<-function(variable){
    sub.df <- subset(cor.df,name==variable,select=c("value","std"))
    sub.df$low.bnd <- sub.df$value - 1.96 * sub.df$std
    sub.df$up.bnd <- sub.df$value + 1.96 * sub.df$std
    if(nrow(sub.df)==length(yrs)) rownames(sub.df) <- yrs
    return(sub.df)
  }
  ret@ssb<-mslh('ssb')
  ret@fbar<-mslh('fbar')
  ret@tsb<-mslh('tsb')
  ret@logssb<-mslh('logssb')
  ret@logfbar<-mslh('logfbar')
  ret@logtsb<-mslh('logtsb')
  ret@logscale<-mslh('logScale')
  ret@logFpar<-mslh('logFpar')
  ret@logCatch<-mslh('logCatch')

  #Extract the state variables
  x<-mslh('U')
  stateEst<-matrix(x$value,ncol=stateDim, byrow=TRUE,dimnames=list(year=yrs,state=NULL))
  stateStd<-matrix(x$std,ncol=stateDim, byrow=TRUE,dimnames=list(year=yrs,state=NULL))
  stateLow<-matrix(x$low.bnd,ncol=stateDim, byrow=TRUE,dimnames=list(year=yrs,state=NULL))
  stateHigh<-matrix(x$up.bnd,ncol=stateDim, byrow=TRUE,dimnames=list(year=yrs,state=NULL))

  #And copy into the appropriate slots as data.frames or FLQuants
  n.stateEst <- stateEst[,1:dims(stck)$age]
  f.stateEst <- exp(t(stateEst[,-c(1:dims(stck)$age)]))
  ret@stock.n <- stck@stock.n
  ret@stock.n@.Data[,,,,,] <- exp(t(stateEst[,1:dims(stck)$age]))
  ret@harvest <- stck@harvest
  for(a in  dimnames(ctrl@states)$age){
    ret@harvest@.Data[a,,,,,] <- f.stateEst[ctrl@states["catch",a],]
  }
  ret@recruitment<-data.frame(value=exp(stateEst[,1]), std=NA, low.bnd=exp(stateLow[,1]),up.bnd= exp(stateHigh[,1]))
  rownames(ret@recruitment) <- yrs

  #Finished!
  return(ret)
}
