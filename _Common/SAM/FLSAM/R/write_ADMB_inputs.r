write.ADMB.dat<-function(stck,tun, file="",miss.val=-99999){
  # Function to write FLStock data in state-space assessment format
  #Setup meta data
  fleet.names <- c(catch="catch",sapply(tun,name))
  fleet.types <- factor(sapply(tun,type),levels=c("con","number","biomass"))
  fleet.types <- c(0,as.numeric(fleet.types))
  names(fleet.types) <- fleet.names
  samp.times <- c(miss.val,sapply(tun,function(x) mean(x@range[c("startf","endf")])))
  samp.times <- ifelse(is.na(samp.times),miss.val,samp.times)
  names(samp.times) <- fleet.names
  yrs <- seq(stck@range["minyear"],stck@range["maxyear"])
  nyrs <- length(yrs)

  #Generate observation matrix
  obs.dat <- as.data.frame(lapply(tun,index))
  obs.dat <- rbind(obs.dat,as.data.frame(FLQuants(catch=stck@catch.n)))
  obs.dat <- subset(obs.dat,obs.dat$year %in% yrs)
  obs.dat$fleet <- as.numeric(factor(obs.dat$qname,levels=fleet.names))
  obs.dat$age[which(fleet.types[obs.dat$fleet]==3)] <- median(as.numeric(obs.dat$age),na.rm=TRUE)   #Set ssb indices equal to median age
  obs.dat <- obs.dat[,c("year","fleet","age","data")]
  obs.dat <- obs.dat[order(obs.dat$year,obs.dat$fleet,obs.dat$age),]
  obs.dat$fleet.name <- paste("#",fleet.names[obs.dat$fleet],sep="")
  obs.dat <- subset(obs.dat,!(obs.dat$data<=0 | is.na(obs.dat$data)))
  idx.start <-which(!duplicated(obs.dat$year))
  idx.end   <-c(idx.start[-1]-1,nrow(obs.dat))
  nobs  <- nrow(obs.dat)

  # Now write the file!
  cat("# Auto generated file\n", file=file)
  cat(sprintf("# Datetime : %s\n\n",Sys.time()),file=file,append=TRUE)

  #Write meta data
  cat("# Number of fleets (res+con+sur)\n",length(fleet.names),"\n", file=file, append=TRUE)
  cat("# Fleet types (res=0, con=1, sur=2, ssb=3)\n",.format.matrix.ADMB(t(fleet.types)),file=file, append=TRUE)
  cat("# Sample times (only relevent for sur)\n",.format.matrix.ADMB(t(samp.times)), file=file, append=TRUE)
  cat("# Number of years\n",nyrs,"\n", file=file, append=TRUE)
  cat("# Years\n",yrs,"\n", file=file, append=TRUE)
  cat("# Number of observations \n",nobs,"\n", file=file, append=TRUE)
  cat("# Index1 (index of first obs in each year) \n",idx.start,"\n", file=file, append=TRUE)
  cat("# Index2 (index of last obs in each year) \n",idx.end,"\n", file=file, append=TRUE)

  #Observations
  cat("# The observation matrix \n#",colnames(obs.dat),"\n", file=file, append=TRUE)
  write.table(obs.dat, row.names=FALSE, col.names=FALSE, quote=FALSE, file=file, append=TRUE)

  #Now write the rest of the stock information
  flqout <- function(desc,flq) { #Local export function
                  cat("#",desc,"\n",file=file, append=TRUE)
                  cat(.format.matrix.ADMB(t(flq[,,drop=TRUE]@.Data)), file=file, append=TRUE)
                  return(invisible(NULL))}
  flqout("Proportion mature",stck@mat)
  flqout("Stock mean weights",stck@stock.wt)
  flqout("Catch mean weights",stck@catch.wt)
  flqout("Natural Mortality", stck@m)
  flqout("Landing Fraction L/(L+D)",stck@landings.n*0+1)
  flqout("Fprop",stck@harvest.spwn)
  flqout("Mprop",stck@m.spwn)
  
  #Finally, write the checksums
  cat("# Checksums to ensure correct reading of input data \n",42,42,"\n", file=file, append=TRUE)
  
  return(invisible(NULL))
}

write.ADMB.cfg <- function(ctrl,file="") {
  #Write configuration file
  # Now write the file!
  cat("# Auto generated file\n", file=file)
  cat(sprintf("# Datetime : %s\n\n",Sys.time()),file=file,append=TRUE)

  #Write the headers
  cat("# Min, max age represented internally in model \n",ctrl@range[c("min","max")],"\n", file=file, append=TRUE)
  cat("# Max age considered a plus group? (0 = No, 1= Yes)\n",as.numeric(ctrl@plus.group[1]),"\n", file=file, append=TRUE)
  
  #Coupling Matrices
  cat("\n# Coupling of fishing mortality STATES (ctrl@states)\n",.format.matrix.ADMB(ctrl@states,na.replace=0),file=file,append=TRUE)
  cat("\n# Coupling of catchability PARAMETERS (ctrl@catchabilities)\n",.format.matrix.ADMB(ctrl@catchabilities,na.replace=0),file=file,append=TRUE)
  cat("\n# Coupling of power law model EXPONENTS (ctrl@power.law.exps)\n",.format.matrix.ADMB(ctrl@power.law.exps,na.replace=0),file=file,append=TRUE)
  cat("\n# Coupling of fishing mortality RW VARIANCES (ctrl@f.vars)\n",.format.matrix.ADMB(ctrl@f.vars,na.replace=0),file=file,append=TRUE)
  cat("\n# Coupling of log N RW VARIANCES (ctrl@logN.vars)\n",ctrl@logN.vars,file=file,append=TRUE)
  cat("\n\n# Coupling of OBSERVATION VARIANCES (ctrl@obs.vars)\n",.format.matrix.ADMB(ctrl@obs.vars,na.replace=0),file=file,append=TRUE)
  
  #Final values
  cat("\n# Stock recruitment model code (0=RW, 1=Ricker, 2=BH, ... more in time\n",ctrl@srr,"\n",file=file,append=TRUE)
  cat("# Years in which catch data are to be scaled by an estimated parameter (mainly cod related)\n",0,"\n",file=file,append=TRUE)
  cat("# Fbar range \n",ctrl@range[c("minfbar","maxfbar")],"\n",file=file,append=TRUE)
  
  #Finally, write the checksums
  cat("\n\n# Checksums to ensure correct reading of input data \n",123456,123456,"\n", file=file, append=TRUE)

}

.format.matrix.ADMB <- function(mat,na.replace="missing") {
                  if(na.replace!="missing") {mat[is.na(mat)] <- na.replace}
                  colnames(mat)[1] <- paste("#",colnames(mat)[1])
                  tbl <- capture.output(write.table(mat, row.names=FALSE, col.names=TRUE, quote=FALSE))
                  tbl <- paste(tbl,"#",c(" ",rownames(mat)),"\n")
                  return(tbl)
}

