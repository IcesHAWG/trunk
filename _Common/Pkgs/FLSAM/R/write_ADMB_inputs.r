write.ADMB.dat<-function(stck,tun, file="",miss.val=-99999){
  # Function to write FLStock data in state-space assessment format
  #Setup meta data
  fleet.names <- c(catch="catch",sapply(tun,name))
  fleet.types <- factor(sapply(tun,type),levels=c("con","number","biomass"))
  fleet.types <- c(0,as.numeric(fleet.types))
  samp.times <- c(miss.val,sapply(NSH.tun,function(x) mean(x@range[c("startf","endf")])))
  samp.times <- ifelse(is.na(samp.times),miss.val,samp.times)
  yrs <- seq(NSH@range["minyear"],NSH@range["maxyear"])
  nyrs <- length(yrs)

  #Generate observation matrix
  obs.dat <- as.data.frame(lapply(NSH.tun,index))
  obs.dat <- rbind(obs.dat,as.data.frame(FLQuants(catch=NSH@catch.n)))
  obs.dat <- subset(obs.dat,obs.dat$year %in% yrs)
  obs.dat$fleet <- as.numeric(factor(obs.dat$qname,levels=fleet.names))
  obs.dat$age[which(fleet.types[obs.dat$fleet]==3)] <- median(as.numeric(obs.dat$age),na.rm=TRUE)   #Set ssb indices equal to median age
  obs.dat <- obs.dat[,c("year","fleet","age","data")]
  obs.dat <- obs.dat[order(obs.dat$year,obs.dat$fleet,obs.dat$age),]
  idx.start <-which(!duplicated(obs.dat$year))
  idx.end   <-c(idx.start[-1]-1,nrow(obs.dat))
  nobs  <- nrow(obs.dat)

  # Now write the file!
  cat("# Auto generated file\n", file=file)
  cat("# \n", file=file, append=TRUE)

  #Write meta data
  cat("# Number of fleets (res+con+sur)\n",length(fleet.names),"\n", file=file, append=TRUE)
  cat("# Fleet types (res=0, con=1, sur=2, ssb=3)\n",fleet.types,"\n", file=file, append=TRUE)
  cat("# Sample times (only relevent for sur)\n",samp.times,"\n", file=file, append=TRUE)
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
                  cat("#",desc,"\n#",rownames(flq),"\n", file=file, append=TRUE)
                  out.dat <- t(flq[,,drop=TRUE]@.Data)
                  tbl <- capture.output(write.table(out.dat, row.names=FALSE, col.names=FALSE, quote=FALSE))
                  tbl <- paste(tbl,"#",rownames(out.dat),"\n")
                  cat(tbl, file=file, append=TRUE)
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

