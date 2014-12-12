catch <- as.data.frame(NSH@catch.n)[,c("age","year","data")]
surv  <- lapply(NSH.tun,function(x){as.data.frame(x@index)[,c("age","year","data")]})

dat <- cbind(catch[,"year"],1,catch[,c("age","data")]); colnames(dat) <- c("year","fleet","age","data")
surv3 <- cbind(surv[[1]][,"year"],3,surv[[1]][,c("age","data")]); colnames(surv3) <- c("year","fleet","age","data")
surv4 <- cbind(surv[[2]][,"year"],4,surv[[2]][,c("age","data")]); colnames(surv4) <- c("year","fleet","age","data")
surv5 <- cbind(surv[[3]][,"year"],5,surv[[3]][,c("age","data")]); colnames(surv5) <- c("year","fleet","age","data")
surv2 <- cbind(surv[[4]][,"year"],2,surv[[4]][,c("age","data")]); colnames(surv2) <- c("year","fleet","age","data")
surv2$age <- surv2$age-1

totdat <- rbind(dat,surv2,surv3,surv4,surv5)
totdat <- subset(totdat,!data %in% c(-1,0,1))
totdat <- subset(totdat,is.na(data)==F)

totdat <- orderBy(~year + fleet + age,data=totdat)
write(totdat[,1],file="obsLAI.txt",sep=" ,")
for(i in 2:4)
  write(totdat[,i],file="obsLAI.txt",sep=" ,",append=T)

idx1 <- which(!duplicated(totdat$year)) -1
idx2 <- idx1[2:length(idx1)] - 1
idx2 <- c(idx2,nrow(totdat)-1)
write(idx1,file="idx1",sep=" ,")
write(idx2,file="idx2",sep=" ,")
