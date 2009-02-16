library(FLCore)
library(FLAssess)
library(FLash)
library(FLBRP)
library(FLXSA)
library(FLICA)

my.dir  <-"C:/Documents and Settings/beggss/My Documents/R/FLICAVIIaN/VIIaNData2006/"

Pop4                      <- read.FLStock(paste(my.dir,"index.txt",sep=""))
Pop4.tun                  <- read.FLIndices(paste(my.dir,"fleet.txt",sep=""),paste(my.dir,"ssb.txt",sep=""),type="ICA")
Pop4@range ["plusgroup"]  <- Pop4@range ["max"]
Pop4@range[c("minfbar","maxfbar")] <- c(2,6)
Pop4@catch.n              <- Pop4@landings.n
Pop4@catch.wt             <- Pop4@landings.wt
Pop4@catch                <- computeCatch(Pop4)
Pop4@discards.wt          <- Pop4@landings.wt
Pop4@discards.n[]         <- 0
Pop4@discards             <- computeDiscards(Pop4)

Pop4.tun <- rev(Pop4.tun) #Put MLAI first
Pop4.tun[[2]]@index.var         <- 1.0/FLQuant (0.125, dimnames = dimnames (Pop4.tun[[2]]@index))
Pop4.tun[[2]]@index.var[ac(1),] <- Pop4.tun[[2]]@index.var[ac(1),]*10
Pop4.tun[[2]]@type              <- "number"
Pop4.tun[[1]]@index.var         <- 1.0/FLQuant (1, dimnames = dimnames (Pop4.tun[[1]]@index))

units(Pop4)[1:17]               <-as.list(c(rep(c("Tonnes","Thousands","Kg"),4),"NA","NA","f","NA","NA"))

Pop4.ctrl<-FLICA.control(sep.nyr=6,sep.age=4,sep.sel=1.0,sr=FALSE,
                                lambda.yr=c(1,1,1,1,1,1),
                                lambda.age =c(0.1, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0),
                                lambda.sr=0.01, index.model=c("l","l"), index.cor=F)

Pop4.ica    <- FLICA(Pop4, Pop4.tun, Pop4.ctrl)
# complete stock
Pop4        <- Pop4 + Pop4.ica
Pop4@stock  <- computeStock(Pop4)
