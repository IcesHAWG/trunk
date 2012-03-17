# Code to construct the model.cfg file

install.packages("FLSAM",repos="http://flr-project.org/Rdevel")
library(FLCore)

my.path<- file.path("C:","X D Drive", "bea WG","herring","2012","IrishSH")
#setwd(my.path,"FLR run")

stck <- readFLStock(file.path(my.path,"data","index.txt"),no.discards=TRUE)
stck <- setPlusGroup(stck,8)
range(stck)[c("minfbar","maxfbar")] <- c(4,6)
units(stck)[1:17]    <- as.list(c(rep(c("tonnes","thousands","kg"),4),                                 rep("NA",2),"f",rep("NA",2))) 
tun <- readFLIndices(file.path(my.path,"data","fleet.txt"))
tun <- readFLIndices(file.path(my.path,"data","fleet.txt"),file.path(my.path,"data","ssb.txt"),type="ICA")

names(tun)[1] <- "FLT01"
names(tun)[2] <- "NINEL"
tun[["FLT01"]]@type <- "number"
summary(stck)
summary(tun)

library(FLSAM)

ctrl <- FLSAM.control(stck,tun)

ctrl@states['catch',]<-c(1:6,7,7)  # catch F by age, or group of ages

# survey catachabilities
ctrl@catchabilities[2,]<-c(1:3,rep(4,5)) #Fleet 1


#Survey Power law application
# ctrl@power.law.exps

# fishing mortality Random walk VARIANCES (ctrl@f.vars)
ctrl@f.vars['catch',]<-c(rep(1,8))   #same variance agross ages

# log N RW VARIANCES (ctrl@logN.vars)
ctrl@logN.vars<-c(1,rep(2,7))

# Coupling of OBSERVATION VARIANCES (ctrl@obs.vars)
ctrl@obs.vars['catch',]<-c(1,2,3,3,4,4,4,4)
ctrl@obs.vars[2,]<-      c(5,6,6,7,7,8,8,8)

# # Stock recruitment model code (0=RW, 1=Ricker, 2=BH, ... more in time
ctrl@srr<-as.integer(0)   # 0


sam <- FLSAM(stck,tun,ctrl)

# useful commmands

getwd()

FLR2SAM(stck,tun,ctrl,run.dir=".")  #makes the wd the rund directory

packageDescription("FLSAM")

save(stck,ctrl,tun,file="IrishSH.RData") # as I changed the wd then it saves the files to that directory

 