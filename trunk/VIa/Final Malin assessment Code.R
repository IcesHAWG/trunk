######################################################################################################
# Malin FLSAM Assessment
#
# Using R 2.13.2 and FLCore 2.4
#
# Performs an assessment of Malin Shelf herring (VIa, VIIb) using the FLSAM package.
#
# January 2015
# Afra Egan

####################################################################################################

### ======================================================================================================
### Initialise system, including convenience functions and title display
### ======================================================================================================
rm(list=ls()); gc(); graphics.off(); start.time <- proc.time()[3]
options(stringsAsFactors=FALSE)



library(FLSAM)
library(FLCore)
library(FLEDA)

log.msg     <-  function(fmt,...) {
	cat(sprintf(fmt,...))
	flush.console()
}

log.msg("\nMalin shelf Herring FLSAM Assessment\n===================================\n")

### ======================================================================================================
### Define parameters for use in the assessment code here
### ======================================================================================================
path<-"C:/Users/Lusseaus/Documents/ICES Working Groups/HAWG/2015/Repository 2015/VIa/"
try(setwd(path),silent=TRUE)

data.source         <-  file.path(".","data")      #Data source, not code or package source!!!
output.dir          <-  file.path(".","results")       #Output directory
output.base         <-  file.path(output.dir) #Output base filename, including directory. Other output filenames are built by appending onto this one
source(file.path("oto.plot.r"))
### ======================================================================================================
### Prepare stock object for assessment
### ======================================================================================================
log.msg("PREPARING STOCK OBJECT...\n")
MSH<- readFLStock(file.path(data.source, "index.txt"),no.discards=TRUE)

#Set no discards
MSH@catch.n                <- MSH@landings.n
MSH@catch                  <- MSH@landings
MSH@catch.wt               <- MSH@landings.wt
MSH@stock.wt[1,ac(2013)]   <- yearMeans(MSH@stock.wt[1,ac(2010:2012)])
units(MSH)[1:17]           <- as.list(c(rep(c("tonnes","thousands","kg"),4), rep("NA",5)))

#Set fbar
range(MSH)[c("minfbar","maxfbar")] <- c(3,6)

#Set plus group
MSH <- setPlusGroup(MSH,MSH@range["max"])

#Set stock object name - this is propagated through into the figure titles
MSH@name    <- "Malin Shelf Herring"

### ======================================================================================================
### Prepare index object for assessment
### ======================================================================================================
log.msg("PREPARING INDEX OBJECT...\n")
#Load and modify all index data
MSH.tun   <- readFLIndices(file.path(data.source, "fleet.txt"))

## Set the Index Type
MSH.tun[[1]]@type               <- "number"
MSH.tun[[2]]@type               <- "number"
MSH.tun[[3]]@type               <- "number"
MSH.tun[[4]]@type               <- "number"

## Call the index whatever
MSH.tun[[1]]@name               <- "MS HERAS"
MSH.tun[[2]]@name               <- "WoS HERAS"
MSH.tun[[3]]@name               <- "IBTS_Q1"
MSH.tun[[4]]@name               <- "IBTS_Q4"

## Set the plus group or specify there isnt one
MSH.tun[[1]]@range["plusgroup"] <- 9
MSH.tun[[2]]@range["plusgroup"] <- 9
MSH.tun[[3]]@range["plusgroup"] <- 9
MSH.tun[[4]]@range["plusgroup"] <- 9

#Set names
names(MSH.tun)                  <- lapply(MSH.tun,name)

## Trim indices to have the required ages
MSH.tun                         <- FLIndices("MS HERAS"=MSH.tun[[1]],"WoS HERAS"=MSH.tun[[2]],"IBTS_Q1"=trim(MSH.tun[[3]],age=2:9),"IBTS_Q4"=trim(MSH.tun[[4]],age=2:9))

### ======================================================================================================
### Prepare FLSAM object
### ======================================================================================================
log.msg("PREPARING FLSAM CONTROL OBJECT...\n")

#Setup configuration - creates an empty control object with appropriate structure
MSH.ctrl <- FLSAM.control(MSH,MSH.tun)

#All fishing mortality states are free except
#oldest ages to ensure stablity
MSH.ctrl@states["catch",] <- c(1:7,8,8)

#Correlated Random walks for fishing mortalities - Default = FALSE = independent)
MSH.ctrl@cor.F<-TRUE

# Catchabilities
MSH.ctrl@catchabilities["MS HERAS",]        <- c(1,2,3,3,3,3,3,3,3)
MSH.ctrl@catchabilities["WoS HERAS",]       <- c(4,5,6,6,6,6,6,6,6)
MSH.ctrl@catchabilities["IBTS_Q1",ac(2:9)]  <- c(rep(7,8))
MSH.ctrl@catchabilities["IBTS_Q4",ac(2:9)]  <- c(rep(8,8))

#Fishing mortality RWs are set from an analysis of ICA VPA results
MSH.ctrl@f.vars["catch",]                   <- c(1,rep(2,8))#c(1,rep(2,8))

#Set the variances. Separate variance for recruitment and plus group
MSH.ctrl@logN.vars[]                        <- c(1,rep(2,dims(MSH)$age-1))

#Bind the observation variances
MSH.ctrl@obs.vars["catch",]                 <- c(1,2,2,2,2,2,2,3,3)
MSH.ctrl@obs.vars["MS HERAS",1:9]           <- c(4,rep(5,8))
MSH.ctrl@obs.vars["WoS HERAS",1:9]          <- c(6,rep(7,8))
MSH.ctrl@obs.vars["IBTS_Q1",2:9]            <- c(8,rep(9,7))
MSH.ctrl@obs.vars["IBTS_Q4",2:9]            <- c(10,rep(11,7))

MSH.ctrl                                    <- update(MSH.ctrl)

### ======================================================================================================
### Perform the assessment
### ======================================================================================================
log.msg("PERFORMING ASSESSMENT...\n")

## If you have difficulty running SAM with the configuration below you may need to save a pin file fom a previous run

#Now perform the asssessment
#MSH.sam   <-  FLSAM(MSH,MSH.tun,MSH.ctrl)

## If you are running the assessment using the saved pin file use the code below.

## Save this file in a specific directory eg "saved pin"
## Read in this pin file
## Run the assessment

###
FLR2SAM(MSH,MSH.tun,MSH.ctrl,run.dir="./saved_pin/")

runSAM(MSH.ctrl,run.dir="./saved_pin/",use.pin=TRUE)

MSH.sam <- SAM2FLR(MSH.ctrl,run.dir="./saved_pin/")

save(MSH.sam,file="./saved_pin/MSHwkwest.sam")

#Update stock object
MSH <- MSH + MSH.sam


#save AIC
write.csv(AIC(MSH.sam),file=file.path(output.dir,"AIC.csv"))




### ============================================================================
### Plots
### ============================================================================
#Setup plots

pdf(paste(output.base,"_MSH_plots.pdf",sep=""))
#png(file.path(output.dir,"figures - %02d.png"),units = "px", height=800,width=672, bg = "white")

### ======================================================================================================
### Diagnostics and plots
### ======================================================================================================


#Bubble plot of survey residuals
# Surveys are in the same order as the input file
res.dat <- subset(residuals(MSH.sam),fleet=="MS HERAS")
res.dat$data <- res.dat$std.res
p <- xyplot(age  ~ year |fleet,res.dat,
       cex=4*abs(res.dat$std.res)/max(abs(res.dat$std.res))+0.1,
       col=ifelse(res.dat$std.res<0,"red","black"),
       pch=16)
print(p)
res3.dat <- subset(residuals(MSH.sam),fleet=="WoS HERAS")
res3.dat$data <- res3.dat$std.res
p <- xyplot(age  ~ year |fleet,res3.dat,
            cex=4*abs(res3.dat$std.res)/max(abs(res3.dat$std.res))+0.1,
            col=ifelse(res3.dat$std.res<0,"red","black"),
            pch=16)
print(p)
res2.dat <- subset(residuals(MSH.sam),fleet=="IBTS_Q1")
res2.dat$data <- res2.dat$std.res
p <- xyplot(age  ~ year |fleet,res2.dat,
            cex=4*abs(res2.dat$std.res)/max(abs(res2.dat$std.res))+0.1,
            col=ifelse(res2.dat$std.res<0,"red","black"),
            pch=16)
print(p)

res3.dat <- subset(residuals(MSH.sam),fleet=="IBTS_Q4")
res3.dat$data <- res3.dat$std.res
p <- xyplot(age  ~ year |fleet,res3.dat,
            cex=4*abs(res3.dat$std.res)/max(abs(res3.dat$std.res))+0.1,
            col=ifelse(res3.dat$std.res<0,"red","black"),
            pch=16)
print(p)


## bubble plot of catch residuals
res.dat <- subset(residuals(MSH.sam),fleet=="catch")
res.dat$data <- res.dat$std.res
p <- xyplot(age  ~ year |fleet,res.dat,
       cex=4*abs(res.dat$std.res)/max(abs(res.dat$std.res))+0.1,
       col=ifelse(res.dat$std.res<0,"red","black"),
       pch=16)
print(p)


#Plot uncertainties as a function of time
CV.yrs <- ssb(MSH.sam)$year
CV.dat <- cbind(SSB=ssb(MSH.sam)$CV,
                   Fbar=fbar(MSH.sam)$CV,Rec=rec(MSH.sam)$CV)
matplot(CV.yrs,CV.dat,type="l",ylim=range(pretty(c(0,CV.dat))),yaxs="i",
    xlab="Year",ylab="CV of estimate",main="Uncertainties of key parameters")
legend("topleft",legend=colnames(CV.dat),lty=1:5,col=1:6,bty="n")

#Plot catchabilities values
catch <- catchabilities(MSH.sam)
print(xyplot(value+ubnd+lbnd ~ age | fleet,catch,
          scale=list(alternating=FALSE,y=list(relation="free")),as.table=TRUE,
          type="l",lwd=c(2,1,1),col=c("black","grey","grey"),
          subset=fleet %in% c("MS HERAS"),
          main="Survey catchability parameters",ylab="Catchability",xlab="Age"))

catch <- catchabilities(MSH.sam)
print(xyplot(value+ubnd+lbnd ~ age | fleet,catch,
          scale=list(alternating=FALSE,y=list(relation="free")),as.table=TRUE,
          type="l",lwd=c(2,1,1),col=c("black","grey","grey"),
          subset=fleet %in% c("WoS HERAS"),
          main="Survey catchability parameters",ylab="Catchability",xlab="Age"))


#Plot obs_variance (weightings)
obv <- obs.var(MSH.sam)
obv$str <- paste(obv$fleet,ifelse(is.na(obv$age),"",obv$age))
obv <- obv[order(obv$value),]
bp <- barplot(obv$value,ylab="Observation Variance",
       main="Observation variances by data source",col=factor(obv$fleet))
axis(1,at=bp,labels=obv$str,las=3,lty=0,mgp=c(0,0,0))
legend("topleft",levels(obv$fleet),pch=15,col=1:nlevels(obv$fleet),pt.cex=1.5)



plot(obv$value,obv$CV,xlab="Observation variance",ylab="CV of estimate",log="x",
  pch=16,col=obv$fleet,main="Observation variance vs uncertainty")
text(obv$value,obv$CV,obv$str,pos=4,cex=0.75,xpd=NA)

#Plot fishery selectivity pattern over time
sel.pat <- merge(f(MSH.sam),fbar(MSH.sam),
             by="year",suffixes=c(".f",".fbar"))
sel.pat$sel <- sel.pat$value.f/sel.pat$value.fbar
sel.pat$age <- as.numeric(as.character(sel.pat$age))
print(xyplot(sel ~ age|sprintf("%i's",floor((year+2)/5)*5),sel.pat,
         groups=year,type="l",as.table=TRUE,
         scale=list(alternating=FALSE),
         main="Selectivity of the Fishery by Period",xlab="Age",ylab="F/Fbar"))



#Plot correlation plot
cor.plot(MSH.sam)


 ##plot stock summary plot
plot(MSH.sam)


#plot(tmp.tun,type="pairwise")
plot(MSH.tun[["MS HERAS"]],type="internal", main="MS HERAS")
plot(MSH.tun[["WoS HERAS"]],type="internal", main="WoS HERAS")
plot(MSH.tun[["IBTS_Q1"]],type="internal", main="IBTS_Q1")
plot(MSH.tun[["IBTS_Q4"]],type="internal", main="IBTS_Q4")

## Otolith Plot
#stock<-MSH
plot.otolith(MSH.sam,n=1000)


#Diagnostic plots
source("diagnostics.r")
residual.diagnostics(MSH.sam)



dev.off()

### ======================================================================================================
### Document Assessment
### ======================================================================================================

#source(file.path(,"_Common","FLSAM.out.r"))
output.dir          <-  file.path(".","results")                #Output directory
log.msg("GENERATING DOCUMENTATION...\n")
#Document the run with alternative table numbering and a reduced width
old.opt           <- options("width","scipen")
options("width"=80,"scipen"=1000)

sam.out.file<- FLSAM.out(MSH,MSH.tun,MSH.sam,format="TABLE X.X.X.%i Malin Shelf Herring.")

write(sam.out.file,file.path(output.dir,"MSH_sam.out",sep=".")) #or could create output.base


options("width"=old.opt$width,"scipen"=old.opt$scipen)


stockSummaryTable <- cbind(rec(MSH.sam)$year,
                           rec(MSH.sam)$value,      rec(MSH.sam)$lbnd,    rec(MSH.sam)$ubnd,
                           tsb(MSH.sam)$value,      tsb(MSH.sam)$lbnd,    tsb(MSH.sam)$ubnd,
                           ssb(MSH.sam)$value,      ssb(MSH.sam)$lbnd,    ssb(MSH.sam)$ubnd,
                           catch(MSH.sam)$value,    catch(MSH.sam)$lbnd,  catch(MSH.sam)$ubnd,
                           catch(MSH.sam)$value / ssb(MSH.sam)$value, catch(MSH.sam)$lbnd / ssb(MSH.sam)$lbnd, catch(MSH.sam)$ubnd / ssb(MSH.sam)$ubnd,
                           fbar(MSH.sam)$value,     fbar(MSH.sam)$lbnd,   fbar(MSH.sam)$ubnd,
                           c(sop(MSH)))
colnames(stockSummaryTable) <-
  c("Year",paste(rep(c("Recruits Age 0 (Thousands)","Total biomass (tonnes)","Spawing biomass (tonnes)",
                       "Landings (tonnes)","Yield / SSB (ratio)","Mean F ages 3-6"),each=3),c("Mean","Low","High")),"SoP (%)")


#write(stockSummaryTable,file.path(output.dir,"stocksummarytable",sep=".")) #or could create output.base



### ============================================================================
### RETRO
### ============================================================================
#Import externals
library(FLSAM);

n.retro.years       <- 5		# (must be a numeric value, not a year range)

MSH.retro <- retro(MSH,MSH.tun,MSH.ctrl,n.retro.years,base.assess=MSH.sam)

save(MSH.retro,file="./MSHRetro.sam")


#Setup plots

pdf(paste(output.base,"_retro.pdf",sep=""))


#Plot retro
plot(MSH.retro,futureYrs=T)


dev.off()


### ============================================================================
### Finish
### ============================================================================

log.msg(paste("COMPLETE IN",sprintf("%0.1f",round(proc.time()[3]-start.time,1)),"s.\n\n"))

