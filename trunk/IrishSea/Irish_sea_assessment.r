####################################################################################################

### ======================================================================================================
### Initialise system, including convenience functions and title display
### ======================================================================================================
rm(list=ls()); gc(); graphics.off(); start.time <- proc.time()[3]
options(stringsAsFactors=FALSE)
FnPrint     <-  function(string) {
	cat(string)
	flush.console()
}
FnPrint("\nIrish Sea Herring FLICA Assessment\n===================================\n")
### ======================================================================================================

### ======================================================================================================
### Define parameters for use in the assessment code here
### ======================================================================================================
data.source         <-  file.path("data")      #Data source, not code or package source!!!
output.dir          <-  file.path("res")       #Output directory
output.base         <-  file.path(output.dir,"nirs.herring Assessment") #Output base filename, including directory. Other output filenames are built by appending onto this one
n.retro.yrs         <-  5
             #Specify specific years to do the retrospective over

### ======================================================================================================
### Output setup
### ======================================================================================================
png(paste(output.base,"figures - %02d.png"),units = "px", height=1200,width=800,pointsize = 24, bg = "white")
#Set default lattice fontsize, so that things are actually readible!
trellis.par.set(fontsize=list(text=24,points=20))

### ======================================================================================================
### ======================================================================================================


library(FLCore)
library(FLAssess)
#library(FLash)
#library(FLBRP)
#library(FLXSA)
library(FLICA)

#my.dir  <-"C:/Documents and Settings/beggss/My Documents/R/FLICAVIIaN/VIIaNData2006/"
#MPA: It's best not to force a working directory - instead, if you load the file in Tinn-R, and then
#launch R from Tinn-R, it will set the working directory automatically to the same as the loaded file!
#If you make everything relative to this path, eg using "." and ".." directories, then you'll be able to
#to move the code around very easily, and other people will be able to use it simply. I've commented it
#out here, and made some changes below to reflect this

### ======================================================================================================
### Load the Common modules
### Uses the common HAWG FLICA Assessment module to do the graphing, diagnostics and output
### ======================================================================================================
source(file.path("..","_Common","HAWG Common assessment module.r"))
### ======================================================================================================

Pop4                      <- readFLStock(file.path(".","data","index.txt"))
Pop4.tun                  <- readFLIndices(file.path(".","data","fleet.txt"),file.path(".","data","ssb.txt"),type="ICA")
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

#MPA: Added names to tuning fleets - needed to make the pretty graphs!
names(Pop4.tun) <- c("NINEL","Northern Ireland Acoustic Surveys")


units(Pop4)[1:17]               <-as.list(c(rep(c("Tonnes","Thousands","Kg"),4),"NA","NA","f","NA","NA"))

Pop4.ctrl<-FLICA.control(sep.nyr=6,sep.age=4,sep.sel=1.0,sr=FALSE,
                                lambda.yr=c(1,1,1,1,1,0.01),
                                lambda.age =c(0.1, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0),
                                lambda.sr=0.01, index.model=c("l","l"), index.cor=F)

Pop4.ica    <- FLICA(Pop4, Pop4.tun, Pop4.ctrl)
# complete stock
Pop4        <- Pop4 + Pop4.ica
Pop4@stock  <- computeStock(Pop4)

### Output setup
png(file.path(".","res","Irish Sea figures - %02d.png"),units = "px", height=1200,width=800,pointsize = 24, bg = "white")
#Set default lattice fontsize, so that things are actually readible!
trellis.par.set(fontsize=list(text=24,points=20))

### Use the standard code from the common modules to produce outputs
do.summary.plots(Pop4,Pop4.ica)
WBSS.retro <- do.retrospective.plots(Pop4,Pop4.tun,Pop4.ctrl,n.retro.yrs=5)
do.SRR.plot(Pop4)

#Close plots
dev.off()

#MPA: See the examples in the WBSS assessment for writing ica.out files, making tables, doing short-term forecasts etc
