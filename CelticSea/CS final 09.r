######################################################################################################
# CS.herring FLICA Assessment
#  February 2009
#
# $Rev: 89 $
# $Date: 2009-02-20 10:32:54 +0000 (Fri, 20 Feb 2009) $
#
# Author: Afra Egan
# Ireland
#
# Performs an assessment of Celtic Sea Herring (cs.herring) using the FLICA package.
#
# Developed with:
#   - R version 2.8.0
#   - FLCore 1.99-111
#   - FLICA, version 1.4-10
#   - FLAssess, version 1.99-102
#   - FLSTF, version 1.99-1
#
# Changes:
# V 5.10 - Reflects modifications to Common Module to work as functions, rather than as a single script
# V 5.00 - Compatiable with Google Code version
# V 0.20 - Modifications
# V 0.10 - Initial version, based on code inherited from Tomas Grösler
#
# To be done:
#
# Notes:
#
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
FnPrint("\nCeltic Sea Herring FLICA Assessment\n===================================\n")

### ======================================================================================================
### Incorporate Common modules
### Uses the common HAWG FLICA Assessment module to do the graphing, diagnostics and output
### ======================================================================================================

source(file.path("..","_Common","HAWG Common assessment module.r"))

### ======================================================================================================

### ======================================================================================================
### Define parameters for use in the assessment code here
### ======================================================================================================
data.source         <-  file.path("data")      #Data source, not code or package source!!!
output.dir          <-  file.path("res")       #Output directory
output.base         <-  file.path(output.dir,"cs.herring Assessment") #Output base filename, including directory. Other output filenames are built by appending onto this one
retro.years         <-  c(2003,2005:2008)
             #Specify specific years to do the retrospective over

### ======================================================================================================
### Output setup
### ======================================================================================================
png(paste(output.base,"figures - %02d.png"),units = "px", height=1200,width=800,pointsize = 24, bg = "white")
#Set default lattice fontsize, so that things are actually readible!
trellis.par.set(fontsize=list(text=24,points=20))

### ======================================================================================================
### Prepare control object for assessment
### We use here two different options - the first is the simpler and more normal:, just setting the control
### directly in the code. The second is reading in the configuration from a file - this is normally not
### necessary, but is a handy feature for using the code on stockassessment.org
### ======================================================================================================
FnPrint("PREPARING CONTROL OBJECTS...\n")
#Set control object straight up (option 1)
#-----------------------------------------
cs.herring.ctrl   <-  FLICA.control(sep.nyr=6,
                             sep.age=3,
                             sep.sel=1.0,
                             lambda.yr=1,
                             lambda.age=c(0.1,1,1,1,1,1),
                              lambda.sr=0,
                              sr=FALSE,
                              index.model=c("l"),
                              index.cor=1)


### ======================================================================================================
### Prepare stock object for assessment
### ======================================================================================================
FnPrint("PREPARING STOCK OBJECT...\n")
cs.herring                        <- readFLStock(file.path(data.source, "index.txt"),no.discards=TRUE)
#Set no discards
cs.herring@catch.n                <- cs.herring@landings.n
cs.herring@catch                  <- cs.herring@landings
cs.herring@catch.wt               <- cs.herring@landings.wt
units(cs.herring)[1:17]           <- as.list(c(rep(c("tonnes","thousands","kg"),4), rep("NA",5)))
#Set fbar
range(cs.herring)[c("minfbar","maxfbar")] <- c(2,5)
#Set plus group
cs.herring                        <- setPlusGroup(cs.herring,cs.herring@range["max"])
#Set stock object name - this is propagated through into the figure titles
cs.herring@name    <- "Celtic Sea Herring"

### ======================================================================================================
### Prepare index object for assessment
### ======================================================================================================
FnPrint("PREPARING INDEX OBJECT...\n")
#Load and modify all index data
cs.herring.tun   <- readFLIndices(file.path(data.source, "fleet.txt"))

#Set names, and parameters etc
names(cs.herring.tun) <-  gsub(":.*$","",names(cs.herring.tun))
cs.herring.tun   <- lapply(cs.herring.tun,function(idx) {
                idx@type 	     <- 	"number"
          		idx@index.var[]  <-	1
                idx@range["plusgroup"] <- NA
          		return(idx)})


names(cs.herring.tun)[1] <- c("Celtic Sea Herring Acoustic")

#Only use the relevant data sets
cs.herring.tun  <- cs.herring.tun[c("Celtic Sea Herring Acoustic")]

### ======================================================================================================
### Perform the assessment
### ======================================================================================================
FnPrint("PERFORMING ASSESSMENT...\n")

#Now perform the asssessment
cs.herring.ica   <-  FLICA(cs.herring,cs.herring.tun,cs.herring.ctrl)
cs.herring       <-  cs.herring + cs.herring.ica
cs.herring@stock=computeStock(cs.herring) # to get TSB in stock slot

################################################################################
## Change Recruitment to mean value 1958-2006

Rec=exp(mean(log(cs.herring@stock.n[1,as.character(1958:(cs.herring@range['maxyear']-2)),,,,])))

# put recruitment into last fishing year
cs.herring@stock.n['1',(as.character(cs.herring@range['maxyear'])),,,,]=Rec

################################################################################

### ======================================================================================================
### Use the standard code from the common modules to produce outputs
### ======================================================================================================
do.summary.plots(cs.herring,cs.herring.ica)
do.SRR.plot(cs.herring)

### ======================================================================================================
### Retrospective analysis
### ======================================================================================================
#Do the retrospective analysis
cs.retro.icas <- lapply(as.list(retro.years),function(yr) {
                  tun.tmp <- window(cs.herring.tun,end=yr)
                  stk.tmp <- window(cs.herring,end=yr)
                  ica <- FLICA(stk.tmp,tun.tmp,cs.herring.ctrl)
                  return(ica)
                  })
#Update the stock with the results of the assessment
names(cs.retro.icas) <- retro.years
cs.retro.stck <- lapply(cs.retro.icas,function(ica) {
                    last.yr <- dims(ica@stock.n)$maxyear
                    tmp.stck <- window(cs.herring,end=last.yr)
                    return(tmp.stck+ica)
                  })      #Returns a list of stock objects
#Now, update the recruitment for each stock object according to the geometric mean
cs.retro.stck <- lapply(cs.retro.stck,function(stk) {
                    last.yr <- dims(stk)$maxyear
                    recs <- rec(stk)
                    gm.recs <- exp(mean(log(window(recs,end=last.yr-2))))
                    stk@stock.n[1,ac(last.yr)] <- gm.recs
                    return(stk)
                  })
#Now, do the plots
cs.retro.stck <- do.call(FLStocks,cs.retro.stck)        #Converts to FLStocks
retro.plots(cs.retro.stck,cs.retro.icas,cs.herring.ctrl)




### ======================================================================================================
### Custom plots
### ======================================================================================================
FnPrint("GENERATING CUSTOM PLOTS...\n")

#Plot of Catch and TAC
## TAC years specified
## Catch all years
TACs    <- data.frame(year=1974:2008,TAC=1000*c(32,25,10.8,0,0,6,6,6,8,8,13,13,17,18,18,20,17.5,21,21,21,21,21,21,22,22,21,21,20,11,13,13,13,11,9.3,7.9))
TAC.plot.dat <- data.frame(year=rep(TACs$year,each=2)+c(-0.5,0.5),TAC=rep(TACs$TAC,each=2))
catch   <- as.data.frame(cs.herring@catch)
plot(0,0,pch=NA,xlab="Year",ylab="Catch",xlim=range(pretty(c(catch$year,TACs$year))),ylim=range(pretty(c(0,TACs$TAC,catch$data))))
rect(catch$year-0.5,0,catch$year+0.5,catch$data,col="grey")
lines(TAC.plot.dat,lwd=5)
legend("topright",legend=c("Catch","TAC"),lwd=c(1,5),lty=c(NA,1),pch=c(22,NA),col="black",pt.bg="grey",pt.cex=c(2))
title(main=paste(cs.herring@name,"Catch and TAC"))

#Proportion of the catch by age
canum.prop.age <- stacked.area.plot(data~year*age,data=as.data.frame(pay(cs.herring@catch.n)),
                    main=paste(cs.herring@name,"Proportion at age (by numbers) in the catch"),
                    xlab="Year",ylab="Prop. at age in the catch")
print(canum.prop.age)


### ======================================================================================================
### Document Assessment
### ======================================================================================================
FnPrint("GENERATING DOCUMENTATION...\n")
#Document the run with alternative table numbering and a reduced width
old.opt <- options("width")
options("width"=80)
ica.out.file <- ica.out(cs.herring,cs.herring.tun,cs.herring.ica,format="TABLE 4.6.2.%i cs.herring HERRING.")
write(ica.out.file,file=paste(output.base,"ica.out",sep="."))
options("width"=old.opt$width)

### ======================================================================================================
### Short Term Forecast
### ======================================================================================================


FnPrint("PERFORMING SHORT TERM FORECAST...\n")
#Make forecast
gm.recs         <- exp(mean(log(rec(trim(cs.herring,year=1958:2006)))))  #cs.herring recruitment is based on a geometric mean of the last few years
stf.ctrl        <- FLSTF.control(nyrs=1,catch.constraint=1000,f.rescale=TRUE,rec=gm.recs)
cs.herring.stf  <- FLSTF(stock=cs.herring,control=stf.ctrl,survivors=NA,quiet=TRUE,sop.correct=FALSE)


#Write the stf results out in the lowestoft VPA format for further analysis eg MFDP
writeFLStock(cs.herring.stf,output.file=paste(output.base,"with STF"))

### ======================================================================================================
### Save workspace and Finish Up
### ======================================================================================================
FnPrint("SAVING WORKSPACES...\n")
save(cs.herring,cs.herring.stf,cs.herring.tun,cs.herring.ctrl,file=paste(output.base,"Assessment.RData"))
save.image(file=paste(output.base,"Assessment Workspace.RData"))
dev.off()
FnPrint(paste("COMPLETE IN",sprintf("%0.1f",round(proc.time()[3]-start.time,1)),"s.\n\n"))


################################################################################
## Output for standard Graphs


writeFLStock(cs.herring,file.path(output.dir,"hawg_her-irls.sum"),type="ICA")


