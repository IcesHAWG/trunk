######################################################################################################
# CS.herring FLICA Assessment
# 2012 Assessment - SALY Run
# Updated canum,weca,west,caton etc. Fleet includes 2011 acoustic survey
# INCLUDING strata 20 which was an extra strata completed in 2011 but not
# in previous years
#
# Updated in February 2013
# Finalised March 2013, at HAWG
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
# V 0.10 - Initial version, based on code inherited from Tomas Gr�sler
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
output.dir          <-  file.path("results")       #Output directory
output.base         <-  file.path(output.dir,"cs.herring Assessment") #Output base filename, including directory. Other output filenames are built by appending onto this one
#Specify specific years to do the retrospective over
retro.years         <- c(2003,2005:2012)		# (no 2004 survey data point)

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

### ======================================================================================================
### Perform the assessment
### ======================================================================================================
FnPrint("PERFORMING ASSESSMENT...\n")

#Now perform the asssessment
cs.herring.ica   <-  FLICA(cs.herring,cs.herring.tun,cs.herring.ctrl)
cs.herring       <-  cs.herring + cs.herring.ica
cs.herring@stock=computeStock(cs.herring) # to get TSB in stock slot


################################################################################
## Change Recruitment to mean value of period 1981 - 2009 (was 1995-2009 for HAWG 2011)


Rec=exp(mean(log(cs.herring@stock.n[1,as.character(1981:(cs.herring@range['maxyear']-2)),,,,])))

# put recruitment into last fishing year
cs.herring@stock.n['1',(as.character(cs.herring@range['maxyear'])),,,,]=Rec

# puts the geomean value into the pop numbers at age 1 in 2012
cs.herring.ica@stock.n['1',(as.character(cs.herring@range['maxyear'])),,,,]=Rec

##Need to adjust the survivors also
gm.recs <- exp(mean(log(rec(trim(cs.herring,year=1981:2010)))))
stf.ctrl        <- FLSTF.control(nyrs=1,catch.constraint=1000,f.rescale=FALSE,rec=gm.recs)
cs.herring.stf  <- FLSTF(stock=cs.herring,control=stf.ctrl,survivors=NA,quiet=TRUE,sop.correct=FALSE)

## Puts value into the survivors slot
cs.herring.ica@survivors['2',ac(2013)]=  cs.herring.stf@stock.n['2',ac(2013)]


#Write the stf results out in the lowestoft VPA format for further analysis eg MFDP
writeFLStock(cs.herring.stf,output.file=paste(output.base,"with STF"))

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


#####internal consistency survey plot  ##################################################################

cor.tun(cs.herring.tun)
## nas in the data 2004 missing 

### ======================================================================================================
### Custom plots
### ======================================================================================================
FnPrint("GENERATING CUSTOM PLOTS...\n")

#Plot of Catch and TAC
## TAC years specified
## Catch all years
TACs    <- data.frame(year=1974:2013,TAC=1000*c(32,25,10.8,0,0,6,6,6,8,8,13,13,17,18,18,20,17.5,21,21,21,21,21,21,22,22,21,21,20,11,13,13,13,11,9.4,7.9,5.9,10.15,13.2,21.1,17.2))
TAC.plot.dat <- data.frame(year=rep(TACs$year,each=2)+c(-0.5,0.5),TAC=rep(TACs$TAC,each=2))
catch   <- as.data.frame(cs.herring@catch)
plot(0,0,pch=NA,xlab="Year",ylab="Catch",xlim=range(pretty(c(catch$year,TACs$year))),ylim=range(pretty(c(0,TACs$TAC,catch$data))))
rect(catch$year-0.5,0,catch$year+0.5,catch$data,col="grey")
lines(TAC.plot.dat,lwd=5)
legend("topright",legend=c("Catch","TAC"),lwd=c(1,5),lty=c(NA,1),pch=c(22,NA),col="black",pt.bg="grey",pt.cex=c(2))
title(main=paste(cs.herring@name,"Catch and TAC"))

### ======================================================================================================
### Document Assessment
### ======================================================================================================

#Now write the file
#Number to corresponds to numbers in the report
old.opt <- options("width","scipen","digits")
options("width"=80,"scipen"=1000,"digits"=3)
ica.out.file <- ica.out(cs.herring,cs.herring.tun,cs.herring.ica,format="TABLE 4.6.1.%i Celtic Sea and Division VIIj Herring.")
write(ica.out.file,file=paste(output.base,"ica.out",sep="."))
options("width"=old.opt$width,"scipen"=old.opt$scipen,"digits"=old.opt$digits)

#And finally, write the results out in the lowestoft VPA format for further analysis eg MFDP
writeFLStock(cs.herring,output.file=output.base);

################################################################################
## Output for standard Graphs

#And for incorporation into the standard graphs
writeFLStock(cs.herring,file.path(output.dir,"hawg_her-irls.sum"),type="ICAsum");
writeFLStock(cs.herring,file.path(output.dir,"hawg_her-irls.ypr"),type="YPR")


### ======================================================================================================
### Projections
### ======================================================================================================
FnPrint("CALCULATING PROJECTIONS...\n")

#Define years
TaY <- dims(cs.herring)$maxyear   #Terminal assessment year
ImY <- TaY+1                      #Intermediate Year
AdY <- TaY+2                      #Advice year
CtY <- TaY+3                      #Continuation year - not of major concern but used in calculations in places
tbl.yrs <- as.character(c(ImY,AdY,CtY))   #Years to report in the output table

#Deal with recruitment - a geometric mean of the five years prior to the terminal assessment year
#geometric mean from 1981 to maxyear-2
#rec.years <- (1995:(cs.herring@range['maxyear']-2));
#changed at HAWG 2012 to the period 1981 to 2 years before terminal year
rec.years <- (1981:(cs.herring@range['maxyear']-2));

gm.recs  <- exp(mean(log(rec(cs.herring)[,as.character(rec.years)])));
cs.herring.srr <- list(model="geomean",params=FLPar(gm.recs));

#Expand stock object
cs.herring.proj <- stf(cs.herring,nyears=4,wts.nyears=3,arith.mean=TRUE,na.rm=TRUE);
cs.herring.proj@stock.n[,ac(ImY)]  <- cs.herring.ica@survivors;
cs.herring.proj@stock.n[1,as.character(c(ImY,AdY,CtY))] <- gm.recs;

#Define some constants 2011
#ImY.catch <- 16196;
#AdY.catch <- 13200;
#numFmsy <- 0.25;

#For 2013
ImY.catch <- 19063;	# 2013 total TAC + carry over into 2013
#AdY.catch <- 17200	# 2013 TAC
AdY.catch <- 17200;
numFmsy <- 0.25;

#Setup options
options.l <- list(#Zero catch
                  "Catch(2014) = Zero"=
                    fwdControl(data.frame(year=c(ImY,AdY,CtY),
                                          quantity="catch",
                                          val=c(ImY.catch,0,0))),
                  # TAC, -15% 
                  "Catch(2014) = 2013 TAC -15% (14620 t)"=
                    fwdControl(data.frame(year=c(ImY,AdY,CtY),
                                          quantity=c("catch","catch","f"),
                                          rel=c(NA,NA,AdY),
                                          val=c(ImY.catch,AdY.catch*0.85,1))),
                  #TAC sq
                  "Catch(2014) = 2013 TAC sq (17200 t)"=
                    fwdControl(data.frame(year=c(ImY,AdY,CtY),
                                          quantity=c("catch","catch","f"),
                                          rel=c(NA,NA,AdY),
                                          val=c(ImY.catch,AdY.catch*1,1))),
                  #TAC +15%
                  "Catch(2014) = 2013 TAC +15% (19780 t)"=
                    fwdControl(data.frame(year=c(ImY,AdY,CtY),
                                          quantity=c("catch","catch","f"),
                                          rel=c(NA,NA,AdY),
                                          val=c(ImY.catch,AdY.catch*1.15,1))),
                  #TAC + 25%
                  "Catch(2014) = 2013 TAC + 25% (21500 t)"=
                    fwdControl(data.frame(year=c(ImY,AdY,CtY),
                                          quantity=c("catch","catch","f"),
                                          rel=c(NA,NA,AdY),
                                          val=c(ImY.catch, AdY.catch*1.25,1))),
                  #TAC +30%                           
                  "Catch(2014) = 2013 TAC + 30% (22360 t)"=
                    fwdControl(data.frame(year=c(ImY,AdY,CtY),
                                          quantity=c("catch","catch","f"),
                                          rel=c(NA,NA,AdY),
                                          val=c(ImY.catch, AdY.catch*1.30,1))),
                    #status quo catch                           
                  "Catch(2014) = 2013 TAC + 10.8% (19063t)"=
                    fwdControl(data.frame(year=c(ImY,AdY,CtY),
                                          quantity=c("catch","catch","f"),
                                          rel=c(NA,NA,AdY),
                                          val=c(ImY.catch, AdY.catch*1.1083,1))),                                                                                                
                  #F =0.25	#Fmsy
                  "Fbar(2014) = 0.25"=
                    fwdControl(data.frame(year=c(ImY,AdY,CtY),
                                          quantity=c("catch","f","f"),
                                          rel=c(NA,NA,AdY),
                                          val=c(ImY.catch,numFmsy,1))),
                  #F =0.23	#Proposed LTMP 
                  "Fbar(2014) = 0.23"=
                    fwdControl(data.frame(year=c(ImY,AdY,CtY),
                                          quantity=c("catch","f","f"),
                                          rel=c(NA,NA,AdY),
                                          val=c(ImY.catch,0.23,1))),
                  #F=0.19	#Rebuilding Plan
                  "Fbar(2013) = 0.19"=
                    fwdControl(data.frame(year=c(ImY,AdY,CtY),
                                          quantity=c("catch","f","f"),
                                          rel=c(NA,NA,AdY),
                                          val=c(ImY.catch,0.19,1))),
                  #F=0.15	#Status Quo F (UPDATE FIGURE)
                  "Fbar(2014) = 0.15"=
                    fwdControl(data.frame(year=c(ImY,AdY,CtY),
                                          quantity=c("catch","f","f"),
                                          rel=c(NA,NA,AdY),
                                          val=c(ImY.catch,0.15,1))),
                                                      
                  "Fbar(2014) = 0.17"=       #F0.1
                    fwdControl(data.frame(year=c(ImY,AdY,CtY),
                                          quantity=c("catch","f","f"),
                                          rel=c(NA,NA,AdY),
                                          val=c(ImY.catch,0.17,1)))
) #End options list



#Multi-options table
fmult.targs  <- seq(0,2,by=0.1)
mult.opts.l <- lapply(as.list(fmult.targs),function(fmult) {
                          fwdControl(data.frame(year=c(ImY,AdY,CtY),
                                          quantity=c("catch","f","f"),
                                          rel=c(NA,ImY,AdY),
                                          val=c(ImY.catch,fmult,1)))
                  })
names(mult.opts.l) <- sprintf("Fmult(2010) = %4.3f",fmult.targs)

#Calculate options
cs.herring.options   <- lapply(options.l,function(ctrl) {fwd(cs.herring.proj,ctrl=ctrl,sr=cs.herring.srr)})
cs.herring.mult.opts <- lapply(mult.opts.l,function(ctrl) {fwd(cs.herring.proj,ctrl=ctrl,sr=cs.herring.srr)})


### ======================================================================================================
### Write Options Tables
### ======================================================================================================
FnPrint("WRITING OPTIONS TABLES...\n")

#Document input settings
input.tbl.file <-paste(output.base,"options - input.csv",sep=".")
write.table(NULL,file=input.tbl.file,col.names=FALSE,row.names=FALSE)
input.tbl.list <- list(N="stock.n",M="m",Mat="mat",PF="harvest.spwn",
                       PM="m.spwn",SWt="stock.wt",Sel="harvest",CWt="catch.wt")
for(yr in c(ImY,AdY,CtY)){
    col.dat <- sapply(input.tbl.list,function(slt) slot(cs.herring.proj,slt)[,as.character(yr),drop=TRUE])
    write.table(yr,file=input.tbl.file,col.names=FALSE,row.names=FALSE,append=TRUE,sep=",")
    write.table(t(c("Age",colnames(col.dat))),file=input.tbl.file,col.names=FALSE,row.names=FALSE,append=TRUE,sep=",")
    write.table(col.dat,file=input.tbl.file,col.names=FALSE,row.names=TRUE,append=TRUE,sep=",",na="-")
    write.table("",file=input.tbl.file,col.names=FALSE,row.names=FALSE,append=TRUE,sep=",")
}

#Detailed options table
options.file <-paste(output.base,"options - details.csv",sep=".")
write.table(NULL,file=options.file,col.names=FALSE,row.names=FALSE)
for(i in 1:length(cs.herring.options)) {
    opt <- names(cs.herring.options)[i]
    stk <- cs.herring.options[[opt]]
    #Now the F and N by age
    nums.by.age <- stk@stock.n[,tbl.yrs,drop=TRUE]
    colnames(nums.by.age) <- sprintf("N(%s)",tbl.yrs)
    f.by.age    <- stk@harvest[,tbl.yrs,drop=TRUE]
    colnames(f.by.age) <- sprintf("F(%s)",tbl.yrs)
    age.tbl     <- cbind(Age=rownames(f.by.age),N=nums.by.age,F=f.by.age)
    #And now the summary tbl
    sum.tbl     <- cbind(Year=tbl.yrs,SSB=ssb(stk)[,tbl.yrs],
                        F.bar=fbar(stk)[,tbl.yrs],Yield=computeCatch(stk)[,tbl.yrs])
    #Now, bind it all together
    sum.tbl.padding <- matrix("",nrow=nrow(age.tbl)-nrow(sum.tbl),ncol=ncol(sum.tbl))
    comb.tbl    <- cbind(age.tbl," ",rbind(sum.tbl,sum.tbl.padding))
    #And write it - hdr first, then the rest
    write.table(sprintf("%s). %s",letters[i],opt),options.file,append=TRUE,col.names=FALSE,row.names=FALSE,sep=",")
    write.table(t(colnames(comb.tbl)),options.file,append=TRUE,col.names=FALSE,row.names=FALSE,sep=",")
    write.table(comb.tbl,options.file,append=TRUE,col.names=FALSE,row.names=FALSE,sep=",")
    write.table(c(""),options.file,append=TRUE,col.names=FALSE,row.names=FALSE,sep=",")
}

#Options summary table
opt.sum.tbl <- function(stcks,fname) {
    options.sum.tbl <- sapply(as.list(1:length(stcks)),function(i) {
                          opt <- names(stcks)[i]
                          stk <- stcks[[opt]]
                          #Build up the summary
                          sum.tbl     <- data.frame(Rationale=opt,
                                          F.ImY=fbar(stk)[,as.character(ImY),drop=TRUE],
                                          Catch.ImY=computeCatch(stk)[,as.character(ImY),drop=TRUE],
                                          SSB.ImY=ssb(stk)[,as.character(ImY),drop=TRUE],
                                          F.AdY=fbar(stk)[,as.character(AdY),drop=TRUE],
                                          Catch.AdY=computeCatch(stk)[,as.character(AdY),drop=TRUE],
                                          SSB.AdY=ssb(stk)[,as.character(AdY),drop=TRUE],
                                          SSB.CtY=ssb(stk)[,as.character(CtY),drop=TRUE])
                          })
    options.sum.tbl <- t(options.sum.tbl)
    colnames(options.sum.tbl) <- c("Rationale",
                                    sprintf("Fbar (%i)",ImY),sprintf("Catch (%i)",ImY),sprintf("SSB (%i)",ImY),
                                    sprintf("Fbar (%i)",AdY),sprintf("Catch (%i)",AdY),sprintf("SSB (%i)",AdY),
                                    sprintf("SSB (%i)",CtY))
    write.csv(options.sum.tbl,file=fname,row.names=FALSE)
}
opt.sum.tbl(stcks=cs.herring.options,fname=paste(output.base,"options - summary.csv",sep="."))
opt.sum.tbl(stcks=cs.herring.mult.opts,fname=paste(output.base,"multi-options - summary.csv",sep="."))


### ======================================================================================================
### Create the figures for the advice sheet and the summary table and reference points
### ======================================================================================================

cs.herring.sr <- ref.pts(cs.herring,"bevholt",100000)

writeStandardOutput(cs.herring,cs.herring.sr,cs.retro.stck,nyrs.=3,recImY=NULL,output.base,Blim=44000,Bpa=26000,Flim=NULL,Fpa=NULL,Bmsy=NULL,Fmsy=NULL)


### ======================================================================================================
### Save workspace and Finish Up
### ======================================================================================================
FnPrint("SAVING WORKSPACES...\n")
save(cs.herring,cs.herring.stf,cs.herring.tun,cs.herring.ctrl,file=paste(output.base,"Assessment.RData"))
save.image(file=paste(output.base,"Assessment Workspace.RData"))
dev.off()
FnPrint(paste("COMPLETE IN",sprintf("%0.1f",round(proc.time()[3]-start.time,1)),"s.\n\n"))


