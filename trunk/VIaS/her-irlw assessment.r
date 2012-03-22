######################################################################################################
# NW.herring FLICA Assessment
# Test Run using FLICA with the northwest herring data
# Assessment settings need to be explored
# October 2011 Afra Egan
# March 2012 HAWG Updates Andrew Campbell
#
#
# Author: Afra Egan, Andrew Campbell
# Ireland
#
# Performs an assessment of Northwest Herring (NW.herring) using the FLICA package.
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
FnPrint("\nNorthwest Herring FLICA Assessment\n===================================\n")

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
output.base         <-  file.path(output.dir,"nw.herring Assessment") #Output base filename, including directory. Other output filenames are built by appending onto this one
retro.years         <-  c(2010:2011)

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
### same settings as celtic sea used in test run

#pg9+
#nw.herring.ctrl   <-  FLICA.control(sep.nyr=6,
#                             sep.age=4,
#                             sep.sel=1.5,
#                             lambda.yr=1,
#                             lambda.age=c(0.1,1,1,1,1,1,1,1,1),
#                             lambda.sr=0,
#                             sr=FALSE,
#                             index.model=c("l"),
#
                             index.cor=1)
#pg7+
nw.herring.ctrl   <-  FLICA.control(sep.nyr=6,
                             sep.age=4,
                             sep.sel=1.0,
                             lambda.yr=1,
                             lambda.age=c(0.1,1,1,1,1,1,1),
                             lambda.sr=0,
                             sr=FALSE,
                             index.model=c("l"),
                             index.cor=1)


### ======================================================================================================
### Prepare stock object for assessment
### ======================================================================================================
FnPrint("PREPARING STOCK OBJECT...\n")
nw.herring                        <- readFLStock(file.path(data.source, "index.txt"),no.discards=TRUE)
#Set no discards
nw.herring@catch.n                <- nw.herring@landings.n
nw.herring@catch                  <- nw.herring@landings
nw.herring@catch.wt               <- nw.herring@landings.wt
units(nw.herring)[1:17]           <- as.list(c(rep(c("tonnes","thousands","kg"),4), rep("NA",5)))
#Set fbar
range(nw.herring)[c("minfbar","maxfbar")] <- c(3,6)

#Set plus group
nw.herring                        <- setPlusGroup(nw.herring,nw.herring@range["max"])

#Set stock object name - this is propagated through into the figure titles
nw.herring@name    <- "Northwest Herring"

### ======================================================================================================
### Prepare index object for assessment
### ======================================================================================================
FnPrint("PREPARING INDEX OBJECT...\n")
#Load and modify all index data
nw.herring.tun   <- readFLIndices(file.path(data.source, "fleet.txt"))

#Set names, and parameters etc
names(nw.herring.tun) <-  gsub(":.*$","",names(nw.herring.tun))
nw.herring.tun   <- lapply(nw.herring.tun,function(idx) {
                idx@type 	     <- 	"number"
          		idx@index.var[]  <-	1
                idx@range["plusgroup"] <- NA
          		return(idx)})


names(nw.herring.tun)[1] <- c("Malin Shelf Herring Acoustic")

#downweight 1yos in survey
#nw.herring.tun[[1]]@index.var["1",]  <- 10;

### ======================================================================================================
### Perform the assessment
### ======================================================================================================
FnPrint("PERFORMING ASSESSMENT...\n")

#Now perform the asssessment
nw.herring.ica   <-  FLICA(nw.herring,nw.herring.tun,nw.herring.ctrl)
nw.herring       <-  nw.herring + nw.herring.ica
nw.herring@stock=computeStock(nw.herring) # to get TSB in stock slot

################################################################################
## Change Recruitment to mean value 1957-2009 but excluding 1963,1981,1983 and 1985 year classes
## this is the 'low recruitment regime' as used in the sVPA short term projections
## (sVPA would only consider years up to 2004 since it is not converged after this time)
## the big year classes will be in the slots for 1965,1983,1985 and 1987 as age 1
Rec=exp(mean(log(nw.herring@stock.n[1,as.character(c(1957:1964,1966:1982,1984,1986,1988:2009)),,,,])));

#put this recruitment into the last fishing year
nw.herring@stock.n['1',(as.character(nw.herring@range['maxyear'])),,,,]=Rec

#and into the ica object
nw.herring.ica@stock.n['1',(as.character(nw.herring@range['maxyear'])),,,,]=Rec

##Need to adjust the survivors also
gm.recs <- exp(mean(log(rec(trim(nw.herring,year=c(1957:1964,1966:1982,1984,1986,1988:2009))))))
stf.ctrl <- FLSTF.control(nyrs=1,catch.constraint=1000,f.rescale=TRUE,rec=gm.recs)
nw.herring.stf  <- FLSTF(stock=nw.herring,control=stf.ctrl,survivors=NA,quiet=TRUE,sop.correct=FALSE)

## Puts value into the survivors slot
nw.herring.ica@survivors['2',ac(2012)] = nw.herring.stf@stock.n['2',ac(2012)]

#Write the stf results out in the lowestoft VPA format for further analysis eg MFDP
writeFLStock(nw.herring.stf,output.file=paste(output.base,"with STF"))

### ======================================================================================================
### Use the standard code from the common modules to produce outputs
### ======================================================================================================
do.summary.plots(nw.herring,nw.herring.ica)
do.SRR.plot(nw.herring)


### ======================================================================================================
### Retrospective analysis
### ======================================================================================================
#Do the retrospective analysis
nw.retro.icas <- lapply(as.list(retro.years),function(yr) {
                  tun.tmp <- window(nw.herring.tun,end=yr)
                  stk.tmp <- window(nw.herring,end=yr)
                  ica <- FLICA(stk.tmp,tun.tmp,nw.herring.ctrl)
                  return(ica)
                  })

#Update the stock with the results of the assessment
names(nw.retro.icas) <- retro.years
nw.retro.stck <- lapply(nw.retro.icas,function(ica) {
                    last.yr <- dims(ica@stock.n)$maxyear
                    tmp.stck <- window(nw.herring,end=last.yr)
                    return(tmp.stck+ica)
                  })      #Returns a list of stock objects

#Now, update the recruitment for each stock object according to the geometric mean
nw.retro.stck <- lapply(nw.retro.stck,function(stk) {
                    last.yr <- dims(stk)$maxyear
                    recs <- rec(stk)
                    gm.recs <- exp(mean(log(window(recs,end=last.yr-2))))
                    stk@stock.n[1,ac(last.yr)] <- gm.recs
                    return(stk)
                  })

#Now, do the plots
nw.retro.stck <- do.call(FLStocks,nw.retro.stck)        #Converts to FLStocks
retro.plots(nw.retro.stck,nw.retro.icas,nw.herring.ctrl)

### ======================================================================================================
### Document Assessment
### ======================================================================================================

#Now write the file
#Number to corresponds to numbers in the report
#old.opt <- options("width","scipen","digits")
#options("width"=80,"scipen"=1000,"digits"=3)
ica.out.file <- ica.out(nw.herring,nw.herring.tun,nw.herring.ica,format="TABLE 6.x.x.%i her-irlw.")
write(ica.out.file,file=paste(output.base,"ica.out",sep="."))
#options("width"=old.opt$width,"scipen"=old.opt$scipen,"digits"=old.opt$digits)

writeFLStock(nw.herring,output.file=output.base)

################################################################################
## Output for standard Graphs

#And for incorporation into the standard graphs
writeFLStock(nw.herring,file.path(output.dir,"hawg_her-irlw.sum"),type="ICAsum");

##############################################################################################################

### ======================================================================================================
### Projections
### ======================================================================================================
FnPrint("CALCULATING PROJECTIONS...\n")

#Define years
TaY <- dims(nw.herring)$maxyear   	#Terminal assessment year
ImY <- TaY+1                		#Intermediate Year
AdY <- TaY+2                		#Advice year
CtY <- TaY+3                		#Continuation year - not of major concern but used in calculations in places
tbl.yrs <- as.character(c(ImY,AdY,CtY))   #Years to report in the output table

#Deal with recruitment
rec.years <- c(1957:1964,1966:1982,1984,1986,1988:2009);
gm.recs <- exp(mean(log(rec(nw.herring)[,as.character(rec.years)])))
nw.herring.srr <- list(model="geomean",params=FLPar(gm.recs))

#Expand stock object
nw.herring.proj <- stf(nw.herring,nyears=4,wts.nyears=3,arith.mean=TRUE,na.rm=TRUE);
nw.herring.proj@stock.n[,ac(ImY)] <- nw.herring.ica@survivors;
nw.herring.proj@stock.n[1,as.character(c(ImY,AdY,CtY))] <- gm.recs;

#For 2012
ImY.catch <- 7607; #(TAC in VIaS/VIIbc (4247) + Irish quota in VIaN (3360))
#2013 advice year
AdY.catch <- 4247;	#?????
numFmsy <- 0.25

#Setup options
options.l <- list(#Zero catch
                  "Catch(2013) = Zero"=
                    fwdControl(data.frame(year=c(ImY,AdY,CtY),
                                          quantity="catch",
                                          val=c(ImY.catch,0,0))),
                  # TAC, -15% 
                  "Catch(2013) = 2012 TAC -15% (3607 t)"=
                    fwdControl(data.frame(year=c(ImY,AdY,CtY),
                                          quantity=c("catch","catch","f"),
                                          rel=c(NA,NA,AdY),
                                          val=c(ImY.catch,AdY.catch*0.85,1))),
                  #TAC sq
                  "Catch(2013) = 2012 TAC sq (4247 t)"=
                    fwdControl(data.frame(year=c(ImY,AdY,CtY),
                                          quantity=c("catch","catch","f"),
                                          rel=c(NA,NA,AdY),
                                          val=c(ImY.catch,AdY.catch*1,1))),
                  #TAC +15%
                  "Catch(2013) = 2012 TAC +15% (4884 t)"=
                    fwdControl(data.frame(year=c(ImY,AdY,CtY),
                                          quantity=c("catch","catch","f"),
                                          rel=c(NA,NA,AdY),
                                          val=c(ImY.catch,AdY.catch*1.15,1))),
                  #TAC + 25%
                  "Catch(2013) = 2012 TAC + 25% (5309 t)"=
                    fwdControl(data.frame(year=c(ImY,AdY,CtY),
                                          quantity=c("catch","catch","f"),
                                          rel=c(NA,NA,AdY),
                                          val=c(ImY.catch, AdY.catch*1.25,1))),
                  #TAC +30%                                                                     
                     "Catch(2013) = 2012 TAC + 30% (5521 t)"=
                    fwdControl(data.frame(year=c(ImY,AdY,CtY),
                                          quantity=c("catch","catch","f"),
                                          rel=c(NA,NA,AdY),
                                          val=c(ImY.catch, AdY.catch*1.30,1))),                                                                     
                  #F =0.20 F0.1
                  "Fbar(2013) = 0.20"=
                    fwdControl(data.frame(year=c(ImY,AdY,CtY),
                                          quantity=c("catch","f","f"),
                                          rel=c(NA,NA,AdY),
                                          val=c(ImY.catch,0.20,1))),
                  #F =0.25 FMSY
                  "Fbar(2013) = 0.25"=
                    fwdControl(data.frame(year=c(ImY,AdY,CtY),
                                          quantity=c("catch","f","f"),
                                          rel=c(NA,NA,AdY),
                                          val=c(ImY.catch,0.25,1))),
                  # F = 0.33 Flim
                  "Fbar(2013) = 0.33"=
                    fwdControl(data.frame(year=c(ImY,AdY,CtY),
                                          quantity=c("catch","f","f"),
                                          rel=c(NA,NA,AdY),
                                          val=c(ImY.catch,0.33,1))),

                  "Fbar(2013) = 0.127"=
                    fwdControl(data.frame(year=c(ImY,AdY,CtY),
                                          quantity=c("catch","f","f"),
                                          rel=c(NA,NA,AdY),
                                          val=c(ImY.catch,0.127,1))),

                  "Fbar(2013) = 0.231"=
                    fwdControl(data.frame(year=c(ImY,AdY,CtY),
                                          quantity=c("catch","f","f"),
                                          rel=c(NA,NA,AdY),
                                          val=c(ImY.catch,0.231,1))),

                  "Fbar(2013) = 0.158"=
                    fwdControl(data.frame(year=c(ImY,AdY,CtY),
                                          quantity=c("catch","f","f"),
                                          rel=c(NA,NA,AdY),
                                          val=c(ImY.catch,0.158,1))),

                  "Fbar(2013) = 0.110"=
                    fwdControl(data.frame(year=c(ImY,AdY,CtY),
                                          quantity=c("catch","f","f"),
                                          rel=c(NA,NA,AdY),
                                          val=c(ImY.catch,0.110,1))),

                   "Fbar(2013) = 0.119"=
                    fwdControl(data.frame(year=c(ImY,AdY,CtY),
                                          quantity=c("catch","f","f"),
                                          rel=c(NA,NA,AdY),
                                          val=c(ImY.catch,0.119,1)))


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
nw.herring.options   <- lapply(options.l,function(ctrl) {fwd(nw.herring.proj,ctrl=ctrl,sr=nw.herring.srr)})
nw.herring.mult.opts <- lapply(mult.opts.l,function(ctrl) {fwd(nw.herring.proj,ctrl=ctrl,sr=nw.herring.srr)})


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
    col.dat <- sapply(input.tbl.list,function(slt) slot(nw.herring.proj,slt)[,as.character(yr),drop=TRUE])
    write.table(yr,file=input.tbl.file,col.names=FALSE,row.names=FALSE,append=TRUE,sep=",")
    write.table(t(c("Age",colnames(col.dat))),file=input.tbl.file,col.names=FALSE,row.names=FALSE,append=TRUE,sep=",")
    write.table(col.dat,file=input.tbl.file,col.names=FALSE,row.names=TRUE,append=TRUE,sep=",",na="-")
    write.table("",file=input.tbl.file,col.names=FALSE,row.names=FALSE,append=TRUE,sep=",")
}

#Detailed options table
options.file <-paste(output.base,"options - details.csv",sep=".")
write.table(NULL,file=options.file,col.names=FALSE,row.names=FALSE)
for(i in 1:length(nw.herring.options)) {
    opt <- names(nw.herring.options)[i]
    stk <- nw.herring.options[[opt]]
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
opt.sum.tbl(stcks=nw.herring.options,fname=paste(output.base,"options - summary.csv",sep="."))
opt.sum.tbl(stcks=nw.herring.mult.opts,fname=paste(output.base,"multi-options - summary.csv",sep="."))

### ======================================================================================================
### Create the figures for the advice sheet and the summary table and reference points
### ======================================================================================================

#nw.herring.sr <- ref.pts(nw.herring,"bevholt",100000)
#writeStandardOutput(cs.herring,cs.herring.sr,cs.retro.stck,nyrs.=3,recImY=NULL,output.base,Blim=44000,Bpa=26000,Flim=NULL,Fpa=NULL,Bmsy=NULL,Fmsy=NULL)

ica.out.file <- ica.out(nw.herring,nw.herring.tun,nw.herring.ica,format="TABLE 4.6.1.%i her-irlw")
write(ica.out.file,file=paste(output.base,"ica.out",sep="."))

dev.off()

