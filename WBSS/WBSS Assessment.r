######################################################################################################
# WBSS FLICA Assessment
#
# $Rev$
# $Date$
#
# Author: Mark Payne
# DIFRES, Charlottenlund, DK
#
# Performs an assessment of Western Baltic Spring Spawning Herring (WBSS) in IIIa using the
# FLICA package.
#
# Developed with:
#   - R version 2.8.0
#   - FLCore 1.99-111
#   - FLICA, version 1.4-3
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
FnPrint("\nWBSS FLICA Assessment\n=====================\n")

### ======================================================================================================
### Incorporate Common modules
### Uses the common HAWG FLICA Assessment module to do the graphing, diagnostics and output
### ======================================================================================================
source(file.path("..","_Common","HAWG Common assessment module.r"))
### ======================================================================================================

### ======================================================================================================
### Define parameters for use in the assessment code here
### ======================================================================================================
data.source         <-  file.path(".","data")      #Data source, not code or package source!!!
output.dir          <-  file.path(".","res")       #Output directory
output.base         <-  file.path(output.dir,"WBSS Assessment") #Output base filename, including directory. Other output filenames are built by appending onto this one
n.retro.years       <-  8                          #Number of years for which to run the retrospective

### ======================================================================================================
### Output setup
### ======================================================================================================
#Output figure sizes for use in report
win.metafile(paste(output.base,"landscape figures - %02d.wmf"),height=100/25.4,width=130/25.4,pointsize=10)
trellis.par.set(fontsize=list(text=10,points=8))    #Set default lattice fontsize, so that things are actually readible!
win.metafile(paste(output.base,"portrait figures - %02d.wmf"),height=180/25.4,width=130/25.4,pointsize = 10)
trellis.par.set(fontsize=list(text=10,points=8))    #Set default lattice fontsize, so that things are actually readible!
#Square output
#png(paste(output.base,"figures - %02d.png"),units = "px", height=720,width=720,pointsize = 16, bg = "white")
#win.metafile(paste(output.base,"figures - %02d.wmf"),height=130/25.4,width=130/25.4,pointsize = 8)
#trellis.par.set(fontsize=list(text=18,points=16))     #Set default lattice fontsize, so that things are actually readible!

### ======================================================================================================
### Prepare control object for assessment
### We use here two different options - the first is the simpler and more normal:, just setting the control
### directly in the code. The second is reading in the configuration from a file - this is normally not
### necessary, but is a handy feature for using the code on stockassessment.org
### ======================================================================================================
FnPrint("PREPARING CONTROL OBJECTS...\n")
#Set control object straight up (option 1)
#-----------------------------------------
#WBSS.ctrl   <-  FLICA.control(sep.nyr=5,
#                              sep.age=4,
#                              sep.sel=1.0,
#                              lambda.yr=1,
#                              lambda.age=c(0.1,1,1,1,1,1,1,1,1),
#                              lambda.sr=0,
#                              sr=FALSE,
#                              sr.age=0,
#                              index.model=c("l","l","l"),
#                              index.cor=1)
#

#Make control object from configuration file (option 2)
#--------------------------------------------------------
#Read in configuration file, and break into vectors as needed
cfg.in <- read.table(file.path(".","conf","WBSS.cfg"),sep="=",comment.char="#",row.names=1,
            strip.white=TRUE,colClasses="character")
cfg    <- strsplit(cfg.in$V2," +|\t+")    #Breaks the input strings into vectors, using white space as a separator
names(cfg) <- row.names(cfg.in)
#Now take the arguments from the configuration file that apply here and make the object
ctrl.list <-  cfg[names(cfg) %in% names(formals(FLICA.control))]
WBSS.ctrl <- do.call(FLICA.control, ctrl.list)

### ======================================================================================================
### Prepare stock object for assessment
### ======================================================================================================
FnPrint("PREPARING STOCK OBJECT...\n")
#Read in data from VPA files
WBSS <- readFLStock(file.path(data.source, "index.dat"),no.discards=TRUE)
#Setup rest of logic
units(WBSS)[1:17] <- as.list(c(rep(c("tonnes","thousands","kg"),4), rep("NA",5)))
range(WBSS)[c("minfbar","maxfbar")] <- c(3,6)
WBSS  <- setPlusGroup(WBSS,WBSS@range["max"])
WBSS@name    <- paste(cfg$run.name,collapse=" ")  #Set stock object name from control file - this is propagated through into the figure titles

### ======================================================================================================
### Prepare index object for assessment
### ======================================================================================================
FnPrint("PREPARING INDEX OBJECT...\n")
#Load and modify all index data
WBSS.tun.in   <- readFLIndices(file.path(data.source, "tun.txt"))

#Set names, and parameters etc
names(WBSS.tun.in) <-  gsub(":.*$","",names(WBSS.tun.in))
for(nme in names(WBSS.tun.in)) { #Most indices have the same characteristics
  WBSS.tun.in[[nme]]@type 	             <- "number"
  WBSS.tun.in[[nme]]@index.var[]         <-	1
  WBSS.tun.in[[nme]]@range["plusgroup"]  <- NA
}
WBSS.tun.in[["HERAS 0-8+ wr"]]@range["plusgroup"] <- 8

#Generate two new indices by truncating current indices etc
WBSS.tun.in[["HERAS 3-6 wr"]]      <-  trim(WBSS.tun.in[["HERAS 0-8+ wr"]],age=3:6,year=1993:2008)           #HERAS 3-6 wr
WBSS.tun.in[["GerAS 1-3 wr"]]      <-  trim(WBSS.tun.in[["GerAS 0-8 wr (SD 21-24)"]],age=1:3,year=1994:2008) #GerAS 1-3 wr
WBSS.tun.in[["GerAS 1-3 wr"]]@index[,"2001"] <- -1     #2001 is excluded from GerAS due to lack of coverage in SD23

#Only use the relevant data sets in the assessment
WBSS.tun  <- WBSS.tun.in[c("HERAS 3-6 wr","GerAS 1-3 wr","N20")]

#Update the names in the trimmed object
for(nme in names(WBSS.tun)) {
      WBSS.tun[[nme]]@desc     <-    ""   #Wipe description, so it doesn't end up in the ica.out
      WBSS.tun[[nme]]@name     <-    nme #Reset index name in the object to that in the list (for consistency)
}

### ======================================================================================================
### Perform the assessment
### ======================================================================================================
FnPrint("PERFORMING ASSESSMENT...\n")
#Now perform the asssessment
WBSS.ica   <-  FLICA(WBSS,WBSS.tun,WBSS.ctrl)
WBSS       <-  WBSS + WBSS.ica
#Update the stock total biomass
WBSS@stock <- computeStock(WBSS)

### ======================================================================================================
### Use the standard code from the common modules to produce outputs
### ======================================================================================================
do.summary.plots(WBSS,WBSS.ica)
WBSS.retro <- do.retrospective.plots(WBSS,WBSS.tun,WBSS.ctrl,n.retro.years)
do.SRR.plot(WBSS)
WBSS.srr <- ref.pts(WBSS,"bevholt",1e6)

### ======================================================================================================
### Custom plots
### ======================================================================================================
FnPrint("GENERATING CUSTOM PLOTS...\n")

#Time series of each index
index.ts.dat  <- lapply(WBSS.tun,slot,"index")
index.ts.dat  <- as.data.frame(index.ts.dat)
index.ts.dat$id <- paste(index.ts.dat$qname,", Age ",index.ts.dat$age,sep="")
index.ts.dat$data[index.ts.dat$data<0] <- NA
index.ts.plot <- xyplot(data~year|id,data=index.ts.dat,
                    type="b",
                    xlab="Year",ylab="Index Value",
                    prepanel=function(...) list(ylim=range(pretty(c(0,list(...)$y)))),
                    pch=19,
                    as.table=TRUE,
                    strip=strip.custom(par.strip.text=list(cex=0.8)),
                    main=paste(WBSS@name,"Input Indices"),
                    scales=list(alternating=1,y=list(relation="free")),
                    panel=function(...) {
                        panel.grid(h=-1,v=-1)
                        panel.xyplot(...)
                    })
print(index.ts.plot)

#Other Custom plots should be in landscape orientation, so close the portrait ones here
dev.off()

#Catch and TAC
TACs    <- data.frame(year=1991:2008,TAC=1000*c(155,174,210,191,183,163,100,97,99,101,101,101,101,91,120,102+47.5,69+49.5,51.7+45))
TAC.plot.dat <- data.frame(year=rep(TACs$year,each=2)+c(-0.5,0.5),TAC=rep(TACs$TAC,each=2))
catch   <- as.data.frame(WBSS@catch)
plot(0,0,pch=NA,xlab="Year",ylab="Catch",xlim=range(pretty(c(catch$year,TAC.plot.dat$year))),ylim=range(pretty(c(0,TAC.plot.dat$TAC,catch$data))))
rect(catch$year-0.5,0,catch$year+0.5,catch$data,col="grey")
lines(TAC.plot.dat,lwd=5)
legend("topright",legend=c("Catch","TAC"),lwd=c(1,5),lty=c(NA,1),pch=c(22,NA),col="black",pt.bg="grey",pt.cex=c(2))
title(main=paste(WBSS@name,"Catch and TAC"))

#Proportion at age in (numbers) in the catch
prop.num.catch  <- stacked.area.plot(data~year*age,data=as.data.frame(pay(WBSS@catch.n)),
                      xlab="Year",
                      ylab="Proportion at age (by numbers) in catch",
                      main=paste(WBSS@name,"Proportion at Age by numbers in Catch"))
print(prop.num.catch)

#Proportion at age (in weight) in the catch
prop.wt.catch  <- stacked.area.plot(data~year*age,data=as.data.frame(pay(WBSS@catch.n*WBSS@catch.wt)),
                      xlab="Year",
                      ylab="Proportion at age (by weight) in catch",
                      main=paste(WBSS@name,"Proportion at Age by weight in Catch"))
print(prop.wt.catch)

#Time series of west
west.ts  <- xyplot(data~year,data=WBSS@stock.wt,
              groups=age,
              auto.key=list(space="right",points=FALSE,lines=TRUE,type="b",cex=0.8),
              type="b",
              xlab="Year",ylab="Weight in the stock (kg)",
              main=paste(WBSS@name,"Weight in the Stock"),
              par.settings=list(superpose.symbol=list(pch=as.character(0:8),cex=1.25)))
print(west.ts)

#Time series of west by cohort
west.by.cohort  <- as.data.frame(FLCohort(WBSS@stock.wt))
west.by.cohort  <-  subset(west.by.cohort,!is.na(west.by.cohort$data))
west.by.cohort$year <- west.by.cohort$age + west.by.cohort$cohort
west.cohort.plot  <- xyplot(data~year,data=west.by.cohort,
              groups=cohort,
              auto.key=list(space="right",points=FALSE,lines=TRUE,type="b",cex=0.8),
              type="b",
              xlab="Year",ylab="Weight in the stock (kg)",
              main=paste(WBSS@name,"Weight in the stock by cohort"),
              par.settings=list(superpose.symbol=list(pch=as.character(unique(west.by.cohort$cohort)%%10),cex=1.25)),
              panel=function(...) {
                panel.grid(h=-1,v=-1)
                panel.xyplot(...)
              })
print(west.cohort.plot)

#Contribution to ssb by age
ssb.dat    <- WBSS@stock.wt*WBSS@stock.n*exp(-WBSS@harvest*WBSS@harvest.spwn - WBSS@m*WBSS@m.spwn)*WBSS@mat/1000
ssb.by.age <- stacked.area.plot(data~year*age,as.data.frame(ssb.dat),ylab="Spawning Biomass (kt)",xlab="Year",
                main=paste(WBSS@name,"Contribution of ages to SSB"))
print(ssb.by.age)

#Proportion of ssb by age
ssb.prop.by.age <- stacked.area.plot(data~year*age,as.data.frame(pay(ssb.dat)),ylab="Proportion of SSB",xlab="Year",
                        main=paste(WBSS@name,"Proportion of ages in SSB"))
print(ssb.prop.by.age)

##Proportion of ssb by cohort
#ssb.cohorts         <- as.data.frame(FLCohort(pay(ssb.dat)))
#ssb.cohorts$year    <- ssb.cohorts$cohort+ssb.cohorts$age
#ssb.cohorts         <- subset(ssb.cohorts,!is.na(ssb.cohorts$data))
#ssb.by.cohort.plot  <- stacked.area.plot(data~year*cohort,ssb.cohorts)
#print(ssb.by.cohort.plot)
#

### ======================================================================================================
### Document Assessment
### ======================================================================================================
FnPrint("GENERATING DOCUMENTATION...\n")
#Document the run with alternative table numbering and a reduced width
old.opt <- options("width","scipen","digits")
options("width"=80,"scipen"=1000,"digits"=3)
#Fix up some of the uglier values with too many decimal places
WBSS.ica@catch.res <- zapsmall(WBSS.ica@catch.res,digits=5)
WBSS.ica@ica.out <- ica.out(WBSS,WBSS.tun,WBSS.ica,format="TABLE 3.6.%i WBSS HERRING.")
write(WBSS.ica@ica.out,file=paste(output.base,"ica.out",sep="."))
do.call(options,old.opt)

#Write the results out in the lowestoft VPA format for further analysis eg MFDP
writeFLStock(WBSS,output.file=output.base)

#And for incorporation into the standard graphs
writeFLStock(WBSS,file.path(output.dir,"hawg_her-3a22.sum"),type="ICAsum")

### ======================================================================================================
### Projections and Options Table
### ======================================================================================================
FnPrint("PERFORMING PROJECTIONS...\n")

#Define years
TaY <- dims(WBSS)$maxyear   #Terminal assessment year
ImY <- TaY+1                #Intermediate Year
FcY <- TaY+2                #Forecast year
CtY <- TaY+3                #Continuation year - not of major concern but used in calculations in places
tbl.yrs     <- as.character(c(ImY,FcY,CtY))   #Years to report in the output table

#Deal with recruitment - a geometric mean of the five years prior to the terminal assessment year
rec.years <- (TaY-5):(TaY-1)
gm.recs  <- exp(mean(log(rec(WBSS)[,as.character(rec.years)])))
WBSS.srr <- list(model="geomean",params=FLPar(gm.recs))

#Expand stock object
WBSS.proj <- stf(WBSS,nyears=4,wts.nyears=3,arith.mean=TRUE,na.rm=TRUE)
WBSS.proj@stock.n[,ac(ImY)]  <- WBSS.ica@survivors
WBSS.proj@stock.n[1,ac(ImY)] <- gm.recs

#Setup options
options.l <- list(#Option 1: F status quo option  #2009 catch is TAC restraint, with 30% misreporting 44915
                  "2009-2011:F Status Quo"=
                    fwdControl(data.frame(year=c(ImY,FcY,CtY),value=1,quantity="f",rel=TaY)),
                  #Option 2: F status quo option in intermediate year, followed by Fbar = 0.25
                  "2009:F Status Quo, 2010-2011:F=0.25"=
                    fwdControl(data.frame(year=c(ImY,FcY,CtY),
                                          quantity="f",
                                          val=c(NA,0.25,0.25),
                                          rel=c(TaY,NA,NA),
                                          value=c(1,NA,NA))),
                  #Option 3: 2009 Catch is 44915, followed by -15% TAC reduction if SSB < 110
                  "2009:Catch 44915, 2010:-15% TAC"=
                    fwdControl(data.frame(year=c(ImY,FcY,CtY),
                                          quantity=c("catch","catch","f"),
                                          rel=c(NA,ImY,FcY),
                                          val=c(44915,0.85,1))),
                  #Option 4: 2009 Catch is 44915, followed Fbar= 0.25
                  "2009:Catch 44915, 2010:Fbar=0.25"=
                    fwdControl(data.frame(year=c(ImY,FcY,CtY),
                                          quantity=c("catch","f","f"),
                                          rel=c(NA,NA,FcY),
                                          val=c(44915,0.25,1))))

#Calculate options
WBSS.options <- lapply(options.l,function(ctrl.l) {
                    tmp.stck <- WBSS.proj
                    if(!is.list(ctrl.l)) ctrl.l <- list(ctrl.l)
                    for(ctrl in ctrl.l) {
                       tmp.stck <- fwd(tmp.stck,ctrl=ctrl,sr=WBSS.srr)}
                    return(tmp.stck)})

#Setup output table
options.tbl <- lapply(as.list(1:length(WBSS.options)),function(i) {
                  opt <- names(WBSS.options)[i]
                  stk <- WBSS.options[[opt]]
                  #Title first
                  hdr <- sprintf('Option %0i. "%s"',i,opt)
                  hdr <- c(hdr,paste(rep("=",nchar(hdr)),collapse=""))
                  #Now the F and N by age
                  nums.by.age <- round(stk@stock.n[,tbl.yrs,drop=TRUE],0)
                  f.by.age    <- round(stk@harvest[,tbl.yrs,drop=TRUE],4)
                  age.tbl     <- cbind(N=nums.by.age,F=f.by.age)
                  age.tbl.out <- capture.output(print.default(age.tbl,quote=FALSE,right=TRUE))
                  #And now the summary tbl
                  sum.tbl     <- cbind(SSB=round(ssb(stk)[,tbl.yrs],0),
                                      F.bar=round(fbar(stk)[,tbl.yrs],4),
                                      Yield=round(computeCatch(stk)[,tbl.yrs],0))
                  row.names(sum.tbl) <- tbl.yrs
                  sum.tbl.out     <- capture.output(print.default(sum.tbl,quote=FALSE,right=TRUE))
                  #Now, bind it all together
                  return(c(hdr,"",age.tbl.out,"",sum.tbl.out,"\n"))
                })

#Write options table
write(do.call(c,options.tbl),file=paste(output.base,"options table.txt",sep="."))

### ======================================================================================================
### Writing Projections Table
### ======================================================================================================
FnPrint("WRITING OPTIONS TABLE...\n")


### ======================================================================================================
### Save workspace and Finish Up
### ======================================================================================================
FnPrint("SAVING WORKSPACES...\n")
save(WBSS,WBSS.ica,WBSS.tun,WBSS.ctrl,file=paste(output.base,".RData"))
save.image(file=paste(output.base,"Workspace.RData"))
dev.off()
FnPrint(paste("COMPLETE IN",sprintf("%0.1f",round(proc.time()[3]-start.time,1)),"s.\n\n"))