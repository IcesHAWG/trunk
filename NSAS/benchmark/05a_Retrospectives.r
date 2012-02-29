################################################################################
# NSH_SAM "Final" Assessment Retrospectives
#
# $Rev$
# $Date$
#
# Author: HAWG model devlopment group
#
# Performs the retrospective analyses for the "Final" NSAS assessment model
#
# Developed with:
#   - R version 2.13.0
#   - FLCore 2.4
#
# To be done:
#
# Notes: Have fun running this assessment!
#
################################################################################
rm(list=ls()); graphics.off(); start.time <- proc.time()[3]
options(stringsAsFactors=FALSE)
log.msg     <-  function(string) {cat(string);}
log.msg("\nNSH Final Assessment\n=====================\n")

### ============================================================================
### Setup assessment
### ============================================================================
#Somewhere to store results
resdir <- file.path("benchmark","resultsSAM")
respref <- "05a_Retrospectives" #Prefix for output files
resfile <- file.path(resdir,paste(respref,".RData",sep=""))

#Import externals
library(FLSAM)
source(file.path("benchmark","Setup_objects.r"))
source(file.path("benchmark","05_Setup_final_configuration.r"))

### ============================================================================
### Run the assessment
### ============================================================================
#Only do the assessment if we are running in batch mode, or
#if the results file is missing
if(!file.exists(resfile) | !interactive()) {
  #- Run default retrospective 
  NSH.retro <- retro(NSH,NSH.tun,NSH.ctrl,10)

  # Save results
  save(NSH,NSH.tun,NSH.retro,file=resfile)

} else {
  #Load the file
  load(resfile)
}

### ============================================================================
### Outputs
### ============================================================================
pdf(file.path(resdir,paste(respref,".pdf",sep="")))

#Basic plot
print(plot(NSH.retro))

#Extract data, and drop intermediate years
ssb.dat <- transform(ssb(NSH.retro),var="Spawning Stock Biomass")
fbar.dat <- transform(fbar(NSH.retro),var="Fishing Mortality")
rec.dat <- transform(rec(NSH.retro),var="Recruitment",age=NULL)
rdat <- rbind(ssb.dat,fbar.dat,rec.dat)
rdat <- subset(rdat,year<=as.numeric(name))
rdat$var <- factor(rdat$var,c("Spawning Stock Biomass","Fishing Mortality","Recruitment"))

#Make basic plot
print(xyplot(value ~ year| var,rdat,
    groups=name,as.table=TRUE,layout=c(1,3),type="l",
    scales=list(y=list(relation="free"))),
    main="Retrospective analysis")

#Revision plots
rev.dat <- split(rdat,list(rdat$var,rdat$year))
rev.dat <- lapply(rev.dat,function(x) {
             x$rev <- c(NA,diff(x$value))/x$value
             return(x)
             })
revs <- do.call(rbind,rev.dat)
revs$offset <- as.numeric(revs$name) - revs$year           

print(xyplot(rev*100 ~ offset | var,revs,
        xlab="Revision number",ylab="Relative revision (%)",main="Retrospective revisions",
        as.table=TRUE,horizontal=FALSE,pch="|",lty=1,fill="grey",
        layout=c(1,3),xlim=c(0,20),
        par.settings=list(box.rectangle=list(lty=1,col=1),
                          box.umbrella=list(lty=1,col=1),
                          plot.symbol=list(col=1)),
        scale=list(alternating=FALSE),
        panel=function(...) {
          panel.abline(h=0,col="black")
          panel.bwplot(...)}))


### ============================================================================
### Finish
### ============================================================================
dev.off()
log.msg(paste("COMPLETE IN",sprintf("%0.1f",round(proc.time()[3]-start.time,1)),"s.\n\n"))



