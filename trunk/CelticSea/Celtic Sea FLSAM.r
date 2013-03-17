######################################################################################################
# CS.herring FLSAM Assessment
# 
# Exploratory assessment
#
# Updated in February 2012
# Finalised March 2012, at HAWG
#
# Author: Mark Payne, DTU Aqua

#
# Performs an assessment of Celtic Sea Herring (cs.herring) using the FLSAM package.
#
####################################################################################################

### ======================================================================================================
### Initialise system, including convenience functions and title display
### ======================================================================================================
rm(list=ls()); gc(); graphics.off(); start.time <- proc.time()[3]
options(stringsAsFactors=FALSE)

library(FLSAM)

log.msg     <-  function(fmt,...) {
	cat(sprintf(fmt,...))
	flush.console()
}

log.msg("\nCeltic Sea Herring FLSAM Assessment\n===================================\n")

### ======================================================================================================
### Define parameters for use in the assessment code here
### ======================================================================================================
data.source         <-  file.path("data")      #Data source, not code or package source!!!
output.dir          <-  file.path("results")       #Output directory
output.base         <-  file.path(output.dir,"CSH") #Output base filename, including directory. Other output filenames are built by appending onto this one

pdf(paste(output.base,"_plots.pdf",sep=""))

### ======================================================================================================
### Prepare stock object for assessment
### ======================================================================================================
log.msg("PREPARING STOCK OBJECT...\n")
CSH<- readFLStock(file.path(data.source, "index.txt"),no.discards=TRUE)

#Set no discards
CSH@catch.n                <- CSH@landings.n
CSH@catch                  <- CSH@landings
CSH@catch.wt               <- CSH@landings.wt
units(CSH)[1:17]           <- as.list(c(rep(c("tonnes","thousands","kg"),4), rep("NA",5)))

#Set fbar
range(CSH)[c("minfbar","maxfbar")] <- c(2,5)

#Set plus group
CSH <- setPlusGroup(CSH,CSH@range["max"])

#Set stock object name - this is propagated through into the figure titles
CSH@name    <- "Celtic Sea Herring"

### ======================================================================================================
### Prepare index object for assessment
### ======================================================================================================
log.msg("PREPARING INDEX OBJECT...\n")
#Load and modify all index data
CSH.tun   <- readFLIndices(file.path(data.source, "fleet.txt"))

#Set names, and parameters etc
names(CSH.tun) <-  gsub(":.*$","",names(CSH.tun))
CSH.tun   <- lapply(CSH.tun,function(idx) {
                  idx@type 	     <- 	"number"
          		    idx@index.var[]  <-	1
                  idx@range["plusgroup"] <- NA
          		return(idx)})

names(CSH.tun)[1] <- c("CS HerAS")

### ======================================================================================================
### Prepare FLSAM object
### ======================================================================================================
log.msg("PREPARING FLSAM CONTROL OBJECT...\n")

#Create object
CSH.ctrl <- FLSAM.control(CSH,CSH.tun)
CSH.ctrl@sam.binary <- file.path("model","sam")

#Bind the observation variances
CSH.ctrl@obs.vars["catch",] <- c(1,2,2,3,3,3)
CSH.ctrl@obs.vars["CS HerAS",2:5] <- 4

#Bind the fishing mortalities on the oldest ages
CSH.ctrl@states["catch",4:6] <- 2

### ======================================================================================================
### Perform the assessment
### ======================================================================================================
log.msg("PERFORMING ASSESSMENT...\n")

#Now perform the asssessment
CSH.sam   <-  FLSAM(CSH,CSH.tun,CSH.ctrl)

#Update stock object
CSH <- CSH + CSH.sam

### ======================================================================================================
### Diagnostics and plots
### ======================================================================================================
#How does the model weight the data?
obsvar.plot(CSH.sam)

#Diagnostic plots
residual.diagnostics(CSH.sam)

#Bubble plot
res.dat <- subset(residuals(CSH.sam),fleet=="CS HerAS")
res.dat$data <- res.dat$std.res
p <- xyplot(age  ~ year |fleet,res.dat,
       cex=4*abs(res.dat$std.res)/max(abs(res.dat$std.res))+0.1,
       col=ifelse(res.dat$std.res<0,"red","black"),
       pch=16)
print(p)

#Plot catchabilities
plt.dat <- catchabilities(CSH.sam)
print(xyplot(value+ubnd+lbnd ~ age | fleet,plt.dat,
             scale=list(alternating=FALSE,y=list(relation="free")),as.table=TRUE,
             type="l",lwd=c(2,1,1),col=c("black","grey","grey"),
             main="Survey catchability parameters",ylab="Catchability",xlab="Age"))

#Plot correlation plot
cor.plot(CSH.sam)

#Stock summary plot
print(plot(CSH.sam))

### ======================================================================================================
### Finish
### ======================================================================================================
dev.off()
log.msg(paste("COMPLETE IN",sprintf("%0.1f",round(proc.time()[3]-start.time,1)),"s.\n\n"))


