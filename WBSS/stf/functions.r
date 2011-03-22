#------------------------------------
# Some basic functions
#------------------------------------

ac <- function(x){return(as.character(x))}
an <- function(x){return(as.numeric(x))}
af <- function(x){return(as.factor(x))}

#------------------------------------
# Fmsy transition period target
#------------------------------------

Fmsytrans     <- function(Fbarminyear,Fmsy,Fpa,SSB,Bpa,tppoints){return(pmin(c(tppoints[1]*Fbarminyear + ifelse(SSB < Bpa,tppoints[2]*Fmsy*SSB/Bpa,0.4*Fmsy)),Fpa,na.rm=T))}

#------------------------------------
# Calculate the survivors
#------------------------------------

#- Calculate the survivors given recruitment, stock numbers and harvest pattern
survivors <- function(rec,stk){
                survivors     <- numeric(length(dimnames(stk@stock.n)$age))
                survivors[1]  <- rec
                stk@harvest   <- unitSums(stk@harvest)
                if(length(dimnames(stk@stock.n)$unit)>1) stk <- stk[,,1] #this is not completely right ofcourse, but it works a only m and stock.n are taken which is ok
                if(is.na(range(stk)["plusgroup"])==F){
                  survivors[2:(length(survivors)-1)]  <- c(stk@stock.n * exp(-stk@harvest-stk@m))[1:(length(survivors)-2)]
                  survivors[length(survivors)]        <- sum(c(stk@stock.n * exp(-stk@harvest-stk@m))[(length(survivors)-1):length(survivors)],na.rm=T)
                } else {
                    survivors[2:length(survivors)]    <- c(stk@stock.n * exp(-stk@harvest-stk@m))[1:(length(survivors)-1)]
                  }
             return(survivors)}

#------------------------------------
# Scale harvest based on catch constraint
#------------------------------------

#- Rescale the harvest of the fleets given a TAC constraint
fleet.harvest <- function(stk,iYr,TACS){
                    nUnits              <- dims(stk)$unit
                    if(length(TACS)!=nUnits) stop("Number of TACS supplied is not equal to number of units")
                    res                 <- nls.lm(par=rep(1,nUnits),rescaleF,stk=stk,iYr=iYr,TACS=TACS,nls.lm.control(ftol = (.Machine$double.eps)),jac=NULL)$par
                    stk@harvest[,iYr]   <- sweep(stk@harvest[,iYr],3,res,"*")
                 return(stk@harvest[,iYr])}

#- Objective function of rescaling harverst function
rescaleF      <- function(mult,stk.=stk,iYr.=iYr,TACS.=TACS){
                    stk.@harvest[,iYr.] <- sweep(stk.@harvest[,iYr.],3,mult,"*")
                    stkZ                <- unitSums(stk.@harvest[,iYr.]) + stk.@m[,iYr.,1]
                    res                 <- sqrt(c((TACS. - c(apply(sweep(stk.@stock.n[,iYr.] * stk.@catch.wt[,iYr.] * sweep(stk.@harvest[,iYr.],c(1:2,4:6),stkZ,"/"),c(1:2,4:6),(1-exp(-stkZ)),"*"),3:6,sum,na.rm=T)))^2))
                 return(res)}

#- Rescale the harvest of the fleets given an F constraint but a TAC constraint for the A fleet
fleetCDF.harvest    <-  function(stk,iYr,catchA,Ftarget,CDFsplit,CDsplit){
                          nUnits                <- dims(stk)$unit
                          res                   <- nls.lm(par=rep(1,nUnits),independentRescaleF,stk=stk,iYr=iYr,TACS=catchA,Ftarget=Ftarget,CDFsplit=CDFsplit,CDsplit=CDsplit,nls.lm.control(ftol = (.Machine$double.eps),nprint=1,maxiter=1000),jac=NULL)$par
                          stk@harvest[,iYr]     <- sweep(stk@harvest[,iYr],3,res,"*")
                        return(stk@harvest[,iYr])}

#- Objective function of fleetCDF rescaling harverst function
independentRescaleF <-  function(mult,stk.=stk,iYr.=iYr,TACS1=TACS,Ftarget.=Ftarget,CDFsplit.=CDFsplit,CDsplit.=CDsplit){
                          stk.@harvest[,iYr.]       <- sweep(stk.@harvest[,iYr.],3,mult,"*")
                          stkZ                      <- unitSums(stk.@harvest[,iYr.]) + stk.@m[,iYr.,1]
                          catch                     <- harvestCatch(stk.,iYr.)
                          Fbar36                    <- mean(unitSums(stk.@harvest[ac(3:6),iYr.]))
                          #-Multiple target: Afleet needs to catch TAC, Cfleet needs to take CDFleet share * CDfleet share, Dfleet needs to take CDFleetshare * 1-CDfleet share and F fleet takes CDFleet share
                          #                  All of them contribute to the F36 target
                          res1                      <- sqrt((TACS1       - c(catch)[1])^2) / c(catch)[1]
                          res2                      <- sqrt((c(catch)[2] - (sum(c(catch))*CDFsplit.*CDsplit.))^2)/c(catch)[2]      + sqrt((Ftarget. - Fbar36)^2)/Ftarget.
                          res3                      <- sqrt((c(catch)[3] - (sum(c(catch))*CDFsplit.*(1-CDsplit.)))^2)/c(catch)[3]  + sqrt((Ftarget. - Fbar36)^2)/Ftarget.
                          res4                      <- sqrt((c(catch)[4] - (sum(c(catch))*CDFsplit.))^2)/c(catch)[4]               + sqrt((Ftarget. - Fbar36)^2)/Ftarget.
                        return(c(res1,res2,res3,res4))}
                        
#- Calculate catch based on an updated harvest pattern
harvestCatch  <-  function(stk.,iYr){
                    stkZ      <- unitSums(stk.@harvest[,iYr]) + stk.@m[,iYr,1]
                    res       <- apply(sweep(stk.@stock.n[,iYr] * stk.@catch.wt[,iYr] * sweep(stk.@harvest[,iYr],c(1:2,4:6),stkZ,"/"),c(1:2,4:6),(1-exp(-stkZ)),"*"),3:6,sum,na.rm=T)
                  return(res)}