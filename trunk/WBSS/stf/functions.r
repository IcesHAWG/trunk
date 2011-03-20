ac <- function(x){return(as.character(x))}
an <- function(x){return(as.numeric(x))}
af <- function(x){return(as.factor(x))}

#HCR
Fmsytrans     <- function(Fbarminyear,Fmsy,Fpa,SSB,Bpa){return(pmin(c(0.8*Fbarminyear + ifelse(SSB < Bpa,0.2*Fmsy*SSB/Bpa,0.2*Fmsy)),Fpa,na.rm=T))}

#Split the TAC over the four fleets
splitTAC <- function(totalTAC,Yr,TAC){
                TACA  <- 0
                TACC  <- 0.5*totalTAC * (TAC[,ac(Yr-1),"C"]/(TAC[,ac(Yr-1),"C"] + TAC[,ac(Yr-1),"D"]))
                TACD  <- 0.5*totalTAC * (TAC[,ac(Yr-1),"D"]/(TAC[,ac(Yr-1),"C"] + TAC[,ac(Yr-1),"D"]))
                TACF  <- 0.5*totalTAC
                TAC[,ac(FcY),"A"] <- TACA/1000; TAC[,ac(FcY),"C"] <- TACC/1000; TAC[,ac(FcY),"D"] <- TACD/1000; TAC[,ac(FcY),"F"] <- TACF/1000;
            return(TAC)}



#Predict the split
predictSplit <- function(object,newdata,fixes=NULL,ranefs=NULL){

                        fixs <- fixef(object)
                        rans <- ranef(object)

                        if(is.null(fixes) == F){
                          fixs            <- fixs[which(names(fixs) %in% fixes)]
                          if(length(fixes[which(!fixes %in% names(fixs))]>0)){
                            fixs2         <- c(fixs,rep(0,length(which(!fixes %in% names(fixs)))))
                            names(fixs2)  <- c(names(fixs),fixes[which(!fixes %in% names(fixs))])
                            fixs          <- fixs2
                          }
                        }
                        if(is.null(fixes) == T){
                          missingAge      <- an(sub("age","",names(fixs)[grep("age",names(fixs))]))
                          missingAge      <- seq(min(missingAge),max(missingAge),1)[which(!seq(min(missingAge),max(missingAge),1) %in% missingAge)]
                          missingQ        <- an(sub("Q","",names(fixs)[grep("Q",names(fixs))]))
                          missingQ        <- (1:4)[which(!1:4 %in% missingQ)]
                          fixs2           <- c(fixs,missingAge,missingQ)
                          names(fixs2)    <- c(names(fixs),paste("age",missingAge,sep=""),paste("Q",missingQ,sep=""))
                          fixs            <- fixs2
                        }
                        if(is.null(ranefs) == F){
                          idxRans             <- numeric(); names. <- names(idxRans)
                          for(i in names(rans)){
                            if(i %in% ranefs){
                              idxRans         <- c(idxRans,unlist(rans[[i]]))
                              names.          <- c(names.,paste(i,rownames(rans[[i]]),sep=""))
                              names(idxRans)  <- names.
                            }
                          }
                          rans <- idxRans
                        }

                        values      <- c(fixs,rans)
                        new.names   <- numeric()
                        for(x in names(values)){
                          i <- 0
                          while(is.na(an(substr(x,nchar(x)-i,nchar(x))))==F) i <- i+1
                          new.names <- c(new.names,substr(x,1,nchar(x)-i))
                        }
                        effectNames <- unique(new.names)

                        if(any(!colnames(newdata) %in% effectNames)) stop(paste("variable names '",colnames(newdata)[which(!colnames(newdata) %in% effectNames)],"'in newdata which are not in model"))

                        newdata$split                       <- exp(values["(Intercept)"]+
                                                                   values[paste("age",newdata$age,sep="")]+
                                                                   values[paste("Q",newdata$Q,sep="")]+
                                                                   values[paste("year",newdata$year,sep="")] +
                                                                   values[paste("yearclass",newdata$yearclass,sep="")] +
                                                                   values["lat"]*newdata$lat+
                                                                   values["long"]*newdata$long+
                                                                   values["lat:long"]*newdata$long*newdata$lat) /
                                                            (1+exp(values["(Intercept)"]+
                                                                   values[paste("age",newdata$age,sep="")]+
                                                                   values[paste("Q",newdata$Q,sep="")]+
                                                                   values[paste("year",newdata$year,sep="")]+
                                                                   values[paste("yearclass",newdata$yearclass,sep="")]+
                                                                   values["lat"]*newdata$lat+
                                                                   values["long"]*newdata$long+
                                                                   values["lat:long"]*newdata$long*newdata$lat))

                return(newdata$split)}

#-Calculate survivors
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