#- Plot the retrospective residual pattern in any fleet
retroResiduals <- function(x,fleet,yrs){

res <- lapply(x,residuals)
res <- lapply(res,function(y){y[which(y$fleet == fleet & y$year %in% yrs),]})
res <- lapply(res,function(y){cbind(y,retro=max(y$year))})
res <- do.call(rbind,res)

xyplot(std.res ~ age | as.factor(year),data=res,type="l",groups=retro,
auto.key=list(space="right",points=FALSE,lines=TRUE,type="l"),main=paste("Residual pattern in",fleet,"at age"),
ylab="Standardized residuals",xlab="Ages",
panel = panel.superpose,
 panel.groups = function(...) {
    panel.grid(v=-1,h=-1,lty=3)
    panel.xyplot(...)
},
scales=list(alternating=1,y=list(relation="free",rot=0)))
}
#- Plot the retrospective selectivity pattern
retroSelectivity <- function(x,yrs){

      res <- lapply(x,f)
      res <- lapply(res,function(y){y[which(y$year %in% (max(y$year)-20):(max(y$year)-1)),]})
      res <- lapply(res,function(y){cbind(y,retro=max(y$year))})
      res <- do.call(rbind,res)

      res <- subset(res,year %in% yrs)

      xyplot(value ~ age | as.factor(year),data=res,type="l",groups=retro,
      auto.key=list(space="right",points=FALSE,lines=TRUE,type="l"),main=paste("Retrospective pattern in F at age"),
      ylab="F",xlab="Ages",
      panel = panel.superpose,
       panel.groups = function(...) {
          panel.grid(v=-1,h=-1,lty=3)
          panel.xyplot(...)
      },
      scales=list(alternating=1,y=list(relation="free",rot=0)))
}

