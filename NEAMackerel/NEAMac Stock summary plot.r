######################################################################################################
# NEA Mackerel Stock summary plot
######################################################################################################
NEAmac.stock.summary.plot <- function(stck) {
  #Make stock summary plots (ie SSB, Fbar, recs)
  summary.data <- as.data.frame(FLQuants(SSB=ssb(stck),"Fbar4-8"=fbar(stck),Recruits=1000*rec(stck),Landings=catch(stck)))
  scaling.factors <- tapply(summary.data$data,summary.data$qname,function(x) trunc(log10(max(pretty(c(0,x))))/3)*3)
  summary.data$data <- summary.data$data/10^scaling.factors[summary.data$qname]
  ylabels <- apply(cbind(lbl=names(scaling.factors),fctr=scaling.factors),1,function(x) {
  if(x[2]=="0"){x[1]}else {bquote(expression(paste(.(x[1])," [",10^.(x[2]),"]"))) }})
  summary.plot <- xyplot(data~year|qname,data=summary.data,
                prepanel=function(...) {list(ylim=range(pretty(c(0,list(...)$y))))},
                main=list(paste(stck@name,"Stock Summary Plot"),cex=0.9),
                ylab=do.call(c,ylabels),
                xlab="Year",
                layout=c(1,4),
                type="l",
                panel=function(...) {
                panel.grid(h=-1,v=-1)
                if(panel.number()==4) {  panel.xyplot(...,col="black")
                                         #panel.abline(h= 2.2)
                                         panel.abline(h= 2.3,lty="dotted")
                                         panel.abline(h= 1.67,lty="dashed")
                                         panel.text(1972,1.67+0.25," Blim",cex=0.8)
                                         panel.text(1972,2.3+0.25,"Bpa",cex=0.8)
                 }
                if(panel.number()==1) {  panel.xyplot(...,col="black")
                                         panel.abline(h= 0.23,lty="dotted")
                                         panel.abline(h= 0.42,lty="dashed")
                                         panel.text(1972,0.23+0.025,"Fpa",cex=0.8)
                                         panel.text(1972,0.42+0.025,"Flim",cex=0.8)
                 }
                if(panel.number()==3) { #Do recruits as bar plot
                panel.barchart(...,horizontal=FALSE,origin=0,box.width=1,col="grey")

                } else {
                panel.xyplot(...,col="black")
                }
                },
                scales=list(alternating=1,y=list(relation="free",rot=0)))

  print(summary.plot)
}

