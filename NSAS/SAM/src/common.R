read.fit<-function(file){
  # Function to read a basic fit
  ret<-list()
  parfile<-as.numeric(scan(paste(file,'.par', sep=''), 
                      what='', n=16, quiet=TRUE)[c(6,11,16)])
  ret$nopar<-as.integer(parfile[1])
  ret$nlogl<-parfile[2]
  ret$maxgrad<-parfile[3]
  rep<-scan(paste(file,'.rep', sep=''), quiet=TRUE)
  ret$res<-read.table(paste(file,'.res', sep=''),header=FALSE)
  ret$stateDim<-rep[1]
  ret$years<-rep[-1]
  file<-paste(file,'.cor', sep='')
  lin<-readLines(file)
  ret$npar<-length(lin)-2
  ret$logDetHess<-as.numeric(strsplit(lin[1], '=')[[1]][2])
  sublin<-lapply(strsplit(lin[1:ret$npar+2], ' '),function(x)x[x!=''])
  ret$names<-unlist(lapply(sublin,function(x)x[2]))
  ret$est<-as.numeric(unlist(lapply(sublin,function(x)x[3])))
  ret$std<-as.numeric(unlist(lapply(sublin,function(x)x[4])))

  ret$cor<-matrix(NA, ret$npar, ret$npar)
  for(i in 1:ret$npar){
    ret$cor[1:i,i]<-as.numeric(unlist(lapply(sublin[i],
      function(x)x[5:(4+i)])))
    ret$cor[i,1:i]<-ret$cor[1:i,i]
  }
  ret$cov<-ret$cor*(ret$std%o%ret$std)
  mslh<-function(name){
    idx<-which(ret$names==name)
    x<-cbind(ret$est[idx], ret$std[idx], ret$est[idx]-2*ret$std[idx], 
             ret$est[idx]+2*ret$std[idx])
    colnames(x)<-c('est', 'std', 'low', 'hig')
    return(x)
  }
  ret$ssb<-mslh('ssb')
  ret$fbar<-mslh('fbar')
  ret$tsb<-mslh('tsb')
  ret$logssb<-mslh('logssb')
  ret$logfbar<-mslh('logfbar')
  ret$logtsb<-mslh('logtsb')
  ret$logscale<-mslh('logScale')
  ret$logFpar<-mslh('logFpar')
  ret$logCatch<-mslh('logCatch')
  x<-mslh('U')
  ret$stateEst<-matrix(x[,1],ncol=ret$stateDim, byrow=TRUE)
  ret$stateStd<-matrix(x[,2],ncol=ret$stateDim, byrow=TRUE)
  ret$stateLow<-matrix(x[,3],ncol=ret$stateDim, byrow=TRUE)
  ret$stateHig<-matrix(x[,4],ncol=ret$stateDim, byrow=TRUE)
  ret$R<-cbind(exp(ret$stateEst[,1]), NA, exp(ret$stateLow[,1]), 
               exp(ret$stateHig[,1]))
  return(ret)
}

baseplot<-function(fit,name, trans=function(x)x, makeroom=NA, drop=0, ci=TRUE, line=TRUE, ...){
  idx<-1:(nrow(fit[[name]])-drop)
  est<-trans(fit[[name]][idx,1])
  low<-trans(fit[[name]][idx,3])
  hig<-trans(fit[[name]][idx,4])
  years<-fit$years[idx]
  plot(years,est,ylim=range(low,hig,0, makeroom, na.rm=TRUE), type='n', xlab='Year', ...)
  if(ci)polygon(c(years, rev(years)), c(low, rev(hig)), col = "thistle2", border = "thistle2")
  if(line)lines(years,est, lwd=3)
}

addlines<-function(fit,name, trans=function(x)x, with.dot=FALSE, drop=0, ...){
  idx<-1:(nrow(fit[[name]])-drop)
  est<-trans(fit[[name]][idx,1])
  low<-trans(fit[[name]][idx,3])
  hig<-trans(fit[[name]][idx,4])
  years<-fit$years[idx]
  lines(years,est, ...)
  lines(years,low, lty='dashed',...)
  lines(years,hig, lty='dashed',...)
  if(with.dot==TRUE)points(years[length(years)],est[length(est)], pch=19, ...)
}

bp<-function(x,y,v, scale=3, ...){
  plot(x,y,cex=sqrt(abs(v))*scale, col=ifelse(v<0,'seagreen3','blue'), pch=ifelse(v<0,16,1), ...)
  points(x[v>0],y[v>0],cex=sqrt(v[v>0])*scale, col='blue', pch=1, ...)
}

xtab<-function(x,caption='Table X.', file=stdout(), width='"100%"', cornername='', dec=rep(1,ncol(x))){
  nc<-ncol(x)
  lin<-paste('<table width=',width,'>', sep='')
  lin<-c(lin,sub('$','</td></tr>',sub('\\. |\\.$','.</b> ',
         sub('^', paste('<tr><td colspan=',nc+1,'><b>',sep=''), caption))))
  hr<-paste('<tr><td colspan=',nc+1,'><hr noshade></td></tr>', sep='')
  lin<-c(lin,hr)
  cnames<-colnames(x)
  cnames<-paste(sub('$','</b></td>',sub('^','<td align=right><b>',cnames)), collapse='\t')
  lin<-c(lin,paste('<tr>',paste('<td align=left><b>',cornername,'</b></td>',sep=''),cnames,'</tr>'))
  lin<-c(lin,hr)
  rnames<-sub('$','</b></td>',sub('^','<tr> <td align=left><b>',rownames(x)))
  #x<-sapply(1:ncol(x),function(i)sub('NA','  ',format(round(x[,i],dec[i]))))
  x<-sapply(1:ncol(x),function(i)sub('NA','  ',formatC(round(x[,i],dec[i]),digits=dec[i], format='f')))
  for(i in 1:nrow(x)){
    thisline<-paste(rnames[i],paste(sub('$','</td>',sub('^','<td align=right>',x[i,])), collapse='\t'),'</tr>', sep='')
    lin<-c(lin,thisline)
  }
  lin<-c(lin,hr)
  lin<-c(lin,'</table><br>\n')
  writeLines(lin,con=file)
}

xtab2<-function(x,caption='Table X.', file=stdout(), width='"100%"', cornername='', dec=matrix(1,ncol=ncol(x),nrow=nrow(x))){
  nc<-ncol(x)
  lin<-paste('<table width=',width,'>', sep='')
  lin<-c(lin,sub('$','</td></tr>',sub('\\. |\\.$','.</b> ',
         sub('^', paste('<tr><td colspan=',nc+1,'><b>',sep=''), caption))))
  hr<-paste('<tr><td colspan=',nc+1,'><hr noshade></td></tr>', sep='')
  lin<-c(lin,hr)
  cnames<-colnames(x)
  cnames<-paste(sub('$','</b></td>',sub('^','<td align=right><b>',cnames)), collapse='\t')
  lin<-c(lin,paste('<tr>',paste('<td align=left><b>',cornername,'</b></td>',sep=''),cnames,'</tr>'))
  lin<-c(lin,hr)
  rnames<-sub('$','</b></td>',sub('^','<tr> <td align=left><b>',rownames(x)))
  xx<-x
  for(i in 1:nrow(x)){
    for(j in 1:ncol(x)){
      x[i,j]<-sub('NA','  ',formatC(round(xx[i,j],dec[i,j]),digits=dec[i,j], format='f'))
    }
  }

  for(i in 1:nrow(x)){
    thisline<-paste(rnames[i],paste(sub('$','</td>',sub('^','<td align=right>',x[i,])), collapse='\t'),'</tr>', sep='')
    lin<-c(lin,thisline)
  }
  lin<-c(lin,hr)
  lin<-c(lin,'</table><br>\n')
  writeLines(lin,con=file)
}

civec<-function(fit){
  i1<-which(fit$names=='fbar')
  i1<-i1[length(i1)-1]
  i2<-which(fit$names=='ssb')
  i2<-i2[length(i2)-1]
  return(c(as.vector(fit$cov[c(i1,i2), c(i1,i2)]),fit$est[c(i1,i2)]))
}

.CI.reg<-function (x, level = 0.95, npoints = 100, col = "blue", border = 0, 
                   density = 20, lwd = 0.1 * par("lwd"), ...) {
    t.quan <- sqrt(qchisq(level, 2))
    centre <- x[5:6]
    x <- matrix(x[1:4], 2, 2)
    r <- x[1, 2]
    scale <- sqrt(diag(x))
    if (scale[1] > 0) {
        r <- r/scale[1]
    }
    if (scale[2] > 0) {
        r <- r/scale[2]
    }
    r <- min(max(r, -1), 1)
    d <- acos(r)
    a <- seq(0, 2 * pi, len = npoints)
    polygon(matrix(c(t.quan * scale[1] * cos(a + d/2) + centre[1], 
        t.quan * scale[2] * cos(a - d/2) + centre[2]), npoints, 
        2), col = col, border = border, density = density, lwd = lwd, 
        ...)
}

dummyplot<-function(text='This plot is intentionally left blank'){
  plot(c(0,1),c(0,1),axes=FALSE, xlab='', ylab='' , type='n')
  box()
  text(.5,.5,labels=text)
}

   