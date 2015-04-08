plot.uirt<-function(item,thetas,type="CRF") {
  ncat<-item$ncat
  p<-item$pdf(thetas)
  
  plot(thetas,p[,ncat],type="n",xlim=range(thetas),ylim=c(0,1),
    xlab="theta",ylab="probability",main=paste(ItemToString(item),"CRF"))
  for (i in 1:ncat) {
    lines(thetas,p[,i],lty=20-i)
    index<-(1:length(thetas))[p[,i]==max(p[,i])][1]
    text(thetas[index],p[index,i],paste0("P",(i-1)))
  }
}

