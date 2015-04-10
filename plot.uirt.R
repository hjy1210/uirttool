plot.uirt<-function(item,thetas,type="crf") {
  ncat<-item$ncat
  revcump<-function(){
    p<-item$pdf(thetas)
    revcp<-p[,-1,drop=FALSE]
    col<-dim(revcp)[2]
    if (col>1){
      for (i in seq(col-1,1,by=-1)){
        revcp[,i]<-revcp[,i]+revcp[,i+1]
      }
    }
    revcp
  }
  plotCrf<-function(){
    p<-item$pdf(thetas)
    plot(thetas,p[,ncat],type="n",xlim=range(thetas),ylim=c(0,1),
      xlab="theta",ylab="probability",main=paste("CRF",ItemString(item,ndigit=3),sep="\r\n"))
    for (i in 1:ncat) {
      lines(thetas,p[,i],lty=20-i)
      index<-(1:length(thetas))[p[,i]==max(p[,i])][1]
      text(thetas[index],p[index,i],paste0("P",(i-1)))
    }
  }
  plotTrf<-function(){
    revcp<-revcump()
    plot(thetas,revcp[,ncat-1],type="n",xlim=range(thetas),ylim=c(0,1),
      xlab="theta",ylab="probability",main=paste("TRF",ItemString(item,ndigit=3),sep="\r\n"))
    for (i in 1:(ncat-1)) {
      lines(thetas,revcp[,i],lty=20-i)
      if (revcp[1,i]>0.5) 
        index<-1
      else if (revcp[length(thetas),i]<0.5) 
        index<-length(thetas)
      else 
        index<-(1:length(thetas))[revcp[,i]>=0.5][1]
      text(thetas[index],revcp[index,i],paste0("P",i))
    }
  }
  plotInf<-function(){
    Itheta<-item$info2(thetas)
    h<-0.001
    icfder<-(icf(thetas+h)-icf(thetas))/h
    plot(thetas,Itheta,type="l",xlim=range(thetas),
      xlab="theta",ylab="information",main=paste("INF",ItemString(item,ndigit=3),sep="\r\n"))
    points(thetas,icfder)
    legend("right",legend=c("circle icf'","line inf"))
  }
  icf<-function(thetas){
    p<-item$pdf(thetas)
    m<-item$ncat-1
    itemcf<-0
    for (i in 1:m){
      if (item$itemtype!="pl" && item$itemtype!="plx") 
        itemcf<-itemcf+item$ak[i]*p[,i+1]
      else {
        itemcf<-itemcf+item$ak*p[,i+1]
      }
    }
    itemcf
  }
  plotIcf<-function(){
    itemcf<-icf(thetas)
    plot(thetas,itemcf,type="l",xlim=range(thetas),
      xlab="theta",ylab="expect score",main=paste("icf",ItemString(item,ndigit=3),sep="\r\n"))
  }
  if(type=="crf")
    plotCrf()
  else if (type=="trf")
    plotTrf()
  else if (type=="inf")
    plotInf()  
  else if (type=="icf")
    plotIcf()
}

