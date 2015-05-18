Nrm <-function(name,ak,ck,a=NULL,as=NULL){
  # length(ak)=length(ck)=ncat-1
  m<-length(ck)
  ncat<-m+1
  if (is.null(a)){
    itemtype<-"nrm"
    ak<-c(0,ak)
  } else if (a==1){
    if (is.null(as)){
      itemtype<-"pcm"
      as<-1:m
    } else {
      itemtype<-"pcx"
    }
    ak<-c(0,as)
  } else {
    if (is.null(as)){
      itemtype<-"gpc"
      as<- 1:m
    } else {
      itemtype<-"gpx"
    }
    ak<-c(0,a*as)
  }
  dk<-ak[-1]-ak[-length(ak)]
  bk<- -ck[1]/dk[1]
  if (ncat>2){
    for (i in 3:ncat){
      bk<-c(bk,-(ck[i-1]+sum(bk*dk[1:(i-2)]))/dk[i-1])
    }
  }
  ck<-c(0,ck)
  pdf<-function(thetas){
    den<-1
    num<-matrix(0,nrow=length(thetas),ncol=ncat)
    num[,1]<-1
    for (i in 2:ncat) {
      num[,i]<-exp(ak[i]*thetas+ck[i])
      den<-den+num[,i]
    }
    for (i in 1:ncat) {
      num[,i]<-num[,i]/den
    }
    num
  }
  # information function info2 is also correct, reson as follows:
  # log(P[x=k])=a_k\theta+c_k-log(1+\sum_{i=1}^K exp(a_i\theta+c_i))
  # -\frac{\partial^2 log(P[x=k])}{\partial \theta^2}=\sum_{i=1}^K a_i^2 P[X=i]-(\sum_{i=1}^K a_i P[X=i])^2, independent of k
  # E[-\frac{\partial^2 log(P[x=k])}{\partial \theta^2}]=\sum_{i=1}^K a_i^2 P[X=i]-(\sum_{i=1}^K a_i P[X=i])^2
  info2<-function(thetas){
    ex<-0
    ex2<-0
    p<-pdf(thetas)
    for (i in 1:m) {
      ex<-ex+ak[i+1]*p[,i+1]
      ex2<-ex2+ak[i+1]*ak[i+1]*p[,i+1]
    }
    ex2-ex*ex
  }
  # Baker, Item Response Theory: parameter estimation techniques 2004
  # page 236/254, table 9.1/9.2
  # use following three commented statements to test
  # item<-Nrm("V1",ak=c(-0.6,1.2),ck=c(1,1.25))
  # thetas<-seq(-3,3,by=0.5)
  # item$info(thetas)

  infoj<-function(theta){
    p<-c(pdf(theta))
    w<-diag(p)-outer(p,p)
    c(ak %*% w %*%ak)
  }
  info<-function(thetas){
    sapply(thetas,function(x){
      infoj(x)
    })
  }
  res<-list(name=name,nfac=1,itemtype=itemtype,ncat=ncat,a=a,as=as,ak=ak[-1],
  ck=ck[-1],bk=bk,pdf=pdf,info=info,info2=info2)
  class(res)<-"uirt"
  res
}
