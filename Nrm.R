Nrm <-function(name,ak,ck){
  # length(ak)=length(ck)=ncat-1
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
  # information need checked
  info<-function(thetas){
    ex<-0
    ex2<-0
    p<-pdf(thetas)
    for (i in 1:m) {
      ex<-ex+ak[i]*p[,i+1]
      ex2<-ex2+ak[i]*ak[i]*p[,i+1]
    }
    ex2-ex*ex
  }
  m<-length(ck)
  ncat<-m+1
  ak<-c(0,ak)
  a<-max(ak)
  as<-ak/a
  dk<-ak[-1]-ak[-length(ak)]
  bk<- -ck[1]/dk[1]
  if (ncat>2){
    for (i in 3:ncat){
      bk<-c(bk,-(ck[i-1]+sum(bk*dk[1:(i-2)]))/dk[i-1])
    }
  }
  ck<-c(0,ck)
  res<-list(name=name,nfac=1,itemtype=3,ncat=ncat,gpc=0,a=a,as=as,ak=ak,ck=ck,bk=bk,pdf=pdf,info=info)
  class(res)<-"uirt"
  res
}
