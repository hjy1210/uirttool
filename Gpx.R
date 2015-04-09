Gpx <-function(name,a,as,bk){
  ds<-c(0,as)
  dk<-ds[-1]-ds[-length(ds)]
  ck<- -cumsum(a*dk*bk)
  Nrm(name=name,ak=a*as,ck=ck,a=a,as=as)
}
