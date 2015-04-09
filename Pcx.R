Pcx <-function(name,as,bk){
  ds<-c(0,as)
  dk<-ds[-1]-ds[-length(ds)]
  ck<- -cumsum(dk*bk)
  Nrm(name=name,ak=as,ck=ck,a=1,as=as)
}
