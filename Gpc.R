Gpc <-function(name,a,bk){
  ak<-a*(1:length(bk))
  ck<- -cumsum(a*bk)
  Nrm(name=name,ak=ak,ck=ck,itemtype="gpc")
}
