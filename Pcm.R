Pcm <-function(name,bk){
  ak<- 1:length(bk)
  ck<- -cumsum(bk)
  Nrm(name=name,ak=ak,ck=ck,a=1)
}
