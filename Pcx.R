Pcx <-function(name,dk,bk){
  ck<- -cumsum(dk*bk)
  Nrm(name=name,ak=cumsum(dk),ck=ck,itemtype="pcx")
}
