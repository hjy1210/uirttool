Gpx <-function(name,a,dk,bk){
  ck<- -cumsum(a*dk*bk)
  Nrm(name=name,ak=cumsum(a*dk),ck=ck,itemtype="gpx")
}
