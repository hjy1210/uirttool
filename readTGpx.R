readTGpx<-function(mod){
  I<-length(mod$item[[1]])
  items<-list()
  for (i in 1:I){
    m<-sum(!is.na(mod$A[i,,1]))
    ak<-mod$B[i,2:m,1]
    ck<-mod$A[i,1:m,]%*%mod$xsi$xsi
    ck<-ck[-1]
    bk<-ck2bk(ak,ck)
    a<-mod$gammaslope[i]
    as<-ak/a
    items[[i]]<-Gpx(name=paste0("V",i),a=a,as=as,bk=bk)
  }
  items
}
