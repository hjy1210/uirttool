readTPcx<-function(mod){
  # Assume D=1
  B<-mod$B
  A<-mod$A
  xsi<-mod$xsi$xsi
  nxsi<-dim(A)[3]
  n<-dim(B)[1]
  m<-dim(B)[2]
  if (dim(B)[3]!=1) stop("Only implement for D=1")
  if (sum(A[,1,]==0)!=n*nxsi) stop("first category score should be zero")
  items<-list()
  for (i in 1:n){
    ck<-A[i,,]%*%xsi
    ck<-ck[!is.na(ck)]
    as<-B[i,2:length(ck),1]
    bk<-ck2bk(ak=as,ck=ck[-1])
    items[[i]]<-Pcx(name=paste0("V",i),as=as,bk=bk)
  }
  items
}
