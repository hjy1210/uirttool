ck2bk<-function(ak,ck) {
  if (length(ak)!=length(ck)) stop("length(ak)==length(ck) is resuired")
  K<-length(ck)
  ak<-c(0,ak)
  dk<-ak[-1]-ak[-length(ak)]
  bk<-numeric(K)
  bk[1]<- ck[1]/(-dk[1])
  if (K>1){
    for (k in 2:K){
      bk[k]<-(ck[k]+sum(bk[1:(k-1)]*dk[1:(k-1)]))/(-dk[k])
    }
  }
  bk
}
