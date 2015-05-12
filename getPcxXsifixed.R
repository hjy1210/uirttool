getPcxXsifixed<-function(fullscores,itemindexes,items,type){
  pcxbk2xsi<-function(sk,bk,type="PCM2"){
    pcm2<-function(dk,bk,dk2){
      delta<-sum(dk*bk)/sum(dk2)
      if (K==1) delta
      else c(delta,(dk*bk)[-K]-delta*dk2[-K])
    }
    pcmor1pl<-function(dk,bk){
      dk*bk
    }
    if(length(sk)!=length(bk)) stop("sk and bk should be of the same length")
    K<-length(sk)
    if (K==1) dk<-sk
    else dk<-c(sk[1],sk[-1]-sk[-K])
    if (type=="PCM2") tamfixed<-pcm2(dk,bk,rep(1,K))
    else tamfixed<-pcmor1pl(dk,bk)
    cqcfixed<-pcm2(dk,bk,dk)
    list(tamfixed=tamfixed,cqcfixed=cqcfixed)
  }
  Ks<-sapply(fullscores,function(score){
    length(score)-1
  })
  tamfixed<-c()
  cqcfixed<-c()
  taustart<-length(Ks)+cumsum(c(0,Ks-1))+1
  start<-cumsum(c(0,Ks))+1
  for (i in seq_along(itemindexes)){
    if (Ks[itemindexes[i]]!=items[[i]]$ncat-1){
      stop(paste("getPcxXsififed i=",i,"Ks[itemindexes[i]]=",Ks[itemindexes[i]],
      "!=items[[i]]$ncat-1",items[[i]]$ncat-1))
    }
    if (max(abs(fullscores[[itemindexes[i]]][-1]-items[[i]]$as))>0.0001) {
      stop(paste("getPcxXsififed i=",i,"score not match"))
    }
    xsis<-pcxbk2xsi(items[[i]]$as,items[[i]]$bk,type)
    index<-itemindexes[i]
    cqcfixed<-rbind(cqcfixed,c(index,xsis$cqcfixed[1]))
    if (Ks[index]>1 )cqcfixed<-rbind(cqcfixed,cbind(taustart[index]:(taustart[index]+Ks[index]-2),
        xsis$cqcfixed[-1]))
    if (type=="PCM2") {
      tamfixed<-rbind(tamfixed,c(index,xsis$tamfixed[1]))
      if (Ks[index]>1 )tamfixed<-rbind(tamfixed,cbind(taustart[index]:(taustart[index]+Ks[index]-2),
        xsis$tamfixed[-1]))
    }
    else
      tamfixed<-rbind(tamfixed,cbind(start[index]:(start[index]+Ks[index]-1),
        xsis$tamfixed))
  }
  list(tamfixed=tamfixed,cqcfixed=cqcfixed)
}