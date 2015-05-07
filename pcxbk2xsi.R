pcxbk2xsi<-function(sk,bk,type="PCM2"){
  pcm2<-function(dk,bk){
    delta<-sum(dk*bk)/K
    if (K==1) delta
    else c(delta,(dk*bk)[-K]-delta)
  }
  pcmor1pl<-function(dk,bk){
    dk*bk
  }
  if(length(sk)!=length(bk)) stop("sk and bk should be of the same length")
  K<-length(sk)
  if (K==1) dk<-sk
  else dk<-c(sk[1],sk[-1]-sk[-K])
  if (type=="PCM2") pcm2(dk,bk)
  else pcmor1pl(dk,bk)
}
