embedRespWithNA<-function(resp,fulllength,respindexes){
  res<-matrix(NA,nrow=dim(resp)[1],ncol=fulllength)
  res[,respindexes]<-resp
  res
}
