writeRespPro<-function(resp,targetfile){
  resp<-as.matrix(resp)
  resp[is.na(resp)]<--1
  names<-colnames(resp)
  full<-rbind(names,matrix(as.character(resp),nrow=dim(resp)[1],ncol=dim(resp)[2]))
  res<-full[,1]
  if (dim(resp)[2]>1){
    for (i in 2:dim(resp)[2]) res<-paste(res,full[,i],sep="\t")
  }
  writeLines(res,targetfile)
}
