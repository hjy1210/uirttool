tamresp2cqcdat<-function(resp){
  cqcresp<-as.character(resp)
  cqcresp[is.na(cqcresp)]<-"."
  dim(cqcresp)<-dim(resp)
  res<-""
  for (i in 1:dim(cqcresp)[2]) res<-paste0(res,cqcresp[,i])
  res
}
