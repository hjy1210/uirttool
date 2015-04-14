readCPcx<-function(listScores,prmfile,designfile){
  trim <- function (x) gsub("^\\s+|\\s+$", "", x)
  designM<-scan(designfile)
  col<-designM[1]
  designM<-matrix(data=designM[-1],ncol=col,byrow=TRUE)
  lines<-readLines(prmfile)
  lines<-strsplit(trim(lines),"\\s+")
  xsi<-sapply(lines,function(x){as.numeric(x[2])})
  ck<-designM %*% xsi
  items<-list()
  start<-1
  i<-1
  for (i in 1:length(listScores)){
    m<-length(listScores[[i]])
    bk<-ck2bk(ak=listScores[[i]][-1],ck=ck[(start+1):(start+m-1)])
    items[[i]]<-Pcx(name=paste0("V",i),as=listScores[[i]][-1],bk=bk)
    start<-start+m
  }
  items
}
