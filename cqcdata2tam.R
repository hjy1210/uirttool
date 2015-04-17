cqcdata2tam <-function(sourcefile,cols,pattern="[ .]",names=NULL){
  lines<-readLines(sourcefile)
  lines<-gsub(pattern," ",lines)
  res<-as.numeric(substr(lines,cols[1],cols[1]))
  for (i in 2:length(cols)) {
    res<-cbind(res,as.numeric(substr(lines,cols[i],cols[i])))
  }
  res
}
