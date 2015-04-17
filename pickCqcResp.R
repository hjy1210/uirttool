pickCqcResp<-function(sourcefile,subjectindex,columnindex,targetfile){
  lines<-readLines(sourcefile)
  lines<-lines[subjectindex]
  res<-substr(lines,columnindex[1],columnindex[1])
  for (i in 2:length(columnindex))
    res<-paste0(res,substr(lines,columnindex[i],columnindex[i]))
  writeLines(res,targetfile)
}
