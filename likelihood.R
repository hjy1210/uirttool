likelihood<-function(resp,items,thetas){
  p<-1
  for (i in 1:length(items)){
    if (!is.na(resp[i])) p=p*items[[i]]$pdf(thetas)[,resp[i]+1]
  }
  p
}