tcc<-function(items,scores,xpoints){
  acc<-numeric(length(xpoints))
  for (i in seq_along(items)) {
    ncat<-items[[i]]$ncat
    pdf<-items[[i]]$pdf(xpoints)
    for (j in 1:ncat) {
      acc<-acc+pdf[,j]*scores[[i]][j]
    }
  }
  acc
}
