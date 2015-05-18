tcc<-function(items,thetas,scores=NULL){
  if (is.null(scores)){
    scores<-lapply(items,function(item){
      c(0,item$as) #c(0,item$ak)
      })
  }
  if (length(items)!=length(scores)) stop("items and scores should be of equal length")
  for (i in 1:length(items))
    if (length(scores[[i]])!=items[[i]]$ncat) 
      stop("length(scores[[i]])!=items[[i]].ncat")
  acc<-numeric(length(thetas))
  for (i in seq_along(items)) {
    ncat<-items[[i]]$ncat
    pdf<-items[[i]]$pdf(thetas)
    for (j in 1:ncat) {
      acc<-acc+pdf[,j]*scores[[i]][j]
    }
  }
  acc
}
