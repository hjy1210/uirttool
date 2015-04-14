getBks<-function(items) {
  bk<-numeric(0)
  for (i in seq_along(items)) {
    if (!is.null(items[[i]]$bk)) bk<-c(bk,items[[i]]$bk)
  }
  bk
}
