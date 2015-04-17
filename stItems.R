stItems<-function(listItem,anchorNew,anchorOld) {
  shiftItems<-function(listItem,shiftAmount) {
    res<-list()
    length(res)<-length(listItem)
    for (i in seq_along(listItem)){
      if (listItem[[i]]$itemtype=="plx") 
        res[[i]]<-PL(name=listItem[[i]]$name,a=listItem[[i]]$a,b=listItem[[i]]$b+shiftAmount,
          c=listItem[[i]]$c,as=listItem[[i]]$as)
      else if (listItem[[i]]$itemtype=="pcx")
        res[[i]]<- Pcx(name=listItem[[i]]$name,as=listItem[[i]]$as,bk=listItem[[i]]$bk+shiftAmount)
      else
        stop("shift adjust Only implement plx, pcx")
    }
    res
  }

  shiftAmount<-mean(getBks(anchorOld))-mean(getBks(anchorNew))
  shiftItems(listItem,shiftAmount)
}
