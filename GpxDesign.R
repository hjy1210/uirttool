GpxDesign<-function(listScores,resp){
  n<-length(listScores)
  Ks<-sapply(listScores,length)-1
  m<-max(Ks)+1
  getSE<-function(){
    starts<-numeric(n)
    ends<-numeric(n)
    start<-1
    for (i in 1:n){
      starts[i]<-start
      ends[i]<-start+Ks[i]-1
      start<-start+Ks[i]
    }
    list(starts=starts,ends=ends)
  }
  getE<-function(){
    E<-array(0,dim=c(n,m,1,n))
    for (i in 1:n){
      E[i,1:(Ks[i]+1),1,i]<-listScores[[i]]
    }
    E
  }
  list(E=getE(),SE=getSE(),Ks=Ks)
}
