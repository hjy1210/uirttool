PcxDesign<-function(listScores,resp){
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
  getB<-function(){
    B<-designMatrices(modeltype="PCM",resp=resp)$B
    if (dim(B)[1]!=n || dim(B)[2]!=m || dim(B)[3]!=1) stop("dim of B error")
    B[,,]<-0
    for (i in 1:n){
      B[i,1:(Ks[i]+1),1]<-listScores[[i]]
    }
    B
  }
  list(B=getB(),SE=getSE(),Ks=Ks)
}
