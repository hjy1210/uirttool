rItems<-function(items,thetas){
# items: list of uirt item
# thetas �N���ǥͯ�O�V�q
# return category random matrix with items(columns),latent thetas(rows)
# ��i,j�����N���ǥ�i�bitems[[j]]���������O
  rItem<-function(item,thetas){
  # item: an uirt item
  # thetas �N���ǥͯ�O�V�q
  # return category random variable with item,latent thetas
  # ��i�����N���ǥ�i�bitem ���������O
  prob<-item$pdf(thetas)
  apply(prob,1,function(x) {
      s<-cumsum(x)
      rnd<-runif(1)
      for (i in 1:(item$ncat-1)) if (rnd<s[i]) return(i-1)
      return(item$ncat-1)
      })
  }
  resp<-sapply(items,rItem,thetas)
  colnames(resp)<-sapply(items,function(item){
    item$name
  })
  resp
}