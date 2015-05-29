readPItems<-function(file){
    TgammaI <- function(m) {
      rbind(rep(0,m-1),diag(m-1))
    }
    TalphaI <- function(m) {
      if (m==2) rbind(0,1)
      else {
        res<-rbind(rep(0,m-2),diag(m-2))
        res<-cbind(rep(0,m-1),res)
        res<-rbind(res,c(m-1,rep(0,m-2)))
        res
      }
    }
    TcPC <- function(m) {
    # m=ncat
    mx<-matrix(0,nrow=m,ncol=m-1)
    mx[1,]<-rep(0,m-1)
    for (i in 2:m) {
        for (j in 1:(i-1)) {
        mx[i,j]<- -1
        }
    }
    mx  
    }
    TF<-function(m) {
        mx <- matrix(0, nrow = m, ncol = m - 1)
        mx[, 1] <- seq(0, m - 1)
        if (m>2) {   # add on 2014/08/14
        for (i in 2:(m - 1)) {
            for (j in 2:(m - 1)) {
                mx[i, j] <- sin(pi * (i - 1) * (j - 1)/(m - 1))
            }
        }
        }
        mx
    }

    lines<-readLines(file)
    linetokens<-strsplit(lines,c("[[:space:]]+"))
    res<-list()
    length(res)<-length(linetokens)
    for (i in seq_along(linetokens)){
      tokens<-linetokens[[i]]
      name<-tokens[1]
      values<-as.numeric(tokens[2:length(tokens)])
      nfac<-values[1]
      if (nfac!=1) stop("Only implement Dimension=1 in readPItems")
      itemtype<-values[2]
      if (itemtype==3){   # 3:nominal
        # name nfac itemtype ncat gpc a(nfac) aType alpha(ncat-1) cType g(ncat-1) ak(ncat) ck(ncat) b dk(ncat)
        ncat<-values[3]
        gpc<-values[4]
        ndx<-5
        a<-values[ndx:(ndx+nfac-1)]
        ndx<-ndx+nfac
        aType<-values[ndx]
        ndx<-ndx+1
        alpha<-values[ndx:(ndx+ncat-2)]
        ndx<-ndx+ncat-1
        if (aType==0)
          as <- TF(ncat) %*% alpha
        else if (aType==1)
          as <- TalphaI(ncat) %*% alpha
        ak<-a*as
        cType<-values[ndx]
        ndx<-ndx+1
        g<-values[ndx:(ndx+ncat-2)]
        ndx<-ndx+ncat-1
        ck<-NA
        if (cType==0)
          ck <- TF(ncat) %*% g
        else if (cType==1)
          ck <- TgammaI(ncat) %*% g
        if (a==1) 
          res[[i]]<-Pcx(name=name,bk=ck2bk(ak[-1],ck[-1]),as=as[-1])
        else 
          res[[i]]<-Gpx(name=name,a=a,bk=ck2bk(ak[-1],ck[-1]),as=as[-1])
      }
      else if (itemtype==2){
        ncat<-values[3]
        if (ncat>2) stop("Only implement ncat=2 in itemtype=2(Gr) 2PL")
        ndx<-4
        a<-values[ndx:(ndx+nfac-1)]
        ndx<-ndx+nfac
        ck<-values[ndx:(ndx+ncat-2)]
        ndx<-ndx+ncat-1
        res[[i]]<-Pcx(name=name,as=a,bk=-ck/a)
      }
      else if (itemtype==1){
        ncat<-values[3]
        a<-values[4]
        c<-values[5]
        lg<-values[6]
        res[[i]]<-PL(name=name,a=a,b=-c/a,c=plogis(lg))
      }
      else if (itemtype==0) {
        mu<-numeric(nfac)
        ndx<-3
        mu<-values[ndx:(ndx+nfac-1)]
        ndx<-ndx+nfac
        n<-nfac*(nfac+1)/2
        cov<-values[ndx:(ndx+n-1)]
        ndx<-ndx+n
        res[[i]]<-list(name=name,nfac=nfac,itemtype=itemtype,mu=mu,cov=cov)
        class(res[[i]])<-"pop"
      }
    }
    res
}
