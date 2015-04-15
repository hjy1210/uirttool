# add readTGpx,GpxDesign, modify tcc
# test Gpx model with tam.mml.3pl and tam.mml.wle2
rm(list=ls())
setwd("L:/github/uirttool")
source("Pcx.R")
source("Gpx.R")
source("Nrm.R")
source("readTPcx.R")
source("readTGpx.R")
source("readCPcx.R")
source("likelihood.R")
source("tcc.R")
source("PcxDesign.R")
source("GpxDesign.R")
source("ck2bk.R")
source("getBks.R")
library(TAM)
setwd("g:/")
data(data.gpcm)
dat<-as.matrix(data.gpcm)
dat[,2][dat[,2]==3]<-2
#scores<-list(c(0,0.9,2.1,3),c(0,1.1,2),c(0,1,1.8,3))
scores<-list(0:3,0:2,0:3)
E<-GpxDesign(scores,dat)
mod2<-tam.mml.2pl(resp=dat,irtmodel="GPCM",est.variance=FALSE)
mod3<-tam.mml.3pl(resp=dat,E=E$E,est.variance=FALSE)
# compare mod2$beta and mod3$beta
c(mod2$beta,mod3$beta)
# compare mod2$variance and mod3$variance
c(mod2$variance,mod3$variance)
#compare mod2$A and mod3$A
sum(mod2$A==mod3$A,na.rm=TRUE)
sum(is.na(mod2$A==mod3$A))
#compare mod2$xsi and mod3$xsi
mod2$xsi$xsi
mod3$xsi$xsi
mod2$xsi$xsi-mod3$xsi$xsi

### starting test tam.mml.3pl with Gpx
scores<-list(c(0,0.9,2.1,3),c(0,1.1,2),c(0,1,1.8,3))
E<-GpxDesign(scores,dat)
mod<-tam.mml.3pl(resp=dat,E=E$E,est.variance=FALSE)
items<-readTGpx(mod)
# check mod$rprobs with item$pdf
for (i in seq_along(items)){
  p<-items[[i]]$pdf(mod$theta)
  for (k in 1:items[[i]]$ncat){
    print(max(abs(mod$rprobs[i,k,]-p[,k])))
  }
}
# check like by likelihood
like<-t(apply(dat,1,likelihood,items=items,thetas=mod$theta))
max(abs(like-mod$like))
# check post 
sd<-sqrt(mod$variance)
mu<-mod$beta
prior<-dnorm(mod$theta,mu,sd)
prior<-prior/(sum(prior))
post<-t(apply(like,1,function(x){
  x*prior/sum(x*prior)
}))
max(abs(post-mod$post))
# check EAP
eap<-apply(post,1,function(x){
  sum(x*mod$theta)
})
max(abs(eap-mod$person$EAP))

# compare MLE with tcc, should use tam.mml.wle2 instead of tam.wle
est<-tam.mml.wle2(mod,WLE=FALSE)
mle<-est[,c("PersonScores","theta")]
mle<-unique(mle)
mle<-mle[order(mle$PersonScores),]
rownames(mle)<-NULL
mle$tcc<-tcc(items,thetas=mle$theta)
mle$tccdiff<-mle$tcc-mle$PersonScores
mle
max(abs(mle$tccdif))
