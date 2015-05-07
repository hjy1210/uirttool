rm(list=ls())
library(TAM)
setwd("L:/github/uirttool")
source("uirttool.txt")
data(data.gpcm)
modpcm2<- tam.mml( resp=data.gpcm , irtmodel="PCM2",est.variance=TRUE )
modpcm2$B
modpcm2$A
modpcm<- tam.mml( resp=data.gpcm , irtmodel="PCM",est.variance=TRUE )
modpcm$B
modpcm$A
mod1pl<- tam.mml( resp=data.gpcm , irtmodel="1PL",est.variance=TRUE )
mod1pl$B
mod1pl$A
max(abs(mod1pl$xsi$xsi-modpcm$xsi$xsi))
scores<-lapply(1:3,function(i){
  if (i==1) c(0,1,2,3)
  else if (i==2) c(0,0.9,2,3)
  else c(0,0.8,1.8,3)
})
AB<-PcxDesign(scores,resp=data.gpcm)
modab1pl<- tam.mml( resp=data.gpcm ,B=AB$B, irtmodel="1PL" ,est.variance=TRUE)
modab1pl$B
modab1pl$A
modabpcm<- tam.mml( resp=data.gpcm ,B=AB$B, irtmodel="PCM",est.variance=TRUE )
modabpcm$B
modabpcm$A
max(abs(modab1pl$xsi$xsi-modabpcm$xsi$xsi))
modabpcm2<- tam.mml( resp=data.gpcm ,B=AB$B, irtmodel="PCM2",est.variance=TRUE )
modabpcm2$B
modabpcm2$A
# transfrom bk parameters of Pcx to xsi parameters of tam.mml
# verify pcxbk2xsi of type "PCM2"
pcm2abitems<-readTPcx(modabpcm2)
p1<-pcxbk2xsi(pcm2abitems[[1]]$ak,pcm2abitems[[1]]$bk,type="PCM2")
p2<-pcxbk2xsi(pcm2abitems[[2]]$ak,pcm2abitems[[2]]$bk,type="PCM2")
p3<-pcxbk2xsi(pcm2abitems[[3]]$ak,pcm2abitems[[3]]$bk,type="PCM2")
p<-c(p1[1],p2[1],p3[1],p1[-1],p2[-1],p3[-1])
names(p)<-NULL
p
modabpcm2$xsi$xsi
p-modabpcm2$xsi$xsi
# verify pcxbk2xsi of type "PCM"
pcmabitems<-readTPcx(modabpcm)
p1<-pcxbk2xsi(pcmabitems[[1]]$ak,pcmabitems[[1]]$bk,type="PCM")
p2<-pcxbk2xsi(pcmabitems[[2]]$ak,pcmabitems[[2]]$bk,type="PCM")
p3<-pcxbk2xsi(pcmabitems[[3]]$ak,pcmabitems[[3]]$bk,type="PCM")
p<-c(p1,p2,p3)
names(p)<-NULL
p
modabpcm$xsi$xsi
p-modabpcm$xsi$xsi
# verify pcxbk2xsi of type "1PL"
plabitems<-readTPcx(modab1pl)
p1<-pcxbk2xsi(plabitems[[1]]$ak,plabitems[[1]]$bk,type="1PL")
p2<-pcxbk2xsi(plabitems[[2]]$ak,plabitems[[2]]$bk,type="1PL")
p3<-pcxbk2xsi(plabitems[[3]]$ak,plabitems[[3]]$bk,type="1PL")
p<-c(p1,p2,p3)
names(p)<-NULL
p
modab1pl$xsi$xsi
p-modab1pl$xsi$xsi
# verify xsi.fixed of tam.mml
p<-pcxbk2xsi(pcm2abitems[[1]]$ak,pcm2abitems[[1]]$bk,type="PCM2")
xsifiled<-cbind(c(1,4,5),p)
mod6_1<-tam.mml(resp=data.gpcm,irtmodel="PCM2",xsi.fixed=xsifiled,
constraint="none",est.variance=TRUE)
mod6_2<-tam.mml(resp=data.gpcm,irtmodel="PCM2",xsi.fixed=xsifiled,
beta.fixed=FALSE,est.variance=TRUE)
items6_1<-readTPcx(mod6_1)
items6_2<-readTPcx(mod6_1)
getBks(pcm2abitems)
getBks(items6_1)
getBks(items6_2)
getBks(items6_1)-getBks(pcm2abitems)
getBks(items6_2)-getBks(pcm2abitems)
getBks(items6_2)-getBks(items6_1)


