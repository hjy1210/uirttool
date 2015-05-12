rm(list=ls())
setwd("L:/github/uirttool")
source("uirttool.txt")
setwd("g:/")
sco<-lapply(1:67,function(i){c(0,1.25,2.5)})

fullitems<-lapply(1:67,function(i){Pcx(name=paste0("V",i),
	as=c(1.25,2.5),bk=rbeta(2,1.1,1.2))})
thetas<-rnorm(3000,0,1.4)
resp<-rItems(fullitems,thetas)
library(TAM)
AB<-PcxDesign(sco,resp)
fixedpcm2<-getPcxXsifixed(sco,55:67,fullitems[55:67],"PCM2")
modpcm2<-tam.mml(resp=resp,B=AB$B,irtmodel="PCM2",beta.fixed=FALSE,est.variance=TRUE,
  xsi.fixed=fixedpcm2$tamfixed)
estpcm2<-readTPcx(modpcm2)
fixedpcm<-getPcxXsifixed(sco,55:67,fullitems[55:67],"PCM")
modpcm<-tam.mml(resp=resp,B=AB$B,irtmodel="PCM",beta.fixed=FALSE,est.variance=TRUE,
  xsi.fixed=fixedpcm$tamfixed)
estpcm<-readTPcx(modpcm)

difpcm2<-getBks(estpcm2)-getBks(fullitems)
names(difpcm2)<-NULL
difpcm2
difpcm<-getBks(estpcm)-getBks(fullitems)
names(difpcm)<-NULL
difpcm

writeLines(tamresp2cqcdat(resp),"cqcta.dat")
write.table(fixedpcm2$cqcfixed,file="cqcfixpcm2.prm",row.names=FALSE,
  col.names=FALSE)
cmds<-createCCmds(datafile="cqcta.dat",
    responses="1-67",constraints="none",
    importprmfile="cqcfixpcm2.prm",
    exportprmfile="cqcpcm2.prm",exportdesignmatrixfile="cqcpcm2.des",
    Model="item+item*step",showmlefile="cqcpcm2.mle",
    exportregfile="cqcpcm2.beta",exportcovfile="cqcpcm2.cov",
    Scores=c("score (0 1 2) (0 1.25 2.5) !items(1-67);"),
    quit=TRUE)
writeLines(cmds,"cqcpcm2.cqc")
system("\"L:\\Acer Conquest3\\conquest3console.exe\" cqcpcm2.cqc;")
cqcpcm2<-readCPcx(listScores=sco,
             prmfile="cqcpcm2.prm",designfile="cqcpcm2.des")

write.table(fixedpcm$cqcfixed,file="cqcfixpcm.prm",row.names=FALSE,
  col.names=FALSE)
cmds<-createCCmds(datafile="cqcta.dat",
    responses="1-67",constraints="none",
    importprmfile="cqcfixpcm.prm",
    exportprmfile="cqcpcm.prm",exportdesignmatrixfile="cqcpcm.des",
    Model="item+item*step",showmlefile="cqcpcm.mle",
    exportregfile="cqcpcm.beta",exportcovfile="cqcpcm.cov",
    Scores=c("score (0 1 2) (0 1.25 2.5) !items(1-67);"),
    quit=TRUE)
writeLines(cmds,"cqcpcm.cqc")
system("\"L:\\Acer Conquest3\\conquest3console.exe\" cqcpcm.cqc;")
cqcpcm<-readCPcx(listScores=sco,
             prmfile="cqcpcm.prm",designfile="cqcpcm.des")

difcqcpcm2<-getBks(cqcpcm2)-getBks(fullitems)
names(difcqcpcm2)<-NULL
difcqcpcm2
difcqcpcm<-getBks(cqcpcm)-getBks(fullitems)
names(difcqcpcm)<-NULL
difcqcpcm
