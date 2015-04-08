item<-Nrm("V1",ak=c(0.5,1),ck=c(1.4,0.5))
plot(item,thetas=seq(-6,6,len=100))

item2<-PL("V3",a=2,bk=0.5,g=0.2)
plot(item2,thetas=seq(-6,6,len=100))

item$name<-"HA"
item3<-eval(parse(text=ItemString(item)))
plot(item3,thetas=seq(-6,6,len=100))

item2$name<-"HJang"
item4<-eval(parse(text=ItemString(item2)))
plot(item4,thetas=seq(-6,6,len=100))
