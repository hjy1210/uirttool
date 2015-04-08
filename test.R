source("plot.uirt.R")
source("ItemString.R")
source("Nrm.R")
source("PL.R")
source("Pcm.R")
source("Pcx.R")
source("Gpc.R")
source("Gpx.R")

library(RGtk2)
require(cairoDevice)
textchanged<-function(tbx){
  item<-eval(parse(text=textbox$getText()))
  plot(item,thetas=seq(-6,6,len=100))
}
win <- gtkWindow(show = F)
da <- gtkDrawingArea()
asCairoDevice(da)
textbox<-gtkEntry()
textbox['width-request']<-300
textbox$setText("Nrm(\"nrm\",ak=c(0.4,1),ck=c(1.1,-1.2))")
gSignalConnect(textbox, "activate", textchanged)
combo <- gtkComboBoxNewText()
sapply( c( "Nrm(\"nrm\",ak=c(0.4,1),ck=c(1.1,-1.2))" , "PL(\"PL\",a=1.2,bk=0.8,g=0.1)") , combo$appendText)
gSignalConnect ( combo , "changed" ,
  f = function ( button , ... ) {
    if ( button$getActive() < 0)
      ""
    else{
	  item<-eval(parse(text=button$getActiveText()))
	  plot(item,thetas=seq(-6,6,len=100))
	}
  })
#scale_cb <- function(range) { plot(1:20,(1:20)^range$getValue()) }
#s <- gtkHScale(,0.05, 2.00, 0.05)
#gSignalConnect(s, "value-changed", scale_cb)
vbox <- gtkVBox(FALSE)
hbox<-gtkHBox(FALSE)
hbox$packStart(textbox,fill=F,expand=F,padding=5)
hbox$packStart(combo,fill=F,expand=F,padding=5)
vbox$packStart(hbox,fill=F,expand=F,padding=5)
vbox$packStart(da)
#vbox$packStart(s, FALSE)
win$add(vbox)
win$setDefaultSize(400,400)
win$showAll() 
#s$setValue(1.00)


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

item5<-Pcm(name="pcm",bk=c(1.2,1.5))

item6<-Pcx(name="pcx",dk=c(0.4,0.6),bk=c(1.1,1.3))

item7<-Gpc("gpc",a=1.2,bk=c(-1.1,1.2))

item8<-Gpx("gpx",a=1.1,dk=c(0.6,0.4),bk=c(-0.5,0.6))
