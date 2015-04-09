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
  if (comboType$getActive()>=0)
   plot(item,thetas=seq(-6,6,len=100),comboType$GetActiveText())
}
win <- gtkWindow(show = F)
da <- gtkDrawingArea()
asCairoDevice(da)
textbox<-gtkEntry()
textbox['width-request']<-300
textbox$setText("Nrm(\"nrm\",ak=c(0.4,1),ck=c(1.1,-1.2))")
gSignalConnect(textbox, "activate", textchanged)
combo <- gtkComboBoxNewText()
sapply( c( "Nrm(\"nrm\",ak=c(0.4,1),ck=c(1.1,-1.2))" , 
  "PL(\"PL\",a=1.2,b=0.8,c=0.1)",
  "PL(\"PL\",a=1.2,b=0.8,c=0.1,as=2)",
  "Gpc(\"gpc\",a=1.2,bk=c(0.8,1.3))",
  "Pcm(\"pcm\",bk=c(0.8,1.3))",  
  "Pcx(\"pcx\",as=c(0.5,1),bk=c(0.8,1.3))",  
  "Gpx(\"gpx\",a=1.2,as=c(0.5,1),bk=c(0.8,1.3))"  
  ) , combo$appendText)
gSignalConnect ( combo , "changed" ,
  f = function ( button , ... ) {
    if ( combo$getActive() < 0 || comboType$getActive()<0)
      ""
    else{
      textbox$setText(combo$getActiveText())
	  item<-eval(parse(text=textbox$getText()))
	  plot(item,thetas=seq(-6,6,len=100),type=comboType$getActiveText())
	}
  })
#gSignalConnect ( combo , "changed" ,
#  f = function ( button , ... ) {
#    if ( button$getActive() < 0)
#      ""
#    else{
#	  item<-eval(parse(text=button$getActiveText()))
#	  plot(item,thetas=seq(-6,6,len=100),type="trf")
#	}
#  })
comboType<-gtkComboBoxNewText()
sapply(c("crf","trf","inf","icf"),comboType$appendText)
gSignalConnect ( comboType , "changed" ,
  f = function ( button , ... ) {
    if ( combo$getActive() < 0 || comboType$getActive()<0)
      ""
    else{
      textbox$setText(combo$getActiveText())
	  item<-eval(parse(text=textbox$getText()))
	  plot(item,thetas=seq(-6,6,len=100),type=comboType$getActiveText())
	}
  })
vbox <- gtkVBox(FALSE)
hbox<-gtkHBox(FALSE)
hbox$packStart(textbox,fill=F,expand=F,padding=5)
hbox$packStart(combo,fill=F,expand=F,padding=5)
hbox$packStart(comboType,fill=F,expand=F,padding=5)
vbox$packStart(hbox,fill=F,expand=F,padding=5)
vbox$packStart(da)
win$add(vbox)
win$setDefaultSize(400,400)
win$showAll() 
