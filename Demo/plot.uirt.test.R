rm(list=ls())
setwd("L:/github/uirttool")
source("uirttool.txt")

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
item_model <- gtkLabel ( "  item model:" )
combo <- gtkComboBoxNewText()
sapply( c( "Nrm(\"nrm\",ak=c(0.4,1),ck=c(1.1,-1.2))" , 
  "PL(\"PL\",a=1.2,b=0.8,c=0.1)",
  "PL(\"PL\",a=1.2,b=0.8,c=0.1,as=2)",
  "Gpc(\"gpc\",a=1.2,bk=c(0.8,1.3))",
  "Pcm(\"pcm\",bk=c(0.8,1.3))",  
  "Pcx(\"pcx\",as=c(0.5,1),bk=c(0.8,1.3))",  
  "Gpx(\"gpx\",a=1.2,as=c(0.5,1),bk=c(0.8,1.3))",
  "PL(name=\"V1\",a=1.2,b=0.3,c=0.1)",
  "PL(name=\"V2\",a=1.2,b=0.3,c=0.1,as=2)",
  "Pcm(name=\"V3\",bk=c(1,-1.2))",
  "Nrm(name=\"V8\",ak=1:2,ck=-cumsum(c(1,-1.2)),a=1)",
  "Pcx(name=\"V4\",as=c(0.4,1),bk=c(1,-1.2))",
  "Nrm(name=\"V9\",ak=c(0.4,1),ck=c(-0.4*1,-0.4*1-(1-0.4)*(-1.2)),a=1,as=c(0.4,1))",
  "Gpc(name=\"V5\",a=1.2,bk=c(1,-1.2))",
  "Nrm(name=\"V10\",ak=c(1,2),ck=c(-1.2*1,-1.2*1-1.2*(-1.2)),a=1.2)",
  "Gpx(name=\"V6\",a=1.2,as=c(0.4,1),bk=c(1,-1.2))",
  "Nrm(name=\"V11\",ak=1.2*c(0.4,1),ck=c(-1.2*0.4*1,-1.2*0.4*1-1.2*(1-0.4)*(-1.2)),a=1.2,as=c(0.4,1))",
  "Nrm(name=\"V7\",ak=c(1,1.2),ck=c(1,-1.2))"
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
plot_type <- gtkLabel ( "  plot type:" )
comboType<-gtkComboBoxNewText()
sapply(c("crf","bcf","inf","icf"),comboType$appendText)
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
hbox$packStart(item_model,fill=F,expand=F,padding=2)
hbox$packStart(combo,fill=F,expand=F,padding=5)
hbox$packStart(plot_type,fill=F,expand=F,padding=2)
hbox$packStart(comboType,fill=F,expand=F,padding=5)
vbox$packStart(hbox,fill=F,expand=F,padding=5)
vbox$packStart(da)
win$add(vbox)
win$setDefaultSize(400,400)
win$showAll() 

# Baker, Item Response Theory: parameter estimation techniques 2004
# page 75, table 3.2
# use following three commented statements to test
item<-PL("V1",a=0.8,b=0,c=0.2)
thetas<-seq(-3,3,by=0.5)
round(item$info(thetas),4)-c(0.0122,0.0217,0.0359,0.0547,0.0758,
 0.0947,0.1067,0.1085,0.1005,0.0859,0.0688,0.0523,0.0383)

# Baker, Item Response Theory: parameter estimation techniques 2004
# page 236/254, table 9.1/9.2
# use following three commented statements to test
item<-Nrm("V1",ak=c(-0.6,1.2),ck=c(1,1.25))
thetas<-seq(-3,3,by=0.5)
round(item$info(thetas),4)-c(0.0362,0.0644,0.1212,0.2305,0.4111,
  0.6193,0.7071,0.5847,0.3694,0.1974,0.0972,0.0465,0.0222)

