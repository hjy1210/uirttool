rm(list=ls())
source("PL.R")
source("likelihood.R")
require(RGtk2)
require(cairoDevice)

# reference guis.pdf p.167-174 about GtkTreeView
items<-list()
view<-gtkTreeView()
selection <- view$getSelection ( )
selection$setMode ( "single" )

window <- gtkWindow ( )
window$setTitle ( "Log likelihood plot experiment" )
hbox<-gtkHBox(F)
hbox["width-request"]<-100
scrolled_textwindow<-gtkScrolledWindow ( )
textview<-gtkTextView()
scrolled_textwindow$add(textview)
hbox$packStart(scrolled_textwindow)
vbox<-gtkVBox(F)
scrolled_window <- gtkScrolledWindow ( )
scrolled_window["height-request"]<-100
vbox$packStart(scrolled_window,fill=F,expand=F)
scrolled_window$add ( view )

hbox2<-gtkHBox(F)
loadButton<-gtkButton("Load Model")
gSignalConnect(loadButton, "clicked", function(button){
 dialog <- gtkFileChooserDialog ( title = "Open a file" ,
  parent = NULL , action = "open" ,
  "gtk-ok" , GtkResponseType [ "ok" ] ,
  "gtk-cancel" , GtkResponseType [ "cancel" ] ,
  show = FALSE )
 gSignalConnect ( dialog , "response" ,
  f = function ( dialog , response , data ) {
    if ( response == GtkResponseType [ "ok" ] ) {
      filename <- dialog$getFilename ( )
      n<-nchar(filename)
	  if (tolower(substr(filename,n-3,n))==".mod") {
	    filemod<-filename
		filersp<-paste0(substr(filename,1,n-4),".rsp")
	  }else{
	    filersp<-filename
		filemod<-paste0(substr(filename,1,n-4),".mod")
	  }
	  itemmodels<-readLines(filemod)
	  modelstring<-character()
      for ( i in seq(itemmodels)) modelstring<-paste(modelstring,itemmodels[i],"\r\n")
	  items<<-lapply(itemmodels,function(x){
        eval(parse(text=x))
      })
	  textview$getBuffer()$setText(modelstring)
	  df<-read.table(filersp)
	  model <- rGtkDataFrame ( df )
	  view $setModel( model )
	  columns<-view$getColumns()
	  for (col in columns) view$removeColumn(col)
      mapply(view$insertColumnWithAttributes ,
        position = -1,
        title = colnames ( model ) ,
        cell = list ( gtkCellRendererText ( ) ) ,
        text = seq_len ( ncol ( model ) ) -1
        )

    }
	dialog$destroy()
  }) 
  fileFilter <- gtkFileFilter ( )
  fileFilter$setName ("model files" )
  fileFilter$addPattern ( "*.mod" )
  fileFilter$addPattern ( "*.rsp" )
  dialog$addFilter ( fileFilter )  

  dialog$Run()
})
hbox2$packStart(loadButton,fill=F,expand=F)
hbox2$packStart(gtkLabel("lower bound:"))
lowerEntry<-gtkEntry()
lowerEntry$setText("-6")
lowerEntry["width-request"]<-60
gSignalConnect(lowerEntry, "activate", function(button){changed(selection)})
hbox2$packStart(lowerEntry)

hbox2$packStart(gtkLabel("upper bound:"))
upperEntry<-gtkEntry()
upperEntry$setText("6")
upperEntry["width-request"]<-60
gSignalConnect(upperEntry, "activate", function(button){changed(selection)})
hbox2$packStart(upperEntry)
vbox$packStart(hbox2,fill=F,expand=F)
da <- gtkDrawingArea()
asCairoDevice(da)
vbox$packStart(da)
hbox$packStart(vbox)
window$add ( hbox )

changed<-function(selection){
  selected_rows <- selection$getSelectedRows ( )
  if ( length ( selected_rows$retval ) ) {
    rows <- sapply ( selected_rows$retval ,
    gtkTreePathGetIndices ) + 1L
    resp<-sapply(selected_rows$model [ rows , ],function(x){x})
	thetas<-seq(as.numeric(lowerEntry$getText()),as.numeric(upperEntry$getText()),len=1000)
	plot(thetas,log(likelihood(resp=resp,items=items,thetas=thetas)),type="l",
	xlab="thetas",ylab="Log likelihood")
  }
}

gSignalConnect ( selection , "changed" , changed)
