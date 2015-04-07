require("RGtk2")
window <- gtkWindow()
window["title"] <- "uirt"
frame <- gtkFrameNew("uirt")
window$add(frame)
box1 <- gtkVBoxNew()
box1$setBorderWidth(30)
frame$add(box1)   #add box1 to the frame
box2 <- gtkHBoxNew(spacing= 10) #distance between elements
box2$setBorderWidth(24)
#TextBox<- gtkEntryNew() #text field with expresion to calculate
TextBox<- gtkTextView() #text field with expresion to calculate
buffer<-gtkTextBuffer()
TextBox$buffer<-buffer
TextBox$setWidthChars(25)
box1$packStart(TextBox)
label = gtkLabelNewWithMnemonic("Result") #text label
box1$packStart(label)

result<- gtkEntryNew() #text field with result of our calculation
result$setWidthChars(25)
box1$packStart(result)

box2 <- gtkHBoxNew(spacing= 10) # distance between elements
box2$setBorderWidth(24)
box1$packStart(box2)

Calculate <- gtkButton("Calculate")
box2$packStart(Calculate,fill=F) #button which will start calculating

Sin <- gtkButton("Sin") #button to paste sin() to TextBox
box2$packStart(Sin,fill=F)

Cos <- gtkButton("Cos") #button to paste cos() to TextBox
box2$packStart(Cos,fill=F)

model<-rGtkDataFrame(c("double","integer"))
combobox <- gtkComboBox(model)
#combobox allowing to decide whether we want result as integer or double

crt <- gtkCellRendererText()
combobox$packStart(crt)
combobox$addAttribute(crt, "text", 0)

gtkComboBoxSetActive(combobox,0)
box2$packStart(combobox)

DoCalculation<-function(button)
{

  if ((TextBox$buffer$text)=="") return(invisible(NULL)) #if no text do nothing
   #display error if R fails at calculating
   tryCatch(
      if (gtkComboBoxGetActive(combobox)==0)
   result$setText((eval(parse(text=TextBox$buffer$text))))
   else (result$setText(as.integer(eval(parse(text=TextBox$buffer$text))))),
   error=function(e)
      {
      ErrorBox <- gtkDialogNewWithButtons("Error",window, "modal","gtk-ok", GtkResponseType["ok"])
      box1 <- gtkVBoxNew()
      box1$setBorderWidth(24)
      ErrorBox$getContentArea()$packStart(box1)

      box2 <- gtkHBoxNew()
      box1$packStart(box2)

      ErrorLabel <- gtkLabelNewWithMnemonic("There is something wrong with your text!")
      box2$packStart(ErrorLabel)
      response <- ErrorBox$run()


      if (response == GtkResponseType["ok"])
         ErrorBox$destroy()

      }
   )

}

PasteSin<-function(button)
{
   TextBox$buffer$text<-paste(TextBox$buffer$text,"sin()",sep="")

}

PasteCos<-function(button)
{
   TextBox$buffer$text<-paste(TextBox$buffer$text,"cos()",sep="")

}

#however button variable was never used inside 
#functions, without it gSignalConnect would not work
gSignalConnect(Calculate, "clicked", DoCalculation)
gSignalConnect(Sin, "clicked", PasteSin)
gSignalConnect(Cos, "clicked", PasteCos)
