ItemString<-function(item,ndigit){
  vecstr<-function(v){
    if (length(v)==1)
      as.character(v)
    else {
      res<-paste0("c(",round(v[1],ndigit))
      for (i in 2:length(v)) res<-paste0(res,",",round(v[i],ndigit))
      paste0(res,")")
    }
  }
  if (item$itemtype=="nrm")
    paste0("Nrm(name=\"",item$name,"\",ak=",vecstr(item$ak),",ck=",vecstr(item$ck),")")
  else if (item$itemtype=="pl")
    paste0("PL(name=\"",item$name,"\",a=",item$a,",b=",item$bk,",c=",item$c,")")
  else if (item$itemtype=="plx")
    paste0("PL(name=\"",item$name,"\",a=",item$a,",as=",round(item$as,ndigit),",b=",item$bk,",c=",item$c,")")
  else if (item$itemtype=="pcm")
    paste0("Pcm(name=\"",item$name,"\",bk=",vecstr(item$bk),")")
  else if (item$itemtype=="pcx")
    paste0("Pcx(name=\"",item$name,"\",as=",vecstr(item$as),",bk=",vecstr(item$bk),")")
  else if (item$itemtype=="gpx")
    paste0("Gpx(name=\"",item$name,"\",a=",item$a,",as=",vecstr(item$as),",bk=",vecstr(item$bk),")")
  else if (item$itemtype=="gpc")
    paste0("Gpc(name=\"",item$name,"\",a=",item$a,",bk=",vecstr(item$bk),")")
  else ""

}
