ItemString<-function(item){
  vecstr<-function(v){
    if (length(v)==1)
      as.character(v)
    else {
      res<-paste0("c(",v[1])
      for (i in 2:length(v)) res<-paste0(res,",",v[i])
      paste0(res,")")
    }
  }
  if (item$itemtype=="nrm")
    paste0("Nrm(name=\"",item$name,"\",ak=",vecstr(item$ak[-1]),",ck=",vecstr(item$ck[-1]),")")
  else if (item$itemtype=="pl")
    paste0("PL(name=\"",item$name,"\",a=",item$a,",bk=",item$bk,",g=",item$g,")")
  else if (item$itemtype=="pcm")
    paste0("Pcm(name=\"",item$name,"\",bk=",item$bk,")")
  else if (item$itemtype=="pcx")
    paste0("Pcx(name=\"",item$name,"\",dk=",vecstr(item$ak[-1]-item$ak[-length(item$ak)]),",bk=",vecstr(item$bk),")")
  else if (item$itemtype=="gpx")
    paste0("Pcx(name=\"",item$name,"\",a=",item$a,",dk=",vecstr((item$ak[-1]-item$ak[-length(item$ak)])/item$a),",bk=",vecstr(item$bk),")")
  else if (item$itemtype=="gpc")
    paste0("Pcx(name=\"",item$name,"\",a=",item$a,",bk=",vecstr(item$bk),")")
  else ""

}
