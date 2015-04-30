createCCmds<-function(datafile,GroupNames=NULL,GroupPositions=NULL,responses,constraints="none",
                      fit="no",warnings="no",nodes=15,converge="0.0001",stderr="none",exportprmfile=NA,showfile=NA,Model="item",
                      importprmfile=NA,exportdesignmatrixfile=NA,quit=NA,
                      Scores=NULL,exportregfile=NULL,exportcovfile=NULL,
                      showmlefile=NULL){
  res<-c("reset;")
  res<-c(res,paste0("Datafile ",datafile,";"))
  
  tmp<-"Format "
  if (!is.null(GroupNames)){
    for (i in seq_along(GroupNames)) {
      tmp<-paste0(tmp, GroupNames[i]," ",GroupPositions[i]," ")
    }
  }
  tmp<-paste0(tmp,"responses ",responses,";")
  res<-c(res,tmp)
  
  if (!is.null(GroupNames)){
    tmp<-""
    for (i in seq_along(GroupNames)) {
      tmp<-paste0(tmp,GroupNames[i]," ")
    }
    tmp<-paste0("regression ",tmp,";")
    res<-c(res,tmp)
  }
  if (!is.na(importprmfile)){
    res<-c(res,paste0("import anchor_parameters << ",importprmfile,";"))
  }
  res<-c(res,paste0("set ","constraints=",constraints,", ","warnings=",warnings,";"))
  res<-c(res,paste0("Model ",Model,";"))
  if (!is.null(Scores)){
    res<-c(res,Scores)
  }
  res<-c(res,paste0("Estimate ! ","stderr=",stderr,", fit=",fit,", nodes=",nodes,", converge=",converge,";"))
  if (!is.na(exportprmfile)){
    res<-c(res,paste0("export parameters >> ",exportprmfile,";"))
  }
  if (!is.na(showfile)){
    res<-c(res,paste0("show >> ",showfile,";"))
  }
  if (!is.na(exportdesignmatrixfile)){
    res<-c(res,paste0("export designmatrix >> ",exportdesignmatrixfile,";"))
  }
  if (!is.null(exportregfile)){
    res<-c(res,paste0("export reg_coefficients >> ",exportregfile,";"))
  }
  if (!is.null(exportcovfile)){
    res<-c(res,paste0("export covariance >> ",exportcovfile,";"))
  }
  if (!is.null(showmlefile)){
    res<-c(res,paste0("show cases ! estimates=mle >>  ",showmlefile,";"))
  }
  if (!is.na(quit)){
    res<-c(res,"quit;")
  }
  res
}