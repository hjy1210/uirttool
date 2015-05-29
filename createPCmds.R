createPCmds<-function(ProjName,Filename,VNames,Ncats){
  Ks<-Ncats-1
  Cmds<-c("Project:",
    paste0("\tName=",ProjName,";\n"),
    paste0("Data:\n","\tFile=",Filename,";\n"))
  Constraints<-c()
  AnalysisCmd<-function(Name,Title,Comments){
    if (length(Constraints)>0) {
      Cmds<<-c(Cmds,"Constraints:",Constraints);
      Constraints<<-c()
    }
    res<-c("Analysis:",
    paste0("\tName = ",Name,";"),
    paste0("\tMode = ","Calibration",";\n"),
    paste0("Title:\n\t",Title,"\n"),
    paste0("Comments:\n\t",Comments,"\n"))
    Cmds<<-c(Cmds,res)
  }
  EstimationCmd<-function(Method="BAEM",EStepIter=500,EStepTol=1e-005,
    SE="S-EM",MStepIter=50,MStepTol=1e-006,QuadratureNodes=49,QuadratureMax=6,
    SEM=0.001,SS=1e-005){
    res<-c("Estimation:",
      paste0("\tMethod=",Method,";"),
      paste0("\tE-Step=",EStepIter,", ",EStepTol,";"),
      paste0("\tSE=",SE,";"),
      paste0("\tM-Step=",MStepIter,", ",MStepTol,";"),
      paste0("\tQuadrature=",QuadratureNodes,", ",QuadratureMax,";"),
      paste0("\tSEM=",SEM,";"),
      paste0("\tSS=",SS,";"))
    Cmds<<-c(Cmds,res)
  }
  SaveCmd<-function(Save=c("PRM")){
    res<-paste(Save,collapse=", ")
    if (nchar(res)>0) 
      res<-c(paste0("Save:\n\t",paste(Save,collapse=", ")))
    else
      res<-c("Save:")
    Cmds<<-c(Cmds,res)
  }
  MiscCmd<-function(Decimal=4,Processor=8,Print=c("M2"),MinExp=1) {
    tmp<-c("Miscellaneous:",
       paste0("\tDecimal=",Decimal,";"),
       paste0("\tProcessors=",Processor,";"),
       paste0("\tPrint ",paste(Print,collapse=", "),";"))
    if (sum(Print=="GOF")>0) tmp<-c(tmp,"\tMin Exp=",MinExp,";")
    Cmds<<-c(Cmds,tmp)
  }
  GroupsCmd<-function(groupindex=NULL){
    if (is.null(groupindex))
      res<-"Groups:"
    else res<-c("Groups:",paste0("\tVariable=",VNames[groupindex],";"))
    Cmds<<-c(Cmds,res)
  }
  getModel<-function(itemname,model) {
    tokens<-strsplit(model,",")[[1]]
    if (length(tokens)==3){
      paste0(tokens[1],";\n\tAlphaMatrix(",itemname,")=",tokens[2],
         ";\n\tGammaMatrix(",itemname,")=",tokens[3],";")
    }
    else if (length(tokens)==2)
      paste0(tokens[1],";\n\tGammaMatrix(",itemname,")=",tokens[2],";")
    else
      paste0(model,";")
  }
  GroupCmd<-function(itemindexes,models,mean,variance,label="",value=NULL,referenced=FALSE){
    if (is.null(value)) res<-c(paste0("Group ",label,":"))
    else res<-c(paste0("Group ",label,":"),paste0("\tValue =(",value,");"))
    res<-c(res,"\tDimension = 1;")
    res<-c(res,paste0("\tItems =",paste(VNames[itemindexes],collapse=", "),";"))
    for (index in 1:length(itemindexes)) {
      res<-c(res,paste0("\tCodes(",VNames[itemindexes[index]],")=",
        paste(sapply(0:Ks[itemindexes[index]],function(x) paste0(x,"(",x,")")),collapse=","),";"))
      res<-c(res,paste0("\tModel(",VNames[itemindexes[index]],")=",
        getModel(VNames[itemindexes[index]],models[index])))
    }
    if (referenced) res<-c(res,"\tReferenced;")
    res<-c(res,
      paste0("\tMean=",mean,";"),
      paste0("\tCovariance=",variance,";"))
    Cmds<<-c(Cmds,res)
  }
  FixCmd<-function(itemindex,parname,zerooffset,value,group=NULL){
    if (is.null(group))
      res<-paste0("\t(",VNames[itemindex],",",parname,"[",zerooffset,"])=",value,";")
    else
      res<-paste0("\t(",group,",",VNames[itemindex],",",parname,"[",zerooffset,"])=",value,";")
    Constraints<<-c(Constraints,res)
  }
  EqualparCmd<-function(Group1name,Group2name,itemindexes,parnames,parindexes){
    equals<-c()
    for (i in 1:length(itemindexes)){
      equals<-c(equals,paste0("\tEqual = (",Group1name,",",VNames[itemindexes[i]],",",
        parnames[i],"[",parindexes[i],"]),(",Group2name,",",VNames[itemindexes[i]],",",
        parnames[i],"[",parindexes[i],"]);"))
    }
    Constraints<<-c(Constraints,equals)
  }
  WriteFile<-function(file) {
    if (length(Constraints)>0) Cmds<<-c(Cmds,"Constraints:",Constraints);
    writeLines(Cmds,file)
  }
  list(AnalysisCmd=AnalysisCmd,EstimationCmd=EstimationCmd,
    SaveCmd=SaveCmd,MiscCmd=MiscCmd,GroupsCmd=GroupsCmd,
    GroupCmd=GroupCmd,FixCmd=FixCmd,EqualparCmd=EqualparCmd,WriteFile=WriteFile)
}
