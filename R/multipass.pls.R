multipass.pls<-function(formula,ncomp,validation,data,nbpass,out=2.5){
  res.outliers<-NULL
  pass<-NULL
  for (p in 1:nbpass){
    pls.trn<-plsr(formula,ncomp=ncomp, validation=validation,data=data,y=T)
    msepcv.trn<-MSEP(pls.trn,estimate=c("train","CV"))
    ncomp.trn<-which.min(msepcv.trn$val["CV",,])-1 
    reg.final.trn<-plsr(formula,ncomp=ncomp.trn, validation=validation,data=data,y=T)
    trn.pred<-predict(reg.final.trn,ncomp=ncomp.trn)[,,1]
    rmsec<-RMSEP(reg.final.trn,estimate="train",ncomp=ncomp.trn,intercept=FALSE)
    ## Recherche d'outliers
    comp.trn<-data.frame(Lab.Value=reg.final.trn$y,Prediction=trn.pred)
    Ts.trn<-abs(comp.trn$Prediction-comp.trn[,1])
    Ts.trn[is.na(Ts.trn)]<-0
    outliers.trn<-comp.trn[Ts.trn>=out*(rmsec$val[,,1]),]
    data<-data[!row.names(data)%in%row.names(outliers.trn),]
    res.outliers<-c(res.outliers,row.names(outliers.trn))
    pass<-c(pass,rep(p,nrow(outliers.trn)))
  }
  #Final pass
  pls.trn<-plsr(formula,ncomp=ncomp, validation=validation,data=data)
  msepcv.trn<-MSEP(pls.trn,estimate=c("train","CV"))
  ncomp.trn<-which.min(msepcv.trn$val["CV",,])-1 
  reg.final.trn<-plsr(formula,ncomp=ncomp.trn, validation=validation,data=data,y=T)
  res<-vector(mode = "list", length = 4)
  names(res)<-c("outliers","plsr","ncomp", "pass")
  res[[1]]<-res.outliers
  res[[2]]<-reg.final.trn
  res[[3]]<-ncomp.trn
  res[[4]]<-pass
  return(res)
}
