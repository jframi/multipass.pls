#'Wrapper for cppls function with multiple pass and outliers elmination
#'
#'
#'@param formula a model formula. Most of the lm formula constructs are supported. See below.
#'@param ncomp the number of components to include in the model (see below).
#'@param Y.add a vector or matrix of additional responses containing relevant information about the observations. Only used for cppls.
#'@param data	an optional data frame with the data to fit the model from.
#'@param validation	character. What kind of (internal) validation to use
#'@param nbpass Number of passes
#'@param out T value threshold for outlier elimination
#'@return A list with the following components
#'@return outliers A vector with outliers names as deduced from data row.names
#'@return plsr the pls model object from the final pass
#'@return ncomp the number of components to be further used
#'@return pass a vector of same length as outliers indicating at which pass outliers were removed
#'@author J.-F. Rami \email{rami@@cirad.fr}
#'@examples
#'\dontrun{
#'}
#'
multipass.cppls<-function(formula,ncomp,validation,data,nbpass,out=2.5,Y.add){
  res.outliers<-NULL
  pass<-NULL
  mf<-match.call(expand.dots = FALSE)
  mf[[1]]<-quote(cppls)
  mf$data<-quote(data)
  mf$Y.add<-Y.add
  mf$formula<-formula
  mf<-mf[-which(names(mf)=="nbpass")]
  mf<-as.call(c(as.list(mf),y=T))
  
  for (p in 1:nbpass){
    pls.trn<-eval(mf)
    #pls.trn<-cppls(formula,data=data,Y.add=Yadd,ncomp=ncomp, validation=validation,y=T)
    msepcv.trn<-MSEP(pls.trn,estimate=c("train","CV"))
    #ncomp.trn<-which.min(msepcv.trn$val["CV",,])-1 
    ncomp.trn<-min(which(round(c(msepcv.trn$val["CV",,][-1],0)-msepcv.trn$val["CV",,],3)==0))
    mf$ncomp<-ncomp.trn
    reg.final.trn<-eval(mf)
    #reg.final.trn<-cppls(formula,data=data,Y.add=Yadd,ncomp=ncomp.trn, validation=validation,y=T)
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
  mf$ncomp<-ncomp
  pls.trn<-eval(mf)
  #pls.trn<-cppls(formula,data=data,Y.add=Yadd,ncomp=ncomp, validation=validation,y=T)
  msepcv.trn<-MSEP(pls.trn,estimate=c("train","CV"))
  #ncomp.trn<-which.min(msepcv.trn$val["CV",,])-1
  ncomp.trn<-min(which(round(c(msepcv.trn$val["CV",,][-1],0)-msepcv.trn$val["CV",,],3)==0))
  mf$ncomp<-ncomp.trn
  reg.final.trn<-eval(mf)
  #reg.final.trn<-cppls(formula,data=data,Y.add=Yadd,ncomp=ncomp.trn, validation=validation,y=T)
  res<-vector(mode = "list", length = 4)
  names(res)<-c("outliers","plsr","ncomp", "pass")
  res[[1]]<-res.outliers
  res[[2]]<-reg.final.trn
  res[[3]]<-ncomp.trn
  res[[4]]<-pass
  return(res)
}