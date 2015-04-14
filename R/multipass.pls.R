#'Wrapper for plsr function with multiple pass and outliers elmination
#'
#'
#'@param formula a model formula. Most of the lm formula constructs are supported. See below.
#'@param ncomp the number of components to include in the model (see below).
#'@param data  an optional data frame with the data to fit the model from.
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
multipass.pls<-function(formula,ncomp,validation,data,nbpass,out=2.5){
  res.outliers<-NULL
  pass.ncomp<-NULL
  pass<-NULL
  for (p in 1:nbpass){
    pls.trn<-plsr(formula,ncomp=ncomp, validation=validation,data=data,y=T)
    msepcv.trn<-MSEP(pls.trn,estimate=c("train","CV"))
    cvder<-as.vector(smooth(sign(c(msepcv.trn$val["CV", , ][-1], 0) - msepcv.trn$val["CV", , ])))
    if (any(cvder[-1]-cvder[-length(cvder)]==2)){
      ncomp.trn<-which.min(msepcv.trn$val["CV",,])-1 
    }else{
      ncomp.trn <- min(which(round(c(msepcv.trn$val["CV", , ][-1], 0) - msepcv.trn$val["CV", , ], 3) == 0))      
    }    
    reg.final.trn<-plsr(formula,ncomp=ncomp.trn, validation=validation,data=data,y=T)
    trn.pred<-predict(reg.final.trn,ncomp=ncomp.trn)[,,1]
    rmsec<-RMSEP(reg.final.trn,estimate="train",ncomp=ncomp.trn,intercept=FALSE)
    ## Recherche d'outliers
    comp.trn<-data.frame(Lab.Value=reg.final.trn$y,Prediction=trn.pred)
    Ts.trn<-abs(comp.trn$Prediction-comp.trn[,1])
    Ts.trn[is.na(Ts.trn)]<-0
    outliers.trn<-comp.trn[Ts.trn>=out*(rmsec$val[,,1]),]
    if (nrow(outliers.trn)==0) break
    data<-data[!row.names(data)%in%row.names(outliers.trn),]
    res.outliers<-c(res.outliers,row.names(outliers.trn))
    pass<-c(pass,rep(p,nrow(outliers.trn)))
    pass.ncomp<-c(pass.ncomp,ncomp.trn)
  }
  #Final pass
  pls.trn<-plsr(formula,ncomp=ncomp, validation=validation,data=data)
  msepcv.trn<-MSEP(pls.trn,estimate=c("train","CV"))
  cvder<-as.vector(smooth(sign(c(msepcv.trn$val["CV", , ][-1], 0) - msepcv.trn$val["CV", , ])))
  if (any(cvder[-1]-cvder[-length(cvder)]==2)){
    ncomp.trn<-which.min(msepcv.trn$val["CV",,])-1 
  }else{
    ncomp.trn <- min(which(round(c(msepcv.trn$val["CV", , ][-1], 0) - msepcv.trn$val["CV", , ], 3) == 0))      
  }
  reg.final.trn<-plsr(formula,ncomp=ncomp.trn, validation=validation,data=data,y=T)
  res<-vector(mode = "list", length = 4)
  names(res)<-c("outliers","plsr","ncomp", "pass")
  if (is.null(res.outliers)) res.outliers<-NA
  if (is.null(pass.ncomp)) pass.ncomp<-NA
  
  res[[1]]<-res.outliers
  res[[2]]<-reg.final.trn
  res[[3]]<-ncomp.trn
  res[[4]]<-pass
  return(res)
}
