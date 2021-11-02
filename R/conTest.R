#' Test significance
#'
#' @param data Binary time-points-by-variable matrix
#' @param lag Non-negative integer indicating how much the second variable is lagged (default 0)
#' @param conFun Contingency measure function (calculating the contingency value between two binary vectors). Built in: FunClassJacc, FunCorrJacc, FunKappa, FunOdds, FunLogOdds, FunPropAgree,FunPhiCC
#' @param typeOfTest String indicating whether a model-based ('model') or a permutation-based ('permut'; default) data generation approach is used.
#' @param adCor Logic indicating the auto-dependence correction should be applied (TRUE; default) or not (FALSE)
#' @param nBlox Number indicating the number of segments (default  10).
#'              Necessary for permutation-based test, accounting for auto-dependence
#'              (typeOfTest='permut'; adCor=TRUE)
#' @param nReps Number of replicates/samples that is used to generate the test distribution
#'
#'
#' @return A conTest-object including
#' @return \code{allLinks} Matrix of pairwise calculated contingency measures
#' @return \code{percentile} Matrix of raw percentiles, situating the observed value in the sample distribution
#' @return \code{pValue}   Matrix of the p-values (upper one-sided significance test) calculated by subtracting the percentile from 1.
#' @return \code{para}: Saving the parameter settings for
#'               \code{typeOfTest}, \code{adCor}, \code{nBlox}, \code{nReps}, \code{funName}, \code{lag}, \code{varNames}
#' @return \code{samples} Saved generated replicates/samples for each variable combination under \code{$NameVariable1$NameVariable2}
#' @export
#' @importFrom Rlab rbern
#'
#' @examples  signdata=cbind(c(1,0,1,0,1,0,1,0),c(1,1,1,1,0,0,0,0),c(0,0,0,0,0,0,1,1))
#'            colnames(signdata) <-c ('momangry', 'momsad','adoangry')
#'            conTest(data=signdata,lag=1,conFun=funClassJacc,
#'            adCor=FALSE)

conTest<-function(data,lag=0,conFun,
                    typeOfTest='permut',adCor=TRUE,nBlox=10,nReps=100){

  nVars=ncol(data)
  nPoints=nrow(data)


  ts_samples<-array(NA,dim=c(nPoints,nReps,nVars))
  sampledLinks<-array(NA,dim=c(nVars,nVars,nReps^2))
  percentile<-array(NA,dim=c(nVars,nVars))

  if(typeOfTest=='model'){

    datsCC <- data[complete.cases(data),]
    ts_para<-array(NA, dim=c(nVars,3))
    for (v in 1: nVars){
      ts_para[v,]<-unlist(getProb(datsCC[,v]))
    }

    for (va in 1: nVars){
      if(adCor==FALSE){
        ts_samples[,,va]<-array(rbern(nReps*nPoints,ts_para[va,1]), dim=c(nPoints,nReps))

      }else{
        ts_samples[1,,va]<-rbern(nReps,ts_para[va,1])
        for (tp in 2:nPoints){
          in1dex1<-which(ts_samples[(tp-1),,va]==1)
          in1dex0<-which(ts_samples[(tp-1),,va]==0)
          ts_samples[tp,in1dex1,va]<-rbern(length(in1dex1),ts_para[va,2])
          ts_samples[tp,in1dex0,va]<-rbern(length(in1dex0),ts_para[va,3])
        }
      }
    }

  }else if(typeOfTest=='permut'){

    for (va in 1: nVars){
      if(adCor==FALSE){
          for(sz in 1:nReps){
            ts_samples[,sz,va]<-permutNO(data[,va])
          }
      }else{
        for(sz in 1:nReps){
          ts_samples[,sz,va]<-permutAD(data[,va],nBlox)
        }
      }
    }
  }


  Mx_obs<-conMx(data,lag=lag, conFun=conFun)
  obsLinks <- Mx_obs$value
  funName <- Mx_obs$para$funName
  varNames <- Mx_obs$para$varNames

  for (var1 in 1:nVars){
    for(var2 in 1:nVars){
      Mx_sample <- conMx(ts_samples[,,var1],
                           ts_samples[,,var2],
                           lag=lag,
                           conFun=conFun)
      sampledLinks[var1,var2,]<-as.vector(Mx_sample$value)
      percentile[var1,var2]<-sum(sampledLinks[var1,var2,]<obsLinks[var1,var2],na.rm=TRUE)/sum(!is.na(sampledLinks[var1,var2,]))
    }
  }
  ones<-array(1,dim=c(ncol(data),ncol(data)))
  pvalue<-ones-percentile
  rownames(pvalue) <- varNames
  colnames(pvalue) <- varNames
  rownames(percentile) <- varNames
  colnames(percentile) <- varNames

  rownames(obsLinks) <- colnames(obsLinks) <- varNames

  samples0 <- lapply(seq_len(nrow(sampledLinks)), function(i) sampledLinks[i,,])
  names(samples0) <- varNames
  samples <- samples0
  for (j in 1:nVars){
    samples[[j]] <- lapply(seq_len(nrow(samples0[[j]])), function(i) samples0[[j]][i,])
    names(samples[[j]]) <- varNames
  }
  para <- list(typeOfTest,adCor,nBlox, nReps, funName,lag,varNames)
  names(para) <-  c("typeOfTest","adCor","nBlox", "nReps","funName","lag","varNames")

  results <- list(obsLinks,percentile,pvalue,para,samples)
  names(results) <- c( "allLinks","percentile","pValue","para","samples")
  class(results) <- "conTest"
  return(results)
}




