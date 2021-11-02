#' Calculate the link strength between multiple behaviors and return them as a matrix (optionally discarting all non-significant links)
#'
#' @param data Binary time-points-by-variable matrix
#' @param lag Non-negative integer indicating how many time points the second variable is lagged (default 0)
#' @param conFun Contingency measure function (calculating the contingency value between two binary vectors).
#'      Built in: funPropAgree, funClassJacc, funKappa, funCorrJacc, funOdds, funLogOdds, funPhiCC
#' @param test Logic indicationg whether a significance test is executed (TRUE) or not (FALSE;default)
#' @param typeOfTest String indicating whether a model-based ('model') or a permutation-based ('permut'; default) data generation approach is used.
#' @param adCor Logic indicating the auto-dependence correction should be applied (TRUE; default) or not (FALSE)
#' @param nBlox Number indicating the number of segments (default  10).
#'              Necessary for permutation-based test, accounting for auto-dependence
#'              (typeOfTest='permut'; adCor=TRUE)
#' @param nReps Number of replicates/samples that is used to generate the test distribution
#' @param signLev Significance level of the test (default 0.05)


#'
#' @return A conNEcT-object including
#' @return \code{allLinks} Matrix of pairwise calculated contingency measures
#' @return \code{signLinks}Matrix of pairwise calculated contingency measures containing only significant links
#'        (others are set to 0)
#' @return \code{pValue} P-values for the one-sided upper significance test
#' @return \code{para} Parameter settings containing
#'                 \code{lag}, \code{test},\code{typeOfTest},
#'                 \code{adCor}, \code{nBlox}, \code{nReps},
#'                 \code{funName}, and \code{varNames}
#' @return \code{probs} Table of relative frequency and auto-dependency

#' @export
#'
#' @examples 
#' netdata=cbind(rep(c(1,1,1,1,1,0,0,0,0,0),100),
#' rep(c(0,0,1,1,1,1,0,0,0,0),100))
#' conNEcT(netdata,lag=1,conFun=funKappa,test=TRUE,nBlox=5)

conNEcT<-function(data,lag=0, conFun,
                  test = FALSE, typeOfTest='permut',adCor=TRUE,nBlox=10,nReps=100,signLev=0.05){
  conMx <-conMx(data = data,lag=lag, conFun=conFun)
  funName <- conMx$para$funName

  allLinks<-round(conMx$value,4)
  varNames <- conMx$para$varNames
  colnames(allLinks) <- varNames
  rownames(allLinks) <- varNames

  signLinks <- array(NA,dim=c(ncol(data),ncol(data)))
  pValue <- array(NA,dim=c(ncol(data),ncol(data)))
  colnames(signLinks) <- varNames
  rownames(signLinks) <- varNames
  colnames(pValue) <- varNames
  rownames(pValue) <- varNames

  if(test==TRUE){
    test.res<-conTest(data=data,lag=lag,
                        conFun=conFun,
                        typeOfTest=typeOfTest,nBlox=nBlox,adCor=adCor,nReps=nReps)

    pValue<-test.res$pValue
    signLinks<-ifelse(pValue<signLev,allLinks,0)
  }


  para <- list(lag, test,typeOfTest,adCor, nBlox,nReps,funName,varNames)
  names(para) <- c("lag","test","typeOfTest","adCor", "nBlox","nReps","funName","varNames")
  probs <- round(conData(data)$probs,4)

  results <- list(allLinks,signLinks,pValue,para,probs)
  names(results) <- c("allLinks",
                      "signLinks",
                      "pValue",
                      "para",
                      "probs")
  class(results) <- 'conNEcT'
  return(results)
}

