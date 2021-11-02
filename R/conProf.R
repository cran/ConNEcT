#' Compare different lags in a contingency profile
#'
#' @param data Binary time-points-by-variable matrix
#' @param maxlag Positive integer indicating how many lags should be investigated
#' @param conFun Contingency measure function (calculating the contingency value between two binary vectors).
#'      Built in: funPropAgree, funClassJacc, funKappa, funCorrJacc, funOdds, funLogOdds, funPhiCC
#'
#' @return A conProf-object consisting of
#' @return \code{value} Contingency matrices for different lags
#' @return \code{para}  Parameters including \code{maxlag}, \code{funName} and \code{varNames}
#' @export
#'
#' @examples  IntData <- cbind(rep(rep(c(0,0,1,0,1,0,1,0,0,0),each=5),times=5),
#'                             rep(rep(c(1,0,0,0), each=10), times=25))
#'            colnames(IntData) <- c('Var1','Var2')
#'            conProf(IntData,maxlag=10,conFun=funClassJacc)
#'
#'
conProf <- function(data, maxlag,conFun){
  CD <- conData(data)
  data <- CD$data
  varNames <- CD$varNames
  nVar <- length(varNames)

  CM <- array(0,c(nVar,nVar,2*maxlag+1))
  CM0 <- conMx(data,lag=0,conFun=conFun)
  CM[,, maxlag+1]<-CM0$value
  for (lag in 1:maxlag){
    CM.temp <- conMx(data,lag=lag,conFun=conFun)
    CM[,,maxlag+1+lag] <- CM.temp$value
    CM[,,maxlag+1-lag] <- t(CM.temp$value)
  }

  colnames(CM) <- varNames
  rownames(CM) <- varNames
  funName <- CM0$para$funName
  para <- list(maxlag,funName,varNames)
  names(para) <- c('maxLags','funName','varNames')
  result <- list(CM,para)
  names(result) <- c("value","para")
  class(result) <- 'conProf'
  return(result)
}

