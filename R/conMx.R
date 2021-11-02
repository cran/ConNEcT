#' Calculate contingency measure values of a (lagged) time series matrix
#'
#' @param data Binary time-points-by-variable matrix
#' @param data2 Second binary time-points-by-variable matrix (optional)
#' @param lag Non-negative integer indicating how many time points the second variable is lagged (default 0)
#' @param conFun Contingency measure function (calculating the contingency value between two binary vectors).
#'      Built in: funPropAgree, funClassJacc, funKappa, funCorrJacc, funOdds, funLogOdds, funPhiCC
#'
#'
#' @return list with two elements:
#' @return \code{value} Matrix of pairwise calculated contingency measures
#' @return \code{para}  Parameter settings \code{lag}, \code{funName} and \code{varNames}
#' @importFrom  stats complete.cases
#' @export
#'
#' @examples conMx(cbind(c(1,0,1,0,1,0,1),c(1,1,1,1,0,0,0)),lag=1,conFun=funCorrJacc)
#'
conMx<-function(data,data2=NULL,lag=0, conFun){
  data1 = conData(data)$data
  data1 = as.matrix(data1)


  if(is.null(data2)){
    data2<-data1
  }else{
    data2 = conData(data2)$data
    data2 = as.matrix(data2)
  }

  data2.ini<-data2


  data2.ini.lagged<-lagthemats(data2.ini,lag=lag)


  data1.prev=data1[complete.cases(cbind(data1,data2.ini.lagged)),]
  data2.next=data2.ini.lagged[complete.cases(cbind(data1,data2.ini.lagged)),]

  nBehav.prev<-ncol(data1.prev)
  nBehav.next<-ncol(data2.next)

  obsMax<-array(NA,dim=c(nBehav.prev,nBehav.next))
  for(i in 1:nBehav.prev){
    for(j in 1:nBehav.next){
      obs <- conFun(data1.prev[,i],data2.next[,j])
      obsMax[i,j]<-obs$value
    }
  }
  funName <- obs$funName
  rownames(obsMax) <- colnames(data1)
  colnames(obsMax) <- colnames(data2)
  varNames <- colnames(data1)
  para <- list(lag,funName,varNames)
  names(para) <- c('lag','funName','varNames')
  result <- list(obsMax,para)
  names(result) <- c('value','para')
  return(result)
}


