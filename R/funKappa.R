#' Calculate Cohen's kappa between two vectors
#'
#' @param vec1 Vector of binary time series (NA not allowed)
#' @param vec2 Vector of binary time series (equal length as vec1, NA not allowed)
#'
#' @return list with two elements
#' @return \code{value} of the Cohen's kappa and
#' @return \code{funName} name of the function
#' @export
#'
#' @examples data1<-rep(c(1,0,1,1),25)
#'         data2<-ifelse(rnorm(100,0,1)<0.7,0,1)
#'         funKappa(data1,data2)
funKappa<-function(vec1,vec2){
  prob1=mean(vec1)
  prob2=mean(vec2)
  bothone<-sum((vec1+vec2)==2)
  bothzero<-sum((vec1+vec2)==0)
  total<-length(vec1)
  obsKappa<-(bothone+bothzero)/total
  expKappa<-(1-prob1)*(1-prob2)+(prob1*prob2)

  value=(obsKappa-expKappa)/(1-expKappa)
  funName <- "Cohen's Kappa"
  result <- list(value,funName)
  names(result) <- c("value","funName")
  return(result)

}
