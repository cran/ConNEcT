#' Calculate the odds ratio between two vectors
#'
#' @param vec1 Vector of binary time series (no missing values)
#' @param vec2 Vector of binary time series (equal length as vec1, no missing values)
#'
#' @return list with two elements
#' @return \code{value} of the odds ratio and
#' @return \code{funName} name of the function
#' @export
#'
#' @examples data1 <- rep(c(1,0,1,1),25)
#'         data2 <- ifelse(rnorm(100,0,1)<0.7,0,1)
#'         funOdds(data1,data2)
funOdds<-function(vec1,vec2){

  bothone<-sum((vec1+vec2)==2)
  bothzero<-sum((vec1+vec2)==0)
  firstone<-sum((vec1==1&vec2==0))
  secondone<-sum((vec1==0&vec2==1))

  value<-bothone*bothzero/(firstone*secondone)
  funName <- 'Odds Ratio'
  result <- list(value,funName)
  names(result) <- c("value","funName")
  return(result)
}

