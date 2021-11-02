#' Calculate the proportion of agreement between two vectors
#'
#' @param vec1 Vector of binary time series (no missing values)
#' @param vec2 Vector of binary time series (equal length as vec1, no missing values)
#'
#' @return list with two elements
#' @return \code{value} of the proportion of agreement and
#' @return \code{funName} name of the function
#' @export
#'
#' @examples data1 <- rep(c(1,0,1,1),25)
#'         data2 <- ifelse(rnorm(100,0,1)<0.7,0,1)
#'         funPropAgree(data1,data2)
funPropAgree<- function(vec1,vec2){
  bothone<-sum((vec1+vec2)==2)
  bothzero<-sum((vec1+vec2)==0)
  total<-length(vec1)
  value<-(bothone+bothzero)/total
  funName <- 'Proportion of Agreement'
  result <- list(value,funName)
  names(result) <- c("value","funName")
  return(result)
}
