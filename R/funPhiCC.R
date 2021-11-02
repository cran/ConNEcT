#' Calculate the phi correllation coefficient index between two vectors
#'
#' @param vec1 Vector of binary time series (no missing values)
#' @param vec2 Vector of binary time series (equal length as vec1, no missing values)
#'
#' @return list with two elements
#' @return \code{value} of the phi correlation coefficient and
#' @return \code{funName} name of the function
#' @export
#'
#' @examples set.seed(1234)
#'           data1<-rep(c(1,0,1,1),25)
#'           data2<-ifelse(rnorm(100,0,1)<0.7,0,1)
#'           funPhiCC(data1,data2)
funPhiCC<- function(vec1, vec2){
  n11<-sum((vec1+vec2)==2)
  n00<-sum((vec1+vec2)==0)
  n10 <- sum(vec1==1&vec2==0)
  n01 <- sum(vec1==0&vec2==1)
  n1x <- sum(vec1==1)
  nx1 <- sum(vec2==1)
  total<- length(vec1)
  value<-(n11*n00-n10*n01)/(sqrt(n1x*(total-n1x))*sqrt(nx1*(total-nx1)))
  funName <- 'Phi Correlation Coefficient'
  result <- list(value,funName)
  names(result) <- c("value","funName")
  return(result)
}


