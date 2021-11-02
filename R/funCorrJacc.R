#' Calculate the corrected Jaccard index between two vectors
#'
#' @param vec1 Vector of binary time series (no missing values)
#' @param vec2 Vector of binary time series (equal length as vec1, no missing values)
#'
#' @return list with two elements
#' @return \code{value} of the corrected Jaccard index and
#' @return \code{funName} name of the function
#' @export
#'
#' @examples data1<-rep(c(1,0,1,1),25)
#'         data2<-ifelse(rnorm(100,0,1)<0.7,0,1)
#'         funCorrJacc(data1,data2)
funCorrJacc<- function(vec1, vec2){

  #Calculate classic Jaccard
  bothone<-sum((vec1+vec2)==2)
  bothzero<-sum((vec1+vec2)==0)
  total<-length(vec1)
  classJacc<-bothone/(total-bothzero)

  #calculate expected Jaccard
  p1<-mean(vec1)
  p2<-mean(vec2)
  expJacc<-(p1*p2)/(1-(1-p1)*(1-p2))

  #Calculate corrected Jaccard
  corrJacc=(classJacc-expJacc)/(1-expJacc)

  funName <- 'Corrected Jaccard'
  result <- list(corrJacc,funName)
  names(result)<-c('value','funName')
  return(result)

}



