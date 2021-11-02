#' Retrieve (conditional) probabilities from binary time series
#'
#' @param ts Binary time series vector
#'
#' @return List of three elements
#' @return \code{p1} the prevalence p(X(t)=1) and
#' @return \code{p1|1} and \code{p1|0} the two auto-conditional probabilities p(X(t)=1|X(t-1)=1) & p(X(t)=1|X(t-1)=0)
#' @export
#'
#' @examples getProb(c(1,0,1,0,1,0,1,1,1,1,0,0,0,0,1,0,1,1,0,0,0,0,0,0,0,0))

getProb<-function(ts){

  N=length(ts)
  prob1<-mean(ts)

  freq11<-sum((ts[-1]+ts[-N])==2)
  freq1<-sum(ts[-N]==1)
  prob11<-freq11/freq1
  prob10<-sum(ts[-1]==1&ts[-N]==0)/sum(ts[-N]==0)

    return(list(prob1,prob11,prob10))
}

