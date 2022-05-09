#' Generate data with model-based approach accounting for auto-dependence
#'
#' This function generates a new time serie that is similar to the original one
#' in relative frequency and auto-dependence, by drawing samples time point per
#' time point from a Bernoulli distribution with the different conditional probabilities
#' as parameter, depending on the state of the previous time point.
#'
#' @param vec Time series vector
#'
#' @return Time series vector that is similar to the original one in relative frequency and auto-dependence
#' @importFrom  stats runif
#' @export
#'
#' @examples ts=rep(c(1,1,1,1,1,0,0,0,0,0),15)
#' modelAD(ts)

modelAD<-function(vec){

  nTpoints<-length(vec)

  #retrieve parameters p1,p11,p10 for each variable and store in Matrix
  para<-unlist(getProb(vec))

  #depends on Rlab no-more!
  x<-array(NA,nTpoints)
  runif_array = runif(nTpoints)
  x[1]<- as.integer(runif_array[1]< para[1])
  for (tim in 2:nTpoints){
    if (x[tim-1]==0){
      x[tim] <- as.integer(runif_array[tim] < para[3])
    }else{
      x[tim] <- as.integer(runif_array[tim] < para[2])
    }
  }
  return(x)
}

