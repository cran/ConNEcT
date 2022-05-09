#' Generate data with model-based approach ignoring auto-dependence
#'
#' This function generates a new time serie that is similar to the original one in relative frequency,
#' but not in auto-dependence by drawing from a Bernoulli distribution with the relative frequency as parameter.
#'
#' @param vec Time series vector
#'
#' @return Time series vector that is similar to the original one considering relative frequency
#'
#' @importFrom  stats runif
#' @export
#'
#' @examples
#' ts=rep(c(1,1,1,1,1,0,0,0,0,0),15)
#' modelNO(ts)
#'

modelNO <- function(vec){

  nTpoints <- length(vec)
  para <- mean(vec,na.rm=TRUE)

  #depends on Rlab nevermore!
  x <- array(NA,nTpoints)
  runif_array = runif(nTpoints)
  x <- as.integer(runif_array < para)

  return(x)
}

