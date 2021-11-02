#' Generate data with permutation-based approach ignoring auto-dependence
#'
#' This function generates a new time serie with exactly the same relative frequency as the original one,
#' but with a lower auto-dependence by shuffeling all time points.
#'
#' @param vec Time series vector
#'
#' @return Time series vector with exactly the same relative frequency as the original vector
#'
#' @export
#'
#' @examples
#' ts=rep(c(1,1,1,1,1,0,0,0,0,0),15)
#' permutNO(ts)

permutNO <- function(vec){
  tpoints <- length(vec)
  index<-sample(1:tpoints)
  x <- vec[index]
  return(x)
}
