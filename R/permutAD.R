#' Generate data with permutation-based approach accounting for auto-dependence
#'
#' This function generates a new time serie with exactly the same relative frequency as the original one,
#' and a similar auto-dependence by cutting the original variable in segments and shuffeling these segements.
#'
#' @param vec Time series vector
#' @param nBloks positive integer indicating the number of segements/blocks (default 10)
#'
#' @return Time series vector with exactly the same relative frequency and a similar auto-dependence as the original vector
#'
#' @export
#'
#' @examples ts=rep(c(1,1,1,1,1,0,0,0),15)
#' permutAD(ts,nBloks=11)

permutAD <- function(vec,nBloks=10){

  nTpoints<-length(vec)
  lengthBloks_basic<-trunc(nTpoints/nBloks)
  lengthBloks_rest <- nTpoints%%nBloks
  lengthVar_basic <- trunc(nBloks/4)
  lengthVar_rest <- nBloks-lengthVar_basic*4
  length <- c(rep(lengthBloks_basic+1,times=lengthBloks_rest),
              rep(lengthBloks_basic,times=nBloks-lengthBloks_rest))
  variation <- c(rep(c(-2,-1,1,2),lengthVar_basic),rep(0,lengthVar_rest))
  x<-as.vector(unlist(sample(split(vec, rep(1:nBloks,(length+variation)))), use.names=FALSE))

  return(x)
}
