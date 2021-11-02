#' Explore and tidy raw data
#'
#' @description Removes not binary columns from multivariate time series data and calculates a table of relative frequency and auto-dependency for each binary variable
#'
#' @param data Binary time-points-by-variable matrix
#' @return A conData-object including:
#' @return \code{data}     Binary data in time points to variable format.
#' @return \code{probs}    Table of relative frequency and auto-dependence for each variable.
#' @return \code{varNames} The names of all variables.
#'
#' @export
#'
#' @examples
#' ExampleData <- cbind(rep(c(0,1),100),
#'                      rep(c(0,0,0,0,0,1,1,1,1,1),20),
#'                      c(
#'                        rep(c(0,0,0,1,1),20),
#'                        rep(c(0,1,1,1,1),20)
#'                      ),
#'                      ifelse(rnorm(200,0,1)<0.95,1,0),
#'                      c(
#'                        ifelse(rnorm(100,0,1)<0.7,1,0),
#'                        ifelse(rnorm(100,0,1)<0.7,0,1)
#'                      ),
#'                      ifelse(rnorm(200,0,1)<(-0.98),1,0))
#' colnames(ExampleData) <- c('Var 1','Var 2','Var 3',
#'                            'Var 4','Var 5','Var 6')
#' conData(ExampleData)
#'
#' @examples data(SymptomData)
#' Sdata <- conData(SymptomData)
#' Sdata$probs
#'


conData <- function(data){

  nTpoints <- nrow(data)
  raw_data <- data


  nonBinary <- which(data!=1 & data!=0 & !is.na(data))
  nonBinaryCols <- unique(ceiling(nonBinary/nTpoints))
  if(length(nonBinaryCols)!=0){
    raw_data <- data[,-nonBinaryCols]
  }


  complete_data <- raw_data[complete.cases(raw_data),]
  nvars <- ncol(complete_data)

  varNames <- colnames(complete_data)


  table <- t(array(unlist(apply(complete_data,2,getProb)), dim=c(3,nvars)))
  colnames(table) <- c("rel.freq","p1|1", "p1|0")
  rownames (table)<- varNames


  result <- list(raw_data,table,varNames)
  names(result) <- c('data','probs','varNames')
  class(result) <- 'conData'
  return(result)
}




