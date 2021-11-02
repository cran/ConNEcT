#' Lag a matrix
#'
#' @param data Binary time-points-by-variable matrix
#' @param lag Non-negative integer indicating the number of time points the second variable is lagged (default 0)
#'
#' @return \code{laggeddata} Matrix in which all variables are lagged \code{lag} time points
#' @export
#'
#' @examples lagthemats(cbind(c(1,0,1,0,1,0,1),c(1,1,1,1,0,0,0)),lag=2)
#'
#'
lagthemats<-function(data,lag){
  if (lag>0){
    for (i in 1:lag){
      N<-nrow(data)
      mats.temp<-data[-1,]
      laggeddata<-rbind(mats.temp,rep(NA,ncol(data)))
      data=laggeddata
    }
  }else if (lag==0){
    laggeddata=data
  }
  laggeddata
}
