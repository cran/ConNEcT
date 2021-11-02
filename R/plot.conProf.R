#' Draw contingency profiles
#'
#' @param x   Object of a conProf class
#' @param ... Parameters to be transfered to the plot function
#'
#' @return Contingency profile matrix
#' @export
#' @importFrom graphics par
#'
#' @examples  IntData <- cbind(rep(rep(c(0,0,1,0,1,0,1,0,0,0),each=5),times=5),
#'                             rep(rep(c(1,0,0,0), each=10), times=25))
#'            colnames(IntData) <- c('Var1','Var2')
#'            CP <- conProf(IntData,maxlag=10,conFun=funClassJacc)
#'            plot(CP)


plot.conProf <- function(x,...){
  oldpar <- par(no.readonly = TRUE) 
  on.exit(par(oldpar))    
  
  maxlag <- x$para$maxLags
  funName <- x$para$funName
  varName <- x$para$varNames
  nVar <- ncol(x$value)

  par(mfrow=c(nVar,nVar), oma=c(2,2,2,2))
  for (i in 1:nVar){
    for (j in 1:nVar){
      par(mar=c(0.2,0.2,0.2,0.2))
      drp <- x$value[i,j,]

      plot(seq(-maxlag,maxlag,1),drp,type='l',
           xlab='',main='',yaxt='n',xaxt='n',
           ylab='',cex.lab=1,lwd=2,...)
      abline(v=0, lty=2)
      if(i==1){mtext(side=3,line=0.5,varName[j],cex=1)}
      if(i==nVar){
        axis(side=1,at=seq(-maxlag,maxlag,5),cex.axis=1)
      }
      if(j==1){mtext(side=2,line=0.5,varName[i],cex=1)}
    }
  }
}

