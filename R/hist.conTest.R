#' Plot histogram matrix of the significance test
#'
#' @param x Object of the class conTest
#' @param signLev Significance level (default .05)
#' @param ... Graphical parameters to be passed to hist()

#'
#' @return Histogram matrix with sample distribution and value from observed data for each variable combination
#' @export
#' @importFrom graphics hist
#' @importFrom graphics abline
#' @importFrom stats complete.cases
#'
#' @examples data(SymptomData)
#'           test.result <- conTest(SymptomData[,c(2,6,8)],conFun=funClassJacc,
#'                           typeOfTest='permut',nBlox=10,adCor=TRUE,nReps=100)
#'           hist(test.result)

hist.conTest <- function(x,signLev=.05,...){
  oldpar <- par(no.readonly = TRUE) 
  on.exit(par(oldpar))  
  
  varNames <- x$para$varNames
  nVar <- length(varNames)
  par(mfrow=c(nVar,nVar), oma=c(2,4,4,2))
  for (var1 in 1:nVar){
    for(var2 in 1:nVar){
      par(mar=c(1,1,1,1))
      nameVar1 <- toString(varNames[var1])
      nameVar2 <- toString(varNames[var2])
      rhs <- paste0("samples <- x$samples$",nameVar1,"$",nameVar2)
      eval(parse(text=rhs))

      samples <- samples[complete.cases(samples)]
      obs0 <- x$allLinks
      obs <- obs0[which(rownames(obs0)==nameVar1),which(colnames(obs0)==nameVar2)]

      xrange=range(obs,samples,na.rm=TRUE)

      lowerdata <- floor(min(xrange)*10)/10
      upperdata <- ceiling(max(xrange)*10)/10

      hist(samples,xlim=c(lowerdata,upperdata),xaxt='n',main="",
           xlab='',...)
      axis(1,at=seq(lowerdata,upperdata,0.1))
      abline(v=obs, col='grey50',lwd=2)
      sams <- sort(samples)
      threshold <- sams[length(sams)*(1-signLev)]
      abline(v=threshold,lty=2, lwd=2)
      if(var1==1){mtext(side=3,line=2,varNames[var2],cex=1)}
      if(var2==1){mtext(side=2,line=2.5,varNames[var1],cex=1)}
    }


  }
}

