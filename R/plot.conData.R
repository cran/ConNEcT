#' Visualize the course of the variables over time
#'
#' @param x conData object
#' @param plottype Character specified as 'interval', 'line', or 'both'
#' @param color  Character string of colornames for all variables (default='black')
#' @param ... Parameters to be transfered to the plot function
#'
#' @export
#' @importFrom graphics axis lines mtext par points
#' @return Plot visualizing the course of the variables over time
#'
#' @examples
#' ExampleData <- cbind(rep(c(0,1),100),
#'                      rep(c(0,0,0,0,0,1,1,1,1,1),20),
#'                      c(rep(c(0,0,0,1,1),20),
#'                        rep(c(0,1,1,1,1),20)),
#'                      ifelse(rnorm(200,0,1)<0.95,1,0),
#'                      c(
#'                       ifelse(rnorm(100,0,1)<0.7,1,0),
#'                       ifelse(rnorm(100,0,1)<0.7,0,1)
#'                      ),
#'                      ifelse(rnorm(200,0,1)<(-0.98),1,0))
#' colnames(ExampleData) <- c('Var 1','Var 2','Var 3',
#'                            'Var 4','Var 5','Var 6')
#' PersData <- conData(ExampleData)
#' fancy.col=c('purple','slateblue', 'royalblue', 'cyan4',
#'             'green3', 'olivedrab3', 'orange', 'orangered')
#' plot(PersData,plottype='line',color=fancy.col)
#'
#' data(SymptomData)
#' Sdata <- conData(SymptomData)
#' fancy.col=c('purple','slateblue', 'royalblue', 'cyan4',
#'             'green3', 'olivedrab3', 'orange', 'orangered')
#' plot(Sdata, plottype='interval',color=fancy.col)
#'
#'

plot.conData <- function(x, plottype='interval',color=NULL,...){

  raw_data <- x$data
  nvars <- ncol(raw_data)
  varNames <- x$varNames
  nTpoints <- nrow(raw_data)
  if(is.null(color)){
    color=c(rep('black',nvars))
  }


  plot(c(1:nTpoints),rep(1,nTpoints),type='l',
       main='', xlab='', ylab='',ylim=c(0.5,nvars+1),yaxt='n',
       lty=3, col="gray80",cex=0.8,...)
  for(li in 2:(nvars))lines(c(1:nTpoints),rep(li,times=nTpoints),lty=3, col='gray80', lwd=1)
  if (plottype=='interval'){
    for (var in 1:nvars) {
      points(c(1:nTpoints),ifelse(raw_data[,var]==1,nvars-var+1,NA), col=color[var],pch='|')
    }
  }else if(plottype=='line'){
    for (var in 1:nvars) lines(c(1:nTpoints),raw_data[,var]*0.5+(nvars-var+1), col=color[var],lwd=2)
  }else if (plottype=='both'){
    for (var in 1:nvars) lines(c(1:nTpoints),raw_data[,var]*0.5+(nvars-var+1), col=color[var],lwd=2)
    for (var in 1:nvars) points(c(1:nTpoints),raw_data[,var]*0.5+(nvars-var+1), col=color[var],pch='|')
  }
  axis(2,at=c(nvars:1),labels=x$varNames,las=2,cex.axis=1)
}







