#' Draws a Network figure
#'
#' @param x A conNEcT-object
#' @param signOnly Logic indicating whether significant links should be depicted (TRUE; default) or not (FALSE)
#' @param ... Parameter settings for qgraph
#'
#' @return A network plot
#' @export
#' @import qgraph
#'
#'
#' @examples 
#' ADOangry=rep(c(1,1,1,1,1,0,0,0,0,0),100)
#' MAangry=rep(c(0,0,1,1,1,1,0,0,0,0),100)
#' MAsad=rep(c(0,0,1,1,1,1,0,0,0,0),100)
#' netdata <- cbind(ADOangry,MAangry,MAsad)
#' netnet <- conNEcT(netdata,lag=1,conFun=funKappa,
#'                            test=TRUE,nBlox=5)
#' qgraph.conNEcT(netnet,signOnly=FALSE)

qgraph.conNEcT<-function(x,signOnly=TRUE,...){
  if(signOnly==TRUE){
    Link_data <- unlist(x$signLinks)
  }else{
    Link_data <- unlist(x$allLinks)
  }

  Link_data <- ifelse(is.na(Link_data),0,Link_data)
  Link_data <- as.matrix(Link_data)

  if(x$para$lag==0) diag(Link_data)<-0


  Node_data<-x$probs[,1]
  labels=x$para$varNames

  if(!x$para$test&signOnly){
    warning('No singificance test had been executed to generate this conNEcT object:
            please rerun conNEcT after adaping your settings or change the settings to print all links')

  }else{
    qgraph(Link_data,vsize=Node_data*10/max(Node_data)+5,
           asize=5,...)

  }
}
