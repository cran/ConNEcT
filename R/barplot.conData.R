#' Depict the relative frequencies (and conditional probabilities) of a binary time series in a barplot
#'
#' @param height  conData object
#' @param plottype needs to be specified
#'             if 'RelFreq' only the relative frequency is depicted
#'             if 'All' both the relative frequency and the conditional probabilities are depicted
#' @param color string of chars contanig as many colornames as variables in the object (default='gray')
#' @param legend indicates whether you want to include a legend or not
#' @param ... parameters to be passed on to barplot
#'
#' @return barplot
#' @export
#' @importFrom graphics barplot legend
#'
#' @examples
#' ExampleData <- cbind(rep(c(0,1),100),
#'                      rep(c(0,0,0,0,0,1,1,1,1,1),20),
#'                      c(rep(c(0,0,0,1,1),20),rep(c(0,1,1,1,1),20)),
#'                      ifelse(rnorm(200,0,1)<0.95,1,0),
#'                      c(ifelse(rnorm(100,0,1)<0.7,1,0),ifelse(rnorm(100,0,1)<0.7,0,1)),
#'                      ifelse(rnorm(200,0,1)<(-0.98),1,0))
#'  colnames(ExampleData) <- c('Var 1','Var 2','Var 3',
#'                             'Var 4','Var 5','Var 6')
#'  fancy.col <- c('purple','slateblue','royalblue','cyan4',
#'                 'green3','olivedrab3')
#'  PersData <- conData(ExampleData)
#'  barplot(PersData, plottype='RelFreq', color=fancy.col)
#'  barplot(PersData, plottype='All', color=fancy.col)
#'
#'  data(SymptomData)
#'  Sdata <- conData(SymptomData)
#'  FANCY= c('purple','slateblue', 'royalblue', 'cyan4', 'green3',
#'           'olivedrab3', 	'orange', 'orangered')
#'  barplot(Sdata,plottype='RelFreq', color = FANCY)

barplot.conData <- function(height,
                            plottype='RelFreq',
                            color=NULL,
                            legend=TRUE,...){

  height.data <- height$probs
  varNames <- height$varNames

  if (plottype == 'All'){
    if(length(color)<3){
      color = c('darkslategray1','darkslategray3','darkslategray4')
    }else if (length(color) > 3){
      color=color[1:3]
    }

    barplot(t(height.data)[,nrow(height.data):1],
            main = "" ,
            xlab = "" ,
            ylab = "" ,
            names.arg = rev(varNames) ,
            col = color ,
            las = 2 , cex.axis = 0.8 ,
            beside=TRUE ,
            horiz=TRUE)
    if(legend==TRUE) legend('topright', c("rel.freq","p1|1","p1|0"), fill = color,cex=0.7)


  }else if (plottype == 'RelFreq'){
    if(is.null(color)) color='darkslategray3'
    barplot(rev(height.data[,1]) ,
            main = "" ,
            xlab = "",
            ylab = "",
            names.arg = rev(varNames),
            col = rev(color),
            las = 2 , cex.axis = 1 ,
            beside = TRUE , horiz = TRUE)
  }
}

