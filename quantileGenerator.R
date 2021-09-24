    
#' @author Pouya Roudaki
#' @description   This function finds p-quantile for each column(variable) of
#' a dataset.  
#' @param  simulatedData: simulated data. n*d
#' @param  p: p-quantile
#' @return qDf: a dataframe of p-quantile of variables
#' @example quantileGenerator(simulatedData, p = 0.9)

quantileGenerator <- function(simulatedData,p){

  n <- dim(simulatedData)[1]
  d <- dim(simulatedData)[2]
  
  qDf <- matrix(0,d,1)
  colnames(qDf) <- paste("p =",p,"quantiles for n =",n)
  
  for (i in 1:d) {
    qDf[i,1] <- quantile(simulatedData[,i], probs = seq(0, 1, p), na.rm = FALSE)[2] 
  }
  
  #' return a dataframe of p-quantile of variables
  return(qDf)
}

