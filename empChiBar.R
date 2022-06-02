################################################################################
# \ChiBar

require(copula)

empChiBar <- function(data, p = 0) {
  if (!is.matrix(data)) {
    stop("The data should be a matrix")
  }
  if (ncol(data) <= 1) {
    stop("The data should be a matrix with at least two columns.")
  }
  
  n <- nrow(data)
  d <- ncol(data)
  
  uniformData <- pobs(data)
  
  univariateExceedanceProb <- matrix(colSums(uniformData > p),byrow = T,
                                     nrow = d,ncol = d)/n
  
  bivariateExceedanceProb <- crossprod(uniformData>p)/n
  
  chiBarHat <- ( log(univariateExceedanceProb) +
              t(log(univariateExceedanceProb)) )/ log(bivariateExceedanceProb)-1
  
  return(chiBarHat)
}


