####### required package and function:
require(graphicalExtremes) 

source("dataSimulationHR")

#' @author Pouya Roudaki
#' @description Data Simulation for the forest with Huesler-Riess distribution 
#' @param forestStructureList a list corresponding to the forest structure in 
#' which we can find "d", "ncc", "CC" and also "adj" matrix.
#' @param n number of observations in simulated data
#' @param p exceedance probability threshold for extreme values
#' @param k for lamba^{k} (product space matters) k=NULL overall average. 
#' @return dataframe of weights on the edges base on extremal Chi and extremal
#' Variogram(Gamma) 
#' @example forestSimulationHR(forestStrLst,n = 10000 , p =0.9)

forestSimulationHR <- function(forestStructureList, n, p=0, k=NULL){
  
  #' Simulated dataset
  simulatedData <- dataSimulationHR(forestStructureList,n = n)
  
  #'Empirical estimation for extremal Chi and extremal Variogram

  #' I expect emp_chi_multdim() works but emp_chi() works here why?
  empChiMat <- emp_chi(simulatedData, p) 
  #' why it doesn't have decimals????!!!!!
  empChiMat 
  
  #' what is the good k?! null is the best.
  empVarioMat <- emp_vario(simulatedData, k=NULL, p) 
  empVarioMat                               
  
  #' Graph of Empirical estimations in dataframe format 

  #' store resultant MSF in the dataframe format each row: each edge's vertices
  #'  with its weight
  empChiGraphDf <- data.frame( FirstVertice = numeric(),  
                                  SecondVertice = numeric(), 
                                  weight = numeric(), 
                                  stringsAsFactors=FALSE)  
  #' store resultant MSF in the dataframe format each row: each edge's vertices 
  #' with its weight
  empVarioGraphDf <- data.frame( FirstVertice = numeric(),  
                                    SecondVertice = numeric(), 
                                    weight = numeric(), 
                                    stringsAsFactors=FALSE) 
  
  # Dataframe of edges and corresponding weights
  e <- 1
  d <- forestStructureList[[1]]
  for (i in (1:(d-1))) {                                           
    for (j in ((i+1):d)) {
      
      empChiGraphDf[e,1] <- i
      empVarioGraphDf[e,1] <- i
      
      empChiGraphDf[e,2] <- j
      empVarioGraphDf[e,2] <- j
      
      empChiGraphDf[e,3] <- empChiMat[i,j]
      empVarioGraphDf[e,3] <- empVarioMat[i,j]  
      
      e <- e + 1
    }
  }
  
  empiricalEstGraphDfList <- list(empChiGraphDf,empVarioGraphDf)
  return(empiricalEstGraphDfList)
}
