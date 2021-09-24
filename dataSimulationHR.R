####### required package:
require(graphicalExtremes) 

#' @author Pouya Roudaki
#' @description Data Simulation for the forest with Huesler-Riess distribution 
#' @param forestStructureList a list corresponding to the forest structure in 
#' which we can find "d", "ncc", "CC" and also "adj" matrix.
#' @param n number of observations in simulated data
#' @param showInitialGamma = False if showInitialGamma = True then we can see
#' the initial Gamma vector
#' @return simulatedData: simulated data of forestStructureList[[1]] (d) 
#' variables and n observations.
#' @example data_simulation_HR(forestStrLst,n = 10000)

dataSimulationHR <- function(forestStructureList, n, showInitialGamma = FALSE){
    
  #' We can exploit information from the forest structure list 
  d <- forestStructureList[[1]]
  ncc <- forestStructureList[[2]]
  adj <- forestStructureList[[3]]
  CC <- forestStructureList[[4]]
  
  #' list of adjacency matrices corresponds to each tree
  adjMatricesList <- list()
  
  for (i in 1:ncc) {  

    adjMatricesList[[i]] <- adj[ CC[[i]][1]:CC[[i]][2] , CC[[i]][1]:CC[[i]][2] ]
    
  }
  
  #' Making the list of trees from their adjacency matrices
  treesList <- list()
  
  for (i in 1:ncc) {   
    treesList[[i]] <- igraph::graph_from_adjacency_matrix(adjMatricesList[[i]],
                                                   mode = "undirected")
  }
  
  #' Gamma assumption for cliques isolated nodes have zero and for other edges
  #' we randomly generate a number between 0 to 10 (whole support is zero to inf)
  #' for extremal gamma which shows high independence, otherwise if this edges 
  #' were able to take larger extremal gamma then it makes problem for distinguishing
  #' between independent(unconndected) nodes and dependent(connected) nodes.
  #' Note that for extramal gamma(Variogram) zero corresponds to highly dependence
  #' and infinity correspoonds to highly independence.

  extremalGammaList <- list()
  extremalChiList <- list()
  
  #' list of Gamma vectors corresponds to cliques(edges) of each tree 
  for (i in 1:ncc) {   
    
    #' isolated trees don't have 
    if (sum(adjMatricesList[[i]]) == 0) { 
      extremalGammaList[[i]] <- matrix(0)
      extremalChiList[[i]] <- matrix(1) 
      
    #' For not isolated nodes  
    }else{ 
      
      cliquesNumbers <- sum(adjMatricesList[[i]])/2
      #' ExtrGammaVecOnCliques <- c(1,1,1,1,1) 
      #' Generate random vector of Gamma values on cliques
      ExtrGammaVecOnCliques <- runif(cliquesNumbers,min = 0,max = 10) 
      #' Max can't be infinity ? maybe it's better to define Chi and  convert it
      #'not complete gamma and complete it or is there and Chi_complete?!
      if (showInitialGamma == TRUE){
        paste("Extremal Gamma Vector On Cliques:", ExtrGammaVecOnCliques)
      }
      #' Use Gamma completion method to make a complete gamma
      extremalGammaList[[i]] <- complete_Gamma(Gamma =  ExtrGammaVecOnCliques,
                                               graph = treesList[[i]])
                                               
   
      
      #' Make Extreaml Chi corresponds to this Gamma
      extremalChiList[[i]] <- Gamma2chi(extremalGammaList[[i]])
      
    }
  }
  #' Simulation of data for each connected component 
  treesDataList <- list()
  for (i in 1:ncc) {
    #' simulation of multivariate random sample corresponds to each tree and its
    #' Gamma
    treesDataList[[i]] <- rmstable_tree(n, "HR", tree = treesList[[i]],
                                        par = extremalGammaList[[i]])
  }
  #' merge data of trees together
  simulatedData <- do.call(cbind,treesDataList)
  #' return whole dataset
  return(simulatedData)
}
