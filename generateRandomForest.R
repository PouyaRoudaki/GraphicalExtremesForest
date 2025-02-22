source("scissorsMaker.R")
source("CCMaker.R")
source("prueferForest.R")


#' @author Pouya Roudaki
#' @description With this function we are able to generate random forests.
#' randomness of each forest comes from two steps: 
#' 1. random specification of nodes in each connected component
#' 2. random generation of a tree for each selected connected component
#' @param d Number of vertices
#' @param type Type of construction of the forest it can be based on number of
#' connected components, ncc, or based on size of connected components, scc,
#' type = "ncc" or "scc"
#' @param ncc Number of connected components
#' @param maxSCC Maximum size of connected components 
#' @param minSCC Minimum size of connected components
#' @param seed seed for randomness.
#' @return forestStructureList: a list containing:
#' [[1]]: d Number of vertices
#' [[2]]: ncc number of connected components
#' [[3]]: adjacency matrix of forest
#' [[4]]: a list of beginning and ending vertices and also size of each 
#' connected components
#' @example 
#' generateRandomForest(14,type = "scc",maxSCC = 6,minSCC = 5)
#' generateRandomForest(10,"scc",maxSCC = 4)
  
  
generateRandomForest <- function(d, type = "ncc", ncc = NULL, maxSCC = d,
                                   minSCC = 1, seed = NULL, showPlot = F){  

  #' Making random scissors scissorsMaker()
  scissors <- scissorsMaker(d = d, type = type, ncc = ncc, maxSCC = maxSCC,
                            minSCC = minSCC, seed = seed)                                                        

  
  #' Making a list of beginning and ending vertices and also size of each 
  #' connected components with CCMaker()
  CC <- CCMaker(d, scissors)

  #' With  prueferForest() we are able to generate random trees for each 
  #' connected component and return the finalized random adjacency matrix for a 
  #' random forest with "CC" list for connected components. 
  adjMat <- prueferForest(d, scissors, CC)
    
  if(showPlot == TRUE){
    #' Making a graph with adjacency matrix
    g <- igraph::graph_from_adjacency_matrix(adjMat, mode='undirected')   
    #' Plotting the graph
    igraph::plot.igraph(g) 
  }
                                              
  forestStructureList <- list(d , length(scissors)+1 , adjMat , CC)
  return(forestStructureList)
}

