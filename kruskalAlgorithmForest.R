
################################################################################
########### Kruskal algorithm for forest non object oriented approach ##########
################################################################################


################################################################################
########################### Find Parent function ###############################
################################################################################

findParent <- function(parent,i){
  
  ##############################################################################
  
  #' @author Pouya Roudaki
  #' @description  Recursive function to find the parent corresponds to each node
  #' @param  parent: a vector of parents corresponding to node
  #' @param  i: a node 
  #' @return parent of node i 
  #' @example 
  #' parent1 <- c(1,1,3,4)
  #' findParent(parent1,3)
  #' parent1[1]
  
  ##############################################################################
  
  if (parent[i] == i) {
    return(i) 
  }
  
  return( findParent(parent,parent[i]) )
  
}


################################################################################
########################### Union Parent function ###############################
################################################################################

unionParent <- function(parent,rank,x,y)
{
  ##############################################################################
  
  #' @author Pouya Roudaki
  #' @description  Function to make parents of neighbors a common node
  #' @param  parent: a vector of parents corresponding to node
  #' @param rank: a vector of rank for preference on choosing a node as a parent 
  #' in equal situation.
  #' @param  x: First node of the edge
  #' @param  y: Second node of the edge
  #' @return return(list(parent,rank)) list of updated parent vector and rank 
  #' vector
  #' @example 
  #' parent <- c(1,2,3,4)
  #' rank <- c(0,0,0,0)
  #' templist <- unionParent(parent,rank,3,4)
  #' parent <- templist[[1]]
  #' parent
  #' rank <- templist[[2]]
  #' rank
  #' unionParent(parent,rank,1,4)
  
  ##############################################################################
  
  xroot <- findParent(parent,x)
  yroot <- findParent(parent,y)
  
  #### attach smaller rank tree under root of high rank tree (Union by rank)
  if( rank[xroot] < rank[yroot]){
    
    parent[xroot] <- yroot
    
  }else if( rank[xroot] > rank[yroot] ){
    
    parent[yroot] <- xroot
    
  }else {
    
    parent[yroot] <- xroot
    rank[xroot] <- rank[xroot] + 1
    
  }
  
  return(list(parent,rank))
}


################################################################################
#####################  KRUSKAL ALGORITHM FOR MSF  ##############################
################################################################################


KruskalMSF <- function( graphDf , maxEdge = d , threshold = Inf)
{
  
  ##############################################################################
  
  #' @author Pouya Roudaki
  #' @description  Kruskal algorithm for making minimum spanning forests:
  #' The suggestion is to stop Kruskal algorithm with 2 methods:
  #' 1. When we attain a maximum number of Edges 
  #' 2. When we pass a threshold value for weights which can be
  #' extremal Variogram or log(extramal Chi)
  
  #' @param  graphDf: a vector of parents corresponding to node
  #' @param  maxEdge: maximum  number of Edges
  #' @param  threshold: threshold for maximum weight
  #' @return empAdj: empirical adjacancy matrix corresponding to empirical weights
  #' on graph edges.
  #' @example 
  #' empAdj <- KruskalMSF(graphDf = emp_vario_graphDf)
  #' # making a graph with adjacency matrix
  #' g <- igraph::graph_from_adjacency_matrix(empAdj, mode='undirected')
  #' # plotting the graph     
  #' igraph::plot.igraph(g)                                               
  
  
  #' empAdj <- KruskalMSF(graphDf = emp_vario_graphDf,k = 7)
  #'  # making a graph with adjacency matrix
  #' g <- igraph::graph_from_adjacency_matrix(empAdj, mode='undirected')    
  #' igraph::plot.igraph(g)   
  
  ##############################################################################
  
  #' store resultant MSF
  result <- data.frame( FirstVertice = numeric(),  
                        SecondVertice = numeric(), 
                        weight = numeric(), 
                        stringsAsFactors=FALSE) 
  empAdj <- matrix(0,nrow = d,ncol = d )
  
  #' index variable used for stored edges
  i <- 1           
  #' index variable used for resultant list
  e <- 0           
  
  ##############################################################################
  #' Step1: Sort all the edges in non-decreasing order of their weights
  
  #' Sorting base on order
  graphDf <- graphDf[order(graphDf[,3]),]         
  
  
  ##############################################################################
  #' Middle Step: Create V subsets with single elements ( parent of each node is
  #'  itself and its rank is 0 )
  
  parent <- vector()
  rank <- vector()
  
  for (node in 1:d) {
    
    parent <- append( parent , node)
    rank <- append( rank , 0)
    
  }
  
  ##############################################################################
  #' Step 2 : Adding the edges in non-decreasing order of weights in a greedy way
  #' and ignoring the edges which make cycle
  
  weightctrl <- 0
  
  #' number of edges to be taken for a tree is equal to V-1 for making forest with
  #' k edges or with weight <  threshold we can change the condition for while 
  #' loop
  while ( (e < (d - 1)) & (e < maxEdge) & (weightctrl < threshold)) { 
    
    #' Pick the edge with smallest weight and increment the index for next 
    #' iteration
    
    #' First node of i_th edge
    u <- graphDf[i,1] 
    #' Second node of i_th edge
    v <- graphDf[i,2] 
    #' Seight of i_th edge
    w <- graphDf[i,3]   
    
    #' We increased i to i+1 in past.
    i <- i + 1
    
    if(i <= dim(graphDf)[1]){ 
      
      weightctrl <- graphDf[i,3]
      
    }
    
    #' Find parent of first node of i_th edge
    x <- findParent(parent,u) 
    #' Find parent of second node of i_th edge
    y <- findParent(parent,v) 
    
    #' if including this edge doesn't cause a cycle then include it in result 
    #' and increment the index of result for next edge.
    
    if( x != y ){
      
      #' We are adding the edge so we must increment the edge counter by one.
      e <- e + 1  
      
      result[e,1] <- u 
      result[e,2] <- v
      result[e,3] <- w
     
      
      templist <- list()
      templist <- unionParent(parent,rank,x,y)
      parent <- templist[[1]]
      rank <- templist[[2]]
      
    }
    #' ELSE: discard the edge
    
  }
  
  minimumCost <- 0
  
  for (j in 1:dim(result)[1]) {
    
    minimumCost <- minimumCost + result[j,3]
    
  }
  
  cat("-")
  
  #' Making the empirical adjacency matrix
  for (k in 1:dim(result)[1]) { 
    
    i <- result[k,1]
    j <- result[k,2]
    
    #' add the edge to adjacency matrix
    empAdj[i,j] <- 1   
    
  }
  
  #' Make the adjacency matrix symmetric 
  empAdj <- empAdj + t(empAdj)  
  
  #' return empirical adjacency matrix
  return(empAdj)
}


