
#' @description With this function we are able to generate random trees for 
#' each connected component and return the finalized random adjacency matrix
#' for a random forest with "CC" list for connected components. 
#' @param d Number of vertices
#' @param scissors vector of scissors
#' @param CC a list of begging, ending, and size of each connected component 
#' @return adj: symmetric adjacency matrix
#' @example prueferForest(d, scissors, CC)
  
prueferForest <- function(d, scissors, CC){
  

  #' Adjacency matrix
  adj <- matrix(0, d, d)  
  for (i in 1:(length(scissors)+1)) {
    #' In this case there are only two vertices in the connected component so 
    #' there is no randomness here! just connect them!
    if(CC[[i]][3] == 2){  
      
      adj[CC[[i]][1],CC[[i]][2]] <- 1
      
      #' In this case there are more than two vertices in the connected component 
      #' so we need to generate random tree  
    }else if (CC[[i]][3]>2) {   
      
      #' Making the pruefer sequence correspond to each tree of forest (each 
      #' connected component)
      pruefer <- floor(stats::runif(n = (CC[[i]][3]-2),  
                                    min = CC[[i]][1],
                                    max = CC[[i]][2]))
      
      #' vertices 
      vertices <- CC[[i]][1]:CC[[i]][2] 
      
      while(length(pruefer)>0){
        #' First vertice: first element of updated sequence
        p <- pruefer[1]      
        #' Second vertice: vertice with minimum label in vertices which are not 
        #' in the updated sequence
        v <- min(setdiff(vertices, pruefer)) 
        #' Connect first and second vertices in adjacency matrix  
        adj[v, p] <- 1                  
        #' Remove first vertice from vertices set
        vertices <- setdiff(vertices, v)   
        #' Remove Second vertice from the sequence
        pruefer <- pruefer[-1]                                         
      }
      #' Connect two vertices to each other. 
      adj[vertices[1], vertices[2]] <- 1                                
    } 
  }
  #' Making a symmetric adjacency matrix
  adj <- adj + t(adj)
  #' return symmetric adjacency matrix for a random forest with specified scissors
  return(adj)
} 