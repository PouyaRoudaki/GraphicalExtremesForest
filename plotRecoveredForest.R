require(dplyr)

plotForestGraph <- function(d,empAdjCumulative){
  
  #' @author Pouya Roudaki
  #' @description plot the a forest with specified thickness for each edge 
  #' proportional to weight of that edge
  #' @param d Number of vertices
  #' @param empAdjCumulative empirical cummulative adjacency matix correponding 
  #' to weights of edges
  #' @return forest plot   

  edges <- c()
  
  for (i in 2:d) {
    for (j in 1:(i-1)) {
      
      edges <- c(edges,i) 
      edges <- c(edges,j)
      
    }
  }
  
  weights_vec <- empAdjCumulative[upper.tri(empAdjCumulative, diag = F)]

  g <- graph( edges = edges, n = d , directed = FALSE)
  E(g)$weight <- weights_vec/m
  
  
  h <- graph.empty(directed = F) + vertices(V(g))
  h <- h + edges(as.vector(t(get.edgelist(g)[order(E(g)$weight),])))
  E(h)$weight <- E(g)$weight[order(E(g)$weight)]
  
  #' To delete zero weighted edges
  h <-  delete_edges(h,E(h)[which(E(h)$weight==0)]) 
    
  E(h)$color <- "gray"
  E(h)[weight>0.5]$color <- "black"
  

  plot(h,edge.width=2+4*E(h)$weight)
  
}
