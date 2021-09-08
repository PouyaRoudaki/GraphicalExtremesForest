plot_pouya_graph <- function(d,emp_adj_cumulative){
  
  require(dplyr)
  edges <- c()
  
  for (i in 2:d) {
    for (j in 1:(i-1)) {
      
      edges <- c(edges,i) 
      edges <- c(edges,j)
      
    }
  }
  
  weights_vec <- emp_adj_cumulative[upper.tri(emp_adj_cumulative, diag = F)]

  g <- graph( edges = edges, n = d , directed = FALSE)
  E(g)$weight <- weights_vec/m
  
  
  h <- graph.empty(directed = F) + vertices(V(g))
  h <- h + edges(as.vector(t(get.edgelist(g)[order(E(g)$weight),])))
  E(h)$weight <- E(g)$weight[order(E(g)$weight)]
  
  h <-  delete_edges(h,E(h)[which(E(h)$weight==0)]) # to delete zero weighted edges
    
  E(h)$color <- "gray"
  E(h)[weight>0.5]$color <- "black"
  

  plot(h,edge.width=2+4*E(h)$weight)
  
 
  
}
