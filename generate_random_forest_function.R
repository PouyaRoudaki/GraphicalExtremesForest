### Forest making 

################################################################################
####################  initial values for prospective function ##################
################################################################################

generate_random_forest <- function(d){  # d : number of vertices 
  
  adj <- matrix(0, d, d)                                     # adjacency matrix
  s <- sample(1:d, 1)                                       # number of separated sets.
  
  # ******if s == 1 : it's a tree go to Manuel's algorithm ELSE ** Should I do it???
  
  separators <- sort(sample.int(d, s,replace = FALSE),decreasing = F)       # placing the separators... 
  separators                                                  # when you have two consecutive numbers. i.g j and j+1 
  # it means j+1 is an isolated one. in the forest
  
  
  ################################################################################
  ####################  Making the separeated sequences   ########################
  ################################################################################
  
  null_seq <- rep(NA,3)
  Sequences_interval <-rep(list(null_seq),s) ### Making an empty list of 3 elements vectors corresponds to each tree sequence.
  
  for (i in 1:s) {
    
    if(i == 1){                                       # Starting point of each sequence
      
      Sequences_interval[[i]][1] <- 1 
      
    }else{
      
      Sequences_interval[[i]][1] <- separators[i-1] + 1 
      
    }
    
    if(i == s){                                       # Ending point of each sequence
      
      Sequences_interval[[i]][2] <- d 
      
    }else{
      
      Sequences_interval[[i]][2] <- separators[i] 
      
    }
    
    Sequences_interval[[i]][3] <- Sequences_interval[[i]][2] -
      Sequences_interval[[i]][1] + 1 #length of sequence = end - start + 1
    
  }
  
  for (i in 1:s) {
    if(Sequences_interval[[i]][3]==2){  #### two vertices: one possibility just connect them
      
      adj[Sequences_interval[[i]][1],Sequences_interval[[i]][2]] <- 1
      
    }else if (Sequences_interval[[i]][3]>2) {   #### we need to generate random tree
      
      
      pruefer <- floor(stats::runif(n = (Sequences_interval[[i]][3]-2),  # making the pruefer sequence correspond to each tree of forest
                                    min = Sequences_interval[[i]][1],
                                    max = Sequences_interval[[i]][2]))
      
      vertices <- Sequences_interval[[i]][1]:Sequences_interval[[i]][2] # vertices 
      
      while(length(pruefer)>0){
        p <- pruefer[1]                                                 # First vertice: first element of updated sequence
        v <- min(setdiff(vertices, pruefer))                            # Second vertice: vertice with minimum label in vertices which are not in the updated sequence 
        adj[v, p] <- 1                                                  # Connect first and second vertices in adjacency matrix  
        vertices <- setdiff(vertices, v)                                # Remove first vertice from vertices set
        pruefer <- pruefer[-1]                                          # Remove Second vertice from the sequence
      }
      adj[vertices[1], vertices[2]] <- 1                                # Connect two vertices to each other. 
      
    }
    
    
  }
  
  adj <- adj + t(adj)
  g <- igraph::graph_from_adjacency_matrix(adj, mode='undirected')     # making a graph with adjacency matrix
  igraph::plot.igraph(g)                                               # plotting the graph
  
  forest_structure_list <- list(d , s , adj , Sequences_interval)
  return(forest_structure_list)
}




