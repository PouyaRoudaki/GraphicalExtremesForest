data_simulation_HR <- function(forest_structure_list, n){
  forest_structure_list <- forest_str_lst
  
  
  ####### required packages:
  
  require(igraph)
  require(graphicalExtremes) 
  
  #######  Data Simulation for the forest with Huesler-Riess distribution ########
  
  # we have "s" we have "Sequences_interval" and we also have "adj" matrix
  # it seems we need a list of adjacency matrices corresponds to each tree
  d <- forest_structure_list[[1]]
  s <- forest_structure_list[[2]]
  adj <- forest_structure_list[[3]]
  Sequences_interval <- forest_structure_list[[4]]
  
  ################################################################################
  ###########  list of adjacency matrices corresponds to each tree  ##############
  ################################################################################
  
  
  forest_structure_list
  
  adj_mat_list <- list()
  
  for (i in 1:s) {   # list of adjacency matrices corresponds to each tree
    
    adj_mat_list[[i]] <- adj[Sequences_interval[[i]][1]:Sequences_interval[[i]][2],
                             Sequences_interval[[i]][1]:Sequences_interval[[i]][2]]
    
    #print(adj_mat_list[[i]])
    
  }
  
  ################################################################################
  #########################    Making the trees    ###############################
  ################################################################################
  
  trees_list <- list()
  
  for (i in 1:s) {   # list of trees
    
    trees_list[[i]] <- graph_from_adjacency_matrix(adj_mat_list[[i]],
                                                   mode = "undirected")
  }
  
  ################################################################################
  ########################  Gamma assumption for cliques #########################
  ################################################################################
  
  #is it isolated node just write "isolated" (univariate max stable) else generate random vector 
  # for Gamma elements corresponds to each clique...
  
  Gamma_list <- list()
  ExtremalChi_list <- list()
  for (i in 1:s) {   # list of Gamma vectors corresponds to cliques(edges) of each tree 
    
    if (sum(adj_mat_list[[i]]) == 0) { # isolated trees don't have 
      
      Gamma_list[[i]] <- matrix(0)
      ExtremalChi_list[[i]] <- matrix(1) ### is it true?
      
    }else{
      
      cliques_number <- sum(adj_mat_list[[i]])/2
      
      ###
      #runif will not generate either of the extreme values unless max = min or max-min is small compared to min,
      #and in particular not for the default arguments.
      ### 
      
      ### Generate random vector of Gamma values on cliques
      
      #Gamma_vec_on_cliques <- c(1,1,1,1,1)
      
      Gamma_vec_on_cliques <- runif(cliques_number,min = 0,max = 10) # Max can't be infinity ? maybe it's better to define Chi and 
      # convert it not complete gamma and complete it or is there and 
      # Chi_complete?!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      # maybe we can say max= threshold (removing the edges with larger weights)
      
      ### Note that everything(if we say max = threshold) is base on Gamma now. but if you want to use Chi you should change the stopping criteria for 
      ### stopping adding the edges because it's in reverse direction since larger Chi means dependence and smaller one means
      ### independence (removing the edge)
      
      ### use Gamma completion method to make a complete gamma
      
      Gamma_list[[i]] <- complete_Gamma(Gamma =  Gamma_vec_on_cliques, graph = trees_list[[i]])
      
      ### Make Extreaml Chi corresponds to this Gamma
      
      ExtremalChi_list[[i]] <- Gamma2chi(Gamma_list[[i]])
      
    }
    
  }
  
  ################################################################################
  #################### Simulation of data for trees ##############################
  ################################################################################
  
  
  ### number of observations
  #n <- 100
  
  iso_gamma_mat <- matrix(0)
  trees_data_list <- list()
  
  for (i in 1:s) {
    #####   simulation of multivariate random sample corresponds to each tree and its Gamma
    trees_data_list[[i]] <- rmstable_tree(n, "HR", tree = trees_list[[i]], par = Gamma_list[[i]])
  }
  
  
  ################################################################################
  ############################         DATA      #################################
  ################################################################################
  
  # merge data of trees together
  Simulated_data <- do.call(cbind,trees_data_list)
  return(Simulated_data)
}
