forest_HR_simulation <- function(forest_structure_list, n, p, k){
  
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
  
  ########################################################  Does it work for univariate case?????? if yes we don't need those if
  #rmstable_tree(n, "HR", tree = graph_from_adjacency_matrix(matrix(0),
  #                                                          mode = "undirected"), par = matrix(0))
  
  
  ################################################################################
  ################# Empirical estimation for variogram and Chi_matrix ############
  ################################################################################
  
  
  #p <- 0.9 #??? What is this p is at an argument how we can set this p?
  
  emp_chi_mat <- emp_chi(Simulated_data, p) ######### I expect emp_chi_multdim() works but emp_chi() works here why?
  emp_chi_mat  ### why it's so good it doesn't have decimals????!!!!!
  
  ################################################################################
  
  emp_vario_mat <- emp_vario(Simulated_data, k=NULL, p) ######## what is the good k???????????????????????????
  emp_vario_mat                               
  
  
  ################################################################################
  ################## Graph of Empirical estimations in dataframe format ##########
  ################################################################################
  
  emp_chi_graph_df <- data.frame( FirstVertice = numeric(),  # store resultant MSF in the dataframe format each row: each edge's vertices with its weight
                                  SecondVertice = numeric(), 
                                  weight = numeric(), 
                                  stringsAsFactors=FALSE)  
  
  emp_vario_graph_df <- data.frame( FirstVertice = numeric(),  # store resultant MSF in the dataframe format each row: each edge's vertices with its weight
                                    SecondVertice = numeric(), 
                                    weight = numeric(), 
                                    stringsAsFactors=FALSE) 
  
  e <- 1
  
  for (i in (1:(d-1))) {                                           
    for (j in ((i+1):d)) {
      
      emp_chi_graph_df[e,1] <- i
      emp_vario_graph_df[e,1] <- i
      
      emp_chi_graph_df[e,2] <- j
      emp_vario_graph_df[e,2] <- j
      
      emp_chi_graph_df[e,3] <- emp_chi_mat[i,j]
      emp_vario_graph_df[e,3] <- emp_vario_mat[i,j]  
      
      e <- e + 1
    }
  }
  
  
  empirical_est_graphdf_list <- list(emp_chi_graph_df,emp_vario_graph_df)
  return(empirical_est_graphdf_list)
}
