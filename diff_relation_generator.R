diff_relation_generator <- function(forest_structure_list){
  
  different_type_of_nodes_df <- data.frame(matrix(data = NA,nrow = 2,ncol = 3))
  colnames(different_type_of_nodes_df) <- c("neighbor","connected","unconnected")
  rownames(different_type_of_nodes_df) <- c("node1","node2")
  
  
  rand <- sample(1:forest_structure_list[[2]],1)
  
  begin <- forest_structure_list[[4]][[rand]][1]
  end <-forest_structure_list[[4]][[rand]][2]
  
  adj_selected_cc <- forest_structure_list[[3]][begin:end,begin:end]
  
  
  first_zero <- T
  first_one <- T
  seq_cc <- begin:end
  
  for (i in 1:(forest_structure_list[[4]][[rand]][3]-1)) {
    for (j in (i+1):forest_structure_list[[4]][[rand]][3]) {
      
      if(( adj_selected_cc[i,j] == 1 )&( first_one == T)){
        
        different_type_of_nodes_df$neighbor[1] <- seq_cc[i]
        different_type_of_nodes_df$neighbor[2] <- seq_cc[j]
        
        first_one <- F 
      }
      
      if(( adj_selected_cc[i,j] == 0 )&( first_zero == T)){
        
        different_type_of_nodes_df$connected[1] <- seq_cc[i]
        different_type_of_nodes_df$connected[2] <- seq_cc[j]
        
        different_type_of_nodes_df$unconnected[1] <- seq_cc[i]
        
        first_zero <- F 
      }
      
      
    }
  }
  
  cc_vec <- 1:forest_structure_list[[2]]
  new_cc_vec <- cc_vec[! cc_vec %in% rand]
  
  new_rand <- sample(new_cc_vec,1)
  
  different_type_of_nodes_df$unconnected[2] <-  forest_structure_list[[4]][[new_rand]][1]
  
  return(different_type_of_nodes_df)
}
