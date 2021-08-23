################################################################################
#############################      main{}    ###################################
################################################################################


source("generate_random_forest_function.R")

d <- 10  
forest_str_lst <- generate_random_forest(d)
forest_str_lst
true_adj <- forest_str_lst[[3]]

# m times simualtion

source("forest_HR_simulation.R")
source("KruskalAlgorithmForForest_WO_OOP.R")

emp_adj_cumulative <- matrix(0,d,d)
m <- 200
recovery_counter <- 0

for (i in 1:m) {
  
  emp_est_list <- forest_HR_simulation(forest_structure_list = forest_str_lst,n = 100 , p = 0.9, k = NULL)
  emp_est_list 
  
  emp_vario_graph_df <- emp_est_list[[2]]
  emp_adj <- KruskalMSF(graph_df = emp_vario_graph_df, max_edge = 8)         # Choosing the tree structure
  
  if (identical(true_adj,emp_adj)) {                                         # to find recovery rate
    recovery_counter <- recovery_counter + 1
  }
 
  emp_adj_cumulative <- emp_adj_cumulative + emp_adj                        # cumulative adj matrix
           
}

recovery_rate <- recovery_counter/m
source("plot_pouya_graph.R")
plot_pouya_graph(d,emp_adj_cumulative)

################################################################################
#####  threshold base recovery rate and wrongly estimated edge proportion ######
################################################################################
emp_adj_cumulative <- matrix(0,d,d)
m <- 200

threshold_vec <- seq(0.5,10,by = 0.5)


performance_measure <- data.frame( threshold = numeric(length = length(threshold_vec)),
                                   wrong_est_edg_prop = numeric(length = length(threshold_vec)),
                                   recovery_rate = numeric(length = length(threshold_vec)),
                                   stringsAsFactors=FALSE) 

performance_measure$threshold <- threshold_vec

for (j in 1:length(threshold_vec)) {
  
  recovery_counter <- 0
  wrong_prop <- 0
  
  for (i in 1:m) {
    
    emp_est_list <- forest_HR_simulation(forest_structure_list = forest_str_lst,n = 100 , p = 0.1, k = NULL)
    emp_est_list 
    
    emp_vario_graph_df <- emp_est_list[[2]]
    emp_adj <- KruskalMSF(graph_df = emp_vario_graph_df, threshold = threshold_vec[j])# Choosing the tree structure
    
    if (identical(true_adj,emp_adj)) {                                         # to find recovery rate
      recovery_counter <- recovery_counter + 1
    }
    
    
    wrong_prop <- wrong_prop + (1 - sum(true_adj*emp_adj)/sum(emp_adj))       #  proportion of wrongly estimated edges
    
    emp_adj_cumulative <- emp_adj_cumulative + emp_adj                        # cumulative adj matrix
    
  }
  
  
  performance_measure$recovery_rate[j] <- recovery_counter/m
  performance_measure$wrong_est_edg_prop[j] <- wrong_prop/m
  
  print(threshold_vec[j])
}

################################################################################
######################     Smaller interval     ################################
################################################################################

emp_adj_cumulative <- matrix(0,d,d)
m <- 200

threshold_vec <- seq(0.5,2.5,by = 0.1)


performance_measure <- data.frame( threshold = numeric(length = length(threshold_vec)),
                                   wrong_est_edg_prop = numeric(length = length(threshold_vec)),
                                   recovery_rate = numeric(length = length(threshold_vec)),
                                   stringsAsFactors=FALSE) 

performance_measure$threshold <- threshold_vec

for (j in 1:length(threshold_vec)) {
  
  recovery_counter <- 0
  wrong_prop <- 0
  
  for (i in 1:m) {
    
    emp_est_list <- forest_HR_simulation(forest_structure_list = forest_str_lst,n = 100 , p = 0.9, k = NULL)
    emp_est_list 
    
    emp_vario_graph_df <- emp_est_list[[2]]
    emp_adj <- KruskalMSF(graph_df = emp_vario_graph_df, threshold = threshold_vec[j])# Choosing the tree structure
    
    if (identical(true_adj,emp_adj)) {                                         # to find recovery rate
      recovery_counter <- recovery_counter + 1
    }
    
    
    wrong_prop <- wrong_prop + (1 - sum(true_adj*emp_adj)/sum(emp_adj))       #  proportion of wrongly estimated edges
    
    emp_adj_cumulative <- emp_adj_cumulative + emp_adj                        # cumulative adj matrix
    
  }
  
  
  performance_measure$recovery_rate[j] <- recovery_counter/m
  performance_measure$wrong_est_edg_prop[j] <- wrong_prop/m
  
  print(threshold_vec[j])
}


library(ggplot2)

mdf <- reshape2::melt(performance_measure, id.var = "threshold")

p <- ggplot(mdf, aes(x = threshold, y = value, colour = variable)) + 
  geom_point() + 
  geom_line() +
  ggtitle("Performance measure  p=0.9")+
  ylab("Prop")+xlab("Threshold")
p



  
