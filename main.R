################################################################################
#############################      main{}    ###################################
################################################################################

setwd("C:/Users/pouya/Desktop/UNIGE/GSEM/Fall2021/Master Thesis/Codes")
source("generate_random_forest_function.R")

d <- 10  
seed <- 9
forest_str_lst <- generate_random_forest(d = d,seed = seed)
forest_str_lst
true_adj <- forest_str_lst[[3]]

# m times simulation

source("forest_HR_simulation.R")
source("KruskalAlgorithmForForest_WO_OOP.R")

emp_adj_cumulative <- matrix(0,d,d)
m <- 200
n <- 10000
connected_comp_rec_counter <- 0
recovery_counter <- 0

for (i in 1:m) {
  
  emp_est_list <- forest_HR_simulation(forest_structure_list = forest_str_lst,n = n)
  emp_est_list 
  
  emp_vario_graph_df <- emp_est_list[[2]]
  emp_adj <- KruskalMSF(graph_df = emp_vario_graph_df, max_edge = 5)         # Choosing the tree structure
  
  if (identical(true_adj,emp_adj)) {                                         # to find recovery rate
    recovery_counter <- recovery_counter + 1
  }
  
  if( identical(components(graph.adjacency(true_adj)) , components(graph.adjacency(emp_adj))) ){                                         # to find recovery rate
    connected_comp_rec_counter <- connected_comp_rec_counter + 1
  }
 
  emp_adj_cumulative <- emp_adj_cumulative + emp_adj                        # cumulative adj matrix
           
}

connected_comp_rec_rate <- connected_comp_rec_counter/m
recovery_rate <- recovery_counter/m
source("plot_pouya_graph.R")
plot_pouya_graph(d,emp_adj_cumulative)

################################################################################
###########################      n = 100      ##################################
################################################################################

n <- 100

################################################################################
#####  threshold base recovery rate and wrongly estimated edge proportion ######
################################################################################
emp_adj_cumulative <- matrix(0,d,d)
m <- 200

threshold_vec <- seq(0.5,2.5,by = 0.5)


performance_measure <- data.frame( threshold = numeric(length = length(threshold_vec)),
                                   wrong_est_edg_prop = numeric(length = length(threshold_vec)),
                                   connected_comp_rec_rate = numeric(length = length(threshold_vec)),
                                   recovery_rate = numeric(length = length(threshold_vec)),
                                   stringsAsFactors=FALSE) 

performance_measure$threshold <- threshold_vec

for (j in 1:length(threshold_vec)) {
  
  connected_comp_rec_counter <- 0
  recovery_counter <- 0
  wrong_prop <- 0
  
  for (i in 1:m) {
    
    emp_est_list <- forest_HR_simulation(forest_structure_list = forest_str_lst,n = n)
    emp_est_list 
    
    emp_vario_graph_df <- emp_est_list[[2]]
    emp_adj <- KruskalMSF(graph_df = emp_vario_graph_df, threshold = threshold_vec[j])# Choosing the tree structure
    
    if (identical(true_adj,emp_adj)) {                                         # to find recovery rate
      recovery_counter <- recovery_counter + 1
    }
    
    if( identical(components(graph.adjacency(true_adj)) , components(graph.adjacency(emp_adj))) ){    # to find connected component recovery rate                                  
      connected_comp_rec_counter <- connected_comp_rec_counter + 1
    }
    
    wrong_prop <- wrong_prop + (1 - sum(true_adj*emp_adj)/sum(emp_adj))       #  proportion of wrongly estimated edges
    
    emp_adj_cumulative <- emp_adj_cumulative + emp_adj                        # cumulative adj matrix
    
  }
  
  performance_measure$connected_comp_rec_rate[j] <- connected_comp_rec_counter/m
  performance_measure$recovery_rate[j] <- recovery_counter/m
  performance_measure$wrong_est_edg_prop[j] <- wrong_prop/m
  
  print(threshold_vec[j])
}

library(ggplot2)

performance_df <- reshape2::melt(performance_measure, id.var = "threshold")

perf_plot <- ggplot(performance_df, aes(x = threshold, y = value, colour = variable)) + 
  geom_point() + 
  geom_line() +
  ggtitle(paste("Performance measure  n =",n))+
  ylab("Prop")+xlab("Threshold")
perf_plot


################################################################################
######################     Smaller interval     ################################
################################################################################

emp_adj_cumulative <- matrix(0,d,d)
m <- 200

threshold_vec <- seq(0.5,1.5,by = 0.1)


performance_measure <- data.frame( threshold = numeric(length = length(threshold_vec)),
                                   wrong_est_edg_prop = numeric(length = length(threshold_vec)),
                                   connected_comp_rec_rate = numeric(length = length(threshold_vec)),
                                   recovery_rate = numeric(length = length(threshold_vec)),
                                   stringsAsFactors=FALSE) 

performance_measure$threshold <- threshold_vec

for (j in 1:length(threshold_vec)) {
  
  connected_comp_rec_counter <- 0
  recovery_counter <- 0
  wrong_prop <- 0
  
  for (i in 1:m) {
    
    emp_est_list <- forest_HR_simulation(forest_structure_list = forest_str_lst,n = n)
    emp_est_list 
    
    emp_vario_graph_df <- emp_est_list[[2]]
    emp_adj <- KruskalMSF(graph_df = emp_vario_graph_df, threshold = threshold_vec[j])# Choosing the tree structure
    
    if (identical(true_adj,emp_adj)) {                                         # to find recovery rate
      recovery_counter <- recovery_counter + 1
    }
    
    if( identical(components(graph.adjacency(true_adj)) , components(graph.adjacency(emp_adj))) ){      # to find connected component recovery rate
      connected_comp_rec_counter <- connected_comp_rec_counter + 1
    }
    
    wrong_prop <- wrong_prop + (1 - sum(true_adj*emp_adj)/sum(emp_adj))       #  proportion of wrongly estimated edges
    
    emp_adj_cumulative <- emp_adj_cumulative + emp_adj                        # cumulative adj matrix
    
  }
  
  performance_measure$connected_comp_rec_rate[j] <- connected_comp_rec_counter/m
  performance_measure$recovery_rate[j] <- recovery_counter/m
  performance_measure$wrong_est_edg_prop[j] <- wrong_prop/m
  
  print(threshold_vec[j])
}


library(ggplot2)

performance_df <- reshape2::melt(performance_measure, id.var = "threshold")


perf_plot <- ggplot(performance_df, aes(x = threshold, y = value, colour = variable)) + 
  geom_point() + 
  geom_line() +
  ggtitle(paste("Performance measure  n =",n))+
  ylab("Prop")+xlab("Threshold")
perf_plot

################################################################################
###########################      n = 1000      #################################
################################################################################

n <- 1000

################################################################################
#####  threshold base recovery rate and wrongly estimated edge proportion ######
################################################################################
emp_adj_cumulative <- matrix(0,d,d)
m <- 200

threshold_vec <- seq(0.5,2.5,by = 0.5)


performance_measure <- data.frame( threshold = numeric(length = length(threshold_vec)),
                                   wrong_est_edg_prop = numeric(length = length(threshold_vec)),
                                   connected_comp_rec_rate = numeric(length = length(threshold_vec)),
                                   recovery_rate = numeric(length = length(threshold_vec)),
                                   stringsAsFactors=FALSE) 

performance_measure$threshold <- threshold_vec

for (j in 1:length(threshold_vec)) {
  
  connected_comp_rec_counter <- 0
  recovery_counter <- 0
  wrong_prop <- 0
  
  for (i in 1:m) {
    
    emp_est_list <- forest_HR_simulation(forest_structure_list = forest_str_lst,n = n)
    emp_est_list 
    
    emp_vario_graph_df <- emp_est_list[[2]]
    emp_adj <- KruskalMSF(graph_df = emp_vario_graph_df, threshold = threshold_vec[j])# Choosing the tree structure
    
    if (identical(true_adj,emp_adj)) {                                         # to find recovery rate
      recovery_counter <- recovery_counter + 1
    }
    
    if( identical(components(graph.adjacency(true_adj)) , components(graph.adjacency(emp_adj))) ){    # to find connected component recovery rate                                  
      connected_comp_rec_counter <- connected_comp_rec_counter + 1
    }
    
    wrong_prop <- wrong_prop + (1 - sum(true_adj*emp_adj)/sum(emp_adj))       #  proportion of wrongly estimated edges
    
    emp_adj_cumulative <- emp_adj_cumulative + emp_adj                        # cumulative adj matrix
    
  }
  
  performance_measure$connected_comp_rec_rate[j] <- connected_comp_rec_counter/m
  performance_measure$recovery_rate[j] <- recovery_counter/m
  performance_measure$wrong_est_edg_prop[j] <- wrong_prop/m
  
  print(threshold_vec[j])
}

library(ggplot2)

performance_df <- reshape2::melt(performance_measure, id.var = "threshold")

perf_plot <- ggplot(performance_df, aes(x = threshold, y = value, colour = variable)) + 
  geom_point() + 
  geom_line() +
  ggtitle(paste("Performance measure  n =",n))+
  ylab("Prop")+xlab("Threshold")
perf_plot


################################################################################
######################     Smaller interval     ################################
################################################################################

emp_adj_cumulative <- matrix(0,d,d)
m <- 200

threshold_vec <- seq(1,2,by = 0.1)


performance_measure <- data.frame( threshold = numeric(length = length(threshold_vec)),
                                   wrong_est_edg_prop = numeric(length = length(threshold_vec)),
                                   connected_comp_rec_rate = numeric(length = length(threshold_vec)),
                                   recovery_rate = numeric(length = length(threshold_vec)),
                                   stringsAsFactors=FALSE) 

performance_measure$threshold <- threshold_vec

for (j in 1:length(threshold_vec)) {
  
  connected_comp_rec_counter <- 0
  recovery_counter <- 0
  wrong_prop <- 0
  
  for (i in 1:m) {
    
    emp_est_list <- forest_HR_simulation(forest_structure_list = forest_str_lst,n = n)
    emp_est_list 
    
    emp_vario_graph_df <- emp_est_list[[2]]
    emp_adj <- KruskalMSF(graph_df = emp_vario_graph_df, threshold = threshold_vec[j])# Choosing the tree structure
    
    if (identical(true_adj,emp_adj)) {                                         # to find recovery rate
      recovery_counter <- recovery_counter + 1
    }
    
    if( identical(components(graph.adjacency(true_adj)) , components(graph.adjacency(emp_adj))) ){      # to find connected component recovery rate
      connected_comp_rec_counter <- connected_comp_rec_counter + 1
    }
    
    wrong_prop <- wrong_prop + (1 - sum(true_adj*emp_adj)/sum(emp_adj))       #  proportion of wrongly estimated edges
    
    emp_adj_cumulative <- emp_adj_cumulative + emp_adj                        # cumulative adj matrix
    
  }
  
  performance_measure$connected_comp_rec_rate[j] <- connected_comp_rec_counter/m
  performance_measure$recovery_rate[j] <- recovery_counter/m
  performance_measure$wrong_est_edg_prop[j] <- wrong_prop/m
  
  print(threshold_vec[j])
}


library(ggplot2)

performance_df <- reshape2::melt(performance_measure, id.var = "threshold")

t2.rect1 <- data.frame (xmin=1.6, xmax=1.8, ymin=-Inf, ymax=Inf)

perf_plot <- ggplot(performance_df, aes(x = threshold, y = value, colour = variable)) + 
  geom_point() + 
  geom_line() +
  ggtitle(paste("Performance measure  n =",n))+
  ylab("Prop")+xlab("Threshold")+
  geom_rect(data=t2.rect1, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill="purple", alpha=0.1, inherit.aes = FALSE)
perf_plot



################################################################################
########################      n = 10000     ####################################
################################################################################

n <- 10000

################################################################################
#####  threshold base recovery rate and wrongly estimated edge proportion ######
################################################################################
emp_adj_cumulative <- matrix(0,d,d)
m <- 200

threshold_vec <- seq(0.5,2.5,by = 0.5)


performance_measure <- data.frame( threshold = numeric(length = length(threshold_vec)),
                                   wrong_est_edg_prop = numeric(length = length(threshold_vec)),
                                   connected_comp_rec_rate = numeric(length = length(threshold_vec)),
                                   recovery_rate = numeric(length = length(threshold_vec)),
                                   stringsAsFactors=FALSE) 

performance_measure$threshold <- threshold_vec

for (j in 1:length(threshold_vec)) {
  
  connected_comp_rec_counter <- 0
  recovery_counter <- 0
  wrong_prop <- 0
  
  for (i in 1:m) {
    
    emp_est_list <- forest_HR_simulation(forest_structure_list = forest_str_lst,n = n)
    emp_est_list 
    
    emp_vario_graph_df <- emp_est_list[[2]]
    emp_adj <- KruskalMSF(graph_df = emp_vario_graph_df, threshold = threshold_vec[j])# Choosing the tree structure
    
    if (identical(true_adj,emp_adj)) {                                         # to find recovery rate
      recovery_counter <- recovery_counter + 1
    }
    
    if( identical(components(graph.adjacency(true_adj)) , components(graph.adjacency(emp_adj))) ){    # to find connected component recovery rate                                  
      connected_comp_rec_counter <- connected_comp_rec_counter + 1
    }
    
    wrong_prop <- wrong_prop + (1 - sum(true_adj*emp_adj)/sum(emp_adj))       #  proportion of wrongly estimated edges
    
    emp_adj_cumulative <- emp_adj_cumulative + emp_adj                        # cumulative adj matrix
    
  }
  
  performance_measure$connected_comp_rec_rate[j] <- connected_comp_rec_counter/m
  performance_measure$recovery_rate[j] <- recovery_counter/m
  performance_measure$wrong_est_edg_prop[j] <- wrong_prop/m
  
  print(threshold_vec[j])
}

library(ggplot2)

performance_df <- reshape2::melt(performance_measure, id.var = "threshold")

perf_plot <- ggplot(performance_df, aes(x = threshold, y = value, colour = variable)) + 
  geom_point() + 
  geom_line() +
  ggtitle(paste("Performance measure  n =",n))+
  ylab("Prop")+xlab("Threshold")
perf_plot


################################################################################
######################     Smaller interval     ################################
################################################################################

emp_adj_cumulative <- matrix(0,d,d)
m <- 200

threshold_vec <- seq(1,2,by = 0.1)


performance_measure <- data.frame( threshold = numeric(length = length(threshold_vec)),
                                   wrong_est_edg_prop = numeric(length = length(threshold_vec)),
                                   connected_comp_rec_rate = numeric(length = length(threshold_vec)),
                                   recovery_rate = numeric(length = length(threshold_vec)),
                                   stringsAsFactors=FALSE) 

performance_measure$threshold <- threshold_vec

for (j in 1:length(threshold_vec)) {
  
  connected_comp_rec_counter <- 0
  recovery_counter <- 0
  wrong_prop <- 0
  
  for (i in 1:m) {
    
    emp_est_list <- forest_HR_simulation(forest_structure_list = forest_str_lst,n = n)
    emp_est_list 
    
    emp_vario_graph_df <- emp_est_list[[2]]
    emp_adj <- KruskalMSF(graph_df = emp_vario_graph_df, threshold = threshold_vec[j])# Choosing the tree structure
    
    if (identical(true_adj,emp_adj)) {                                         # to find recovery rate
      recovery_counter <- recovery_counter + 1
    }
    
    if( identical(components(graph.adjacency(true_adj)) , components(graph.adjacency(emp_adj))) ){      # to find connected component recovery rate
      connected_comp_rec_counter <- connected_comp_rec_counter + 1
    }
    
    wrong_prop <- wrong_prop + (1 - sum(true_adj*emp_adj)/sum(emp_adj))       #  proportion of wrongly estimated edges
    
    emp_adj_cumulative <- emp_adj_cumulative + emp_adj                        # cumulative adj matrix
    
  }
  
  performance_measure$connected_comp_rec_rate[j] <- connected_comp_rec_counter/m
  performance_measure$recovery_rate[j] <- recovery_counter/m
  performance_measure$wrong_est_edg_prop[j] <- wrong_prop/m
  
  print(threshold_vec[j])
}


library(ggplot2)

performance_df <- reshape2::melt(performance_measure, id.var = "threshold")

t2.rect1 <- data.frame (xmin=1.7, xmax=1.9, ymin=-Inf, ymax=Inf)

perf_plot <- ggplot(performance_df, aes(x = threshold, y = value, colour = variable)) + 
  geom_point() + 
  geom_line() +
  ggtitle(paste("Performance measure  n =",n))+
  ylab("Prop")+xlab("Threshold")+
  geom_rect(data=t2.rect1, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill="purple", alpha=0.1, inherit.aes = FALSE)
perf_plot


################################################################################
############################       31th August #################################
################################################################################
n <- 100000
p <- 0.9
emp_adj_cumulative <- matrix(0,d,d)
m <- 200

threshold_vec <- seq(1,2,by = 0.1)


performance_measure <- data.frame( threshold = numeric(length = length(threshold_vec)),
                                   wrong_est_edg_prop = numeric(length = length(threshold_vec)),
                                   connected_comp_rec_rate = numeric(length = length(threshold_vec)),
                                   recovery_rate = numeric(length = length(threshold_vec)),
                                   stringsAsFactors=FALSE) 

performance_measure$threshold <- threshold_vec

for (j in 1:length(threshold_vec)) {
  
  connected_comp_rec_counter <- 0
  recovery_counter <- 0
  wrong_prop <- 0
  
  for (i in 1:m) {
    
    emp_est_list <- forest_HR_simulation(forest_structure_list = forest_str_lst,n = n,p = p)
    emp_est_list 
    
    emp_vario_graph_df <- emp_est_list[[2]]
    emp_adj <- KruskalMSF(graph_df = emp_vario_graph_df, threshold = threshold_vec[j])# Choosing the tree structure
    
    if (identical(true_adj,emp_adj)) {                                         # to find recovery rate
      recovery_counter <- recovery_counter + 1
    }
    
    if( identical(components(graph.adjacency(true_adj)) , components(graph.adjacency(emp_adj))) ){      # to find connected component recovery rate
      connected_comp_rec_counter <- connected_comp_rec_counter + 1
    }
    
    wrong_prop <- wrong_prop + (1 - sum(true_adj*emp_adj)/sum(emp_adj))       #  proportion of wrongly estimated edges
    
    emp_adj_cumulative <- emp_adj_cumulative + emp_adj                        # cumulative adj matrix
    
  }
  
  performance_measure$connected_comp_rec_rate[j] <- connected_comp_rec_counter/m
  performance_measure$recovery_rate[j] <- recovery_counter/m
  performance_measure$wrong_est_edg_prop[j] <- wrong_prop/m
  
  print(threshold_vec[j])
}


library(ggplot2)

performance_df <- reshape2::melt(performance_measure, id.var = "threshold")

t2.rect1 <- data.frame (xmin=1.88, xmax=1.92, ymin=-Inf, ymax=Inf)

perf_plot <- ggplot(performance_df, aes(x = threshold, y = value, colour = variable)) + 
  geom_point() + 
  geom_line() +
  ggtitle(paste("Performance measure  n =",n,"and p =",p))+
  ylab("Prop")+xlab("Threshold")+
  geom_rect(data=t2.rect1, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill="purple", alpha=0.1, inherit.aes = FALSE)
perf_plot
