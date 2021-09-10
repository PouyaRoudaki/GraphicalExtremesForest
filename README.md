# GraphicalExtremesForest
Forest structure estimation for multivariate extremes 

##############################################################
functions:
##############################################################

generate_random_forest_function(d, type="ncc", ncc = NULL,max_scc = d, min_scc = 1,seed=NULL)

In this function, user specifies important features for making the random forest. 

The algorithm in this function has two step: 
1. Randomly separation of different nodes  corresponding to each tree with "scissors_maker_function"
2. Using pruefer sequence trick to make random trees 

There are two methods to specify important features: 
1. Based on the number of connected components of forest
2. Based on the minimum and maximum size of connected components in the expected forest.

Arguments:
d: number of vertices in the forest (corresponding to the number of variables)
type: ncc = number of connected components 
           scc =  size of connected components 
ncc: number of connected components
min_scc: minimum size of connected components 
max_scc: maximum size of connected components
seed: seed for randomness.

Output: "forest_structure_list" 
forest_structure_list[[1]]: d
forest_structure_list[[2]]: ncc
forest_structure_list[[3]]: adjacancy matrix
forest_structure_list[[4]]: list of sequence corresponding to each tree

##############################################################

scissors_maker_function(d, type="ncc", ncc = NULL, max_scc=d, min_scc=1, seed = NULL)

Randomly separation of different nodes  corresponding to each tree.

Arguments:
d: number of vertices in the forest (corresponding to the number of variables)
type: ncc = number of connected components 
           scc =  size of connected components 
ncc: number of connected components
min_scc: minimum size of connected components 
max_scc: maximum size of connected components
seed: seed for randomness.

Output: array of "scissors"

#############################################################

forest_HR_simulation(forest_structure_list, n, p=0, k=NULL)

Simulation of data for any forest structure based on Huesler-Riess distribution. Finding the empirical extremal Gamma and empirical extremal Chi for simulated data.


Arguments:
forest_structure_list: The list of a forest structure
n : number of simulated data
p : threshold for extreme values
k : coordinate. Null means average of all coordinates

Output: list of empirical weights on each edge of graph at "empirical_est_graphdf_list"
empirical_est_graphdf_list[[1]] :extremal_Chi *** REMEMBER: you have to use -log(extremal_Chi) for weights of graph ***
empirical_est_graphdf_list[[2]] : extremal_Gamma

#############################################################

data_simulation_HR(forest_structure_list, n)

Simulation of data for any forest structure based on Huesler-Riess distribution. 

Arguments:
forest_structure_list: The list of a forest structure
n : number of simulated data

Output: "Simulated_data"  matrix of simulated data corresponding to the forest structure.

#############################################################

KruskalAlgorithmForForest_WO_OOP( graph_df , max_edge = d , threshold = Inf)

Kruskal algorithm on weights of graph to make forest with two criteria "max_edge" and "threshold" of weight .
Continue usual Kruskal before attaining the limit for "max_edge" and "threshold" of weight.

Arguments: 
graph_df: empirical weights on each edge of graph
max_edge: maximum number of edges
threshold: maximum threshold for weights on edges

Output: "emp_adj" empirically estimated of adjacany matrix.

############################################################## 

