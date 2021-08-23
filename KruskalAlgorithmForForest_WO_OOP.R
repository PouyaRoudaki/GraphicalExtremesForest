
################################################################################
########### Kruskal algorithm for forest non object oriented approach ##########
################################################################################


################################################################################
########################### Find Parent function ###############################
################################################################################

findParent <- function(parent,i){
  
  if (parent[i] == i) {
    return(i) 
  }
  
  return( findParent(parent,parent[i]) )
  
}

################################################################################
#TEST

#parent1 <- c(1,1,3,4)
#findParent(parent1,3)
#parent1[1]

################################################################################
########################### Union Parent function ###############################
################################################################################

unionParent <- function(parent,rank,x,y)
{
  #### find the parents of two nodes corresponding to one edge 
  xroot <- findParent(parent,x)
  yroot <- findParent(parent,y)
  
  #### attach smaller rank tree under root of high rank tree (Union by rank)
  if( rank[xroot] < rank[yroot]){
    
    parent[xroot] <- yroot
    
  }else if( rank[xroot] > rank[yroot] ){
    
    parent[yroot] <- xroot
    
  }else {
    
    parent[yroot] <- xroot
    rank[xroot] <- rank[xroot] + 1
    
  }
  
  return(list(parent,rank))
}

################################################################################
#TEST
#parent <- c(1,2,3,4)
#rank <- c(0,0,0,0)
#templist <- unionParent(parent,rank,3,4)

#parent <- templist[[1]]
#parent
#rank <- templist[[2]]
#rank
#unionParent(parent,rank,1,4)




################################################################################
#####################  KRUSKAL ALGORITHM FOR MSF  ##############################
################################################################################

## use d instead of object@V

KruskalMSF <- function( graph_df , max_edge = d , threshold = Inf)
{
  
  result <- data.frame( FirstVertice = numeric(),  # store resultant MSF
                        SecondVertice = numeric(), 
                        weight = numeric(), 
                        stringsAsFactors=FALSE) 
  emp_adj <- matrix(0,nrow = d,ncol = d )
  
  i <- 1           # index variable used for stored edges
  e <- 0           # index variable used for resultant list
  
  ####################################################################
  # Step1: Sort all the edges in non-decreasing order of their weights
  
  graph_df <- graph_df[order(graph_df[,3]),]         # Sorting base on order
  
  
  ####################################################################
  # Middle Step: Create V subsets with single elements ( parent of each node is itself and its rank is 0 )
  
  parent <- vector()
  rank <- vector()
  
  for (node in 1:d) {
    
    parent <- append( parent , node)
    rank <- append( rank , 0)
    
  }
  
  ####################################################################
  # Step 2 : Adding the edges in non-decreasing order of weights in a greedy way and ignoring the edges which 
  #          make cycle
  
  weightctrl <- 0
  
  while ( (e < (d - 1)) & (e < max_edge) & (weightctrl < threshold)) { # number of edges to be taken for a tree is equal to V-1 for making forest with
    # k edges or with weight <  threshold we can change the condition for while
    
    # Pick the edge with smallest weight and increment the index for next iteration
    
    u <- graph_df[i,1]   # first node of i_th edge
    v <- graph_df[i,2]   # second node of i_th edge
    w <- graph_df[i,3]   # weight of i_th edge
    
    i <- i + 1
    
    if(i <= dim(graph_df)[1]){ # Is if necessary????????????????????????? just write it without if
      
      weightctrl <- graph_df[i,3]
      
    }
    
    x <- findParent(parent,u) # find parent of first node of i_th edge
    y <- findParent(parent,v) # find parent of second node of i_th edge
    
    # if including this edge doesn't cause a cycle then include it in result and increment the index of result
    # for next edge.
    
    if( x != y ){
      
      e <- e + 1  # We are adding the edge so we must increment the edge counter by one.
      result[e,1] <- u # don't use object@graph[[i]] instead of list(c(u,v,w)) since 
      result[e,2] <- v
      result[e,3] <- w
      # we increased i to i+1 in past.
      
      templist <- list()
      templist <- unionParent(parent,rank,x,y)
      parent <- templist[[1]]
      rank <- templist[[2]]
      
    }#ELSE: discard the edge
    
  }
  
  minimumCost <- 0
  
  
  for (j in 1:dim(result)[1]) {
    
    minimumCost <- minimumCost + result[j,3]
    
  }
  
  print(paste("Minimum spanning tree cost: ",minimumCost))
  
  for (k in 1:dim(result)[1]) { # making the empirical adjacency matrix
    
    i <- result[k,1]
    j <- result[k,2]
    
    emp_adj[i,j] <- 1   # make the adjacency matrix symmetric 
    
  }
  
  emp_adj <- emp_adj + t(emp_adj)  
  return(emp_adj)
}


   

################################################################################
################################################################################
#########################    TEST THE Function    ################################
################################################################################
################################################################################


#emp_adj <- KruskalMSF(graph_df = emp_vario_graph_df)


#g <- igraph::graph_from_adjacency_matrix(emp_adj, mode='undirected')     # making a graph with adjacency matrix
#igraph::plot.igraph(g)                                               # plotting the graph


#emp_adj <- KruskalMSF(graph_df = emp_vario_graph_df,k = 7)

#g <- igraph::graph_from_adjacency_matrix(emp_adj, mode='undirected')     # making a graph with adjacency matrix
#igraph::plot.igraph(g)   






