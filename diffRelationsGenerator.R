  
#' @author Pouya Roudaki
#' @description   This function helps us to find random pair of variables(nodes)
#' which have 3 types of relations in terms of independence:
#' 1. Dependent(adjacent or neighbor)
#' 2. Conditionally independent(connected but not neighbors)
#' 3. Independent(unconnected nodes) 
#' Note that this function only works for a forest with minimum size of connected
#' component equal to 3 (min_scc = 3)  
#' @param  ncc: forestStructure: the structure of forest which is a list 
#' corresponding to each forest 
#' @return diffRelationPairsDataframe: a dataframe which gives an example from 
#' each type of 3 relationships
#' @example diffRelationsGenerator(forestStructure)

diffRelationsGenerator <- function(forestStructure){

  diffRelationPairsDataframe <- data.frame(matrix(data = NA,nrow = 2,ncol = 3))
  colnames(diffRelationPairsDataframe) <- c("neighbor","connected","unconnected")
  rownames(diffRelationPairsDataframe) <- c("node1","node2")
  
  #' Select one of the connected components
  rand <- sample(1:forestStructure[[2]],1)
  
  #' Find the smallest and largest node index on the selected component
  begin <- forestStructure[[4]][[rand]][1]
  end <-forestStructure[[4]][[rand]][2]
  
  #' Adjacency matrix corresponding to selected component
  adjSelectedCC <- forestStructure[[3]][begin:end,begin:end]
  
  
  firstZero <- T
  firstOne <- T
  seqCC <- begin:end
  
  for (i in 1:(forestStructure[[4]][[rand]][3]-1)) {
    for (j in (i+1):forestStructure[[4]][[rand]][3]) {
      
      if(( adjSelectedCC[i,j] == 1 )&( firstOne == T)){
        
        diffRelationPairsDataframe$neighbor[1] <- seqCC[i]
        diffRelationPairsDataframe$neighbor[2] <- seqCC[j]
        
        firstOne <- F 
      }
      
      if(( adjSelectedCC[i,j] == 0 )&( firstZero == T)){
        
        diffRelationPairsDataframe$connected[1] <- seqCC[i]
        diffRelationPairsDataframe$connected[2] <- seqCC[j]
        
        diffRelationPairsDataframe$unconnected[1] <- seqCC[i]
        
        firstZero <- F 
      }
      
      
    }
  }
  
  CCVec <- 1:forestStructure[[2]]
  
  #' Switch to different connected component to select the second node for 
  #' unconnected or independence structure
  #' All components except the previous one
  newCCVec <- CCVec[! CCVec %in% rand]
  
  #' Random sampling from those components
  newRand <- sample(newCCVec,1)
  
  #' Choose the first node of that component as a second node for unconnected or
  #' independence structure.
  diffRelationPairsDataframe$unconnected[2] <-  forestStructure[[4]][[newRand]][1]
  
  #' Return a dataFrame which have pairs corresponding to each 
  return(diffRelationPairsDataframe)
}
