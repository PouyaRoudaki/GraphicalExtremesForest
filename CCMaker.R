  
#' @author Pouya Roudaki
#' @description Making a list of 3 elements vectors corresponding to each 
#' connected components which are trees.List of beginning and ending vertices
#' and also length of each connected components.
#' @param d Number of vertices
#' @param scissors scissors vector
#' @return CC: List of beginning and ending vertices and also length of each
#' connected components.
#' @example CCMaker(d, scissors)

CCMaker <- function(d, scissors){

  null_seq <- rep(NA,3)
  CC <-rep(list(null_seq),length(scissors)+1) 
  
  
  for (i in 1:(length(scissors)+1)) {
    #' Starting point or first vertice of each connected component
    if(i == 1){                                      
      CC[[i]][1] <- 1 
    }else{
      CC[[i]][1] <- scissors[i-1] + 1 
    }
    #' Ending point or last vertice of each connected component
    if(i == (length(scissors)+1)){                                      
      CC[[i]][2] <- d 
    }else{
      CC[[i]][2] <- scissors[i] 
    }
    #' Size of connected component = end - start + 1
    CC[[i]][3] <- CC[[i]][2] - CC[[i]][1] + 1 
  }
  #' return CC list
  return(CC)
}

