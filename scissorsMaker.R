 

#' @author Pouya Roudaki
#' @description   In this function we just select our scissors to make disjoint
#' connected components. 
#' @param  ncc: number of connected components
#' @param  scc: size of connected components
#' @return scissors: a vector of vertices which disconnnect different connected
#' component I called them scissors (for cutting the sequence to small pieces 
#' of connected components)
#' @example scissorsMaker(d = 10, type = "scc",maxSCC = 1)

scissorsMaker <- function(d, type="ncc", ncc = NULL, maxSCC=d, minSCC=1,
                          seed = NULL){

  #' Conditions for making the scissors
  stopifnot(d >= 1)
  stopifnot((1 <= ncc) & (ncc <= d))
  stopifnot((1 <= maxSCC) & (maxSCC <= d))
  stopifnot(((1 <= minSCC) & (minSCC <= d/2))| (minSCC == d) )
  stopifnot(minSCC <= maxSCC)
  
  if (minSCC == maxSCC){
    stopifnot( d %% minSCC == 0)
  }
  
  #' Setting a random seed if it's not specified in the inputs. 
  if(is.null(seed)){
    seed <- sample.int(1000000,1)
  }
  
  set.seed(seed)
  
  #' Based on number of connected components
  if(type == "ncc"){                                              
    
    if(is.null(ncc)){
      #' number of connected components.
      ncc <- sample(1:d, 1)                                      
    }
    
    #' placing the scissors 
    scissors <- sort(sample.int(d-1, ncc-1,replace = FALSE),decreasing = F)  
    
    #' return scissors vector in case of "ncc"
    return(scissors)
    
  #' Based on length of size of connected components
  }else if(type == "scc"){                                        
    
    
    if(maxSCC == 1 | minSCC == d){                                
      
      #' In this case, the forest has only isolated nodes
      if(maxSCC == 1){
        #' placing the scissors 
        scissors <- c(1:(d-1))
      }
      
      #' In this case, the forest is a spanning tree
      if(minSCC == d){
        #' placing the scissors in this case we don't have any scissors.
        scissors <- integer()
      }
      
    }else{
    
      #' We want to use a while loop enabling the function to search for a possible
      #' structure of the forest with specified characters. So with random selection
      #' of scissors the function tries to find a possible structure for desired 
      #' forest.

      unacceptable_last_scissor <- T
      
      
      #' Sometimes it's not possible to find a way to make a forest with the
      #' specified number of nodes(d), minimum size of connected components(minSCC),
      #' and maximum size of connected components(maxSCC) so we have to stop function 
      #' otherwise it continues to find a way to make the forest which is impossible!
      #' So we need to count number of efforts made by function and stop it after 
      #' a large number of efforts. 
      counter_effort<-0
      
      while (unacceptable_last_scissor){
        
        scissors <- vector()

        start_interval <- minSCC
        end_interval <- maxSCC
        
        continue_decision <- T
        
        #' Do it until you have the decision to continue splitting the sequence
        while (continue_decision) {                 
          
          scissor <- sample(x = start_interval:end_interval,size = 1)
          
          #' the scissor shouldn't be equal or larger than d
          if(scissor > d-minSCC){                         
            
            break
          }
          scissors <- cbind(scissors,scissor)
            
          start_interval <- scissor + minSCC 
          
          #' In this case, this scissor is the last scissor because the last part
          #' is going to be smaller than minSCC
          if (start_interval > d - minSCC){              
            break
          }
          
          end_interval <- scissor + maxSCC 
          
          #' If the distance between the last scissor and d is smaller than max size 
          #' of connected component then by chance you can decide whether to continue 
          #' or not. 
          if(end_interval > d ){                     
            continue_decision <- sample(c(TRUE,FALSE),1)
          }
        }
        
       if(length(scissors)!=0){

         #' The last scissor shouldn't make a connected component size which is 
         #' bigger than maxSCC
         if( tail(as.numeric(scissors),1) >= d - maxSCC){                
           break 
         }
         
       }
        
       counter_effort<- counter_effort+1
       
       #' Have you already did many efforts and they were not successful? it's 
       #' enough! you did your job man maybe the construction of desired forest
       #' is IMPOSSIBLE! 
       if(counter_effort>1000000){      
         stop("Error: scissors_maker_function tried more than 1M times to find a
              way to make this forest but it seems impossible! Please change the
              arguments")    
       }   
      }
    }
    #' return scissors vector in case of "scc"
    return(scissors)
  }
}


