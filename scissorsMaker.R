 
scissorsMaker <- function(d, type="ncc", ncc = NULL, max_scc=d, min_scc=1,
                          seed = NULL){

  ##############################################################################
  #' @author Pouya Roudaki
  #' @description   In this function we just select our scissors to make disjoint
  #' connected components.
  
  #' @param  ncc: number of connected components
  #' @param  scc: size of connected components
  #' @return scissors: a vector of vertices which disconnnect different connected
  #' component I called them scissors (for cutting the sequence to small pieces 
  #' of connected components)
  #' @example scissorsMaker(d = 10, type = "scc",max_scc = 1)

  ##############################################################################
  
  
  #' Conditions for making the scissors
  stopifnot(d >= 1)
  stopifnot((1 <= ncc) & (ncc <= d))
  stopifnot((1 <= max_scc) & (max_scc <= d))
  stopifnot(((1 <= min_scc) & (min_scc <= d/2))| (min_scc == d) )
  stopifnot(min_scc <= max_scc)
  
  if (min_scc == max_scc){
    stopifnot( d %% min_scc == 0)
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
    
    
    if(max_scc == 1 | min_scc == d){                                
      
      #' In this case, the forest has only isolated nodes
      if(max_scc == 1){
        #' placing the scissors 
        scissors <- c(1:(d-1))
      }
      
      #' In this case, the forest is a spanning tree
      if(min_scc == d){
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
      #' specified number of nodes(d), minimum size of connected components(min_scc),
      #' and maximum size of connected components(max_scc) so we have to stop function 
      #' otherwise it continues to find a way to make the forest which is impossible!
      #' So we need to count number of efforts made by function and stop it after 
      #' a large number of efforts. 
      counter_effort<-0
      
      while (unacceptable_last_scissor){
        
        scissors <- vector()

        
        start_interval <- min_scc
        end_interval <- max_scc
        
        continue_decision <- T
        
        #' Do it until you have the decision to continue splitting the sequence
        while (continue_decision) {                 
          
          scissor <- sample(x = start_interval:end_interval,size = 1)
          
          #' the scissor shouldn't be equal or larger than d
          if(scissor > d-min_scc){                         
            
            break
          }
          scissors <- cbind(scissors,scissor)
          
          
          start_interval <- scissor + min_scc 
          
          #' In this case, this scissor is the last scissor because the last part
          #' is going to be smaller than min_scc
          if (start_interval > d - min_scc){              
            break
          }
          
          end_interval <- scissor + max_scc 
          
          #' If the distance between the last scissor and d is smaller than max size 
          #' of connected component then by chance you can decide whether to continue 
          #' or not. 
          if(end_interval > d ){                    
            
            continue_decision <- sample(c(TRUE,FALSE),1)
            
          }
        }
        
       if(length(scissors)!=0){
         
         #' The last scissor shouldn't make a connected component size which is 
         #' bigger than max_scc
         if( tail(as.numeric(scissors),1) >= d - max_scc){                
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


