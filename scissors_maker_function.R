
#scissors_maker(d = 10, type = "scc",min_scc = 5,max_scc = 5)

scissors_maker <- function(d, type="ncc", ncc = NULL, max_scc=d, min_scc=1, seed = NULL){
  
  ##############################################################################
  ##############################################################################
  
  # In this function we just select our scissors to make disjoint connected 
  # components.
  
  # ncc: number of connected components
  # scc: size of connected components
  
  ##############################################################################
  ##############################################################################
  
  
  ### Conditions for making the scissors
  stopifnot(d >= 1)
  stopifnot((1 <= ncc) & (ncc <= d))
  stopifnot((1 <= max_scc) & (max_scc <= d))
  stopifnot(((1 <= min_scc) & (min_scc <= d/2))| (min_scc == d) )
  stopifnot(min_scc <= max_scc)
  
  if (min_scc == max_scc){
    stopifnot( d %% min_scc == 0)
  }
  ##############################################################################
  ##############################################################################
  
  if(is.null(seed)){
    seed <- sample.int(1000000,1)
  }
  print(paste0("seed =",seed))
  set.seed(seed)
  
  if(type == "ncc"){                                              # Based on number of connected components
    
    if(is.null(ncc)){
      ncc <- sample(1:d, 1)                                       # number of connected components.
    }
    
    scissors <- sort(sample.int(d-1, ncc-1,replace = FALSE),decreasing = F)       # placing the scissors #change it to d-1
    return(scissors)
    
  }else if(type == "scc"){                                        # Based on length of size of connected components
    
    #avg_length <- vector()
    #sizes <- vector(mode = "numeric",length = d)
    
    if(max_scc == 1 | min_scc == d){                                # It's a spanning tree
      
      if(max_scc == 1){
        scissors <- c(1:d)
      }
      
      if(min_scc == d){
        scissors <- integer()
      }
      
    }else{
    
      unacceptable_last_scissor <- T
      while (unacceptable_last_scissor){
        
        #total<-0
        scissors <- vector()
        
        ###$$$
        #d<-10
        #min_scc<-3
        #max_scc<-5
        ###$$$
        
        start_interval <- min_scc
        end_interval <- max_scc
        
        continue_decision <- T
        
        
        
        while (continue_decision) {                 # do it until you have the decision to continue splitting the sequence
          
          scissor <- sample(x = start_interval:end_interval,size = 1)
          
          ###$$$
          #scissor <- 6
          ###$$$
          
          #print(paste("scissor:",scissor))
          
          if(scissor > d-min_scc){                         # the scissor shouldn't be equal or larger than d
            #print("BREAK")
            break
          }
          scissors <- cbind(scissors,scissor)
          #print(paste("scissors:",scissors))
          
          start_interval <- scissor + min_scc 
          #print(paste("start_interval:",start_interval))
          
          if (start_interval > d - min_scc){              # this scissor is the last scissor because the last part is going to be smaller than min_scc
            break
          }
          
          end_interval <- scissor + max_scc 
          #print(paste("end_interval:",end_interval))
          
          if(end_interval > d ){                    # if the distance between the last scissor and d is smaller than max size 
                                                   # of connected component then by chance you can decide whether to continue 
                                                   # or not. 
            
            continue_decision <- sample(c(TRUE,FALSE),1)
            
          }
        }
        
       if(length(scissors)!=0){
         
         if( tail(as.numeric(scissors),1) >= d - max_scc){                  # the last scissor shouldn't make a connected component size which
                                                                   # is bigger than max_scc
           break 
         }
         
       }
        

         
      }
      
    }
    return(scissors)
  }

}

#v<-vector()
#for (i in 1:100000) {
#  v <- cbind(v,scissors_maker(d=10,type = "scc",max_scc = 5,min_scc = 3))
#}

#table(v)/sum(table(v))
#barplot(table(v)/sum(table(v)))  
#scissors_maker(d=10,type = "scc",max_scc = 5,min_scc = 3)
### 5 and 5 is okay 
### 3 and 7 is okay 
### 3 and 6 is okay
### 4 and 7 is okay

#scissors_maker(d=10,type = "scc",max_scc = 5,min_scc = 4)
