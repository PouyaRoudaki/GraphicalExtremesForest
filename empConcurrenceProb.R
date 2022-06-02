






n <- 100
#obs

data <- cbind(sample(1:10,n,replace = T),
      sample(1:10,n,replace = T),
      sample(1:10,n,replace = T))
blockSize <- 5



maxFinder <- function(dataframe){
  # Find the maximum index in each column
  sapply(dataframe[,-1], which.max)
  
}


empConcurrentExtremes <- function(data,blockSize){
  #start_time <- Sys.time()
  # required packages
  require(stringr)
  require(dplyr)
  
  # Add auxiliary column to data unique value for each block  
  data <- cbind(rep(1:(dim(data)[1]/blockSize),each = blockSize),data)
  
  # Standardize the data format to a data frame
  data <- as.data.frame(data)
  
  # Standardize the data frame's name
  colnames(data) <- as.character(c(1:4))
  
  # Split data to different blocks. insert each block in an element of a list
  list <- split(data,data[,1], drop = FALSE)
  
  # Find the index of maximum of each variable in each block
  maxList <- lapply(list, maxFinder)
  
  # Combine list's elements in a data frame 
  maxDF <- bind_rows(maxList)
  
  # All Combinations of columns
  toCheck <- combn(colnames(maxDF), 2, simplify = FALSE)
  names(toCheck) <- sapply(toCheck, paste0, collapse = "-")
  
  # Find empirical concurrency probabilities insert it in a vector
  vecConcurrentProb <- sapply(toCheck, function(x){
    
    # Find the maximum index in each column
    mean(maxDF[,x[1]] == maxDF[, x[2]])
    
  })
  
  # Build the empirical concurrency probabilities matrix using the vector
  concurrentProbMat <- matrix(data = 0 ,nrow = dim(data)[2]-1,
                                        ncol = dim(data)[2]-1)
  
  for (i in 1:length(vecConcurrentProb)) {
    indices <- as.integer(str_split_fixed(names(vecConcurrentProb[i]),"-",2))-1
    concurrentProbMat[indices[1],indices[2]] <- vecConcurrentProb[i]
  }
  concurrentProbMat <- concurrentProbMat + t(concurrentProbMat)
  diag(concurrentProbMat) <- 1
  
  
  #end_time <- Sys.time()
  #print(end_time - start_time)
  
  # Return the empirical concurrency probabilities matrix
  return(concurrentProbMat)
  
}

empCE <- empConcurrentExtremes(data,blockSize)