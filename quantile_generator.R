quantile_generator <- function(Simulated_data,p){
  
  n <- dim(Simulated_data)[1]
  d <- dim(Simulated_data)[2]
  
  #p<-0.1
  #d<-10

  q_df <- matrix(0,d,1)
  colnames(q_df) <- paste("p =",p,"quantiles for n =",n)
  
  for (i in 1:d) {
    q_df[i,1] <- quantile(Simulated_data[,i], probs = seq(0, 1, p), na.rm = FALSE)[2] 
  }
  
  return(q_df)
}

