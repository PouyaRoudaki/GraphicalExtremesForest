

require("graphicalExtremes")
source("generateRandomForest.R")
################################################################################
# Define forest structure

d <- 10  
seed <- 9
forestStrLst <- generateRandomForest(d = d,seed = seed,showPlot = T)

################################################################################
# Make a random Extremal Chi and Variogram for the defined structure

forestStructureList <- forestStrLst

d <- forestStructureList[[1]]
ncc <- forestStructureList[[2]]
adj <- forestStructureList[[3]]
CC <- forestStructureList[[4]]

#' list of adjacency matrices corresponds to each tree
adjMatricesList <- list()

for (i in 1:ncc) {  
  
  adjMatricesList[[i]] <- adj[ CC[[i]][1]:CC[[i]][2] , CC[[i]][1]:CC[[i]][2] ]
  
}

#' Making the list of trees from their adjacency matrices
treesList <- list()

for (i in 1:ncc) {   
  treesList[[i]] <- igraph::graph_from_adjacency_matrix(adjMatricesList[[i]],
                                                        mode = "undirected")
}

#' Gamma assumption for cliques isolated nodes have zero and for other edges
#' we randomly generate a number between 0 to 10 (whole support is zero to inf)
#' for extremal gamma which shows high independence, otherwise if this edges 
#' were able to take larger extremal gamma then it makes problem for distinguishing
#' between independent(unconndected) nodes and dependent(connected) nodes.
#' Note that for extramal gamma(Variogram) zero corresponds to highly dependence
#' and infinity correspoonds to highly independence.

extremalGammaList <- list()
extremalChiList <- list()

showInitialGamma <- TRUE


#' list of Gamma vectors corresponds to cliques(edges) of each tree 
for (i in 1:ncc) {   
  
  #' isolated trees don't have 
  if (sum(adjMatricesList[[i]]) == 0) { 
    extremalGammaList[[i]] <- matrix(0)
    extremalChiList[[i]] <- matrix(1) 
    
    #' For not isolated nodes  
  }else{ 
    
    cliquesNumbers <- sum(adjMatricesList[[i]])/2
    #' ExtrGammaVecOnCliques <- c(1,1,1,1,1) 
    #' Generate random vector of Gamma values on cliques
    ExtrGammaVecOnCliques <- runif(cliquesNumbers,min = 0,max = 10) 
    #' Max can't be infinity ? maybe it's better to define Chi and  convert it
    #'not complete gamma and complete it or is there and Chi_complete?!
    if (showInitialGamma == TRUE){
      paste("Extremal Gamma Vector On Cliques:", ExtrGammaVecOnCliques)
    }
    #' Use Gamma completion method to make a complete gamma
    extremalGammaList[[i]] <- complete_Gamma(Gamma =  ExtrGammaVecOnCliques,
                                             graph = treesList[[i]])
    
    
    
    #' Make Extreaml Chi corresponds to this Gamma
    extremalChiList[[i]] <- Gamma2chi(extremalGammaList[[i]])
    
  }
}



################################################################################
# Finding connected nodes' indices (We make )

# Population Extremal Chi Matrix
popChiMat <- matrix(data = 0, nrow = d, ncol = d)
# A Matrix which has "TRUE" elements to indicate connected nodes
connectedMat <- matrix(data = FALSE, nrow = d, ncol = d)


for (i in 1:ncc) {
  if (i==1){
    base <- 0
  }else{
    base <- CC[[i-1]][3] + base
  }
  start <- CC[[i]][1]
  end <- CC[[i]][2]
  
  # in next loops we are in the connected components
  for (j in start:end) {
    for (k in start:end) {
      # Put the extremal Chi matrix of each connected component in the right place
      popChiMat[j,k] <- extremalChiList[[i]][j-base,k-base]
      # In connected components we only have connected nodes
      connectedMat[j,k] <- TRUE
    }
  }
  
}

# A Matrix which has "TRUE" elements to indicate connected nodes
connectedInd <- c(connectedMat)



################################################################################
# Generate the data samples corresponding to the population Extremal Chi Matrix
# Then for any size of data and any extremal threshold (p = k/n) find the difference
# between "empirical" extremal Chi and population level extremal Chi.


# Sequence size of simulated samples
nSeq <- as.integer(10**seq(1,5,0.1)) # DANGER!!!!!
# Sequences of "k"s (where p = k/n)
kSeq <- as.integer(10**seq(1,5,0.1)) 

################################################################################
################################################################################

# !!!!!  IF YOU USE nSeq <- as.integer(10**seq(2,6,0.1)) and
#               pSeq <- 1 - 1/as.integer(10**seq(1,6,1)) it ends much Faster !!!

################################################################################



library(foreach)
library(doParallel)

#setup parallel backend to use many processors
cores=detectCores()
cl <- makeCluster(cores[1]-1) #not to overload your computer
registerDoParallel(cl)


finalResult<-vector()
counter <- 0


finalresult <- foreach(i = 31:length(nSeq),
                        .combine = rbind,
                        .packages = "graphicalExtremes")%dopar%{
  
  result<-vector()
  #Making the dataset
  
  n <- nSeq[i]
  #print(n)
  #print(round(log10(n),2))
  
  # Simulations iterations
  for (s in 1:1000){
    
    treesDataList <- list()
    
    for (k in 1:ncc) {
      #' simulation of multivariate random sample corresponds to each tree and its
      #' Gamma
      treesDataList[[k]] <- rmstable_tree(n, "HR", tree = treesList[[k]],
                                          par = extremalGammaList[[k]])
    }
    #' merge data of trees together
    simulatedData <- do.call(cbind,treesDataList)
    #print(log10(n)-1)
    
    
    for (j in 1:(i-1)) {
      
      counter <- counter + 1
      #print(counter)
      p <- 1 - kSeq[j]/nSeq[i]
      #print(p)
      empChiMat <- emp_chi(simulatedData, p)
      
      newRow <- c(nSeq[i], kSeq[j],p,s,empChiMat[!connectedInd])
      result <- rbind(result,newRow)
    }
    #print(s)
  }
  #saveRDS(result, file=paste0("result",(i-30),".RData"))
  #print(paste("n:",nSeq[i],"_progress in n:",(i-30)/(length(nSeq)-30)))
  result
}

stopCluster(cl)
head(result)

#save.dta13(result, file=paste0(getwd(),'/', 'fullEmpChiNseq106Kseq106simulations2', '.dta'))


finalResult <- data.frame(finalresult)
colnames(finalResult) <- c("n","k","p",
                          "simulation",
                          as.character(c(1:length(empChiMat[!connectedInd]))))

saveRDS(finalresult, file="finalresult.RData")
saveRDS(finalResult, file="finalResultDF.RData")





