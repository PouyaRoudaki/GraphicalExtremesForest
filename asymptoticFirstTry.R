
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
nSeq <- as.integer(10**seq(2,7,0.1)) # DANGER!!!!!
# Sequences of "p"s (where p = k/n)
pSeq <- 1 - 1/as.integer(10**seq(1,7,1)) 

################################################################################
################################################################################

# !!!!!  IF YOU USE nSeq <- as.integer(10**seq(2,6,0.1)) and
#               pSeq <- 1 - 1/as.integer(10**seq(1,6,1)) it ends much Faster !!!

################################################################################

result<-vector()

for (n in nSeq) {
  
  #Making the dataset
  
  #print(n)
  print(round(log10(n),2))
  treesDataList <- list()
  for (i in 1:ncc) {
    #' simulation of multivariate random sample corresponds to each tree and its
    #' Gamma
    treesDataList[[i]] <- rmstable_tree(n, "HR", tree = treesList[[i]],
                                        par = extremalGammaList[[i]])
  }
  #' merge data of trees together
  simulatedData <- do.call(cbind,treesDataList)
  #print(log10(n)-1)

  for (j in 1:floor(log10(n))) {
    
   
    p <- pSeq[j]
    #print(p)
    empChiMat <- emp_chi(simulatedData, p)
    
    diffChiMat <- popChiMat - empChiMat
    diffChiVec <- c(diffChiMat)
    maxDiffChiConnected <- max(abs(diffChiVec[connectedInd]))
    meanSumSquareDiffChiConnected <- sqrt(mean(diffChiVec[connectedInd]**2))
    maxDiffChiUnconnected <- max(abs(diffChiVec[!connectedInd]))
    meanSumSquareDiffChiUnonnected <- sqrt(mean(diffChiVec[!connectedInd]**2))
    
    #if (meanSumSquareDiffChiUnonnected == 0){
    #  print(p)
    #  print(popChiMat)
    #  print(empChiMat)
    #  print(diffChiMat)
    #  print(diffChiVec)
    #  print(diffChiVec[!connectedInd]**2)
    #  break
    #}
    
    #print("log10(n):")
    #print(log10(n))
    
    newraw <- c(log10(n),-log10(1-p),maxDiffChiConnected,meanSumSquareDiffChiConnected,
                maxDiffChiUnconnected,meanSumSquareDiffChiUnonnected)
    result <- rbind(result,newraw)
  }
}

#load(file = "asymptotic1.RData")

result <- data.frame(result)
colnames(result) <- cbind("log10(n)","-log10(1-p)","maxDiffChiConnected",
                          "meanSumSquareDiffChiConnected",
      "maxDiffChiUnconnected","meanSumSquareDiffChiUnconnected")

result$`log10(n)` <- round(result$`log10(n)`,2)
result$`-log10(1-p)` <- round(result$`-log10(1-p)`,2)





#library("plot3D")
#scatter3D(result$`log10(n)`, result$`-log10(1-p)`, result$maxDiffChiUnconnected,
#          phi = 0,xlab = "log10(n)",ylab = "-log10(1-p)", 
#          zlab = "maxDiffChiUnconnected",
#          cex = 2,type = "p")


################################################################################
# PLOTS

library(rgl)

x<- result$`log10(n)`
y<- result$`-log10(1-p)`
z<- result$maxDiffChiUnconnected


plot3d(x,y,z,xlab = "log10(n)",ylab = "-log10(1-p)", zlab = "maxDiffChiUnconnected"
       ,size = 10)

loess(z~x+y,control=loess.control(surface='direct'),span=.5,degree=2) -> fit.loess

xnew <- seq(min(x), max(x), len=20)
ynew <- seq(min(y), max(y), len=20)

df <- expand.grid(x = xnew, y = ynew)

df$ind <- ifelse(df$x >= df$y ,TRUE,FALSE)
df$z <- as.vector(predict(fit.loess,newdata=df))
df$z[df$ind == FALSE]<-0
############################################
surface3d(xnew, ynew, df$z, col="green",alpha=0.2)


###################

 
z<- result$meanSumSquareDiffChiUnconnected

plot3d(x,y,z,xlab = "log10(n)",ylab = "-log10(1-p)", zlab = "meanDiffChiUnconnected",
       size = 10,col = "black")

loess(z~x+y,control=loess.control(surface='direct'),span=.5,degree=2) -> fit.loess

df$z <- as.vector(predict(fit.loess,newdata=df))
df$z[df$ind == FALSE]<-0

df1 <- df
df1$z1 <- df$z
############################################
surface3d(xnew, ynew, df$z, col="green",alpha=0.2)

#############


z<- result$maxDiffChiConnected

plot3d(x,y,z,xlab = "log10(n)",ylab = "-log10(1-p)", zlab = "maxDiffChiConnected"
       ,size = 10)

loess(z~x+y,control=loess.control(surface='direct'),span=.5,degree=2) -> fit.loess

df$z <- as.vector(predict(fit.loess,newdata=df))
df$z[df$ind == FALSE]<-0


############################################
surface3d(xnew, ynew, df$z, col="green",alpha=0.2)


###################


z<- result$meanSumSquareDiffChiConnected


plot3d(x,y,z,xlab = "log10(n)",ylab = "-log10(1-p)", zlab = "meanDiffChiConnected",
       size = 10,col = "black")

loess(z~x+y,control=loess.control(surface='direct'),span=.5,degree=2) -> fit.loess

df$z <- as.vector(predict(fit.loess,newdata=df))
df$z[df$ind == FALSE]<-0


############################################
surface3d(xnew, ynew, df$z, col="green",alpha=0.2)


################################################################################
################################################################################

#### LINEAR MODELS????

data <- result[which(result$`-log10(1-p)`==4),]
lmMeanConnected <- lm(data = data,formula = `meanSumSquareDiffChiConnected` ~ `log10(n)`)
coef(lmMeanConnected)

lmMeanUnconnected <- lm(data = data,formula = `meanSumSquareDiffChiUnconnected` ~ `log10(n)`)
coef(lmMeanUnconnected)

##############

data <- result[which(result$`-log10(1-p)`==3),]
lmMeanConnected <- lm(data = data,formula = `meanSumSquareDiffChiConnected` ~ `log10(n)`)
coef(lmMeanConnected)

lmMeanUnconnected <- lm(data = data,formula = `meanSumSquareDiffChiUnconnected` ~ `log10(n)`)
coef(lmMeanUnconnected)

#############

data <- result[which(result$`-log10(1-p)`==2),]
lmMeanConnected <- lm(data = data,formula = `meanSumSquareDiffChiConnected` ~ `log10(n)`)
coef(lmMeanConnected)

lmMeanUnconnected <- lm(data = data,formula = `meanSumSquareDiffChiUnconnected` ~ `log10(n)`)
coef(lmMeanUnconnected)

###################################

