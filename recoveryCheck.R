################################################################################
#############################    Recovery Check   ##############################
################################################################################


################################################################################
################ Based on complexity of forest or number of edges  #############
################################################################################
library(graphicalExtremes) 
library(dplyr)
library(igraph)
library(ggplot2)
library(gridExtra)

source("generateRandomForest.R")
source("dataSimulationHR.R")
source("forestSimulationHR.R")
source("kruskalAlgorithmForest.R")
source("plotRecoveredForest.R")


#' An instance of a generated forest
d <- 10  
seed <- 9
forestStrLst <- generateRandomForest(d = d,seed = seed,showPlot = T)
forestStrLst
trueAdj <- forestStrLst[[3]]

empAdjCumulative <- matrix(0,d,d)
#' m times simulation
m <- 200
n <- 10000
CCRecoveryCounter <- 0
RecoveryCounter <- 0

for (i in 1:m) {
  
  empEstList <- forestSimulationHR( forestStructureList = forestStrLst, n = n)
  empEstList 
  
  empVarioGraphDf <- empEstList[[2]]
  # Choosing the tree structure
  empAdj <- kruskalMSF(graphDf = empVarioGraphDf, maxEdge = 5)         
  
  # to find recovery rate
  if (identical(trueAdj,empAdj)) {                                         
    RecoveryCounter <- RecoveryCounter + 1
  }
  
  # to find recovery rate
  if( identical(components(graph.adjacency(trueAdj)) ,
                components(graph.adjacency(empAdj))) ){
    CCRecoveryCounter <- CCRecoveryCounter + 1
  }
  
  # cumulative adj matrix
  empAdjCumulative <- empAdjCumulative + empAdj                        
           
}

CCRecoveryRate <- CCRecoveryCounter/m
RecoveryRate <- RecoveryCounter/m

plotForestGraph(d,empAdjCumulative)


################################################################################
#####  threshold base recovery rate and wrongly estimated edge proportion ######
################################################################################

#' An instance of a generated forest
d <- 10  
seed <- 9
forestStrLst <- generateRandomForest(d = d,seed = seed,showPlot = T)
forestStrLst
trueAdj <- forestStrLst[[3]]

empAdjCumulative <- matrix(0,d,d)
#' m times simulation
m <- 200
n <- 10000
CCRecoveryCounter <- 0
RecoveryCounter <- 0

for (i in 1:m) {
  
  empEstList <- forestSimulationHR( forestStructureList = forestStrLst, n = n)
  empEstList 
  
  empVarioGraphDf <- empEstList[[2]]
  # Choosing the tree structure
  empAdj <- kruskalMSF(graphDf = empVarioGraphDf, threshold = 3.5)         
  
  # to find recovery rate
  if (identical(trueAdj,empAdj)) {                                         
    RecoveryCounter <- RecoveryCounter + 1
  }
  
  # to find recovery rate
  if( identical(components(graph.adjacency(trueAdj)) ,
                components(graph.adjacency(empAdj))) ){
    CCRecoveryCounter <- CCRecoveryCounter + 1
  }
  
  # cumulative adj matrix
  empAdjCumulative <- empAdjCumulative + empAdj                        
  
}

CCRecoveryRate <- CCRecoveryCounter/m
RecoveryRate <- RecoveryCounter/m

plotForestGraph(d,empAdjCumulative)

################################################################################


n <- 100000
d <- 10
seed <- 9
forestStrLst <- generateRandomForest(d = d,seed = seed)
forestStrLst
p <- 0.9
trueAdj <- forestStrLst[[3]]
empAdjCumulative <- matrix(0,d,d)
m <- 200

thresholdVec <- seq(1.5,2,by = 0.05)


performanceMeasure <- data.frame( threshold = numeric(length = length(thresholdVec)),
                                   wrongEstEdgeProp = numeric(length = length(thresholdVec)),
                                   CCRecoveryRate = numeric(length = length(thresholdVec)),
                                   RecoveryRate = numeric(length = length(thresholdVec)),
                                   stringsAsFactors = FALSE) 

performanceMeasure$threshold <- thresholdVec

for (j in 1:length(thresholdVec)) {
  
  CCRecoveryCounter <- 0
  RecoveryCounter <- 0
  wrongProp <- 0
  
  for (i in 1:m) {
    
    empEstList <- forestSimulationHR(forestStructureList = forestStrLst, n = n
                                     ,p = p)
    empEstList 
    
    empVarioGraphDf <- empEstList[[2]]
    #' Choosing the tree structure
    empAdj <- kruskalMSF(graphDf = empVarioGraphDf, threshold = thresholdVec[j])
    
    #' to find recovery rate
    if (identical(trueAdj,empAdj)) {                                        
      RecoveryCounter <- RecoveryCounter + 1
    }
    
    #' to find connected component recovery rate 
    if( identical(components(graph.adjacency(trueAdj)) ,
                  components(graph.adjacency(empAdj))) ){      
      CCRecoveryCounter <- CCRecoveryCounter + 1
    }
    
    #' proportion of wrongly estimated edges
    wrongProp <- wrongProp + (1 - sum(trueAdj*empAdj)/sum(empAdj))       
    
    #' cumulative adj matrix
    empAdjCumulative <- empAdjCumulative + empAdj                        
    
  }
  
  performanceMeasure$CCRecoveryRate[j] <- CCRecoveryCounter/m
  performanceMeasure$RecoveryRate[j] <- RecoveryCounter/m
  performanceMeasure$wrongEstEdgeProp[j] <- wrongProp/m
  
  print(thresholdVec[j])
}


library(ggplot2)

performanceDf <- reshape2::melt(performanceMeasure, id.var = "threshold")

t2.rect1 <- data.frame (xmin=1.9, xmax=1.95, ymin=-Inf, ymax=Inf)

perfPlot <- ggplot(performanceDf, aes(x = threshold, y = value,
                                       colour = variable)) + 
  geom_point() + 
  geom_line() +
  ggtitle(paste("Performance measure  n =",n,"and p =",p))+
  ylab("Prop")+xlab("Threshold")+
  geom_rect(data = t2.rect1, aes( xmin = xmin, xmax = xmax, ymin = ymin,
                                  ymax = ymax),
            fill = "purple", alpha=0.1, inherit.aes = FALSE)
perfPlot
