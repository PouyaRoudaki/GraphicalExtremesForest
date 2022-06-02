
library(graphicalExtremes) 
library(dplyr)
library(igraph)
library(ggplot2)
library(gridExtra)

source("generateRandomForest.R")
source("diffRelationsGenerator.R")
source("dataSimulationHR.R")

forestStrLst <- generateRandomForest(d=10,type = "scc",minSCC = 3,
                                       seed = 655050, showPlot = T)
forestStrLst

dfRelation <- diffRelationsGenerator(forestStrLst)

################################################################################
########################   Convergence plots ###################################
################################################################################



################################################################################
########################   data Simulation   ###################################
################################################################################


simulatedData <- dataSimulationHR(forestStrLst,n = 10000)

standardizedData <- data2mpareto(simulatedData,p = 0.9)
### Quantiles
#source("quantileGenerator.R")
#qMat <- quantile_generator(standardizedData,p=0.9)
qMat <- rep(1,dim(standardizedData)[2])
################################################################################
#############################   neighbors   #####################################
################################################################################



Var1 <- dfRelation$neighbor[1]
Var2 <- dfRelation$neighbor[2]

stdDataTheArea <- data.frame(standardizedData[which(standardizedData[,Var1]>qMat[Var1] |
                                                    standardizedData[,Var2]>qMat[Var2]),c(Var1,Var2)])
colnames(stdDataTheArea) <- c("Var1","Var2")



ggplot(stdDataTheArea,aes(x=Var1, y=Var2)) +
  xlab(paste("Variable :",as.character(Var1))) +
  ylab(paste("Variable :",as.character(Var2))) +
  labs(title = "independence convergence for dependent variables")+
  geom_point()

ggplot(stdDataTheArea,aes(x=Var1, y=Var2)) +
  xlab(paste("Variable :",as.character(Var1))) +
  ylab(paste("Variable :",as.character(Var2))) +
  labs(title = "independence convergence for dependent variables")+
  geom_point()+
  scale_x_continuous(limits = c(0, 100))+
  scale_y_continuous(limits = c(0, 100))

ggplot(stdDataTheArea,aes(x=Var1, y=Var2)) +
  xlab(paste("Variable :",as.character(Var1))) +
  ylab(paste("Variable :",as.character(Var2))) +
  labs(title = "independence convergence for dependent variables")+
  geom_point()+
  scale_x_continuous(limits = c(0, 10))+
  scale_y_continuous(limits = c(0, 10))





################################################################################
##########################   conditional indep   ###############################
################################################################################

Var1 <- dfRelation$connected[1]
Var2 <- dfRelation$connected[2]

stdDataTheArea <- data.frame(standardizedData[which(standardizedData[,Var1]>qMat[Var1] |
                                                    standardizedData[,Var2]>qMat[Var2]),c(Var1,Var2)])
colnames(stdDataTheArea) <- c("Var1","Var2")



ggplot(stdDataTheArea,aes(x=Var1, y=Var2)) +
  xlab(paste("Variable :",as.character(Var1))) +
  ylab(paste("Variable :",as.character(Var2))) +
  labs(title = "independence convergence for Conditionally Independent variables")+
  geom_point()

ggplot(stdDataTheArea,aes(x=Var1, y=Var2)) +
  xlab(paste("Variable :",as.character(Var1))) +
  ylab(paste("Variable :",as.character(Var2))) +
  labs(title = "independence convergence for Conditionally Independent variables")+
  geom_point()+
  scale_x_continuous(limits = c(0, 100))+
  scale_y_continuous(limits = c(0, 100))

ggplot(stdDataTheArea,aes(x=Var1, y=Var2)) +
  xlab(paste("Variable :",as.character(Var1))) +
  ylab(paste("Variable :",as.character(Var2))) +
  labs(title = "independence convergence for Conditionally Independent variables")+
  geom_point()+
  scale_x_continuous(limits = c(0, 10))+
  scale_y_continuous(limits = c(0, 10))


################################################################################
##############################   independence   ################################
################################################################################

Var1 <- dfRelation$unconnected[1]
Var2 <- dfRelation$unconnected[2]

stdDataTheArea <- data.frame(standardizedData[which(standardizedData[,Var1]>qMat[Var1] |
                                                    standardizedData[,Var2]>qMat[Var2]),c(Var1,Var2)])
colnames(stdDataTheArea) <- c("Var1","Var2")

ggplot(stdDataTheArea,aes(x=Var1, y=Var2)) +
  xlab(paste("Variable :",as.character(Var1))) +
  ylab(paste("Variable :",as.character(Var2))) +
  labs(title = "independence convergence for Independent variables")+
  geom_point()

ggplot(stdDataTheArea,aes(x=Var1, y=Var2)) +
  xlab(paste("Variable :",as.character(Var1))) +
  ylab(paste("Variable :",as.character(Var2))) +
  labs(title = "independence convergence for Independent variables")+
  geom_point()+
  scale_x_continuous(limits = c(0, 100))+
  scale_y_continuous(limits = c(0, 100))

ggplot(stdDataTheArea,aes(x=Var1, y=Var2)) +
  xlab(paste("Variable :",as.character(Var1))) +
  ylab(paste("Variable :",as.character(Var2))) +
  labs(title = "independence convergence for Independent variables")+
  geom_point()+
  scale_x_continuous(limits = c(0, 10))+
  scale_y_continuous(limits = c(0, 10))

################################################################################
##########################  handmade independence  #############################
################################################################################

P1 <- stdDataTheArea %>% filter(Var1>=qMat[Var1])
P1$Var2 <- 0

P2 <- stdDataTheArea %>% filter(Var2>=qMat[Var2])
P2$Var1 <- 0

################################################################################

bernoulliResult <- table(sample(c("P1","P2"), dim(stdDataTheArea)[1], TRUE))
bernoulliResult
sampledP1 <- P1[sample((1:dim(stdDataTheArea)[1]),
                       size = as.numeric(bernoulliResult[1]),replace = T),]
sampledP2 <- P2[sample((1:dim(stdDataTheArea)[1]),
                       size = as.numeric(bernoulliResult[2]),replace = T),]
handmadeIndep <- rbind(sampledP1,sampledP2)
row.names(handmadeIndep)<-1:dim(stdDataTheArea)[1]

handmadeIndep$type <- "handmade"
realIndep <- stdDataTheArea
realIndep$type <- "real"  
newData <- rbind(realIndep,handmadeIndep)
###############################################################################3

ggplot(newData,aes(x=Var1, y=Var2)) +
  xlab(paste("Variable :",as.character(Var1))) +
  ylab(paste("Variable :",as.character(Var2))) +
  labs(title = "independence convergence for Independent variables",color="Type")+
  geom_point(aes(color = as.factor(type)))

ggplot(newData,aes(x=Var1, y=Var2)) +
  xlab(paste("Variable :",as.character(Var1))) +
  ylab(paste("Variable :",as.character(Var2))) +
  labs(title = "independence convergence for Independent variables",color="Type")+
  geom_point(aes(color = as.factor(type)))+
  scale_x_continuous(limits = c(0, 100))+
  scale_y_continuous(limits = c(0, 100))

ggplot(newData,aes(x=Var1, y=Var2)) +
  xlab(paste("Variable :",as.character(Var1))) +
  ylab(paste("Variable :",as.character(Var2))) +
  labs(title = "independence convergence for Independent variables",color="Type")+
  geom_point(aes(color = as.factor(type)))+
  scale_x_continuous(limits = c(0, 10))+
  scale_y_continuous(limits = c(0, 10))

################################################################################
######################## Convergence prob limit ################################
################################################################################

# for fixed n p should go to 1 and then we have to see convergence.
# p -> 1 : p in c(0,0.9,0.99,0.999,0.9999)

################################################################################
########################   data Simulation   ###################################
################################################################################

fixedn <- 10e6
simulatedDataFixn <- dataSimulationHR(forestStrLst,n = fixedn)

################################################################################
#######################    Standardization   ###################################
################################################################################
pSeq <- c(0,0.9,0.99,0.999,0.9999,0.99999,0.999999)
length(pSeq)


stdDataList <- list()
for (i in 1:length(pSeq)) {
  stdDataList[[i]] <- data2mpareto(simulatedDataFixn,p = pSeq[i])
}

################################################################################

epsVec <- 10^seq(from = 2, to = -4,by = -1)
epsNames <- paste0(rep("eps",length(epsVec)),c(1:length(epsVec)),
                    rep("=",length(epsVec)),epsVec)
################################################################################
#############################   neighbors   ####################################
################################################################################

probMatList <- list()

Var1 <- dfRelation$neighbor[1]
Var2 <- dfRelation$neighbor[2]

probMat <- matrix(data = NA,nrow = length(pSeq),ncol = length(epsVec)) 

for (i in 1:length(epsVec)) {
  print(paste("eps = ", epsVec[i],":"))
  
  for (j in 1:length(pSeq)) {
    print(c(i,j))
    prodExtremeAreaData <- stdDataList[[j]][which(stdDataList[[j]][,Var1]>epsVec[i] &
                                                  stdDataList[[j]][,Var2]>epsVec[i]),c(Var1,Var2)]
    freq <- dim(prodExtremeAreaData)[1]
    # if the area is empty!
    if(is.null(freq)){                         
      freq <- 0
    }
    
    probMat[j,i] <- freq / dim(stdDataList[[j]])[1]
  }      
}

colnames(probMat) <- epsNames
rownames(probMat) <- paste("Probability for p = ",as.character(pSeq))
probMatList[[1]] <- probMat

################################################################################
##########################   conditional indep   ###############################
################################################################################

Var1 <- dfRelation$connected[1]
Var2 <- dfRelation$connected[2]

probMat <- matrix(data = NA,nrow = length(pSeq),ncol = length(epsVec)) 

for (i in 1:length(epsVec)) {
  print(paste("eps = ", epsVec[i],":"))
  
  for (j in 1:length(pSeq)) {
    print(c(i,j))
    prodExtremeAreaData <- stdDataList[[j]][which(stdDataList[[j]][,Var1]>epsVec[i] &
                                                  stdDataList[[j]][,Var2]>epsVec[i]),c(Var1,Var2)]
    freq <- dim(prodExtremeAreaData)[1]
    # if the area is empty!
    if(is.null(freq)){                        
      freq <- 0
    }
    
    probMat[j,i] <- freq / dim(stdDataList[[j]])[1]
  }      
}

colnames(probMat) <- epsNames
rownames(probMat) <- paste("Probability for p = ",as.character(pSeq))
probMatList[[2]] <- probMat

################################################################################
##############################   independence   ################################
################################################################################

Var1 <- dfRelation$unconnected[1]
Var2 <- dfRelation$unconnected[2]

probMat <- matrix(data = NA,nrow = length(pSeq),ncol = length(epsVec)) 

for (i in 1:length(epsVec)) {
  print(paste("eps = ", epsVec[i],":"))
  
  for (j in 1:length(pSeq)) {
    print(c(i,j))
    prodExtremeAreaData <- stdDataList[[j]][which(stdDataList[[j]][,Var1]>epsVec[i] &
                                                  stdDataList[[j]][,Var2]>epsVec[i]),c(Var1,Var2)]
    freq <- dim(prodExtremeAreaData)[1]
    # if the area is empty!
    if(is.null(freq)){                        
      freq <- 0
    }
    
    probMat[j,i] <- freq / dim(stdDataList[[j]])[1]
  }      
}

colnames(probMat) <- epsNames
rownames(probMat) <- paste("Probability for p = ",as.character(pSeq))
probMatList[[3]] <- probMat

################################################################################
#######################     Convergence Plots   ################################
################################################################################

plotsList <- list()
for (k in 1:length(epsVec)) {
  probDf <- matrix(data = 0,nrow = 3*length(pSeq),ncol = 3)
  
  probDf[,3]<-rep(c("Dependent","condIndep","Indep"),each = length(pSeq),times=1)
  
  for (i in 1:3) {
    for (j in 1:length(pSeq)) {
      
      probDf[(i-1)*length(pSeq)+j,1] <- log10(1-pSeq[j])
      probDf[(i-1)*length(pSeq)+j,2] <- probMatList[[i]][j,k]
      
    }
  }
  
  probDf <- data.frame(probDf)
  colnames(probDf) <- c("p","prob","type")
  
  library(ggplot2)
  probPlot <- ggplot(probDf, aes(x = as.numeric(p), y = as.numeric(prob),
                                   colour = type)) + 
    geom_point() + 
    geom_line() +
    ggtitle(paste("Probability convergence n =",fixedn,"epsilon = ",
                  epsVec[k]))+
    theme(plot.title = element_text(size = 8))+
    guides(shape = guide_legend(override.aes = list(size = 0.1)))+
    ylab("Probability")+xlab("log(1-p)")+
    scale_x_reverse()
  plotsList[[k]] <- probPlot
}

do.call(grid.arrange,plotsList)

par(mfrow=c(1,1))

################################################################################
###############   Find equivalent of Inf in pre-asymptotic case ################
################################################################################

p <- 0.9

empChiMat <- emp_chi(simulatedData, p) 
empChiMat  

-log(empChiMat)
################################################################################

empVarioMat <- emp_vario(simulatedData, k=NULL, p) 
empVarioMat                               

empVarioMatCopy <- empVarioMat
empVarioMatCopy[empVarioMatCopy>1.8]<-Inf
empVarioMatCopy

################################################################################


p <- 0.99

empChiMat <- emp_chi(simulatedData, p)
empChiMat 

-log(empChiMat)
################################################################################

empVarioMat <- emp_vario(simulatedData, k=NULL, p)
empVarioMat                               

empVarioMatCopy <- empVarioMat
empVarioMatCopy[empVarioMatCopy>2.6]<-Inf
empVarioMatCopy

################################################################################


