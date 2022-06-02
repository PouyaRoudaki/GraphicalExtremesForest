#asymptotics
library(dplyr)
library(tidyr)
library(ggplot2)
library(latex2exp)

options(scipen=999)

duplicatedColumnsUnconnected<-duplicated(t(finalResult))
estimatedChiUnconnected <- finalResult[!duplicatedColumnsUnconnected]

index <- which(connectedMat)
finalIndex <- vector()
for (i in index) {
  row <- i%/%10 + 1
  col <- i%%10 
  if (col == 0){
    col <- 10
    row <- i%/%10
  }
  popChi <- popChiMat[row,col]
  row <- as.character(row) 
  col <- as.character(col)
  temp<-paste0(row,",",col,"; $\\chi_{",row,col,"}=",round(popChi,2),"$")
  #temp<-paste0(row,",",col,"; Chi = ",round(popChi,2))
  finalIndex <- c(finalIndex,temp)
}
finalIndex


colnames(finalResultConnectedDF) <- c(c("n","k","p","simulation"),finalIndex)

duplicatedColumnsConnected<-duplicated(t(finalResultConnectedDF))
estimatedChiConnected <- finalResultConnectedDF[!duplicatedColumnsConnected]
estimatedChiConnected <- estimatedChiConnected[,-5]


chiHatUnconnected <- tidyr::gather(estimatedChiUnconnected[,5:42], "Var", 
"ChiHat")

knvector <- cbind(rep(estimatedChiUnconnected$n,times=38),
                  rep(estimatedChiUnconnected$k,times=38),
                  rep(estimatedChiUnconnected$p,times=38),
                  rep(estimatedChiUnconnected$simulation,times=38))

knChiHatUnconnected <- cbind(knvector,chiHatUnconnected)
colnames(knChiHatUnconnected) <- c("n","k","p","simulation","Var", 
"ChiHat")

knChiHatUnconnected %>% 
  group_by(n,k,p) %>% 
  summarise(quantile95 = quantile(ChiHat,0.95)) -> errorI95quantiles

estimatedChiConnected95Quantile <- full_join(errorI95quantiles, 
                                             estimatedChiConnected,
                                             by = c("n","k","p"))


estimatedChiConnected95Quantile %>% 
  group_by(n,k,p,quantile95) %>% 
  summarise(typeII2 = mean(`1,2; $\\chi_{12}=0.53$`<= quantile95),
            typeII3 = mean(`1,3; $\\chi_{13}=0.15$`<= quantile95),
            typeII6 = mean(`2,3; $\\chi_{23}=0.12$`<= quantile95),
            typeII11 = mean(`4,5; $\\chi_{45}=0.32$`<= quantile95),
            typeII12 = mean(`4,6; $\\chi_{46}=0.08$`<= quantile95),
            typeII15 = mean(`5,6; $\\chi_{56}=0.15$`<= quantile95),
            typeII22 = mean(`9,10; $\\chi_{910}=0.85$`<= quantile95)) -> estimatedChiConnectedTypeIIError

estimatedChiConnectedTypeIIError %>% 
  mutate(maxTypeIIError = max(typeII2,typeII3,typeII6,typeII11,typeII12,
                               typeII15,typeII22),
         meanTypeIIError = sum(typeII2,typeII3,typeII6,typeII11,typeII12,
                              typeII15,typeII22)/7) -> estimatedChiConnectedTypeIIError1

################################################################################
#Plots
library(scales)
require(grid)
library(tikzDevice)



estimatedChiDensityPlotter <- function(nValue,kValue){
  
  maxErrorTypeII <- estimatedChiConnectedTypeIIError1$maxTypeIIError[
    which(estimatedChiConnectedTypeIIError1$n == nValue &
            estimatedChiConnectedTypeIIError1$k == kValue)]
  
  quantile95 <- as.numeric(errorI95quantiles[which(errorI95quantiles$n == nValue
                                                   & errorI95quantiles$k == kValue),4])
  
  fileName <- paste0("multipleDensitiesForn",
                     nValue,"k",kValue,".tex")
  
  #
  options(tikzMetricPackages = c("\\usepackage[utf8]{inputenc}",
                                 "\\usepackage[T1]{fontenc}",
                                 "\\usetikzlibrary{calc}",
                                 "\\usepackage{amssymb}",
                                 "\\usepackage{amsmath}"))
  
  tikz(file = fileName,width = 6, height = 6*(2/(1+sqrt(5))))
  
  estimatedChiConnected %>% 
    filter(n == nValue, k==kValue) -> connected
  
  connected <- tidyr::gather(connected[,5:11], "Var", "ChiHat")
  
  knChiHatUnconnected %>% 
    filter(n == nValue, k==kValue) -> unconnected
  
  unconnected <- cbind(rep("unconnected; $\\chi = 0$",dim(unconnected)[2]),unconnected$ChiHat)
  colnames(unconnected) <- c("Var", "ChiHat")
  
  plotData <- rbind(unconnected,connected)
  plotData$ChiHat <- as.numeric(plotData$ChiHat)
  
  
  
  titleText <- paste0("Multiple densities of $\\hat{\\chi}_{ij}$ \n n = ",
                      nValue," and k = ",kValue,"; max type II error = ",maxErrorTypeII)
  
  plot <- plotData %>%
    ggplot( aes(x=ChiHat, fill=Var)) +
    xlim(0, 1)+
    ylim(0,50)+
    geom_density(aes(y=..density..),alpha=0.2) +
    labs(fill="")+
    geom_vline(xintercept = quantile95, 
               color = "red", size=1)+
    annotate(geom = "text",
             label = paste("Quantile 0.95 = ",quantile95),
             x = quantile95,
             y = 25,
             angle = 90, 
             vjust = 1,
             color = "red",
             size = 3)+
    #
    labs(title=titleText,
         x = '$\\hat{\\chi}_{ij}$')+
    #"Estimated Extremal Correlation"
    #TeX(r'($\hat{\chi}_{ij}$)')
    theme_bw( base_size = 9)
  print(plot)
  dev.off()
}

nValues <- estimatedChiConnectedTypeIIError1$n
kValues <- estimatedChiConnectedTypeIIError1$k

setwd("./estimatedChiDensityPlots")

for (i in 1:dim(estimatedChiConnectedTypeIIError1)[1]) {
  n <- nValues[i]
  k <- kValues[i]
  estimatedChiDensityPlotter(nValue = n,kValue = k)
  print(i)
}
dev.off()
setwd("..")


################################################################################
#\tau functionality


nValues <- unique(errorI95quantiles$n)
nFixPlots <- list()

setwd("./quantile95VSkForDifferentFixedn")

for (i in 1:length(nValues)) {
  errorI95quantiles %>% 
    filter(n == nValues[i]) -> data
  
  nFixPlots[[i]] <- ggplot(data,aes(k,quantile95))+ geom_point(color = "blue")+
                           labs(title = paste0("Quantile $0.95$ as threshold v.s. variation of $k$ for $n = ",
                                               nValues[i],"$"),
                                y = "Quantile 0.95 as threshold") +
                           theme_bw( base_size = 9)
  
  fileName <- paste0("quantile95VSkForn",nValues[i],".tex")
  options(tikzMetricPackages = c("\\usepackage[utf8]{inputenc}",
                                 "\\usepackage[T1]{fontenc}",
                                 "\\usetikzlibrary{calc}",
                                 "\\usepackage{amssymb}",
                                 "\\usepackage{amsmath}"))
  tikz(fileName,standAlone = F,width = 6, height = 6*(2/(1+sqrt(5))))
  print(nFixPlots[[i]])
  dev.off()
  print(i)
  
}

setwd("..")
getwd()


################################################################################

kValues <- unique(errorI95quantiles$k)
kFixPlots <- list()

setwd("./quantile95VSnForDifferentFixedk")

for (i in 1:length(kValues)) {
  errorI95quantiles %>% 
    filter(k == kValues[i]) -> data
  
  kFixPlots[[i]] <- ggplot(data,aes(n,quantile95))+ geom_point(color = "blue")+
    labs(title = paste0("Quantile $0.95$ as threshold v.s. variation of $n$ for $k = ",
                        kValues[i],"$"),
         y = "Quantile 0.95 as threshold") +
    theme_bw( base_size = 9)
  
  fileName <- paste0("quantile95VSnFork",kValues[i],".tex")
  options(tikzMetricPackages = c("\\usepackage[utf8]{inputenc}",
                                 "\\usepackage[T1]{fontenc}",
                                 "\\usetikzlibrary{calc}",
                                 "\\usepackage{amssymb}",
                                 "\\usepackage{amsmath}"))
  tikz(fileName,standAlone = F,width = 6, height = 6*(2/(1+sqrt(5))))
  print(kFixPlots[[i]])
  dev.off()
  print(i)
  
}

setwd("..")
getwd()
################################################################################

nValues <- estimatedChiConnectedTypeIIError1$n
kValues <- estimatedChiConnectedTypeIIError1$k

getwd()

for (i in 1:dim(estimatedChiConnectedTypeIIError1)[1]) {
  n <- nValues[i]
  k <- kValues[i]
  estimatedChiDensityPlotterConvergenceRate(nValue = n,kValue = k)
  print(i)
}
setwd("..")
getwd()
dev.off()


estimatedChiDensityPlotterConvergenceRate <- function(nValue,kValue){
  
  
  maxErrorTypeII <- estimatedChiConnectedTypeIIError1$maxTypeIIError[
    which(estimatedChiConnectedTypeIIError1$n == nValue &
            estimatedChiConnectedTypeIIError1$k == kValue)]
  
  quantile95 <- as.numeric(errorI95quantiles[which(errorI95quantiles$n == nValue
                                                   & errorI95quantiles$k == kValue),4])

  
  estimatedChiConnected %>% 
    filter(n == nValue, k==kValue) -> connected
  
  connected <- tidyr::gather(connected[,5:11], "Var", "ChiHat")
  
  
  connected %>% 
    mutate(Chi = as.numeric(str_extract(string = Var,"\\d\\.\\d+"))) -> connected

  
  connected %>% 
    group_by(Var) %>% 
    summarise(ChiHatBar = mean(ChiHat)) -> connectedChiHatMeans
  connected <- merge(connected,connectedChiHatMeans,"Var")
  
  connected$stdChiWithChi <- sqrt(kValue)*(connected$ChiHat - connected$Chi)
  connected$stdChiWithChiHatBar <- sqrt(kValue)*(connected$ChiHat - connected$ChiHatBar)
  head(connected)
  
  knChiHatUnconnected %>% 
    filter(n == nValue, k==kValue) -> unconnected
  
  unconnected <- as.data.frame(cbind(rep("unconnected; $\\chi = 0$",dim(unconnected)[2]),unconnected$ChiHat))
  colnames(unconnected) <- c("Var", "ChiHat")
  
  unconnected$ChiHat <- as.numeric(unconnected$ChiHat)
  unconnected$Chi <- 0 
  unconnected$ChiHatBar <- mean(unconnected$ChiHat)
  unconnected$stdChiWithChi <- sqrt(kValue)*(unconnected$ChiHat - unconnected$Chi)
  unconnected$stdChiWithChiHatBar <- sqrt(kValue)*(unconnected$ChiHat - unconnected$ChiHatBar)
  
  head(unconnected)
  
  plotData <- rbind(unconnected,connected)
  head(plotData)
 
  options(tikzMetricPackages = c("\\usepackage[utf8]{inputenc}",
                                 "\\usepackage[T1]{fontenc}",
                                 "\\usetikzlibrary{calc}",
                                 "\\usepackage{amssymb}",
                                 "\\usepackage{amsmath}"))
  
  fileNameStdWithChi <- paste0("multipleStdDensitiesWithChiForn",
                               nValue,"k",kValue,".tex")
  
  setwd("./multipleStdDensitiesWithChi")
  
  tikz(file = fileNameStdWithChi,width = 6, height = 6*(2/(1+sqrt(5))))
  
  titleText <- paste0("Multiple densities of $\\hat{\\chi}_{ij}$ with $\\chi_{ij}$ \n n = ",
                      nValue," and k = ",kValue)
  
  plot <- plotData %>%
    ggplot( aes(x=stdChiWithChi, fill=Var)) +
    #xlim(0, 1)+
    ylim(0,50)+
    geom_density(aes(y=..density..),alpha=0.2) +
    labs(fill="")+
    #geom_vline(xintercept = quantile95, 
    #           color = "red", size=1)+
    #annotate(geom = "text",
    #         label = paste("Quantile 0.95 = ",quantile95),
    #         x = quantile95,
    #         y = 25,
    #         angle = 90, 
    #         vjust = 1,
    #         color = "red",
    #         size = 3)+
    #
    labs(title=titleText,
         x = '$\\sqrt{k}(\\hat{\\chi}_{ij}-\\chi_{ij})$')+
    #"Estimated Extremal Correlation"
    #TeX(r'($\hat{\chi}_{ij}$)')
    theme_bw( base_size = 9)
  print(plot)
  dev.off()
  setwd("..")
  
  ##############################################################################
  options(tikzMetricPackages = c("\\usepackage[utf8]{inputenc}",
                                 "\\usepackage[T1]{fontenc}",
                                 "\\usetikzlibrary{calc}",
                                 "\\usepackage{amssymb}",
                                 "\\usepackage{amsmath}"))
  
  fileNameStdWithChiHatMean <- paste0("multipleStdDensitiesWithChiHatMeanForn",
                                      nValue,"k",kValue,".tex")
  
  setwd("./multipleStdDensitiesWithChiHatMean")
  
  tikz(file = fileNameStdWithChiHatMean,width = 6, height = 6*(2/(1+sqrt(5))))
  
  titleText <- paste0("Multiple densities of std $\\hat{\\chi}_{ij}$ with $\\bar{\\hat{\\chi}}_{ij}$ \n n = ",
                      nValue," and k = ",kValue)
  
  plot <- plotData %>%
    ggplot( aes(x=stdChiWithChiHatBar, fill=Var)) +
    #xlim(0, 1)+
    ylim(0,50)+
    geom_density(aes(y=..density..),alpha=0.2) +
    labs(fill="")+
    #geom_vline(xintercept = quantile95, 
    #           color = "red", size=1)+
    #annotate(geom = "text",
    #         label = paste("Quantile 0.95 = ",quantile95),
    #         x = quantile95,
    #         y = 25,
    #         angle = 90, 
    #         vjust = 1,
    #         color = "red",
    #         size = 3)+
    #
  labs(title=titleText,
       x = '$\\sqrt{k}(\\hat{\\chi}_{ij}-\\bar{\\hat{\\chi}}_{ij})$')+
    #"Estimated Extremal Correlation"
    #TeX(r'($\hat{\chi}_{ij}$)')
    theme_bw( base_size = 9)
  print(plot)
  dev.off()
  setwd("..")
}
