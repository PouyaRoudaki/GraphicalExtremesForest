
library(ggplot2)
library(dplyr)
library(stringr)


knChiHatUnconnected %>% 
  filter(k>100) %>% 
  group_by(n,k) %>% 
  summarise(chiHatBar = mean(ChiHat))->unconnectedMeanChiHat

unconnectedMeanChiHat$prop <- unconnectedMeanChiHat$k/unconnectedMeanChiHat$n


ggplot() + geom_point(data = unconnectedMeanChiHat,
                      aes(x = prop, y =chiHatBar,color = "red"))+
  xlim(0,0.001) + ylim(0,0.001)

ggplot() + geom_point(data = unconnectedMeanChiHat,
                      aes(x = prop, y =chiHatBar,color = "red"))+
  xlim(0,0.01) + ylim(0,0.01)

ggplot() + geom_point(data = unconnectedMeanChiHat,
                      aes(x = prop, y =chiHatBar,color = "red"))+
  xlim(0,0.1) + ylim(0,0.1)

ggplot() + geom_point(data = unconnectedMeanChiHat,
                      aes(x = prop, y =chiHatBar,color = "red"))+
  xlim(0,1) + ylim(0,1)


summary(lm(unconnectedMeanChiHat$chiHatBar~unconnectedMeanChiHat$prop))

a <- unconnectedMeanChiHat

a$var <- "chiHatBar_0"
new_a <- a[,3:5]
colnames(new_a) <- c("chiHatBarDifference","prop","var")
new_a <- select(new_a, prop, var, chiHatBarDifference)

estimatedChiConnected

estimatedChiConnected %>% 
  filter(k>100) %>% 
  group_by(n,k) %>% 
  summarise(chiHatBar12_0.53 = mean(`1,2; $\\chi_{12}=0.53$`)-0.53,
            chiHatBar13_0.15 = mean(`1,3; $\\chi_{13}=0.15$`)-0.15,
            chiHatBar23_0.12 = mean(`2,3; $\\chi_{23}=0.12$`)-0.12,
            chiHatBar45_0.32 = mean(`4,5; $\\chi_{45}=0.32$`)-0.32,
            chiHatBar46_0.08 = mean(`4,6; $\\chi_{46}=0.08$`)-0.08,
            chiHatBar56_0.15 = mean(`5,6; $\\chi_{56}=0.15$`)-0.15,
            chiHatBar910_0.85 = mean(`9,10; $\\chi_{910}=0.85$`)-0.85)->b



b$prop <- b$k/b$n
b %>% 
  filter(prop<1) -> c
plot(c$prop,c$chiHatBar12_0.53)
plot(c$prop,c$chiHatBar13_0.15)
plot(c$prop,c$chiHatBar23_0.12)
plot(c$prop,c$chiHatBar45_0.32)
plot(c$prop,c$chiHatBar46_0.08)
plot(c$prop,c$chiHatBar56_0.15)
plot(c$prop,c$chiHatBar910_0.85)

d <- tidyr::gather(c[,3:9])
d <- cbind(rep(c$prop,times = 7),d) 
colnames(d) <- c("prop","var","chiHatBarDifference")

d <- rbind(new_a,d)
d$var <- as.factor(d$var)




library(ggplot2)
ggplot() + geom_point(data = d, aes(x = prop, y = chiHatBarDifference, color = var))+
  xlim(0,0.1) + ylim(-0.1,0.1)
summary(lm(chiHatBarDifference ~ prop * var,data = d))


################################################################################
nValue = 100000
kValue = 3162

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

#View(connected)

connected$stdChiWithChi <- sqrt(kValue)*(connected$ChiHat - connected$Chi -kValue/nValue)
connected$stdChiWithChiHatBar <- sqrt(kValue)*(connected$ChiHat - connected$ChiHatBar)


knChiHatUnconnected %>% 
  filter(n == nValue, k==kValue) -> unconnected

unconnected <- as.data.frame(cbind(rep("unconnected; $\\chi = 0$",dim(unconnected)[2]),unconnected$ChiHat))
colnames(unconnected) <- c("Var", "ChiHat")

unconnected$ChiHat <- as.numeric(unconnected$ChiHat)
unconnected$Chi <- 0 
unconnected$ChiHatBar <- mean(unconnected$ChiHat)
unconnected$stdChiWithChi <- sqrt(kValue)*(unconnected$ChiHat  - unconnected$Chi-kValue/nValue)
unconnected$stdChiWithChiHatBar <- sqrt(kValue)*(unconnected$ChiHat - unconnected$ChiHatBar )

head(unconnected)

#(1+kValue/nValue)*

plotData <- rbind(unconnected,connected)
head(plotData)


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
#dev.off()
#setwd("..")

#################################################################################

