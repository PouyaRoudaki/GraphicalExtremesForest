#\tau functionality


errorI95quantiles %>% 
  filter(n==100000)->n100000

plot(n100000$k,n100000$quantile95)
plot(log(n100000$k),n100000$quantile95)


lm1 <- lm(errorI95quantiles$quantile95~errorI95quantiles$k)
summary(lm1)


errorI95quantiles %>% 
  filter(k==1000)->k1000

plot(k1000$n,k1000$quantile95)
plot(-log(k1000$n),k1000$quantile95)

errorI95quantiles %>% 
  filter(k==10000)->k10000

plot(k10000$n,k10000$quantile95)
plot(-log(k10000$n),k10000$quantile95)

errorI95quantilesNew <- errorI95quantiles %>% 
  mutate(negLogn = log(n))

###############################################################################

lm2 <- lm(errorI95quantilesNew$quantile95 ~ errorI95quantilesNew$k + 
errorI95quantilesNew$negLogn + I(errorI95quantilesNew$negLogn ^ 2) )
summary(lm2)

errorI95quantilesNew %>% 
  filter(k>1000) -> errorI95quantilesNew1

lm3 <- lm(quantile95 ~ k + negLogn*k 
             ,data = errorI95quantilesNew1)
summary(lm3)

lm4 <- lm(quantile95 ~ k + negLogn*k 
          ,data = errorI95quantilesNew)
summary(lm4)

lm5 <- lm(quantile95 ~ k + negLogn*k + I(negLogn^2)*k
          ,data = errorI95quantilesNew)
summary <- summary(lm5)

lm6 <- glm(quantile95 ~ k + negLogn*k + I(negLogn^2)*k,
          link = "logit",data = errorI95quantilesNew)
summary1 <- summary(lm6)

