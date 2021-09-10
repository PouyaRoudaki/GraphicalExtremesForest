setwd("C:/Users/pouya/Desktop/UNIGE/GSEM/Fall2021/Master Thesis/Codes")
source("generate_random_forest_function.R")

forest_str_lst <- generate_random_forest(d=10,type = "scc",min_scc = 3,seed = 655050)
forest_str_lst



source("diff_relation_generator.R")

df_relation <- diff_relation_generator(forest_str_lst)

################################################################################
########################   Convergence plots ###################################
################################################################################



################################################################################
########################   data Simulation   ###################################
################################################################################

source("data_simulation_HR.R")
Simulated_data <- data_simulation_HR(forest_str_lst,n = 10000)

library(graphicalExtremes)
standardized_data <- data2mpareto(Simulated_data,p=0.9)
### Quantiles

source("quantile_generator.R")
#q_mat <- quantile_generator(standardized_data,p=0.9)
q_mat <- rep(1,dim(standardized_data)[2])
################################################################################
#############################   neighbors   #####################################
################################################################################



Var1 <- df_relation$neighbor[1]
Var2 <- df_relation$neighbor[2]

std_data_TheArea <- data.frame(standardized_data[which(standardized_data[,Var1]>q_mat[Var1] | standardized_data[,Var2]>q_mat[Var2]),c(Var1,Var2)])
colnames(std_data_TheArea) <- c("Var1","Var2")

library(ggplot2)

ggplot(std_data_TheArea,aes(x=Var1, y=Var2)) +
  xlab(paste("Variable :",as.character(Var1))) +
  ylab(paste("Variable :",as.character(Var2))) +
  labs(title = "independence convergence for dependent variables")+
  geom_point()

ggplot(std_data_TheArea,aes(x=Var1, y=Var2)) +
  xlab(paste("Variable :",as.character(Var1))) +
  ylab(paste("Variable :",as.character(Var2))) +
  labs(title = "independence convergence for dependent variables")+
  geom_point()+
  scale_x_continuous(limits = c(0, 100))+
  scale_y_continuous(limits = c(0, 100))

ggplot(std_data_TheArea,aes(x=Var1, y=Var2)) +
  xlab(paste("Variable :",as.character(Var1))) +
  ylab(paste("Variable :",as.character(Var2))) +
  labs(title = "independence convergence for dependent variables")+
  geom_point()+
  scale_x_continuous(limits = c(0, 10))+
  scale_y_continuous(limits = c(0, 10))





################################################################################
##########################   conditional indep   ###############################
################################################################################

Var1 <- df_relation$connected[1]
Var2 <- df_relation$connected[2]

std_data_TheArea <- data.frame(standardized_data[which(standardized_data[,Var1]>q_mat[Var1] | standardized_data[,Var2]>q_mat[Var2]),c(Var1,Var2)])
colnames(std_data_TheArea) <- c("Var1","Var2")

library(ggplot2)

ggplot(std_data_TheArea,aes(x=Var1, y=Var2)) +
  xlab(paste("Variable :",as.character(Var1))) +
  ylab(paste("Variable :",as.character(Var2))) +
  labs(title = "independence convergence for Conditionally Independent variables")+
  geom_point()

ggplot(std_data_TheArea,aes(x=Var1, y=Var2)) +
  xlab(paste("Variable :",as.character(Var1))) +
  ylab(paste("Variable :",as.character(Var2))) +
  labs(title = "independence convergence for Conditionally Independent variables")+
  geom_point()+
  scale_x_continuous(limits = c(0, 100))+
  scale_y_continuous(limits = c(0, 100))

ggplot(std_data_TheArea,aes(x=Var1, y=Var2)) +
  xlab(paste("Variable :",as.character(Var1))) +
  ylab(paste("Variable :",as.character(Var2))) +
  labs(title = "independence convergence for Conditionally Independent variables")+
  geom_point()+
  scale_x_continuous(limits = c(0, 10))+
  scale_y_continuous(limits = c(0, 10))


################################################################################
##############################   independence   ################################
################################################################################

Var1 <- df_relation$unconnected[1]
Var2 <- df_relation$unconnected[2]

std_data_TheArea <- data.frame(standardized_data[which(standardized_data[,Var1]>q_mat[Var1] | standardized_data[,Var2]>q_mat[Var2]),c(Var1,Var2)])
colnames(std_data_TheArea) <- c("Var1","Var2")

library(ggplot2)

ggplot(std_data_TheArea,aes(x=Var1, y=Var2)) +
  xlab(paste("Variable :",as.character(Var1))) +
  ylab(paste("Variable :",as.character(Var2))) +
  labs(title = "independence convergence for Independent variables")+
  geom_point()

ggplot(std_data_TheArea,aes(x=Var1, y=Var2)) +
  xlab(paste("Variable :",as.character(Var1))) +
  ylab(paste("Variable :",as.character(Var2))) +
  labs(title = "independence convergence for Independent variables")+
  geom_point()+
  scale_x_continuous(limits = c(0, 100))+
  scale_y_continuous(limits = c(0, 100))

ggplot(std_data_TheArea,aes(x=Var1, y=Var2)) +
  xlab(paste("Variable :",as.character(Var1))) +
  ylab(paste("Variable :",as.character(Var2))) +
  labs(title = "independence convergence for Independent variables")+
  geom_point()+
  scale_x_continuous(limits = c(0, 10))+
  scale_y_continuous(limits = c(0, 10))

################################################################################
##########################  handmade independence  #############################
################################################################################

P1 <- std_data_TheArea %>% filter(Var1>=q_mat[Var1])
P1$Var2 <- 0

P2 <- std_data_TheArea %>% filter(Var2>=q_mat[Var2])
P2$Var1 <- 0

################################################################################

Bernoulli_result <- table(sample(c("P1","P2"), dim(std_data_TheArea)[1], TRUE))

sampledP1 <- P1[sample((1:dim(std_data_TheArea)[1]),size = as.numeric(Bernoulli_result[1]),replace = T),]
sampledP2 <- P2[sample((1:dim(std_data_TheArea)[1]),size = as.numeric(Bernoulli_result[2]),replace = T),]
handmade_indep <- rbind(sampledP1,sampledP2)
row.names(handmade_indep)<-1:dim(std_data_TheArea)[1]

handmade_indep$type <- "handmade"
real_indep <- std_data_TheArea
real_indep$type <- "real"  
new_data <- rbind(real_indep,handmade_indep)
###############################################################################3

library(ggplot2)

ggplot(new_data,aes(x=Var1, y=Var2)) +
  xlab(paste("Variable :",as.character(Var1))) +
  ylab(paste("Variable :",as.character(Var2))) +
  labs(title = "independence convergence for Independent variables",color="Type")+
  geom_point(aes(color = as.factor(type)))

ggplot(new_data,aes(x=Var1, y=Var2)) +
  xlab(paste("Variable :",as.character(Var1))) +
  ylab(paste("Variable :",as.character(Var2))) +
  labs(title = "independence convergence for Independent variables",color="Type")+
  geom_point(aes(color = as.factor(type)))+
  scale_x_continuous(limits = c(0, 100))+
  scale_y_continuous(limits = c(0, 100))

ggplot(new_data,aes(x=Var1, y=Var2)) +
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

source("data_simulation_HR.R")

fixed_n <- 10e6
Simulated_data_fix_n <- data_simulation_HR(forest_str_lst,n = fixed_n)

################################################################################
#######################    Standardization   ###################################
################################################################################
p_seq <- c(0,0.9,0.99,0.999,0.9999,0.99999,0.999999)
length(p_seq)


std_data_list <- list()
for (i in 1:length(p_seq)) {
  print(i)
  std_data_list[[i]] <- data2mpareto(Simulated_data_fix_n,p = p_seq[i])
}

################################################################################

eps_vec <- 10^seq(from = 2, to = -4,by = -1)
eps_names <- paste0(rep("eps",length(eps_vec)),c(1:length(eps_vec)),rep("=",length(eps_vec)),eps_vec)
################################################################################
#############################   neighbors   ####################################
################################################################################

prob_mat_list <- list()

Var1 <- df_relation$neighbor[1]
Var2 <- df_relation$neighbor[2]

prob_mat <- matrix(data = NA,nrow = length(p_seq),ncol = length(eps_vec)) 

for (i in 1:length(eps_vec)) {
  print(paste("eps = ", eps_vec[i],":"))
  
  for (j in 1:length(p_seq)) {
    print(c(i,j))
    prod_extreme_area_data <- std_data_list[[j]][which(std_data_list[[j]][,Var1]>eps_vec[i] & std_data_list[[j]][,Var2]>eps_vec[i]),c(Var1,Var2)]
    freq <- dim(prod_extreme_area_data)[1]
    if(is.null(freq)){                         # if the area is empty!
      freq <- 0
    }
    
    prob_mat[j,i] <- freq / dim(std_data_list[[j]])[1]
  }      
}

colnames(prob_mat) <- eps_names
rownames(prob_mat) <- paste("Probability for p = ",as.character(p_seq))
prob_mat_list[[1]] <- prob_mat

################################################################################
##########################   conditional indep   ###############################
################################################################################

Var1 <- df_relation$connected[1]
Var2 <- df_relation$connected[2]

prob_mat <- matrix(data = NA,nrow = length(p_seq),ncol = length(eps_vec)) 

for (i in 1:length(eps_vec)) {
  print(paste("eps = ", eps_vec[i],":"))
  
  for (j in 1:length(p_seq)) {
    print(c(i,j))
    prod_extreme_area_data <- std_data_list[[j]][which(std_data_list[[j]][,Var1]>eps_vec[i] & std_data_list[[j]][,Var2]>eps_vec[i]),c(Var1,Var2)]
    freq <- dim(prod_extreme_area_data)[1]
    if(is.null(freq)){                         # if the area is empty!
      freq <- 0
    }
    
    prob_mat[j,i] <- freq / dim(std_data_list[[j]])[1]
  }      
}

colnames(prob_mat) <- eps_names
rownames(prob_mat) <- paste("Probability for p = ",as.character(p_seq))
prob_mat_list[[2]] <- prob_mat

################################################################################
##############################   independence   ################################
################################################################################

Var1 <- df_relation$unconnected[1]
Var2 <- df_relation$unconnected[2]

prob_mat <- matrix(data = NA,nrow = length(p_seq),ncol = length(eps_vec)) 

for (i in 1:length(eps_vec)) {
  print(paste("eps = ", eps_vec[i],":"))
  
  for (j in 1:length(p_seq)) {
    print(c(i,j))
    prod_extreme_area_data <- std_data_list[[j]][which(std_data_list[[j]][,Var1]>eps_vec[i] & std_data_list[[j]][,Var2]>eps_vec[i]),c(Var1,Var2)]
    freq <- dim(prod_extreme_area_data)[1]
    if(is.null(freq)){                         # if the area is empty!
      freq <- 0
    }
    
    prob_mat[j,i] <- freq / dim(std_data_list[[j]])[1]
  }      
}

colnames(prob_mat) <- eps_names
rownames(prob_mat) <- paste("Probability for p = ",as.character(p_seq))
prob_mat_list[[3]] <- prob_mat

################################################################################
#######################     Convergence Plots   ################################
################################################################################

plots_list <- list()
for (k in 1:length(eps_vec)) {
  prob_df <- matrix(data = 0,nrow = 3*length(p_seq),ncol = 3)
  
  prob_df[,3]<-rep(c("dependent","cond_indep","indep"),each = length(p_seq),times=1)
  
  for (i in 1:3) {
    for (j in 1:length(p_seq)) {
      
      prob_df[(i-1)*length(p_seq)+j,1] <- log10(1-p_seq[j])
      prob_df[(i-1)*length(p_seq)+j,2] <- prob_mat_list[[i]][j,k]
      
    }
  }
  
  prob_df <- data.frame(prob_df)
  colnames(prob_df) <- c("p","prob","type")
  
  library(ggplot2)
  prob_plot <- ggplot(prob_df, aes(x = as.numeric(p), y = as.numeric(prob), colour = type)) + 
    geom_point() + 
    geom_line() +
    ggtitle(paste("Probability convergence n =",fixed_n,"epsilon = ",eps_vec[k]))+
    theme(plot.title = element_text(size = 8))+
    guides(shape = guide_legend(override.aes = list(size = 0.1)))+
    ylab("Probability")+xlab("log(1-p)")+
    scale_x_reverse()
  plots_list[[k]] <- prob_plot
}
library(gridExtra)
do.call(grid.arrange,plots_list)

par(mfrow=c(1,1))

################################################################################
###############   Find equivalent of Inf in pre-asymptotic case ################
################################################################################

p <- 0.9

emp_chi_mat <- emp_chi(Simulated_data, p) ######### I expect emp_chi_multdim() works but emp_chi() works here why?
emp_chi_mat  ### why it's so good it doesn't have decimals????!!!!!

-log(emp_chi_mat)
################################################################################

emp_vario_mat <- emp_vario(Simulated_data, k=NULL, p) ######## what is the good k???????????????????????????
emp_vario_mat                               

emp_vario_mat_copy <- emp_vario_mat
emp_vario_mat_copy[emp_vario_mat_copy>1.8]<-Inf
emp_vario_mat_copy

################################################################################


p <- 0.99

emp_chi_mat <- emp_chi(Simulated_data, p) ######### I expect emp_chi_multdim() works but emp_chi() works here why?
emp_chi_mat  ### why it's so good it doesn't have decimals????!!!!!

-log(emp_chi_mat)
################################################################################

emp_vario_mat <- emp_vario(Simulated_data, k=NULL, p) ######## what is the good k???????????????????????????
emp_vario_mat                               

emp_vario_mat_copy <- emp_vario_mat
emp_vario_mat_copy[emp_vario_mat_copy>2.6]<-Inf
emp_vario_mat_copy

################################################################################
