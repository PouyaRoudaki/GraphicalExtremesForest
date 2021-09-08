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

### Quantiles

source("quantile_generator.R")
q_mat <- quantile_generator(Simulated_data,p=0.9)

################################################################################
#############################   neighbors   #####################################
################################################################################

Var1 <- df_relation$neighbor[1]
Var2 <- df_relation$neighbor[2]

p1 <- plot(Simulated_data[which(Simulated_data[,Var1]>q_mat[Var1] | Simulated_data[,Var2]>q_mat[Var2]), Var1],
     Simulated_data[which(Simulated_data[,Var1]>q_mat[Var1] | Simulated_data[,Var2]>q_mat[Var2]), Var2],
     xlab = paste("Variable :",as.character(Var1)),
     ylab = paste("Variable :",as.character(Var2)),
     main = "independence convergence_neighbors")


p2 <- plot(Simulated_data[which(Simulated_data[,Var1]>q_mat[Var1] | Simulated_data[,Var2]>q_mat[Var2]), Var1],
     Simulated_data[which(Simulated_data[,Var1]>q_mat[Var1] | Simulated_data[,Var2]>q_mat[Var2]), Var2],
     xlab = paste("Variable :",as.character(Var1)),
     ylab = paste("Variable :",as.character(Var2)),
     main = "independence convergence_neighbors",
     xlim = c(1,100),ylim = c(1,100))


################################################################################
##########################   conditional indep   ###############################
################################################################################

Var1 <- df_relation$connected[1]
Var2 <- df_relation$connected[2]

p3 <- plot(Simulated_data[which(Simulated_data[,Var1]>q_mat[Var1] | Simulated_data[,Var2]>q_mat[Var2]), Var1],
           Simulated_data[which(Simulated_data[,Var1]>q_mat[Var1] | Simulated_data[,Var2]>q_mat[Var2]), Var2],
           xlab = paste("Variable :",as.character(Var1)),
           ylab = paste("Variable :",as.character(Var2)),
           main = "independence convergence_connecteds not neighbors")


p4 <- plot(Simulated_data[which(Simulated_data[,Var1]>q_mat[Var1] | Simulated_data[,Var2]>q_mat[Var2]), Var1],
           Simulated_data[which(Simulated_data[,Var1]>q_mat[Var1] | Simulated_data[,Var2]>q_mat[Var2]), Var2],
           xlab = paste("Variable :",as.character(Var1)),
           ylab = paste("Variable :",as.character(Var2)),
           main = "independence convergence_connecteds not neighbors",
           xlim = c(1,100),ylim = c(1,100))


################################################################################
##############################   independence   ################################
################################################################################

Var1 <- df_relation$unconnected[1]
Var2 <- df_relation$unconnected[2]

p5 <- plot(Simulated_data[which(Simulated_data[,Var1]>q_mat[Var1] | Simulated_data[,Var2]>q_mat[Var2]), Var1],
           Simulated_data[which(Simulated_data[,Var1]>q_mat[Var1] | Simulated_data[,Var2]>q_mat[Var2]), Var2],
           xlab = paste("Variable :",as.character(Var1)),
           ylab = paste("Variable :",as.character(Var2)),
           main = "independence convergence_unconnected")


p6 <- plot(Simulated_data[which(Simulated_data[,Var1]>q_mat[Var1] | Simulated_data[,Var2]>q_mat[Var2]), Var1],
           Simulated_data[which(Simulated_data[,Var1]>q_mat[Var1] | Simulated_data[,Var2]>q_mat[Var2]), Var2],
           xlab = paste("Variable :",as.character(Var1)),
           ylab = paste("Variable :",as.character(Var2)),
           main = "independence convergence_unconnected",
           xlim = c(1,100),ylim = c(1,100))

################################################################################
##########################  handmade independence  #############################
################################################################################





################################################################################
######################## Convergence prob limit ################################
################################################################################

# n -> Inf : n in c(100,1000,10000,100000,1000000)

power_seq <- seq(from = 2 , to = 7 , by =0.5)
n_seq <- floor(10^power_seq)
length(n_seq)
################################################################################
########################   data Simulation   ###################################
################################################################################

source("data_simulation_HR.R")
Simulated_data_list <- list()
for (i in 1:length(n_seq)) {
        print(i)
        Simulated_data_list[[i]] <- data_simulation_HR(forest_str_lst,n = n_seq[i])
}

################################################################################
#############################  quantiles   #####################################
################################################################################

d <- dim(Simulated_data_list[[1]])[2]
q_m <- matrix(nrow = d,ncol = length(n_seq))
p <- 0.9

for (i in 1:length(n_seq)) {
        q_m[,i] <- quantile_generator(Simulated_data_list[[i]],p)
}

colnames(q_m) <- as.character(n_seq)

eps1 <- runif(1,0,floor(min(q_m)))                          # To make sure that epsilon 1 is smaller than q_{p}
eps2 <- runif(1,10*ceiling(max(q_m)), 20*ceiling(max(q_m)))       # To make sure that epsilon 2 is larger than q_{p}
eps_vec <- c(eps1,eps2)
################################################################################
#############################   neighbors   #####################################
################################################################################


prob_mat_list <- list()

Var1 <- df_relation$neighbor[1]
Var2 <- df_relation$neighbor[2]

prob_mat <- matrix(data = NA,nrow = length(n_seq),ncol = 2) 

for (i in 1:length(eps_vec)) {
        print(paste("eps = ", eps_vec[i],":"))
        
        for (j in 1:length(n_seq)) {
                print(c(i,j))
                prod_extreme_area_data <- Simulated_data_list[[j]][which(Simulated_data_list[[j]][,Var1]>eps_vec[i] & Simulated_data_list[[j]][,Var2]>eps_vec[i]),c(Var1,Var2)]
                freq <- dim(prod_extreme_area_data)[1]
                if(is.null(freq)){                         # if the area is empty!
                        freq <- 0
                }
               
                prob_mat[j,i] <- freq / n_seq[j]
        }      
}
  
colnames(prob_mat) <- c(paste("eps1:",round(eps1,2)),paste("eps2:",round(eps2,2)))
rownames(prob_mat) <- paste("Probability for log(n) = ",as.character(power_seq))
prob_mat_list[[1]] <- prob_mat

################################################################################
##########################   conditional indep   ###############################
################################################################################

Var1 <- df_relation$connected[1]
Var2 <- df_relation$connected[2]

prob_mat <- matrix(data = NA,nrow = length(n_seq),ncol = 2) 

for (i in 1:length(eps_vec)) {
        print(paste("eps = ", eps_vec[i],":"))
        
        for (j in 1:length(n_seq)) {
                print(c(i,j))
                prod_extreme_area_data <- Simulated_data_list[[j]][which(Simulated_data_list[[j]][,Var1]>eps_vec[i] & Simulated_data_list[[j]][,Var2]>eps_vec[i]),c(Var1,Var2)]
                freq <- dim(prod_extreme_area_data)[1]
                if(is.null(freq)){                         # if the area is empty!
                        freq <- 0
                }
                
                prob_mat[j,i] <- freq / n_seq[j]
        }      
}

colnames(prob_mat) <- c(paste("eps1:",round(eps1,2)),paste("eps2:",round(eps2,2)))
rownames(prob_mat) <- paste("Probability for log(n) = ",as.character(power_seq))
prob_mat_list[[2]] <- prob_mat

################################################################################
##############################   independence   ################################
################################################################################

Var1 <- df_relation$unconnected[1]
Var2 <- df_relation$unconnected[2]

prob_mat <- matrix(data = NA,nrow = length(n_seq),ncol = 2) 

for (i in 1:length(eps_vec)) {
        print(paste("eps = ", eps_vec[i],":"))
        
        for (j in 1:length(n_seq)) {
                print(c(i,j))
                prod_extreme_area_data <- Simulated_data_list[[j]][which(Simulated_data_list[[j]][,Var1] > eps_vec[i] & Simulated_data_list[[j]][,Var2]>eps_vec[i]),c(Var1,Var2)]
                freq <- dim(prod_extreme_area_data)[1]
                if(is.null(freq)){                         # if the area is empty!
                        freq <- 0
                }
                
                prob_mat[j,i] <- freq / n_seq[j]
        }      
}

colnames(prob_mat) <- c(paste("eps1:",round(eps1,2)),paste("eps2:",round(eps2,2)))
rownames(prob_mat) <- paste("Probability for log(n) = ",as.character(power_seq))
prob_mat_list[[3]] <- prob_mat
################################################################################

print(prob_mat_list)

prob_df <- matrix(data = 0,nrow = 3*length(n_seq),ncol = 3)

prob_df[,3]<-rep(c("dependent","cond_indep","indep"),each = length(n_seq),times=1)

for (i in 1:3) {
  for (j in 1:length(n_seq)) {
    
    prob_df[(i-1)*length(n_seq)+j,1] <- power_seq[j]
    prob_df[(i-1)*length(n_seq)+j,2] <- prob_mat_list[[i]][j,2]
    
  }
}

prob_df <- data.frame(prob_df)
colnames(prob_df) <- c("n","prob","type")

library(ggplot2)
prob_plot <- ggplot(prob_df, aes(x = as.numeric(n), y = as.numeric(prob), colour = type)) + 
  geom_point() + 
  geom_line() +
  ggtitle(paste("Probability convergence p =",p))+
  ylab("Probability")+xlab("log n")
prob_plot
