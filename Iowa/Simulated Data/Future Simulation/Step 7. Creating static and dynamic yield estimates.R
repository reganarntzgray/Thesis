
setwd("C:/Users/Regan/Google Drive/Thesis Presentations and Proposal/Thesis/Data needed to complete empirical work/Data sets for model testing/Iowa")

N <- 99
T <- 58

Dummy <- read.delim("county_region_matrix.txt")

Dummy <- Dummy[,2:9]

Dummy1 <- apply(Dummy,2,as.numeric)

Dummy <- matrix( rep( t(Dummy1) , T ) , ncol = ncol(Dummy1) , byrow = TRUE )

NW <- Dummy[,1]
NC <- Dummy[,2]
NE <- Dummy[,3]
WC <- Dummy[,4]
C <- Dummy[,5]
EC <- Dummy[,6]
SC <- Dummy[,7]
SE <- Dummy[,8]

W <- read.delim("Iowa_W_norm.txt", header=F)
W <- as.matrix(W)

K <- kronecker(diag(T),W)

yield_data <- readRDS("Data_d_iv_iowa")


setwd("C:/Users/Regan/Google Drive/Thesis Presentations and Proposal/Thesis/Data needed to complete empirical work/simulated weather/Iowa")

df <- readRDS("Data_d_iv_iowa_sim_future")

df[,3]<-rep(59,nrow(df))

attach(df)


p_opt_dyn <- read.table("p_opt_dyn")
p_opt_dyn <- p_opt_dyn[1,1]

p_opt_stat <- read.table("p_opt_stat")
p_opt_stat <- p_opt_stat[1,1]

A_dyn <- (diag(N*T)-(p_opt_dyn*K))

A_stat <- (diag(N*T)-(p_opt_stat*K))

A_inv_dyn <- read.table("A_inv_dyn")

A_inv_dyn <- as.matrix(A_inv_dyn)

A_inv_stat <- read.table("A_inv_stat")

A_inv_stat <- as.matrix(A_inv_stat)

Beta_dynamic <- read.table("Beta_dynamic")
Beta_dynamic <- as.matrix(Beta_dynamic)

Beta_static <- read.table("Beta_static")
Beta_static <- as.matrix(Beta_static)

SD_dyn <- read.table("SD_dyn")
SD_dyn <- as.vector(SD_dyn)

SD_stat <- read.table("SD_stat")
SD_stat <- as.vector(SD_stat)

thresholds_dynamic <- read.table("thresholds_dynamic")

threshold_static <- read.table("threshold_static")


####now creating the yields based on simulated data####

T <- 100

k_dynamic <- thresholds_dynamic[1,1]+thresholds_dynamic[2,1]*136



k_static <- threshold_static[1,1]



Dummy <- matrix( rep( t(Dummy1) , T ) , ncol = ncol(Dummy1) , byrow = TRUE )

NW <- Dummy[,1]
NC <- Dummy[,2]
NE <- Dummy[,3]
WC <- Dummy[,4]
C <- Dummy[,5]
EC <- Dummy[,6]
SC <- Dummy[,7]
SE <- Dummy[,8]

K <- kronecker(diag(T),W)

A_dyn <- (diag(N*T)-(p_opt_dyn*K))
A_stat <- (diag(N*T)-(p_opt_stat*K))

const <- rep(1,N*T)
X1_dynamic <- as.matrix(cbind(df[,3:ncol(df)],NW,NC,NE,WC,C,EC,SC,SE))

pcp_pol_sp_d <- (pcp_pol_i-k_dynamic)*(pcp_pol_i>k_dynamic)

X_dynamic <- cbind(X1_dynamic,pcp_pol_sp_d)

Xnew_dynamic <- A_dyn%*%X_dynamic

Ynew_hat_dynamic <- cbind(const,Xnew_dynamic)%*%Beta_dynamic

#generating the error as a random norm mean 0 var with sd equal to the sd of the error in that region for last 10 yrs

set.seed(94251461) #idynf1 in numbers

dyn_sim_error <- matrix(0,N,T)
for(i in 1:N)
{dyn_sim_error[i,]<-rnorm(T,0,SD_dyn[i,1])}

error_dyn_sim <- as.vector(dyn_sim_error)


Y_dynamic <- A_inv_dyn%*%(Ynew_hat_dynamic+error_dyn_sim)






#Static yields

const <- rep(1,N*T)
X1_static <- as.matrix(cbind(df[,3:ncol(df)],NW,NC,NE,WC,C,EC,SC,SE))

pcp_pol_sp_s <- (pcp_pol_i-k_static)*(pcp_pol_i>k_static)

X_static <- cbind(X1_static,pcp_pol_sp_s)

Xnew_static <- A_stat%*%X_static

Ynew_hat_static <- cbind(const,Xnew_static)%*%Beta_static

#generating the error as a random norm mean 0 var with sd equal to the sd of the error in that county for last 10 yrs

set.seed(9192061) #istf1

stat_sim_error <- matrix(0,N,T)
for(i in 1:N)
{stat_sim_error[i,]<-rnorm(T,0,SD_stat[i,1])}

error_stat_sim <- as.vector(stat_sim_error)

Y_static <- A_inv_stat%*%(Ynew_hat_static+error_stat_sim)

#############################

Yield_future_sim_Iowa <- data.frame(county,year,Y_dynamic,Y_static)

colnames(Yield_future_sim_Iowa)<-c("county","year","Y_dynamic","Y_static")

write.table(Yield_future_sim_Iowa,"Yield_future_sim_Iowa")

### we will be using the expected yield for future period given climate change to compare to no cc so saving that now for dyn mod

#exp yield is given by Ynew_hat_dynamic

Y_change <- A_inv_dyn%*%Ynew_hat_dynamic

Exp_Yield_change <- data.frame(county,year,Y_change)

colnames(Exp_Yield_change)<-c("county","year","Y_dynamic_expected")

write.table(Exp_Yield_change,"Exp_Yield_change")





