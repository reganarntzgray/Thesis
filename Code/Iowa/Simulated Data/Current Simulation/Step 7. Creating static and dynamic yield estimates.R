
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

pcp_pol_year <- NULL
for (i in 1:T)
{pcp_pol_year[i]<- mean(yield_data$pcp_pol_i[((i-1)*N+1):(i*N)])}
setwd("C:/Users/Regan/Google Drive/Thesis Presentations and Proposal/Thesis/Data needed to complete empirical work/simulated weather/Iowa")

df <- readRDS("Data_d_iv_iowa_sim_current")
attach(df)

p_opt_dyn <- 0.9419123

write.table(p_opt_dyn,"p_opt_dyn")

p_opt_stat <- 0.94179

write.table(p_opt_stat,"p_opt_stat")

A_dyn <- (diag(N*T)-(p_opt_dyn*K))

A_stat <- (diag(N*T)-(p_opt_stat*K))


#Dynamic thresholds and beta

thresholds_dynamic <- read.table("thresholds_dynamic")



#Static thresholds and beta

threshold_static <- read.table("threshold_static")

#solving for the Betas

#dynamic


#determining variance of the error in the dynamic model

k_dynamic <- thresholds_dynamic[1,1]+thresholds_dynamic[2,1]*yield_data[,4]

const <- rep(1,N*T)

pcp_pol_sp_d <- (yield_data$pcp_pol_i-k_dynamic)*(yield_data$pcp_pol_i>k_dynamic)

year_num <- seq(1,nrow(yield_data),N)
over_threshold <- NULL
for (i in 1:length(year_num))
{over_threshold[i] <- sum(pcp_pol_sp_d[(year_num[i]:(year_num[i]+N-1))]>0)}


y <- yield_data[,3]

Ynew_dyn <- A_dyn%*%y

X11 <-  as.matrix(cbind(yield_data[,4:ncol(yield_data)],NW,NC,NE,WC,C,EC,SC,SE))

X_1_d <- cbind(X11,pcp_pol_sp_d)

Xnew_dynamic <- A_dyn%*%X_1_d


mod_dyn <- lm(Ynew_dyn~Xnew_dynamic)

Beta_dynamic <- summary(mod_dyn)$coef[,1]

write.table(Beta_dynamic,"C:/Users/Regan/Google Drive/Thesis Presentations and Proposal/Thesis/Data needed to complete empirical work/simulated weather/Iowa/Beta_dynamic")

e_dynamic <- Ynew_dyn-cbind(const,Xnew_dynamic)%*%Beta_dynamic


#coming up with the approximate variance in standard error for each region in past 10 years so it can be added on
#58 years total - we want 49-58

Dummy2 <- read.delim("C:/Users/Regan/Google Drive/Thesis Presentations and Proposal/Thesis/Data needed to complete empirical work/Data sets for model testing/Iowa/county_region_matrix_full.txt")

Dummy2 <- Dummy2[,2:10]

Dummy12 <- apply(Dummy2,2,as.numeric)

Dummy2 <- matrix( rep( t(Dummy12) , 10 ) , ncol = ncol(Dummy12) , byrow = TRUE )


e_dynamic_rec <- e_dynamic[(99*48+1):length(e_dynamic)]


sd_e_dyn_region <- NULL

for(i in 1:ncol(Dummy2))
{regioni1 <- e_dynamic_rec*Dummy2[,i]
regioni <- regioni1[regioni1!=0]
sd_e_dyn_region[i] <- sd(regioni)}

Dummy3 <- Dummy2[1:99,]
SD_dyn_mat <- matrix(0,N,ncol(Dummy2))
for(i in 1:length(sd_e_dyn_region))
{SD_dyn_mat[,i]<- sd_e_dyn_region[i]*Dummy3[,i]}

SD_dyn <- rowSums(SD_dyn_mat)

write.table(SD_dyn,"SD_dyn")

##getting static model error##

k_static <- threshold_static[1,1]

const <- rep(1,N*T)

pcp_pol_sp_s <- (yield_data$pcp_pol_i-k_static)*(yield_data$pcp_pol_i>k_static)

year_num <- seq(1,nrow(yield_data),N)
over_threshold_stat <- NULL
for (i in 1:length(year_num))
{over_threshold_stat[i] <- sum(pcp_pol_sp_s[(year_num[i]:(year_num[i]+N-1))]>0)}


y <- yield_data[,3]

Ynew_stat <- A_stat%*%y

X11 <-  as.matrix(cbind(yield_data[,4:ncol(yield_data)],NW,NC,NE,WC,C,EC,SC,SE))

X_1_s <- cbind(X11,pcp_pol_sp_s)

Xnew_static <- A_stat%*%X_1_s


mod_stat <- lm(Ynew_stat~Xnew_static)

Beta_static <- summary(mod_stat)$coef[,1]

write.table(Beta_static,"C:/Users/Regan/Google Drive/Thesis Presentations and Proposal/Thesis/Data needed to complete empirical work/simulated weather/Iowa/Beta_static")


e_static <- Ynew_stat-cbind(const,Xnew_static)%*%Beta_static

#coming up with the approximate variance in standard error for each region in past 10 years so it can be added on
#58 years total - we want 49-58



e_static_rec <- e_static[(99*48+1):length(e_static)]


sd_e_stat_region <- NULL

for(i in 1:ncol(Dummy2))
{regioni1 <- e_static_rec*Dummy2[,i]
regioni <- regioni1[regioni1!=0]
sd_e_stat_region[i] <- sd(regioni)}

SD_stat_mat <- matrix(0,N,ncol(Dummy2))
for(i in 1:length(sd_e_stat_region))
{SD_stat_mat[,i]<- sd_e_stat_region[i]*Dummy3[,i]}

SD_stat <- rowSums(SD_stat_mat)

write.table(SD_stat,"SD_stat")

####now creating the yields based on simulated data####

T <- 100

k_dynamic <- thresholds_dynamic[1,1]+thresholds_dynamic[2,1]*df[,3]


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

set.seed(942514) #idyn in numbers

dyn_sim_error <- matrix(0,N,T)
for(i in 1:N)
{dyn_sim_error[i,]<-rnorm(T,0,SD_dyn[i])}

error_dyn_sim <- as.vector(dyn_sim_error)


A_inv_dyn <- solve(A_dyn)
write.table(A_inv_dyn,"C:/Users/Regan/Google Drive/Thesis Presentations and Proposal/Thesis/Data needed to complete empirical work/simulated weather/Iowa/A_inv_dyn")

Y_dynamic <- A_inv_dyn%*%(Ynew_hat_dynamic+error_dyn_sim)






#Static yields

const <- rep(1,N*T)
X1_static <- as.matrix(cbind(df[,3:ncol(df)],NW,NC,NE,WC,C,EC,SC,SE))

pcp_pol_sp_s <- (pcp_pol_i-k_static)*(pcp_pol_i>k_static)

X_static <- cbind(X1_static,pcp_pol_sp_s)

Xnew_static <- A_stat%*%X_static

Ynew_hat_static <- cbind(const,Xnew_static)%*%Beta_static

#generating the error as a random norm mean 0 var with sd equal to the sd of the error in that county for last 10 yrs

set.seed(91920120) #istat

stat_sim_error <- matrix(0,N,T)
for(i in 1:N)
{stat_sim_error[i,]<-rnorm(T,0,SD_stat[i])}

error_stat_sim <- as.vector(stat_sim_error)


A_inv_stat <- solve(A_stat)
write.table(A_inv_stat,"C:/Users/Regan/Google Drive/Thesis Presentations and Proposal/Thesis/Data needed to complete empirical work/simulated weather/Iowa/A_inv_stat")

Y_static <- A_inv_stat%*%(Ynew_hat_static+error_stat_sim)

#############################

Yield_current_sim_Iowa <- data.frame(county,year,Y_dynamic,Y_static)

colnames(Yield_current_sim_Iowa)<-c("county","year","Y_dynamic","Y_static")

write.table(Yield_current_sim_Iowa,"Yield_current_sim_Iowa")

### we will be using the expected yield for current period given no climate change to compare to cc so saving that now for dyn mod

#exp yield is given by Ynew_hat_dynamic

Y_nochange <- A_inv_dyn%*%Ynew_hat_dynamic

Exp_Yield_nochange <- data.frame(county,year,Y_nochange)

colnames(Exp_Yield_nochange)<-c("county","year","Y_dynamic_expected")

write.table(Exp_Yield_nochange,"Exp_Yield_nochange")





