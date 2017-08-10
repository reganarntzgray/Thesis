setwd("C:/Users/Regan/Google Drive/Thesis Presentations and Proposal/Thesis/Data needed to complete empirical work/Data sets for model testing/Ontario")

N <- 32
T <-100


Dummy <- diag(N)
Dummy <- matrix( rep( t( Dummy) , T ) , ncol = ncol(Dummy) , byrow = TRUE )

W <- read.delim("W_ONT_norm.txt", header=F)
W <- as.matrix(W)

K <- kronecker(diag(T),W)

setwd("C:/Users/Regan/Google Drive/Thesis Presentations and Proposal/Thesis/Data needed to complete empirical work/simulated weather/Ontario")

df <- readRDS("Data_d_iii_ont_sim_future")

attach(df)

p_opt <- read.table("p_opt")
p_opt <- p_opt[1,1]

A <- (diag(N*T)-(p_opt*K))

#Dynamic thresholds and beta

thresholds_dynamic <- read.table("thresholds_dynamic")


Beta_dynamic <- read.table("Beta_dynamic")
Beta_dynamic <- as.matrix(Beta_dynamic)

#Static thresholds and beta

threshold_static <- read.table("threshold_static")

Beta_static <- read.table("Beta_static")
Beta_static <-  as.matrix(Beta_static)

df[,3]<- rep(65, nrow(df))
df[,4]<- rep(15,nrow(df))

k_dynamic <- thresholds_dynamic[1,1]+thresholds_dynamic[2,1]*df[,3]


const <- rep(1,N*T)
X1_dynamic <- as.matrix(cbind(const,df[,3:ncol(df)],Dummy[,1:31]))

pcp_july_sp_d <- (mean_pcp_july-k_dynamic)*(mean_pcp_july>k_dynamic)

X_dynamic <- cbind(X1_dynamic,pcp_july_sp_d)

Xnew_dynamic <- A%*%X_dynamic

Ynew_hat_dynamic <- Xnew_dynamic%*%Beta_dynamic

A_inv <- solve(A)

Y_dynamic <- A_inv%*%(Ynew_hat_dynamic)

Yield_future_sim_Ont <- data.frame(county,year,Y_dynamic)

colnames(Yield_future_sim_Ont)<-c("county","year","Y_dynamic")

write.table(Yield_future_sim_Ont,"Yield_future_sim_Ontario")

