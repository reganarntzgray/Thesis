
setwd("C:/Users/Regan/Google Drive/Thesis Presentations and Proposal/Thesis/Data needed to complete empirical work/Data sets for model testing/Ontario")

N <- 32
T <- 64


Dummy <- diag(N)
Dummy <- matrix( rep( t( Dummy) , T ) , ncol = ncol(Dummy) , byrow = TRUE )

W <- read.delim("W_ONT_norm.txt", header=F)
W <- as.matrix(W)

K <- kronecker(diag(T),W)

yield_data <- readRDS("Data_d_iii_ont")

pcp_july_year <- NULL
for (i in 1:T)
{pcp_july_year[i]<- mean(yield_data$mean_pcp_july[((i-1)*N+1):(i*N)])}



setwd("C:/Users/Regan/Google Drive/Thesis Presentations and Proposal/Thesis/Data needed to complete empirical work/simulated weather/Ontario")

df <- readRDS("Data_d_iii_ont_sim_current")


df[,4]<- rep(15,nrow(df))

counties <- c("brant","bruce","chatham-kent","dufferin","elgin","essex","grey","haldimand-norfolk",
              "halton","hamilton","hastings","huron","kawartha lakes","lambton","lanark",
              "leeds-grenville","lennox-addington","middlesex","niagara","northumberland",
              "ottawa","oxford","peel","perth","prescott-russell","prince edward","renfrew",
              "simcoe","stormont-dundas-glengarry","waterloo","wellington","york")


df <- df[df$county %in% counties,]


attach(df)

p_opt_dyn <- 0.7006246

write.table(p_opt_dyn,"p_opt_dyn")

p_opt_stat <- 0.7080934

write.table(p_opt_stat,"p_opt_stat")

A_dyn <- (diag(N*T)-(p_opt_dyn*K))

A_stat <- (diag(N*T)-(p_opt_stat*K))


#Dynamic thresholds and beta

thresholds_dynamic <- read.table("thresholds_dynamic")



#Static thresholds and beta

threshold_static <- read.table("threshold_static")


#determining variance of the error in the dynamic model


k_dynamic <- thresholds_dynamic[1,1]+thresholds_dynamic[2,1]*yield_data[,4]

const <- rep(1,N*T)
X1 <- as.matrix(cbind(yield_data[,4:ncol(yield_data)],Dummy[,1:31]))

pcp_july_sp_d <- (yield_data$mean_pcp_july-k_dynamic)*(yield_data$mean_pcp_july>k_dynamic)

X_dynamic <- cbind(X1,pcp_july_sp_d)

Xnew_dynamic <- A_dyn%*%X_dynamic

y <- yield_data[,3]

Ynew_dyn <- A_dyn%*%y

mod_dyn <- lm(Ynew_dyn~Xnew_dynamic)

Beta_dynamic <- summary(mod_dyn)$coef[,1]

write.table(Beta_dynamic,"C:/Users/Regan/Google Drive/Thesis Presentations and Proposal/Thesis/Data needed to complete empirical work/simulated weather/Ontario/Beta_dynamic")

e_dynamic <- Ynew_dyn-cbind(const,Xnew_dynamic)%*%Beta_dynamic
#> mean(e_dynamic)
[#1] 5.844333e-15

#getting sd of error by county over past 10 years

sd_e_dyn_county <- NULL
county_num <- seq(1,length(e_dynamic),N)
for(i in 1:N)
{countyi <- e_dynamic[county_num[55:64]+(i-1)]
sd_e_dyn_county[i] <- sd(countyi)}

write.table(sd_e_dyn_county,"C:/Users/Regan/Google Drive/Thesis Presentations and Proposal/Thesis/Data needed to complete empirical work/simulated weather/Ontario/sd_e_dyn_county")

#> sd_e_dyn_county
#[1]  8.620503  4.035720  6.559197 12.699073  7.307062  8.899786  4.198572  8.779419 14.023564  7.461287 12.682783
#[12]  4.118700 11.807582  9.541821 14.009431  9.477022  8.840149  7.200398  8.538447  7.971911  7.974336  5.893993
#[23] 12.974574  5.650564  7.969472 10.518919  7.009316  8.435254  7.072114  5.271710  4.389497  5.339090

##getting static model error##

k_static <- threshold_static[1,1]

const <- rep(1,N*T)
X1 <- as.matrix(cbind(yield_data[,4:ncol(yield_data)],Dummy[,1:31]))

pcp_july_sp_s <- (yield_data$mean_pcp_july-k_static)*(yield_data$mean_pcp_july>k_static)

X_static <- cbind(X1,pcp_july_sp_s)

Xnew_static <- A_stat%*%X_static

y <- yield_data[,3]

Ynew_stat <- A_stat%*%y

mod_stat <- lm(Ynew_stat~Xnew_static)

Beta_static <- summary(mod_stat)$coef[,1]

write.table(Beta_static,"C:/Users/Regan/Google Drive/Thesis Presentations and Proposal/Thesis/Data needed to complete empirical work/simulated weather/Ontario/Beta_static")

e_static <- Ynew_stat-cbind(const,Xnew_static)%*%Beta_static

sd_e_stat_county <- NULL
county_num <- seq(1,length(e_static),N)
for(i in 1:N)
{countyi <- e_static[county_num[55:64]+(i-1)]
sd_e_stat_county[i] <- sd(countyi)}

write.table(sd_e_stat_county,"C:/Users/Regan/Google Drive/Thesis Presentations and Proposal/Thesis/Data needed to complete empirical work/simulated weather/Ontario/sd_e_stat_county")

#> sd_e_stat_county
#[1]  8.748204  5.024687  5.397777 11.940867  7.682012  8.615304  3.799257  9.654169 15.267444  7.315407 12.283020
#[12]  5.027111 12.247353  9.255849 15.216090  8.800138  9.417154  7.357094  9.133839  7.716038  9.045991  5.839204
#[23] 12.473539  5.384361  8.094274  9.949530  7.965408  7.508913  5.562024  5.316847  5.018698  5.152232


####now creating the yields based on simulated data####

T <- 100


Dummy <- diag(N)
Dummy <- matrix( rep( t( Dummy) , T ) , ncol = ncol(Dummy) , byrow = TRUE )


K <- kronecker(diag(T),W)

A_dyn <- (diag(N*T)-(p_opt_dyn*K))

A_stat <- (diag(N*T)-(p_opt_stat*K))



k_dynamic <- thresholds_dynamic[1,1]+thresholds_dynamic[2,1]*df[,3]


const <- rep(1,N*T)
X1_dynamic <- as.matrix(cbind(df[,3:ncol(df)],Dummy[,1:31]))

pcp_july_sp_d <- (mean_pcp_july-k_dynamic)*(mean_pcp_july>k_dynamic)

X_dynamic <- cbind(X1_dynamic,pcp_july_sp_d)

Xnew_dynamic <- A_dyn%*%X_dynamic

Ynew_hat_dynamic <- cbind(const,Xnew_dynamic)%*%Beta_dynamic

set.seed(1542514) #odyn
e_dyn_sim <- matrix(0,N,T)
for(i in 1:N)
{e_dyn_sim[i,]<- rnorm(T,0,sd_e_dyn_county[i])}
e_dynamic_sim <- as.vector(e_dyn_sim)

#> rowSums(e_dyn_sim)
#[1] -98.861704  34.406165 -66.164335  27.237415 -20.122032  77.289276   9.905577 103.153408 -48.058871  36.828385
#[11] 318.258320 -17.054811   6.296716 -64.815138 194.815368 -92.284763 -70.510324  42.131014  16.250050  -5.406194
#[21]  33.880480  58.178457  47.154408  45.092880 -63.406757  -9.560362 -75.370662 -76.901514 -39.469575 -52.541314
#[31] -18.735038 -84.756280

A_inv_dyn <- solve(A_dyn)
write.table(A_inv_dyn,"C:/Users/Regan/Google Drive/Thesis Presentations and Proposal/Thesis/Data needed to complete empirical work/simulated weather/Ontario/A_inv_dyn")

Y_new_error <- Ynew_hat_dynamic+e_dynamic_sim

Y_dynamic <- A_inv_dyn%*%(Ynew_hat_dynamic+e_dynamic_sim)

#> mean(Y_dynamic)
#[1] 150.8667

#Static yields

const <- rep(1,N*T)
X1_static <- as.matrix(cbind(df[,3:ncol(df)],Dummy[,1:31]))

pcp_july_sp_s <- (mean_pcp_july-k_static)*(mean_pcp_july>k_static)

X_static <- cbind(X1_static,pcp_july_sp_s)

Xnew_static <- A_stat%*%X_static

Ynew_hat_static <- cbind(const,Xnew_static)%*%Beta_static


set.seed(151920120)  #ostat
e_stat_sim <- matrix(0,N,T)
for(i in 1:N)
{e_stat_sim[i,]<- rnorm(T,0,sd_e_stat_county[i])}
e_static_sim <- as.vector(e_stat_sim)

A_inv_stat <- solve(A_stat)
write.table(A_inv_stat,"C:/Users/Regan/Google Drive/Thesis Presentations and Proposal/Thesis/Data needed to complete empirical work/simulated weather/Ontario/A_inv_stat")

Ynew_stat_e <- Ynew_hat_static+e_static_sim
Ynew_dyn_e <- Ynew_hat_dynamic+e_dynamic_sim



Y_static <- A_inv_stat%*%(Ynew_hat_static+e_static_sim)

sd_dyn <- NULL
for(i in 1:N)
{county_yield <- Y_dynamic[seq(i,length(Y_dynamic),N)]
sd_dyn[i]<-sd(county_yield)}

sd_stat <- NULL
for(i in 1:N)
{county_yield <- Y_static[seq(i,length(Y_static),N)]
sd_stat[i]<-sd(county_yield)}


SD_yields <- data.frame(unique(df$county),sd_stat,sd_dyn)

colnames(SD_yields)<- c("county","sd_stat","sd_dyn")

write.table(SD_yields,"SD_yields_cur_Ont")

Yield_current_sim_Ontario <- data.frame(county,year,Y_dynamic,Y_static)

colnames(Yield_current_sim_Ontario)<-c("county","year","Y_dynamic","Y_static")

write.table(Yield_current_sim_Ontario,"Yield_current_sim_Ontario")



##future yield with current weather, current trend

####now creating the yields based on simulated data####


Y_nochange <- A_inv_dyn%*%(Ynew_hat_dynamic)

Yield_future_nochange_sim_Ontario <- data.frame(county,year,Y_nochange)

colnames(Yield_future_nochange_sim_Ontario)<-c("county","year","Y_dynamic")

write.table(Yield_future_nochange_sim_Ontario,"Yield_future_nochange_sim_Ontario")
