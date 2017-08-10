setwd("C:/Users/Regan/Google Drive/Thesis Presentations and Proposal/Thesis/Data needed to complete empirical work/Data sets for model testing/Ontario")



N <- 32
T <- 64

Dummy <- diag(N)
Dummy <- matrix( rep( t( Dummy) , T ) , ncol = ncol(Dummy) , byrow = TRUE )



W <- read.delim("C:/Users/Regan/Google Drive/Thesis Presentations and Proposal/Thesis/Code/Ontario/W_ONT_norm.txt",header=F)
W <- as.matrix(W)


K <- kronecker(diag(T),W)

eig <- eigen(W, symmetric = FALSE, only.values = TRUE)
lambda <- eig$values

rhomin <- (1/min(lambda))
rhomax <- (1/max(lambda))

p_bound <- seq(rhomin,rhomax,.1)


setwd("C:/Users/Regan/Google Drive/Thesis Presentations and Proposal/Thesis/Data needed to complete empirical work/simulated weather/Ontario")


#Dynamic thresholds and beta

thresholds_dynamic <- read.table("thresholds_dynamic")


Beta_dynamic <- read.table("Beta_dynamic")
Beta_dynamic <- as.matrix(Beta_dynamic)

#Static thresholds and beta

threshold_static <- read.table("threshold_static")

Beta_static <- read.table("Beta_static")
Beta_static <-  as.matrix(Beta_static)

#determining variance of the error in the dynamic model

setwd("C:/Users/Regan/Google Drive/Thesis Presentations and Proposal/Thesis/Data needed to complete empirical work/Data sets for model testing/Ontario")
yield_data <- readRDS("Data_d_iii_ont")

Y <- yield_data[,3]


k_dynamic <- thresholds_dynamic[1,1]+thresholds_dynamic[2,1]*yield_data[,4]

Constant<- rep(1,N*T)

df <- yield_data

X1 <- as.matrix(cbind(Constant,df[,4:ncol(df)],Dummy[,1:31]))

pcp_july_sp <- (yield_data$mean_pcp_july-k_dynamic)*(yield_data$mean_pcp_july>k_dynamic)

X <- cbind(X1,pcp_july_sp)

log_L <- function(p)
{A <- (diag(N*T)-p*K)
log_det_I_pW <- log(1-p*lambda)
log_detA <- T*sum(log_det_I_pW)
Beta <- solve(t(A%*%X)%*%(A%*%X))%*%t(A%*%X)%*%(A%*%Y)
U <- Y-X%*%Beta
sigma_sq <- (1/N*T)*(t(U)%*%t(A)%*%A%*%U)
log_L <- (-(N*T)/2)*log(2*pi*sigma_sq)+log_detA-(1/(2*sigma_sq))*t(U)%*%t(A)%*%A%*%U
return(-log_L)}

p_opt <- optim(mean(p_bound),(log_L),method="Brent",lower=rhomin, upper=rhomax)$par

A <- (diag(N*T)-p_opt*K)

X11 <- as.matrix(cbind(df[,4:ncol(df)],Dummy[,1:31]))

X_1 <- cbind(X11,pcp_july_sp)




Ynew <- A%*%Y
Xnew <- A%*%X_1

mod_dyn <- lm(Ynew~Xnew)

b_dyn <- summary(mod_dyn)$coef[,1]

Xnew_dyn <- cbind(Constant,Xnew)

e_dyn <- Ynew-Xnew_dyn%*%b_dyn

SSE_dyn <- sum(e_dyn^2)

######################3

k_static <- threshold_static[1,1]

pcp_july_sp <- (yield_data$mean_pcp_july-k_static)*(yield_data$mean_pcp_july>k_static)

X <- cbind(X1,pcp_july_sp)

log_L <- function(p)
{A <- (diag(N*T)-p*K)
log_det_I_pW <- log(1-p*lambda)
log_detA <- T*sum(log_det_I_pW)
Beta <- solve(t(A%*%X)%*%(A%*%X))%*%t(A%*%X)%*%(A%*%Y)
U <- Y-X%*%Beta
sigma_sq <- (1/N*T)*(t(U)%*%t(A)%*%A%*%U)
log_L <- (-(N*T)/2)*log(2*pi*sigma_sq)+log_detA-(1/(2*sigma_sq))*t(U)%*%t(A)%*%A%*%U
return(-log_L)}

p_opt <- optim(mean(p_bound),(log_L),method="Brent",lower=rhomin, upper=rhomax)$par

A <- (diag(N*T)-p_opt*K)



X11 <- as.matrix(cbind(df[,4:ncol(df)],Dummy[,1:31]))

X_1 <- cbind(X11,pcp_july_sp)




Ynew <- A%*%Y
Xnew <- A%*%X_1


mod_stat <- lm(Ynew~Xnew)

b_stat <- summary(mod_stat)$coef[,1]

Xnew_stat <- cbind(Constant,Xnew)

e_stat <- Ynew-Xnew_stat%*%b_stat

SSE_stat <- sum(e_stat^2)







install.packages("sandwich")
library(sandwich)

# Adjust standard errors
cov_dyn         <- vcovHC(mod_dyn, type = "HC3")
robust_dyn    <- sqrt(diag(cov_dyn))

cov_stat        <- vcovHC(mod_stat, type = "HC3")
robust_stat    <- sqrt(diag(cov_stat))

install.packages("lmtest")
library(lmtest)

wald_results_dyn <- waldtest(mod_dyn, vcov = cov_dyn)
wald_results_stat <- waldtest(mod_stat, vcov = cov_stat)


install.packages("stargazer")
library(stargazer)

count <- as.character(c(1:31))

stargazer(mod_dyn,mod_stat,title = "Ontario: Optimal Threshold Dynamic and Static Yield Models", column.labels = c("Dynamic", "Static"),se=list(robust_dyn,robust_stat), covariate.labels = c("T","Trend","GDD","HDD","VPD$_{May}$","VPD$_{June}$","VPD$_{July}$","VPD$_{Aug}$","PCP$_{May}$", "PCP$_{June}$","PCP$_{July}$","PCP$_{Aug}$",count,"PCP$_{{July}_{sp}}$"),align=TRUE)

exp((AIC(mod_stat)-AIC(mod_dyn))/(-2))

AIC(mod_dyn)
AIC(mod_stat)

#calculating the F stat to compare these two models

#number of additional parameters is q=1, total vars in unrestricted model is k=19 (not including constant)

q=1
k=length(b_dyn)+1 #plus to account for 2 threshold parameters which aren't in b_dyn minus 1 for the constant term
n=length(Y)

F <- ((SSE_stat-SSE_dyn)/q)/(SSE_dyn/(n-k-1))

> F
[1] 54.65258



####DONT FORGET TO MODIFY THE F STATISTICS IN THE TABLE and TAKE OUT THE LOCATION VARIABLES and put constant parameters in scale####