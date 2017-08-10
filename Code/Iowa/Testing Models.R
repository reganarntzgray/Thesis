setwd("C:/Users/Regan/Google Drive/Thesis Presentations and Proposal/Thesis")

N <- 99
T <- 58

Dummy <- read.delim("Data needed to complete empirical work/Data sets for model testing/Iowa/county_region_matrix.txt")

Dummy <- Dummy[,2:9]

Dummy <- apply(Dummy,2,as.numeric)

Dummy <- matrix( rep( t(Dummy) , T ) , ncol = ncol(Dummy) , byrow = TRUE )

NW <- Dummy[,1]
NC <- Dummy[,2]
NE <- Dummy[,3]
WC <- Dummy[,4]
C <- Dummy[,5]
EC <- Dummy[,6]
SW <- Dummy[,7]
SC <- Dummy[,8]
#SE is the point of comparison#

install.packages("stargazer")
library(stargazer)

####0####

###a_0###

df <- readRDS("Data needed to complete empirical work/Data sets for model testing/Iowa/Data_a_0_iowa")

###b_0###

df <- readRDS("Data needed to complete empirical work/Data sets for model testing/Iowa/Data_b_0_iowa")

###c_0###

df <- readRDS("Data needed to complete empirical work/Data sets for model testing/Iowa/Data_c_0_iowa")

###d_0###

df <- readRDS("Data needed to complete empirical work/Data sets for model testing/Iowa/Data_d_0_iowa")

###e_0###

df <- readRDS("Data needed to complete empirical work/Data sets for model testing/Iowa/Data_e_0_iowa")

###f_0###

df <- readRDS("Data needed to complete empirical work/Data sets for model testing/Iowa/Data_f_0_iowa")

###g_0###

df <- readRDS("Data needed to complete empirical work/Data sets for model testing/Iowa/Data_g_0_iowa")

####i####

###a_i###

df <- readRDS("Data needed to complete empirical work/Data sets for model testing/Iowa/Data_a_i_iowa")

###b_i###

df <- readRDS("Data needed to complete empirical work/Data sets for model testing/Iowa/Data_b_i_iowa")

###c_i###

df <- readRDS("Data needed to complete empirical work/Data sets for model testing/Iowa/Data_c_i_iowa")

###d_i###

df <- readRDS("Data needed to complete empirical work/Data sets for model testing/Iowa/Data_d_i_iowa")

###e_i###

df <- readRDS("Data needed to complete empirical work/Data sets for model testing/Iowa/Data_e_i_iowa")

###f_i###

df <- readRDS("Data needed to complete empirical work/Data sets for model testing/Iowa/Data_f_i_iowa")

###g_i###

df <- readRDS("Data needed to complete empirical work/Data sets for model testing/Iowa/Data_g_i_iowa")

####ii####

###a_ii###

df <- readRDS("Data needed to complete empirical work/Data sets for model testing/Iowa/Data_a_ii_iowa")

###b_ii###

df <- readRDS("Data needed to complete empirical work/Data sets for model testing/Iowa/Data_b_ii_iowa")

###c_ii###

df <- readRDS("Data needed to complete empirical work/Data sets for model testing/Iowa/Data_c_ii_iowa")

###d_ii###

df <- readRDS("Data needed to complete empirical work/Data sets for model testing/Iowa/Data_d_ii_iowa")

###e_ii###

df <- readRDS("Data needed to complete empirical work/Data sets for model testing/Iowa/Data_e_ii_iowa")

###f_ii###

df <- readRDS("Data needed to complete empirical work/Data sets for model testing/Iowa/Data_f_ii_iowa")

###g_ii###

df <- readRDS("Data needed to complete empirical work/Data sets for model testing/Iowa/Data_g_ii_iowa")

####iii####

###a_iii###

df <- readRDS("Data needed to complete empirical work/Data sets for model testing/Iowa/Data_a_iii_iowa")

###b_iii###

df <- readRDS("Data needed to complete empirical work/Data sets for model testing/Iowa/Data_b_iii_iowa")

###c_iii###

df <- readRDS("Data needed to complete empirical work/Data sets for model testing/Iowa/Data_c_iii_iowa")

###d_iii###

df <- readRDS("Data needed to complete empirical work/Data sets for model testing/Iowa/Data_d_iii_iowa")

###e_iii###

df <- readRDS("Data needed to complete empirical work/Data sets for model testing/Iowa/Data_e_iii_iowa")

###f_iii###

df <- readRDS("Data needed to complete empirical work/Data sets for model testing/Iowa/Data_f_iii_iowa")

###g_iii###

df <- readRDS("Data needed to complete empirical work/Data sets for model testing/Iowa/Data_g_iii_iowa")

####iv####

###a_iv###

df <- readRDS("Data needed to complete empirical work/Data sets for model testing/Iowa/Data_a_iv_iowa")

###b_iv###

df <- readRDS("Data needed to complete empirical work/Data sets for model testing/Iowa/Data_b_iv_iowa")

###c_iv###

df <- readRDS("Data needed to complete empirical work/Data sets for model testing/Iowa/Data_c_iv_iowa")

###d_iv###

df <- readRDS("Data needed to complete empirical work/Data sets for model testing/Iowa/Data_d_iv_iowa")

###e_iv###

df <- readRDS("Data needed to complete empirical work/Data sets for model testing/Iowa/Data_e_iv_iowa")

###f_iv###

df <- readRDS("Data needed to complete empirical work/Data sets for model testing/Iowa/Data_f_iv_iowa")

###g_iv###

df <- readRDS("Data needed to complete empirical work/Data sets for model testing/Iowa/Data_g_iv_iowa")

####v####

###a_v###

df <- readRDS("Data needed to complete empirical work/Data sets for model testing/Iowa/Data_a_v_iowa")

###b_v###

df <- readRDS("Data needed to complete empirical work/Data sets for model testing/Iowa/Data_b_v_iowa")

###c_v###

df <- readRDS("Data needed to complete empirical work/Data sets for model testing/Iowa/Data_c_v_iowa")

###d_v###

df <- readRDS("Data needed to complete empirical work/Data sets for model testing/Iowa/Data_d_v_iowa")

###e_v###

df <- readRDS("Data needed to complete empirical work/Data sets for model testing/Iowa/Data_e_v_iowa")

###f_v###

df <- readRDS("Data needed to complete empirical work/Data sets for model testing/Iowa/Data_f_v_iowa")

###g_v###

df <- readRDS("Data needed to complete empirical work/Data sets for model testing/Iowa/Data_g_v_iowa")

####vi####

###a_vi###

df <- readRDS("Data needed to complete empirical work/Data sets for model testing/Iowa/Data_a_vi_iowa")

###b_vi###

df <- readRDS("Data needed to complete empirical work/Data sets for model testing/Iowa/Data_b_vi_iowa")

###c_vi###

df <- readRDS("Data needed to complete empirical work/Data sets for model testing/Iowa/Data_c_vi_iowa")

###d_vi###

df <- readRDS("Data needed to complete empirical work/Data sets for model testing/Iowa/Data_d_vi_iowa")

###e_vi###

df <- readRDS("Data needed to complete empirical work/Data sets for model testing/Iowa/Data_e_vi_iowa")

###f_vi###

df <- readRDS("Data needed to complete empirical work/Data sets for model testing/Iowa/Data_f_vi_iowa")

###g_vi###

df <- readRDS("Data needed to complete empirical work/Data sets for model testing/Iowa/Data_g_vi_iowa")

Y <- df[,3]

const <- rep(1,N*T)
X <- as.matrix(cbind(const,df[,4:ncol(df)],NW,NC,NE,WC,C,EC,SW,SC))

W <- read.delim("Data needed to complete empirical work/Data sets for model testing/Iowa/Iowa_W_norm.txt", header=F)
W <- as.matrix(W)

K <- kronecker(diag(T),W)

eig <- eigen(W, symmetric = FALSE, only.values = TRUE)
lambda <- eig$values

rhomin <- (1/min(lambda))
rhomax <- (1/max(lambda))

p_bound <- seq(rhomin,rhomax,.1)

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
Ynew <- A%*%Y

X1 <- as.matrix(cbind(df[,4:ncol(df)],NW,NC,NE,WC,C,EC,SW,SC))

Xnew <- A%*%X1

mod <- lm(Ynew~Xnew)


a_vi <- mod
b_vi <- mod
c_vi <- mod
d_vi <- mod
e_vi <- mod
f_vi <- mod
g_vi <- mod


install.packages("sandwich")
library(sandwich)

# Adjust standard errors
cov_a_vi         <- vcovHC(a_vi, type = "HC3")
robust_a_vi    <- sqrt(diag(cov_a_vi))

cov_b_vi         <- vcovHC(b_vi, type = "HC3")
robust_b_vi    <- sqrt(diag(cov_b_vi))

cov_c_vi         <- vcovHC(c_vi, type = "HC3")
robust_c_vi    <- sqrt(diag(cov_c_vi))

cov_d_vi         <- vcovHC(d_vi, type = "HC3")
robust_d_vi    <- sqrt(diag(cov_d_vi))

cov_e_vi         <- vcovHC(e_vi, type = "HC3")
robust_e_vi    <- sqrt(diag(cov_e_vi))

cov_f_vi         <- vcovHC(f_vi, type = "HC3")
robust_f_vi    <- sqrt(diag(cov_f_vi))

cov_g_vi         <- vcovHC(g_vi, type = "HC3")
robust_g_vi    <- sqrt(diag(cov_g_vi))


install.packages("lmtest")
library(lmtest)

wald_results_a_vi <- waldtest(a_vi, vcov = cov_a_vi)
wald_results_b_vi <- waldtest(b_vi, vcov = cov_b_vi)
wald_results_c_vi <- waldtest(c_vi, vcov = cov_c_vi)
wald_results_d_vi <- waldtest(d_vi, vcov = cov_d_vi)
wald_results_e_vi <- waldtest(e_vi, vcov = cov_e_vi)
wald_results_f_vi <- waldtest(f_vi, vcov = cov_f_vi)
wald_results_g_vi <- waldtest(g_vi, vcov = cov_g_vi)



install.packages("stargazer")
library(stargazer)

stargazer(a_vi,b_vi,c_vi,d_vi,e_vi,f_vi,g_vi,title = "Iowa: Model Testing Results - Timing 0", column.labels = c("a$_vi$","b$_vi$", "c$_vi$", "d$_vi$", "e$_vi$", "f$_vi$", "g$_vi$" ),se=list(robust_a_vi,robust_b_vi,robust_c_vi,robust_d_vi,robust_e_vi,robust_f_vi,robust_g_vi),align=TRUE)

stargazer(a_vi,b_vi,c_vi,d_vi,e_vi,f_vi,g_vi,title = "Iowa: Model Testing Results - Timing 0", column.labels = c("a_vi","b_vi", "c_vi", "d_vi", "e_vi$", "f_vi", "g_vi" ),se=list(robust_a_vi,robust_b_vi,robust_c_vi,robust_d_vi,robust_e_vi,robust_f_vi,robust_g_vi),align=TRUE)

####DONT FORGET TO MODIFY THE F STATISTICS IN THE TABLE and TAKE OUT THE LOCATION VARIABLES####

stargazer(a_vi,b_vi,c_vi,d_vi,e_vi,f_vi,g_vi)


stargazer(a_vi,b_vi,c_vi,d_vi,e_vi,f_vi,g_vi,type="html",title = "Iowa: Model Testing Results - Timing vi",column.labels = c("a_vi","b_vi", "c_vi", "d_vi", "e_vi$", "f_vi", "g_vi" ),se=list(robust_a_vi,robust_b_vi,robust_c_vi,robust_d_vi,robust_e_vi,robust_f_vi,robust_g_vi), align=TRUE,out = "Result tables/Iowa_model_a_vi_results.html")
