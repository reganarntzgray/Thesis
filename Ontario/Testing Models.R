######Model Testing######

#####Regional Dummy#####

setwd("C:/Users/Regan/Google Drive/Thesis Presentations and Proposal/Thesis")

N <- 32
T <- 64

Dummy <- diag(N)
Dummy <- matrix( rep( t( Dummy) , T ) , ncol = ncol(Dummy) , byrow = TRUE )


###a_0###

df <- readRDS("Data needed to complete empirical work/Data sets for model testing/Ontario/Data_a_0_ont")

###b_0###

df <- readRDS("Data needed to complete empirical work/Data sets for model testing/Ontario/Data_b_0_ont")

###c_0###

df <- readRDS("Data needed to complete empirical work/Data sets for model testing/Ontario/Data_c_0_ont")

###d_0###

df <- readRDS("Data needed to complete empirical work/Data sets for model testing/Ontario/Data_d_0_ont")

###e_0###

df <- readRDS("Data needed to complete empirical work/Data sets for model testing/Ontario/Data_e_0_ont")

###f_0###

df <- readRDS("Data needed to complete empirical work/Data sets for model testing/Ontario/Data_f_0_ont")

###g_0###

df <- readRDS("Data needed to complete empirical work/Data sets for model testing/Ontario/Data_g_0_ont")


###a_i###

df <- readRDS("Data needed to complete empirical work/Data sets for model testing/Ontario/Data_a_i_ont")

###b_i###

df <- readRDS("Data needed to complete empirical work/Data sets for model testing/Ontario/Data_b_i_ont")

###c_i###

df <- readRDS("Data needed to complete empirical work/Data sets for model testing/Ontario/Data_c_i_ont")

###d_i###

df <- readRDS("Data needed to complete empirical work/Data sets for model testing/Ontario/Data_d_i_ont")

###e_i###

df <- readRDS("Data needed to complete empirical work/Data sets for model testing/Ontario/Data_e_i_ont")

###f_i###

df <- readRDS("Data needed to complete empirical work/Data sets for model testing/Ontario/Data_f_i_ont")

###g_i###

df <- readRDS("Data needed to complete empirical work/Data sets for model testing/Ontario/Data_g_i_ont")

####ii####

###a_ii###

df <- readRDS("Data needed to complete empirical work/Data sets for model testing/Ontario/Data_a_ii_ont")

###b_ii###

df <- readRDS("Data needed to complete empirical work/Data sets for model testing/Ontario/Data_b_ii_ont")

###c_ii###

df <- readRDS("Data needed to complete empirical work/Data sets for model testing/Ontario/Data_c_ii_ont")

###d_ii###

df <- readRDS("Data needed to complete empirical work/Data sets for model testing/Ontario/Data_d_ii_ont")

###e_ii###

df <- readRDS("Data needed to complete empirical work/Data sets for model testing/Ontario/Data_e_ii_ont")

###f_ii###

df <- readRDS("Data needed to complete empirical work/Data sets for model testing/Ontario/Data_f_ii_ont")

###g_ii###

df <- readRDS("Data needed to complete empirical work/Data sets for model testing/Ontario/Data_g_ii_ont")

####iii####

###a_iii###

df <- readRDS("Data needed to complete empirical work/Data sets for model testing/Ontario/Data_a_iii_ont")

###b_iii###

df <- readRDS("Data needed to complete empirical work/Data sets for model testing/Ontario/Data_b_iii_ont")

###c_iii###

df <- readRDS("Data needed to complete empirical work/Data sets for model testing/Ontario/Data_c_iii_ont")

###d_iii###

df <- readRDS("Data needed to complete empirical work/Data sets for model testing/Ontario/Data_d_iii_ont")

###e_iii###

df <- readRDS("Data needed to complete empirical work/Data sets for model testing/Ontario/Data_e_iii_ont")

###f_iii###

df <- readRDS("Data needed to complete empirical work/Data sets for model testing/Ontario/Data_f_iii_ont")

###g_iii###

df <- readRDS("Data needed to complete empirical work/Data sets for model testing/Ontario/Data_g_iii_ont")

####iv####

###a_iv###

df <- readRDS("Data needed to complete empirical work/Data sets for model testing/Ontario/Data_a_iv_ont")

###b_iv###

df <- readRDS("Data needed to complete empirical work/Data sets for model testing/Ontario/Data_b_iv_ont")

###c_iv###

df <- readRDS("Data needed to complete empirical work/Data sets for model testing/Ontario/Data_c_iv_ont")

###d_iv###

df <- readRDS("Data needed to complete empirical work/Data sets for model testing/Ontario/Data_d_iv_ont")

###e_iv###

df <- readRDS("Data needed to complete empirical work/Data sets for model testing/Ontario/Data_e_iv_ont")

###f_iv###

df <- readRDS("Data needed to complete empirical work/Data sets for model testing/Ontario/Data_f_iv_ont")

###g_iv###

df <- readRDS("Data needed to complete empirical work/Data sets for model testing/Ontario/Data_g_iv_ont")

####v####

###a_v###

df <- readRDS("Data needed to complete empirical work/Data sets for model testing/Ontario/Data_a_v_ont")

###b_v###

df <- readRDS("Data needed to complete empirical work/Data sets for model testing/Ontario/Data_b_v_ont")

###c_v###

df <- readRDS("Data needed to complete empirical work/Data sets for model testing/Ontario/Data_c_v_ont")

###d_v###

df <- readRDS("Data needed to complete empirical work/Data sets for model testing/Ontario/Data_d_v_ont")

###e_v###

df <- readRDS("Data needed to complete empirical work/Data sets for model testing/Ontario/Data_e_v_ont")

###f_v###

df <- readRDS("Data needed to complete empirical work/Data sets for model testing/Ontario/Data_f_v_ont")

###g_v###

df <- readRDS("Data needed to complete empirical work/Data sets for model testing/Ontario/Data_g_v_ont")

####vi####

###a_vi###

df <- readRDS("Data needed to complete empirical work/Data sets for model testing/Ontario/Data_a_vi_ont")

###b_vi###

df <- readRDS("Data needed to complete empirical work/Data sets for model testing/Ontario/Data_b_vi_ont")

###c_vi###

df <- readRDS("Data needed to complete empirical work/Data sets for model testing/Ontario/Data_c_vi_ont")

###d_vi###

df <- readRDS("Data needed to complete empirical work/Data sets for model testing/Ontario/Data_d_vi_ont")

###e_vi###

df <- readRDS("Data needed to complete empirical work/Data sets for model testing/Ontario/Data_e_vi_ont")

###f_vi###

df <- readRDS("Data needed to complete empirical work/Data sets for model testing/Ontario/Data_f_vi_ont")

###g_vi###

df <- readRDS("Data needed to complete empirical work/Data sets for model testing/Ontario/Data_g_vi_ont")




Y <- df[,3]

const <- rep(1,N*T)
X <- as.matrix(cbind(const,df[,4:ncol(df)],Dummy[,1:31]))

W <- read.delim("Code/Ontario/W_ONT_norm.txt",header=F)
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

X1 <- as.matrix(cbind(df[,4:ncol(df)],Dummy[,1:31]))

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


stargazer(a_vi,b_vi,c_vi,d_vi,e_vi,f_vi,g_vi,type="html",title = "Ontario: Model Testing Results - Timing vi",column.labels = c("a_vi","b_vi", "c_vi", "d_vi", "e_vi$", "f_vi", "g_vi" ),se=list(robust_a_vi,robust_b_vi,robust_c_vi,robust_d_vi,robust_e_vi,robust_f_vi,robust_g_vi), align=TRUE,out = "Result tables/Ontario_model_a_vi_results.html")
