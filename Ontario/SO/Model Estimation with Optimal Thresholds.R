setwd("C:/Users/Regan/Google Drive/Thesis Presentations and Proposal/Thesis")

N <- 8
T <- 64

Dummy <- diag(N)
Dummy <- matrix( rep( t( Dummy) , T ) , ncol = ncol(Dummy) , byrow = TRUE )


df1 <- readRDS("Data needed to complete empirical work/Data sets for model testing/Ontario/Data_d_iii_ont")
attach(df1)

SO <- c("brant","chatham-kent","elgin","essex","haldimand-norfolk","lambton","middlesex","oxford")

df <- df1[county %in% SO,]
attach(df)

Y <- df[,3]

const <- rep(1,N*T)
X <- as.matrix(cbind(const,df[,4:ncol(df)],Dummy[,1:7]))

W <- read.delim("Code/Ontario/W_SO.txt",header=F)
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

p_opt1 <- optim(mean(p_bound),(log_L),method="Brent",lower=rhomin, upper=rhomax)$par

X1 <- X

#Method 1#

a_l <- 30.18
b_l <-  0.91875
a_h <- 118.37
b_h <- 0.09375

kl <- a_l + b_l*df[,4]
kh <- a_h + b_h*df[,4]

pcp_july_sp_low <- (mean_pcp_july-kl)*(mean_pcp_july>kl)
pcp_july_sp_high <- (mean_pcp_july-kh)*(mean_pcp_july>kh)
X <- cbind(X1,pcp_july_sp_low,pcp_july_sp_high)

log_L <- function(p)
{A <- (diag(N*T)-p*K)
log_det_I_pW <- log(1-p*lambda)
log_detA <- T*sum(log_det_I_pW)
Beta <- solve(t(A%*%X)%*%(A%*%X))%*%t(A%*%X)%*%(A%*%Y)
U <- Y-X%*%Beta
sigma_sq <- (1/N*T)*(t(U)%*%t(A)%*%A%*%U)
log_L <- (-(N*T)/2)*log(2*pi*sigma_sq)+log_detA-(1/(2*sigma_sq))*t(U)%*%t(A)%*%A%*%U
return(-log_L)}

p_opt <- optim(p_opt1,(log_L),method="Brent",lower=rhomin, upper=rhomax)$par

A <- (diag(N*T)-p_opt*K)
Ynew <- A%*%Y
Xnew <- A%*%X

M1 <- lm(Ynew~0+Xnew)

X11 <- as.matrix(cbind(df[,4:ncol(df)],Dummy[,1:7]))

X_1 <- cbind(X11,pcp_july_sp_low,pcp_july_sp_high)
X_1new <- A%*%X_1

M11 <- lm(Ynew~X_1new)

beta_1 <- summary(M11)$coef[,1]



#Method 2#

a_l <- 30.18
b_l <-  0.91875
a_h <- 116.37
b_h <- 0.125

kl <- a_l + b_l*df[,4]
kh <- a_h + b_h*df[,4]

pcp_july_sp_low <- (mean_pcp_july-kl)*(mean_pcp_july>kl)
pcp_july_sp_high <- (mean_pcp_july-kh)*(mean_pcp_july>kh)
X <- cbind(X1,pcp_july_sp_low,pcp_july_sp_high)

log_L <- function(p)
{A <- (diag(N*T)-p*K)
log_det_I_pW <- log(1-p*lambda)
log_detA <- T*sum(log_det_I_pW)
Beta <- solve(t(A%*%X)%*%(A%*%X))%*%t(A%*%X)%*%(A%*%Y)
U <- Y-X%*%Beta
sigma_sq <- (1/N*T)*(t(U)%*%t(A)%*%A%*%U)
log_L <- (-(N*T)/2)*log(2*pi*sigma_sq)+log_detA-(1/(2*sigma_sq))*t(U)%*%t(A)%*%A%*%U
return(-log_L)}

p_opt <- optim(p_opt1,(log_L),method="Brent",lower=rhomin, upper=rhomax)$par

A <- (diag(N*T)-p_opt*K)
Ynew <- A%*%Y
Xnew <- A%*%X

M2 <- lm(Ynew~0+Xnew)

X11 <- as.matrix(cbind(df[,4:ncol(df)],Dummy[,1:7]))

X_1 <- cbind(X11,pcp_july_sp_low,pcp_july_sp_high)
X_1new <- A%*%X_1

M12 <- lm(Ynew~X_1new)

beta_2 <- summary(M12)$coef[,1]


#Method 3#

a <- 32.18
b <-  1.15625

k <- a + b*df[,4]

pcp_july_sp <- (mean_pcp_july-k)*(mean_pcp_july>k)
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

p_opt <- optim(p_opt1,(log_L),method="Brent",lower=rhomin, upper=rhomax)$par

A <- (diag(N*T)-p_opt*K)
Ynew <- A%*%Y
Xnew <- A%*%X

M3 <- lm(Ynew~0+Xnew)

X11 <- as.matrix(cbind(df[,4:ncol(df)],Dummy[,1:7]))

X_1 <- cbind(X11,pcp_july_sp)
X_1new <- A%*%X_1

M13 <- lm(Ynew~X_1new)

beta_3 <- summary(M13)$coef[,1]


#Method 4#

a <- 34.18
b <-  1.15625

k <- a + b*df[,4]

pcp_july_sp <- (mean_pcp_july-k)*(mean_pcp_july>k)
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

p_opt <- optim(p_opt1,(log_L),method="Brent",lower=rhomin, upper=rhomax)$par

A <- (diag(N*T)-p_opt*K)
Ynew <- A%*%Y
Xnew <- A%*%X

M4 <- lm(Ynew~0+Xnew)

X11 <- as.matrix(cbind(df[,4:ncol(df)],Dummy[,1:7]))

X_1 <- cbind(X11,pcp_july_sp)
X_1new <- A%*%X_1

M14 <- lm(Ynew~X_1new)

beta_4 <- summary(M14)$coef[,1]

###generating the results with robust errors

install.packages("sandwich")
library(sandwich)

# Adjust standard errors
cov_M1         <- vcovHC(M11, type = "HC3")
robust_M1    <- sqrt(diag(cov_M1))

cov_M2         <- vcovHC(M12, type = "HC3")
robust_M2    <- sqrt(diag(cov_M2))

cov_M3         <- vcovHC(M13, type = "HC3")
robust_M3    <- sqrt(diag(cov_M3))

cov_M4         <- vcovHC(M14, type = "HC3")
robust_M4    <- sqrt(diag(cov_M4))


install.packages("lmtest")
library(lmtest)

wald_results_M1 <- waldtest(M11, vcov = cov_M1)
wald_results_M2 <- waldtest(M12, vcov = cov_M2)
wald_results_M3 <- waldtest(M13, vcov = cov_M3)
wald_results_M4 <- waldtest(M14, vcov = cov_M4)


install.packages("stargazer")
library(stargazer)

stargazer(M11,M12,M13,M14,type="html",title = "Southern Ontario: Regression results with optimal threshold levels",se=list(robust_M1,robust_M2,robust_M3,robust_M4), covariate.labels = c("T","Trend","GDD","HDD","VPD$_{May}$","VPD$_{June}$","VPD$_{July}$","VPD$_{August}$","PCP$_{May}$","PCP$_{June}$","PCP$_{July}$","PCP$_{August}$",c(1:7),"PCP$_{{july}_{sp_{low}}}$","PCP$_{{july}_{sp_{high}}}$","PCP$_{{july}_{sp}}$"),align=TRUE,out = "Result tables/Ontario_SO_threshold_results.html")



