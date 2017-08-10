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
SC <- Dummy[,7]
SE <- Dummy[,8]
#SW is the point of comparison#

install.packages("stargazer")
library(stargazer)

df <- readRDS("Data needed to complete empirical work/Data sets for model testing/Iowa/Data_d_iv_iowa")

attach(df)

Y <- df[,3]

const <- rep(1,N*T)
X1 <- as.matrix(cbind(const,df[,4:ncol(df)],NW,NC,NE,WC,C,EC,SC,SE))

W <- read.delim("Data needed to complete empirical work/Data sets for model testing/Iowa/Iowa_W_norm.txt", header=F)
W <- as.matrix(W)

K <- kronecker(diag(T),W)

eig <- eigen(W, symmetric = FALSE, only.values = TRUE)
lambda <- eig$values

rhomin <- (1/min(lambda))
rhomax <- (1/max(lambda))

p_bound <- seq(rhomin,rhomax,.1)


#Method 1: With Optimal threshold, 2 thresholds, rho recalculated for each combination#

a_l <- 60.416
b_l <- .9827586
a_h <- 345.1
b_h <- -3.713793

kl <- a_l + b_l*df[,4]
kh <- a_h + b_h*df[,4]

pcp_pol_sp_low <- (pcp_pol_i-kl)*(pcp_pol_i>kl)
pcp_pol_sp_high <- (pcp_pol_i-kh)*(pcp_pol_i>kh)
X <- cbind(X1,pcp_pol_sp_low,pcp_pol_sp_high)

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
Xnew <- A%*%X

M1 <- lm(Ynew~0+Xnew)

X11 <- as.matrix(cbind(df[,4:ncol(df)],NW,NC,NE,WC,C,EC,SC,SE))

X_1 <- cbind(X11,pcp_pol_sp_low,pcp_pol_sp_high)
X_1new <- A%*%X_1

M11 <- lm(Ynew~X_1new)

beta_1 <- summary(M11)$coef[,1]

#Method 2: With Optimal threshold, 2 thresholds, rho recalculated once#

a_l <- 64.416
b_l <- .8137931
a_h <- 288.1
b_h <- -2.831034

kl <- a_l + b_l*df[,4]
kh <- a_h + b_h*df[,4]

pcp_pol_sp_low <- (pcp_pol_i-kl)*(pcp_pol_i>kl)
pcp_pol_sp_high <- (pcp_pol_i-kh)*(pcp_pol_i>kh)
X <- cbind(X1,pcp_pol_sp_low,pcp_pol_sp_high)

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
Xnew <- A%*%X

M2 <- lm(Ynew~0+Xnew)

X11 <- as.matrix(cbind(df[,4:ncol(df)],NW,NC,NE,WC,C,EC,SC,SE))

X_1 <- cbind(X11,pcp_pol_sp_low,pcp_pol_sp_high)
X_1new <- A%*%X_1

M12 <- lm(Ynew~X_1new)

beta_2 <- summary(M12)$coef[,1]

#Method 3: With Optimal threshold, 1 threshold, rho recalculated for each combination#

a <- 86.416
b <- .4344828

k <- a + b*df[,4]

pcp_pol_sp<- (pcp_pol_i-k)*(pcp_pol_i>k)
X <- cbind(X1,pcp_pol_sp)

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
Xnew <- A%*%X

M3 <- lm(Ynew~0+Xnew)

X11 <- as.matrix(cbind(df[,4:ncol(df)],NW,NC,NE,WC,C,EC,SC,SE))

X_1 <- cbind(X11,pcp_pol_sp)
X_1new <- A%*%X_1

M13 <- lm(Ynew~X_1new)

beta_3 <- summary(M13)$coef[,1]

#Method 4: With Optimal threshold, 1 threshold, rho calculated once#

M4 <- M3
M14 <- M13

beta_4 <- beta_3

a <- 86.416
b <- .4344828

k <- a + b*df[,4]

pcp_pol_sp<- (pcp_pol_i-k)*(pcp_pol_i>k)
X <- cbind(X1,pcp_pol_sp)

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
Xnew <- A%*%X

M4 <- lm(Ynew~0+Xnew)

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


stargazer(M11,M12,M13,M14,title = "Iowa: Regression results with optimal threshold levels",se=list(robust_M1,robust_M2,robust_M3,robust_M4), covariate.labels = c("T","GDD","HDD","VPD$_{b_i}$","VPD$_{{pol}_i}$","VPD$_{e_i}$","PCP$_{b_i}$", "PCP$_{{pol}_i}$","PCP$_{e_i}$","NW","NC","NE","WC","C","EC","SC","SE","PCP$_{{pol}_{sp_{low}}}$","PCP$_{{pol}_{sp_{high}}}$","PCP$_{{pol}_{sp}}$"),align=TRUE)

stargazer(M11,M12,M13,M14,type="html",title = "Iowa: Regression results with optimal threshold levels",se=list(robust_M1,robust_M2,robust_M3,robust_M4), covariate.labels = c("T","GDD","HDD","VPD$_{b_i}$","VPD$_{{pol}_i}$","VPD$_{e_i}$","PCP$_{b_i}$", "PCP$_{{pol}_i}$","PCP$_{e_i}$","NW","NC","NE","WC","C","EC","SC","SE","PCP$_{{pol}_{sp_{low}}}$","PCP$_{{pol}_{sp_{high}}}$","PCP$_{{pol}_{sp}}$"),align=TRUE,out = "Result tables/Iowa_threshold_results.html")


