setwd("C:/Users/Regan/Google Drive/Thesis Presentations and Proposal/Thesis/Data needed to complete empirical work/Data sets for model testing/Ontario")

N <- 32
T <- 64

Dummy <- diag(N)
Dummy <- matrix( rep( t( Dummy) , T ) , ncol = ncol(Dummy) , byrow = TRUE )


df <- readRDS("Data_d_iii_ont")
attach(df)

Y <- df[,3]

const <- rep(1,N*T)
X <- as.matrix(cbind(const,df[,4:ncol(df)],Dummy[,1:31]))

W <- read.delim("C:/Users/Regan/Google Drive/Thesis Presentations and Proposal/Thesis/Code/Ontario/W_ONT_norm.txt",header=F)
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

write.table(p_opt,"C:/Users/Regan/Google Drive/Thesis Presentations and Proposal/Thesis/Data needed to complete empirical work/simulated weather/Ontario/p_opt")

X1 <- X

A <- (diag(N*T)-p_opt*K)
Ynew <- A%*%Y


SSE <- function(x)
{k <- x[1] + x[2]*df[,4]

pcp_july_sp <- (mean_pcp_july-k)*(mean_pcp_july>k)

X <- cbind(X1,pcp_july_sp)

Xnew <- A%*%X

mod <- lm(Ynew~0+Xnew)
Beta <- summary(mod)$coeff[,1]
e <- Ynew-Xnew%*%Beta
SSE <- sum(e^2)
return(SSE)}

mean <- mean(mean_pcp_july)
min <- summary(mean_pcp_july)[1]+1
Q1 <- summary(mean_pcp_july)[2]
Q2 <- summary(mean_pcp_july)[3]
Q3 <- summary(mean_pcp_july)[5]
max <- summary(mean_pcp_july)[6]

a <- seq((min),160,1)

SSEmain <- matrix(0,30,length(a))
for(j in 1:length(a))
{print(j)
  
  b <- seq(max((min-a[j]),((min-a[j])/T)),min((max-a[j]),((max-a[j])/T)),.1)
  
  for (i in 1:length(b))
  { print(i)
    x <- c(a[j],b[i])
    SSEmain[i,j]<-SSE(x)}}

ind = which(SSEmain == min(SSEmain), arr.ind = TRUE)

a_opt <- a[ind[1]]

b_opt <- b[ind[2]]

#> which(SSEmain == min(SSEmain), arr.ind = TRUE)
#row col
#[1,]  17  10

#a_opt <- a[10]
#b_opt <- b[17]

#> a_opt
#[1] 10
#> b_opt
#[1] 1.459375

thresholds <- c(a_opt,b_opt)

write.table(thresholds,"C:/Users/Regan/Google Drive/Thesis Presentations and Proposal/Thesis/Data needed to complete empirical work/simulated weather/Ontario/thresholds_dynamic")


###STATIC MODEL###


SSE <- function(x)
{k <- x

pcp_july_sp <- (mean_pcp_july-k)*(mean_pcp_july>k)

X <- cbind(X1,pcp_july_sp)

Xnew <- A%*%X

mod <- lm(Ynew~0+Xnew)
Beta <- summary(mod)$coeff[,1]
e <- Ynew-Xnew%*%Beta
SSE <- sum(e^2)
return(SSE)}

mean <- mean(mean_pcp_july)
min <- summary(mean_pcp_july)[1]+1
Q1 <- summary(mean_pcp_july)[2]
Q2 <- summary(mean_pcp_july)[3]
Q3 <- summary(mean_pcp_july)[5]
max <- summary(mean_pcp_july)[6]

a <- seq(min,max,1)

SSEmain <- NULL
for(j in 1:length(a))
{print(j)
x<- a[j]
  
     SSEmain[j]<-SSE(x)}

ind = which(SSEmain == min(SSEmain), arr.ind = TRUE)

a_opt <- a[ind[1]]

b_opt <- b[ind[2]]

#> which.min(SSEmain)
#[1] 96

#a_opt <- a[96]

#> a_opt
#[1] 96

threshold <- a_opt

write.table(threshold,"C:/Users/Regan/Google Drive/Thesis Presentations and Proposal/Thesis/Data needed to complete empirical work/simulated weather/Ontario/threshold_static")


###Now getting the Beta_dynamic and Beta_static###

#dynamic#

k_d <- thresholds[1] + thresholds[2]*df[,4]

pcp_july_sp_d <- (mean_pcp_july-k_d)*(mean_pcp_july>k_d)

X_d <- cbind(X1,pcp_july_sp_d)

Xnew_d <- A%*%X_d

mod_d <- lm(Ynew~0+Xnew_d)
Beta_dynamic <- summary(mod_d)$coeff[,1]

write.table(Beta_dynamic,"C:/Users/Regan/Google Drive/Thesis Presentations and Proposal/Thesis/Data needed to complete empirical work/simulated weather/Ontario/Beta_dynamic")

#static#

k_s <- threshold[1]

pcp_july_sp_s <- (mean_pcp_july-k_s)*(mean_pcp_july>k_s)

X_s <- cbind(X1,pcp_july_sp_s)

Xnew_s <- A%*%X_s

mod_s <- lm(Ynew~0+Xnew_s)
Beta_static <- summary(mod_s)$coeff[,1]

write.table(Beta_static,"C:/Users/Regan/Google Drive/Thesis Presentations and Proposal/Thesis/Data needed to complete empirical work/simulated weather/Ontario/Beta_static")


