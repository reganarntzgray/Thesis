setwd("C:/Users/Regan/Google Drive/Thesis Presentations and Proposal/Thesis/Data needed to complete empirical work/Data sets for model testing/Ontario")

N <- 19
T <- 64

Dummy <- diag(N)
Dummy <- matrix( rep( t( Dummy) , T ) , ncol = ncol(Dummy) , byrow = TRUE )

df1 <- readRDS("Data_d_iii_ont")
attach(df1)

ON <- c("brant","bruce","chatham-kent","elgin","essex","haldimand-norfolk","huron","lambton","middlesex","ottawa","oxford","peel","perth","prescott-russell","simcoe","stormont-dundas-glengarry","waterloo","wellington","york")

df <- df1[county %in% ON,]
attach(df)

Y <- df[,3]

const <- rep(1,N*T)
X <- as.matrix(cbind(const,df[,4:ncol(df)],Dummy[,1:18]))

W <- read.delim("C:/Users/Regan/Google Drive/Thesis Presentations and Proposal/Thesis/Code/Ontario/W_ONT_trim_norm.txt",header=F)
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

SSE <- function(x)
{k <- x[1] + x[2]*df[,4]

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

mod <- lm(Ynew~0+Xnew)
Beta <- summary(mod)$coeff[,1]
e <- Ynew-Xnew%*%Beta
SSE_r <- sum(e^2)
return(SSE_r)}

mean <- mean(mean_pcp_july)
min <- summary(mean_pcp_july)[1]+1
Q1 <- summary(mean_pcp_july)[2]
Q2 <- summary(mean_pcp_july)[3]
Q3 <- summary(mean_pcp_july)[5]
max <- summary(mean_pcp_july)[6]

a <- seq((min),160,10)
 SSEmat <- matrix(0,length(a),15)
    for(k in 1:length(a))
{print(k)
    b <- seq(max((min-a[k]),((min-a[k])/T)),min((max-a[k]),((max-a[k])/T)),.2)
    
    
    for(l in 1:length(b))
{print(l)
    x <- c(a[k],b[l])
    SSEmat[k,l] <- SSE(x)}}
  
ind = which(SSEmat == min(SSEmat), arr.ind = TRUE)
 
a_opt <- a[ind[1]]

#a_opt <- a[2]

a <- seq((a_opt-10),(a_opt+10),1)
 SSEmat <- matrix(0,length(a),29)
    for(k in 1:length(a))
{print(k)
    b <- seq(max((min-a[k]),((min-a[k])/T)),min((max-a[k]),((max-a[k])/T)),.1)
    
    
    for(l in 1:length(b))
{print(l)
    x <- c(a[k],b[l])
    SSEmat[k,l] <- SSE(x)}}

ind = which(SSEmat == min(SSEmat), arr.ind = TRUE)
 
a_opt <- a[ind[1]]
 
b <- seq(max((min-a_opt),((min-a_opt)/T)),min((max-a_opt),((max-a_opt)/T)),.1)
 
b_opt <- b[ind[2]]
 
#a_opt <- a[5]

#b <- seq(max((min-a_opt),((min-a_opt)/T)),min((max-a_opt),((max-a_opt)/T)),.1)
    
#b_opt <- b[16]

#> a_opt
#[1] 5
#> b_opt
#[1] 1.4375
