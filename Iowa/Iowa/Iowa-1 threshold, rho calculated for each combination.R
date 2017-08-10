setwd("C:/Users/Regan/Google Drive/Thesis Presentations and Proposal/Thesis/Data needed to complete empirical work/Data sets for model testing/Iowa")

N <- 99
T <- 58

Dummy <- read.delim("county_region_matrix.txt")

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
#SE is the point of comparison#

df <- readRDS("Data_d_iv_iowa")
attach(df)

Y <- df[,3]

const <- rep(1,N*T)
X1 <- as.matrix(cbind(const,df[,4:ncol(df)],NW,NC,NE,WC,C,EC,SC,SE))

W <- read.delim("C:/Users/Regan/Google Drive/Thesis Presentations and Proposal/Thesis/Code/Iowa/W_IA_norm.txt", header=F)
W <- as.matrix(W)

K <- kronecker(diag(T),W)

eig <- eigen(W, symmetric = FALSE, only.values = TRUE)
lambda <- eig$values

rhomin <- (1/min(lambda))
rhomax <- (1/max(lambda))

p_bound <- seq(rhomin,rhomax,.1)

SSE <- function(x)
{k <- x[1] + x[2]*df[,4]

pcp_pol_sp <- (pcp_pol_i-k)*(pcp_pol_i>k)

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

mod <- lm(Ynew~0+Xnew)
Beta <- summary(mod)$coeff[,1]
e <- Ynew-Xnew%*%Beta
SSE <- sum(e^2)
return(SSE)}

mean <- mean(pcp_pol_i)
min <- summary(pcp_pol_i)[1]+1
Q1 <- summary(pcp_pol_i)[2]
Q2 <- summary(pcp_pol_i)[3]
Q3 <- summary(pcp_pol_i)[5]
max <- summary(pcp_pol_i)[6]

a <- seq((min),300,5)

SSEmain <- matrix(0,17,length(a))
for(j in 43:length(a))
{print(j)
  
  b <- seq(max((min-a[j]),((min-a[j])/58)),min((max-a[j]),((max-a[j])/58)),.4)
  
  for (i in 1:length(b))
  { print(i)
    x <- c(a[j],b[i])
    SSEmain[i,j]<-SSE(x)}}


#min (SSEmain)=row-6, column-17#

a_opt <- a[17]

 b <- seq(max((min-a_opt),((min-a_opt)/58)),min((max-a_opt),((max-a_opt)/58)),.4)

b_opt <- b[6]  

#> a_opt
#[1] 81.416
#> b_opt
#[1] 0.6206897

a <- seq((a_opt-5),(a_opt+5),1)

SSEmain <- matrix(0,68,length(a))
for(j in 1:length(a))
{print(j)
  
  b <- seq(max((min-a[j]),((min-a[j])/58)),min((max-a[j]),((max-a[j])/58)),.1)
  
  for (i in 1:length(b))
  { print(i)
    x <- c(a[j],b[i])
    SSEmain[i,j]<-SSE(x)}}
ind = which(SSEmain == min(SSEmain), arr.ind = TRUE)

a_opt = a[ind[1]]

b <- seq(max((min-a_opt),((min-a_opt)/58)),min((max-a_opt),((max-a_opt)/58)),.1)

b_opt <- b[ind[2]]

#a_opt <- a[11]

#b_opt <- b[20]

#> a_opt
#[1] 86.416
#> b_opt
#[1] 0.4344828


