setwd("C:/Users/Regan/Google Drive/Thesis Presentations and Proposal/Thesis/Data needed to complete empirical work/Data sets for model testing/Ontario")

N <- 8
T <- 64

Dummy <- diag(N)
Dummy <- matrix( rep( t( Dummy) , T ) , ncol = ncol(Dummy) , byrow = TRUE )


df1 <- readRDS("Data_d_iii_ont")
attach(df1)

WS <- c("huron","middlesex","ottawa","peel","perth","prescott-russell","stormont-dundas-glengarry","york")

df <- df1[county %in% WS,]
attach(df)

Y <- df[,3]

const <- rep(1,N*T)
X <- as.matrix(cbind(const,df[,4:ncol(df)],Dummy[,1:7]))

W <- read.delim("C:/Users/Regan/Google Drive/Thesis Presentations and Proposal/Thesis/Code/Ontario/W_weather_station.txt",header=F)
W <- as.matrix(W)

K <- kronecker(diag(T),W)

eig <- eigen(W, symmetric = FALSE, only.values = TRUE)
lambda <- eig$values

rhomin <- (1/min(lambda))
rhomax <- (1/max(lambda))

p_bound <- seq(rhomin,rhomax,.1)


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

p_opt <- optim(mean(p_bound),(log_L),method="Brent",lower=rhomin, upper=rhomax)$par

A <- (diag(N*T)-p_opt*K)
Ynew <- A%*%Y
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

SSEmain <- matrix(0,28,length(a))
for(j in 1:length(a))
{print(j)
  
  b <- seq(max((min-a[j]),((min-a[j])/T)),min((max-a[j]),((max-a[j])/T)),.1)
 
   for(i in 1:length(b))
    {print(i)
     x <- c(a[j],b[i])
   
    SSEmain[i,j]<-SSE(x)}}

ind = which(SSEmain == min(SSEmain), arr.ind = TRUE)

a_opt <- a[ind[1]]

b <- seq(max((min-a_opt),((min-a_opt)/T)),min((max-a_opt),((max-a_opt)/T)),.1)

b_opt <- b[ind[2]]

#row (i)5, column(j)8#

#a_opt <- a[4]

#b_opt <- b[14]

#> a_opt
#[1] 7.06
#> b_opt
#[1] 1.3