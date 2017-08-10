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

X1 <- X

A <- (diag(N*T)-p_opt*K)
Ynew <- A%*%Y




SSE <- function(x)
{kl <- x[1] + x[3]*df[,4]
kh <- x[2] + x[4]*df[,4]

pcp_july_sp_low <- (mean_pcp_july-kl)*(mean_pcp_july>kl)
pcp_july_sp_high <- (mean_pcp_july-kh)*(mean_pcp_july>kh)
X <- cbind(X1,pcp_july_sp_low,pcp_july_sp_high)

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

al <- seq((min),Q2,1)
ah <-seq(Q3,160,1)

SSEmain <- matrix(0,length(al),length(ah))
for(j in 1:length(ah))
{print(j)
  
  bh <- seq(max((Q3-ah[j]),((Q3-ah[j])/T)),min((max-ah[j]),((max-ah[j])/T)),.1)
  
  for(i in 1:length(al))
  {print(i)
    
    bl <- seq(max((min-al[i]),((min-al[i])/T)),min((Q3-al[i]),((Q3-al[i])/T)),.1)
    
    SSEmat <- matrix(0,length(bl),length(bh))
    for(k in 1:length(bl))
    {for(l in 1:length(bh))
    {x <- c(al[i],ah[j],bl[k],bh[l])
    SSEmat[k,l] <- SSE(x)}}
    SSEmain[i,j]<-min(SSEmat)}}

ind = which(SSEmain == min(SSEmain), arr.ind = TRUE)

a_l <- al[ind[1]]
a_h <- ah[ind[2]]

#a_l <- al[3]
#a_h <- ah[33]

bh <- seq(max((Q3-a_h),((Q3-a_h)/T)),min((max-a_h),((max-a_h)/T)),.1)
bl <- seq(max((min-a_l),((min-a_l)/T)),min((Q3-a_l),((Q3-a_l)/T)),.1)
  
  SSEmat <- matrix(0,length(bl),length(bh))
  for(k in 1:length(bl))
  {for(l in 1:length(bh))
  {x <- c(a_l,a_h,bl[k],bh[l])
  SSEmat[k,l] <- SSE(x)}}
  ind = which(SSEmat == min(SSEmat), arr.ind = TRUE)
  
  b_l <- bl[ind[1]]
  b_h <- bh[ind[2]]
  
#  b_l <- bl[14]
#  b_h <- bh[12]
  
#  > a_l
#  [1] 9.06
#  > a_h
#  [1] 132.2
#  > b_l
#  [1] 1.26875
#  > b_h
#  [1] 0.6