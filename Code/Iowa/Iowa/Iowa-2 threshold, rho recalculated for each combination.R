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
{kl <- x[1] + x[3]*df[,4]
kh <- x[2] + x[4]*df[,4]

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

al <- seq((min),Q2,10)
ah <-seq(Q3,300,30)

SSEmain <- matrix(0,length(al),length(ah))
for(j in 1:length(ah))
{print(j)
  
  bh <- seq(max((Q3-ah[j]),((Q3-ah[j])/58)),min((max-ah[j]),((max-ah[j])/58)),.6)
 
  for(i in 1:length(al))
  {print(i)
    
    bl <- seq(max((min-al[i]),((min-al[i])/58)),min((Q3-al[i]),((Q3-al[i])/58)),.3)
    
    SSEmat <- matrix(0,length(bl),length(bh))
    for(k in 1:length(bl))
    {print(k)
      for(l in 1:length(bh))
    {print(l)
        x <- c(al[i],ah[j],bl[k],bh[l])
    SSEmat[k,l] <- SSE(x)}}
    SSEmain[i,j]<-min(SSEmat)}}

ind = which(SSEmain == min(SSEmain), arr.ind = TRUE)

a_l <- al[ind[1]]
a_h <- ah[ind[2]]


#a_l <- al[8]
#a_h <- ah[7]

#> a_l
#[1] 71.416
#> a_h
#[1] 298.1

al <-  seq((a_l-10),(a_l+10),5)
ah <- seq((a_h-30),(a_h+30),12)

SSEmain <- matrix(0,length(al),length(ah))
for(j in 1:length(ah))
{print(j)
  
  bh <- seq(max((Q3-ah[j]),((Q3-ah[j])/58)),min((max-ah[j]),((max-ah[j])/58)),.4)
  
  for(i in 1:length(al))
  {print(i)
    
    bl <- seq(max((min-al[i]),((min-al[i])/58)),min((Q3-al[i]),((Q3-al[i])/58)),.2)
    
    SSEmat <- matrix(0,length(bl),length(bh))
    for(k in 1:length(bl))
    {for(l in 1:length(bh))
    {x <- c(al[i],ah[j],bl[k],bh[l])
    SSEmat[k,l] <- SSE(x)}}
    SSEmain[i,j]<-min(SSEmat)}}

ind = which(SSEmain == min(SSEmain), arr.ind = TRUE)

a_l <- al[ind[1]]
a_h <- ah[ind[2]]

#> which(SSEmain == min(SSEmain), arr.ind = TRUE)
#row col
#[1,]   1   6

#a_l <- al[1]
#a_h <- ah[6]

al <-  seq((a_l-5),(a_l+5),2)
ah <- seq((a_h-12),(a_h+12),4)

SSEmain <- matrix(0,length(al),length(ah))
for(j in 1:length(ah))
{print(j)

  
  bh <- seq(max((Q3-ah[j]),((Q3-ah[j])/58)),min((max-ah[j]),((max-ah[j])/58)),.4)
  
  for(i in 1:length(al))
  {print(i)
    
    bl <- seq(max((min-al[i]),((min-al[i])/58)),min((Q3-al[i]),((Q3-al[i])/58)),.2)
    
    SSEmat <- matrix(0,length(bl),length(bh))
    for(k in 1:length(bl))
    {print(k)
      for(l in 1:length(bh))
    {x <- c(al[i],ah[j],bl[k],bh[l])
    SSEmat[k,l] <- SSE(x)}}
    SSEmain[i,j]<-min(SSEmat)
    write.table(SSEmain,file = "SSEmain_Iowa_2knot_recalc")}}

ind = which(SSEmain == min(SSEmain), arr.ind = TRUE)

a_l <- al[ind[1]]
a_h <- ah[ind[2]]

#> which(SSEmain == min(SSEmain), arr.ind = TRUE)
#row col
#[1,]   3   7

#a_l <- al[3]
#a_h <- ah[7]


al <-  seq((a_l-1),(a_l+1),1)
ah <- seq((a_h-3),(a_h+3),2)

SSEmain <- matrix(0,length(al),length(ah))
for(j in 1:length(ah))
{print(j)
  
  
  bh <- seq(max((Q3-ah[j]),((Q3-ah[j])/58)),min((max-ah[j]),((max-ah[j])/58)),.2)
  
  for(i in 1:length(al))
  {print(i)
    
    bl <- seq(max((min-al[i]),((min-al[i])/58)),min((Q3-al[i]),((Q3-al[i])/58)),.2)
    
    SSEmat <- matrix(0,length(bl),length(bh))
    for(k in 1:length(bl))
    {print(k)
      for(l in 1:length(bh))
      {x <- c(al[i],ah[j],bl[k],bh[l])
      SSEmat[k,l] <- SSE(x)}}
    SSEmain[i,j]<-min(SSEmat)
    write.table(SSEmain,file = "SSEmain_Iowa_2knot_recalc2")}}

ind = which(SSEmain == min(SSEmain), arr.ind = TRUE)

a_l <- al[ind[1]]
a_h <- ah[ind[2]]

#a_l <- al[2]
#a_h <- ah[4]


bl <- seq(max((min-a_l),((min-a_l)/58)),min((Q3-a_l),((Q3-a_l)/58)),.1)

ah <- seq((a_h-2),(a_h+2),1)

SSEmain <- NULL
for(j in 5:length(ah))
{print(j)
  
  
  bh <- seq(max((Q3-ah[j]),((Q3-ah[j])/58)),min((max-ah[j]),((max-ah[j])/58)),.1)

    
    SSEmat <- matrix(0,length(bl),length(bh))
    for(k in 1:length(bl))
    {print(k)
      for(l in 1:length(bh))
      {print(l)
        x <- c(a_l,ah[j],bl[k],bh[l])
      SSEmat[k,l] <- SSE(x)}}
    SSEmain[j]<-min(SSEmat)
    write.table(SSEmain,file = "SSEmain_Iowa_2knot_recalc3")}

ind = which(SSEmain == min(SSEmain), arr.ind = TRUE)

a_h <- ah[ind]

#> SSEmain
#[1] 247246.6 247244.5 247242.5 247240.8  247239.3

#a_h <- ah[5]

bl <- seq(max((min-a_l),((min-a_l)/58)),min((Q3-a_l),((Q3-a_l)/58)),.1)

bh <- seq(max((Q3-a_h),((Q3-a_h)/58)),min((max-a_h),((max-a_h)/58)),.1)

    
    SSEmat <- matrix(0,length(bl),length(bh))
    for(k in 1:length(bl))
    {print(k)
      for(l in 1:length(bh))
      {print(l)
        x <- c(a_l,ah[j],bl[k],bh[l])
      SSEmat[k,l] <- SSE(x)}}
  
ind = which(SSEmain == min(SSEmain), arr.ind = TRUE)
  
b_l <- bl[ind[1]]
b_h <- bh[ind[2]]
  
  
#b_l <- bl[21]
#b_h <- bh[3]

#> a_l
#[1] 60.416
#> a_h
#[1] 345.1
#> b_l
#[1] 0.9827586
#> b_h
#[1] -3.713793
