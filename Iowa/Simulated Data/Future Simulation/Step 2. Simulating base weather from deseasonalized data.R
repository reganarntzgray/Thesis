
setwd("C:/Users/Regan/Google Drive/Thesis Presentations and Proposal/Thesis/Data needed to complete empirical work/simulated weather/Iowa")

N <- 99
T <- 13

weather_deseasonalized <- read.table("weather_base_deseasonalized_Iowa")

W <- weather_deseasonalized[,3:ncol(weather_deseasonalized)]

#k-NN sampling to generate weather#

sigma <- cov(W)
sigma_inv <- solve(sigma)

distance <- function(w1,w2)
{distance<-t(w1-w2)%*%sigma_inv%*%(w1-w2)
return(sqrt(distance))}

#choosing the starting vector#
#k small due to very large d - meant to be prop. to n^4/(d+4)=1.117871 - d=297

k <- 10

for(j in 1:50)
{seed <- j*1000
  set.seed(seed)

w1 <- c(rnorm(297))
w1 <- as.vector(w1)

Sample_weather <- matrix(0,365,ncol(W))

Prob_num <- 1/c(1:k)
denom <- sum(Prob_num)
Prob <- Prob_num/denom

for(n in 1:365)
{
  Distance <- rep(0,nrow(W))
  
  for (i in 1:nrow(W))
  {w2 <- t(W[i,])
  Distance[i] <- distance(w1,w2)}
  ord <- c(1:nrow(W))
  Distance1 <- data.frame(ord,Distance)
  Distance_ord <- Distance1[order(Distance),]
  
  Neighbours <- Distance_ord[1:k,1]
  
  successor <- sample(x=Neighbours,1, prob=Prob)+1
  
  w1 <- t(W[successor,]) 
  
  Sample_weather[n,] <- w1
  print(n)}
print(j)
write.table(Sample_weather,paste("weather_base_simulated_year",j,sep=""))
}
##above code will ggenerate one year of sample weather data##
