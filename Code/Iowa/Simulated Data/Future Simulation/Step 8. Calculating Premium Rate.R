setwd("C:/Users/Regan/Google Drive/Thesis Presentations and Proposal/Thesis/Data needed to complete empirical work/simulated weather/Iowa")

Yield <- read.table("Yield_future_sim_Iowa")

Y_dynamic <- Yield[,3]
Y_static <- Yield[,4]

N <- 99
T <- 100

sd_dyn <- NULL
for(i in 1:N)
{county_yield <- Y_dynamic[seq(i,length(Y_dynamic),N)]
sd_dyn[i]<-sd(county_yield)}

sd_stat <- NULL
for(i in 1:N)
{county_yield <- Y_static[seq(i,length(Y_static),N)]
sd_stat[i]<-sd(county_yield)}


SD_yields <- data.frame(unique(Yield$county),sd_stat,sd_dyn)

colnames(SD_yields)<- c("county","sd_stat","sd_dyn")

write.table(SD_yields,"SD_yields_fut_Iowa")

#get non-parametric estimate of yield distribution for each case 


n <- T
Dynamic_Kern_Estimate <- function(y)
{
sigma_y<- sd(Y_county)
n <- length(Y_county)
Q3 <- quantile(Y_county)[4]
Q1 <- quantile(Y_county)[2]
range <- (Q3-Q1)/1.34
A <- min(sigma_y^2,range)
h<- 0.9*A*(n^(-1/5))

prob <-(1/(n*h))*sum(dnorm((y-Y_county)/h))
return(prob)}

#calculate the expected loss given a coverage level and compute the county specific premia levels - compare the dynamic to static results

#APH is the average county yield from dynamic yield estimates#


yield <- Y_static

APH <- NULL
for(i in 1:N)
{ 
  yield_county <- yield[seq(i,length(yield),N)]
  APH[i] <- mean(yield_county)
}

coverage <- 0.9
insured_amount <- coverage*APH

by <- insured_amount/1000

grid <- matrix(0,1000,N)
for(i in 1:N)
{grid[,i] <- seq((0+(by[i]/2)),insured_amount[i],by[i])}

probability <- matrix(0,N,1000)
premium_dyn <- NULL
for (i in 1:N)
{   Y_county <- Y_dynamic[seq(i,length(Y_dynamic),N)]
    int <- NULL
  for(j in 1:1000)
{y <- grid[j,i]
    payout <- (APH[i]-y)
 prob <- Dynamic_Kern_Estimate(y)
 probability[i,j]<-prob
 int[j]<-prob*payout*by[i]
  }
premium_dyn[i]<-(1/i)*sum(int)

print(i)}

premium_stat <- NULL
for (i in 1:N)
{   Y_county <- Y_static[seq(i,length(Y_static),N)]
int <- NULL
for(j in 1:1000)
{y <- grid[j,i]
payout <- (APH[i]-y)
prob <- Dynamic_Kern_Estimate(y)
int[j]<-prob*payout*by[i]
}
premium_stat[i]<-(1/i)*sum(int)

print(i)}

ratio <- premium_stat/premium_dyn

county <- unique(Yield$county)

setwd("C:/Users/Regan/Google Drive/Thesis Presentations and Proposal/Thesis/Data needed to complete empirical work/simulated weather/Iowa")

Premiums_Iowa_future <- data.frame(county,premium_dyn,premium_stat,ratio)

write.table(Premiums_Iowa_future,"Premiums_Iowa_future")

install.packages("stargazer")
library(stargazer)

stargazer(Premiums_Iowa_future, nobs=F,summary.logical = F, flip=TRUE)


t_ratio_mean = t.test(ratio,mu=1,conf.level =.95)


#paired t test#

t_paired = t.test(premium_stat,premium_dyn,paired = TRUE,alternative = "greater", conf.level = .99)

n <- sum((premium_dyn-premium_stat)>0)

