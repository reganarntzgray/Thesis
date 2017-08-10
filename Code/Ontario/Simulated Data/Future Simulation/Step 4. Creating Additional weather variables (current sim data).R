setwd("C:/Users/Regan/Google Drive/Thesis Presentations and Proposal/Thesis/Data needed to complete empirical work/simulated weather/Ontario")

weather <- read.table("future_sim_stacked_daily_weather")
attach(weather)

N <- 38
T <- 100

#creating daily gdd and hdd 

hdd <- NULL
tot <- NULL
UPP <- 29
LOW <- 10

for (i in 1:nrow(weather))
  {
  ifelse(i/20000==floor(i/20000),print(i),0)
  
  M <- (max[i]+min[i])/2
  W <- (max[i]-min[i])/2

  psi <- ifelse(max[i]>UPP,ifelse(min[i]<UPP,asin((UPP-M)/W),(-pi/2)),0)

  hdd[i] <- ifelse(max[i]>UPP,ifelse(min[i]<UPP,((M-UPP)*((pi/2)-psi)+W*cos(psi))/pi,M-UPP),0 )

  theta <- ifelse(max[i]>LOW,ifelse(min[i]<LOW,asin((LOW-M)/W),(-pi/2)),0)

  tot[i] <- ifelse(max[i]>LOW,ifelse(min[i]<LOW,((M-LOW)*((pi/2)-theta)+W*cos(theta))/pi,M-LOW),0)
}

gdd <- tot-hdd


#creating daily vpd

#(0.6107 * exp(17.269*max[1]/ (237.3 + max[1])))-(0.6107 * exp(17.269*min[1]/ (237.3 + min[1]))) calc for vpd#


vpd <- (0.6107 * exp(17.269*max/ (237.3 + max)))-(0.6107 * exp(17.269*min/ (237.3 + min))) 


#calculating the gdd according to the agronomic formula in farenheit

max_farenheit <- (max*(9/5))+32
min_farenheit <- (min*(9/5))+32

max_gdd_ag <- (max_farenheit*as.numeric(max_farenheit<86))+(86*(max_farenheit>86))
max_gdd_ag <- (max_gdd_ag*as.numeric(max_gdd_ag>50))+(50*(max_gdd_ag<50))

min_gdd_ag <- (min_farenheit*as.numeric(min_farenheit>50))+(50*(min_farenheit<50))
min_gdd_ag <- (min_gdd_ag*as.numeric(min_gdd_ag<86))+(86*(min_gdd_ag>86))

gdd_ag <- ((max_gdd_ag+min_gdd_ag)/2-50)

daily_future_sim_weather_full <- data.frame(weather,vpd,gdd,hdd,gdd_ag)


write.table(daily_future_sim_weather_full,"daily_future_sim_weather_full")






