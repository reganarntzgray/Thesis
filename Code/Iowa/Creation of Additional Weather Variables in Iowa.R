setwd("C:/Users/Regan/Google Drive/Thesis Presentations and Proposal/Thesis")

N <- 99
T <- 58

iowa <- readRDS("Data needed to complete empirical work/Daily weather data/Iowa/weather_raw_daily.rds")
df <- iowa[order(iowa$county),]
df <- df[order(df$year),]

df <- df[df$days %in% c(1:65,67:366),]

attach(df)

hdd <- NULL
tot <- NULL
UPP <- 29
LOW <- 10

for (i in 1:nrow(df))
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

fdd <- NULL
FREEZE <- -2
tot_cold <- NULL
COLD <- 2

for(i in 1:nrow(df))
{ifelse(i/20000==floor(i/20000),print(i),0)
  M <- (max[i]+min[i])/2
  W <- (max[i]-min[i])/2

  psi <- ifelse(min[i]<FREEZE,ifelse(max[i]>FREEZE,asin((FREEZE-M)/W),pi/2),0)
  
  fdd[i] <- ifelse(min[i]<FREEZE,ifelse(max[i]>FREEZE,((FREEZE-M)*(psi+(pi/2))+W*cos(psi))/pi,FREEZE-M),0)
  
  theta <- ifelse(min[i]<COLD,ifelse(max[i]>COLD,asin((COLD-M)/W),pi/2),0)
  
  tot_cold[i] <- ifelse(min[i]<COLD,ifelse(max[i]>COLD,((COLD-M)*(theta+pi/2)+W*cos(theta))/pi,COLD-M),0)}


cdd <- tot_cold-fdd

#calculating the gdd according to the agronomic formula in farenheit

max_farenheit <- (max*(9/5))+32
min_farenheit <- (min*(9/5))+32

max_gdd_ag <- (max_farenheit*as.numeric(max_farenheit<86))+(86*(max_farenheit>86))
max_gdd_ag <- (max_gdd_ag*as.numeric(max_gdd_ag>50))+(50*(max_gdd_ag<50))

min_gdd_ag <- (min_farenheit*as.numeric(min_farenheit>50))+(50*(min_farenheit<50))
min_gdd_ag <- (min_gdd_ag*as.numeric(min_gdd_ag<86))+(86*(min_gdd_ag>86))

gdd_ag <- ((max_gdd_ag+min_gdd_ag)/2-50)


#calculating the CHU according to method of OMAFRA

Ymax <- 3.33*(max-10)-0.084*((max-10)^2)
Ymax <- Ymax*(Ymax>0)

Ymin <- 1.8*(min-4.4)
Ymin <- Ymin*(Ymin>0)


CHU <- (Ymax+Ymin)/2

daily_weather_iowa <- data.frame(year,county,days,min,max,gdd,hdd,cdd,fdd,gdd_ag,pcp,vpd,CHU)

saveRDS(daily_weather_iowa,file="Data needed to complete empirical work/Daily weather data/Iowa/daily_weather_iowa")
