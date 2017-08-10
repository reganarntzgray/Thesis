setwd("C:/Users/Regan/Google Drive/Thesis Presentations and Proposal/Thesis/Data needed to complete empirical work/simulated weather/Ontario")

weather <- read.table("daily_current_sim_weather_full")

#planting and silking dates#

Mar1 <-  seq(60,nrow(weather),365)
June1 <- seq(152,nrow(weather),365)
plant <- NULL
for(i in 1:length(Mar1))
{for(j in Mar1[i]:June1[i])
{if(((weather$max[j]+weather$min[j])/2)>=12.8 && ((weather$max[j+1]+weather$min[j+1])/2)>=12.8  && ((weather$max[j+2]+weather$min[j+2])/2)>=12.8)
{plant[i]<- (j+3)
break}
else{plant[i] <- (June1[i]+3)}}}

adjust <- seq(0,nrow(weather)-1,365)

silk <- NULL

accum_gdd_ag <- 0
Aug30 <- seq(242,nrow(weather),365)
for(i in 1:length(plant))
{accum_gdd_ag <- 0
for(j in plant[i]:Aug30[i])
{accum_gdd_ag<- weather$gdd_ag[j]+accum_gdd_ag
if(accum_gdd_ag>=1400)
{silk[i] <- j
break}
else{silk[i] <- Aug30[i]}}}

county <- unique(weather$county)

N=38
T=100

county <- rep(county,T)

year <- c(1:100)
year <- rep(year,each=N)

Plant_silk <- data.frame(county,year,plant,silk)

write.table(Plant_silk, "current_sim_planting_silking")
